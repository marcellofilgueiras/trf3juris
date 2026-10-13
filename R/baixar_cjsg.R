POR_PAGINA <- 50L

#' Baixa resultados da pesquisa de jurisprudencia do TRF3
#'
#' Pesquisa em <https://web.trf3.jus.br/jurisprudencia/> e grava cada pagina
#' de resultados (50 decisoes) como HTML em `diretorio`. Os arquivos depois
#' sao lidos por [trf3_ler_cjsg()].
#'
#' O periodo e fatiado em janelas (`por`), cada uma com sua propria
#' pesquisa, o que mantem as sessoes curtas. Os nomes de arquivo sao
#' deterministicos: rodar de novo a mesma chamada pula o que ja foi baixado
#' e retoma do ponto em que parou.
#'
#' @param livre Pesquisa livre. Aceita os operadores do site: `E`, `OU`,
#'   `NAO`, `ADJn`, `PROXn`, `MESMO`, `COM`, `$` e "aspas".
#' @param base `"acordaos"` (TRF3), `"monocraticas"` (decisoes monocraticas
#'   do TRF3 e das turmas recursais) ou `"recursais"` (acordaos das turmas
#'   recursais/JEF e TRU).
#' @param numero Numero do processo.
#' @param relator,classe,orgao Filtros. Aceitam o id ou o texto (ou trecho
#'   unico do texto, sem acento) de uma opcao de [trf3_opcoes()]. O filtro
#'   de relator do site tambem traz decisoes em que o magistrado foi
#'   relator para o acordao, entao a coluna `relator` pode trazer outros
#'   nomes.
#' @param data_inicial,data_final Datas `"dd/mm/aaaa"`.
#' @param tipo_data `"julgamento"` (data da decisao) ou `"publicacao"`.
#' @param ementa,indexacao,legislacao Pesquisa nesses campos especificos.
#' @param por Tamanho das janelas de data: `"mes"`, `"semana"`, `"dia"`,
#'   `"ano"` ou `"nenhum"`.
#' @param n Numero maximo de paginas por janela (`NULL` = todas).
#' @param motor Ver [trf3_sessao()].
#' @param sono Pausa media, em segundos, entre paginas.
#' @param diretorio Pasta onde gravar os arquivos.
#'
#' @return Invisivel: vetor com os caminhos dos arquivos.
#' @export
#' @examples
#' \dontrun{
#' trf3_baixar_cjsg(
#'   base = "monocraticas",
#'   data_inicial = "01/09/2026", data_final = "05/09/2026",
#'   diretorio = "dados/mono"
#' )
#' trf3_baixar_cjsg("aposentadoria ADJ especial", relator = "nabarrete",
#'                  data_inicial = "01/01/2026", data_final = "30/06/2026")
#' }
trf3_baixar_cjsg <- function(livre = "",
                             base = c("acordaos", "monocraticas", "recursais"),
                             numero = "", relator = "", classe = "", orgao = "",
                             data_inicial = "", data_final = "",
                             tipo_data = c("julgamento", "publicacao"),
                             ementa = "", indexacao = "", legislacao = "",
                             por = c("mes", "semana", "dia", "ano", "nenhum"),
                             n = NULL, motor = c("auto", "httr2", "chromote"),
                             sono = 1, diretorio = ".") {
  base <- match.arg(base)
  tipo_data <- match.arg(tipo_data)
  por <- match.arg(por)
  motor <- match.arg(motor)

  inicio <- validar_data(data_inicial, "data_inicial")
  fim <- validar_data(data_final, "data_final")
  if (xor(is.na(inicio), is.na(fim))) {
    cli::cli_abort("Informe {.arg data_inicial} e {.arg data_final} juntas.")
  }
  textos <- c(livre, numero, relator, classe, orgao, ementa, indexacao, legislacao)
  if (all(textos == "") && is.na(inicio)) {
    cli::cli_abort("Informe ao menos um crit\u00e9rio de pesquisa ou um per\u00edodo.")
  }

  sessao <- trf3_sessao(motor)
  on.exit(sessao$fechar(), add = TRUE)

  filtros <- list(relator = NULL, classe = NULL, orgao = NULL)
  if (any(c(relator, classe, orgao) != "")) {
    opcoes <- ler_opcoes(sessao$abrir(caminho_base(base))$texto)
    filtros$relator <- resolver_filtro(relator, "magistrado", opcoes)
    filtros$classe <- resolver_filtro(classe, "classe", opcoes)
    filtros$orgao <- resolver_filtro(orgao, "orgao", opcoes)
  }

  chave <- hash_curto(livre, numero, filtros$relator$id, filtros$classe$id,
                      filtros$orgao$id, ementa, indexacao, legislacao)
  janelas <- agrupar_datas(inicio, fim, por)

  arquivos <- purrr::map(seq_len(nrow(janelas)), function(i) {
    ini <- janelas$inicio[i]
    fi <- janelas$fim[i]
    periodo <- if (is.na(ini)) "sem-data" else
      paste(format(ini, "%Y%m%d"), format(fi, "%Y%m%d"), substr(tipo_data, 1, 1), sep = "_")
    prefixo <- file.path(diretorio, paste("trf3", base, periodo, chave, sep = "_"))
    campos <- montar_campos(base, livre, numero, ini, fi, tipo_data,
                            filtros$relator, filtros$classe, filtros$orgao,
                            ementa, indexacao, legislacao)
    if (!is.na(ini)) cli::cli_h3("{base}: {formatar_data(ini)} a {formatar_data(fi)}")
    baixar_janela(sessao, base, campos, prefixo, n, sono)
  })

  invisible(unlist(arquivos))
}

baixar_janela <- function(sessao, base, campos, prefixo, n, sono) {
  arquivo <- function(p) sprintf("%s_p%04d.html", prefixo, p)

  pesquisar <- function() {
    com_tentativas(function() {
      sessao$abrir(caminho_base(base))
      sessao$post_form("/jurisprudencia/Home/ResultadoTotais", campos)
    }, rotulo = "pesquisa")
  }

  # Retomada: com a 1a pagina em disco, o total e conhecido sem ir ao site
  pesquisa_ativa <- FALSE
  if (file.exists(arquivo(1))) {
    total <- ler_total(ler_texto(arquivo(1)))
  } else {
    r <- pesquisar()
    pesquisa_ativa <- TRUE
    total <- ler_total(r$texto)
    if (total == 0) {
      cli::cli_alert_info("Nenhum resultado.")
      return(character())
    }
    gravar(r$texto, arquivo(1))
  }

  paginas <- ceiling(total / POR_PAGINA)
  if (!is.null(n)) paginas <- min(paginas, n)
  faltam <- Filter(function(p) !file.exists(arquivo(p)), seq_len(paginas))
  cli::cli_alert_info(
    "{total} resultado{?s}, {paginas} p\u00e1gina{?s} ({length(faltam)} a baixar)."
  )

  if (length(faltam) > 0) {
    if (!pesquisa_ativa) {
      r <- pesquisar()
      novo_total <- ler_total(r$texto)
      if (novo_total != total) {
        cli::cli_alert_warning(
          "O total mudou de {total} para {novo_total} desde o download anterior; apague os arquivos da janela para rebaixar do zero."
        )
      }
    }
    cli::cli_progress_bar("Baixando p\u00e1ginas", total = length(faltam))
    for (p in faltam) {
      r <- obter_pagina(sessao, p, pesquisar)
      gravar(r$texto, arquivo(p))
      cli::cli_progress_update()
      dormir(sono)
    }
    cli::cli_progress_done()
  }

  arquivo(seq_len(paginas))
}

#' Busca a pagina `p` da pesquisa ativa e confere se ela comeca no indice
#' esperado; se a sessao expirou ou desalinhou, refaz a pesquisa
#' @noRd
obter_pagina <- function(sessao, p, pesquisar, tentativas = 3) {
  esperado <- (p - 1L) * POR_PAGINA + 1L
  for (i in seq_len(tentativas)) {
    r <- tryCatch(
      sessao$get(sprintf("/jurisprudencia/Home/ListaResumida/2?np=%d", p)),
      error = function(e) e
    )
    estado <- if (inherits(r, "error")) conditionMessage(r) else estado_resposta(r)
    if (estado == "ok") {
      primeiro <- primeiro_indice(r$texto)
      if (identical(primeiro, esperado)) return(r)
      estado <- sprintf("p\u00e1gina come\u00e7a em %s, esperado %s", primeiro, esperado)
    }
    if (i < tentativas) {
      cli::cli_alert_warning("P\u00e1gina {p}: {estado}. Refazendo a pesquisa.")
      Sys.sleep(2^i)
      pesquisar()
    }
  }
  cli::cli_abort("P\u00e1gina {p}: falhou ap\u00f3s {tentativas} tentativas ({estado}).")
}

ler_texto <- function(arquivo) {
  paste(readLines(arquivo, encoding = "UTF-8", warn = FALSE), collapse = "\n")
}

ler_total <- function(texto) {
  total <- stringr::str_match(texto, "class=\"qtd-total\">\\s*(\\d+)")[, 2]
  if (is.na(total)) {
    if (grepl("acordao-retorno", texto, fixed = TRUE)) {
      cli::cli_abort("N\u00e3o achei o total de resultados na p\u00e1gina; o layout do site pode ter mudado.")
    }
    return(0L)
  }
  as.integer(total)
}

primeiro_indice <- function(texto) {
  as.integer(stringr::str_match(texto, "class=\"contagem\">\\s*(\\d+)/")[, 2])
}
