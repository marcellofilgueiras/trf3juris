BASES <- c(acordaos = 0L, recursais = 1L, monocraticas = 2L)

caminho_base <- function(base) sprintf("/jurisprudencia/home/index/%d", BASES[[base]])

#' Lista as opcoes de filtro (relator, classe, orgao) de uma base
#'
#' Os filtros do formulario do TRF3 usam ids numericos que mudam conforme a
#' base e ao longo do tempo. Esta funcao raspa as listas atuais para voce
#' escolher o texto a passar em [trf3_baixar_cjsg()].
#'
#' @param base `"acordaos"`, `"monocraticas"` ou `"recursais"`.
#' @param motor Ver [trf3_sessao()].
#' @param sessao Sessao aberta com [trf3_sessao()] (opcional).
#'
#' @return tibble com `campo` (magistrado, classe, orgao), `id`, `texto` e
#'   `texto_jef`.
#' @export
trf3_opcoes <- function(base = c("acordaos", "monocraticas", "recursais"),
                        motor = c("auto", "httr2", "chromote"), sessao = NULL) {
  base <- match.arg(base)
  if (is.null(sessao)) {
    sessao <- trf3_sessao(match.arg(motor))
    on.exit(sessao$fechar(), add = TRUE)
  }
  ler_opcoes(sessao$abrir(caminho_base(base))$texto)
}

ler_opcoes <- function(html) {
  doc <- xml2::read_html(html)
  selects <- xml2::xml_find_all(
    doc,
    "//form[@id='frm-pesquisa-avancada']//select[@name='magistrado' or @name='classe' or @name='orgao']"
  )
  purrr::map(selects, function(s) {
    op <- xml2::xml_find_all(s, ".//option")
    tibble::tibble(
      campo = xml2::xml_attr(s, "name"),
      id = xml2::xml_attr(op, "value"),
      texto = stringr::str_squish(xml2::xml_text(op)),
      texto_jef = xml2::xml_attr(op, "data-val-jef")
    )
  }) |>
    dplyr::bind_rows() |>
    dplyr::filter(.data$id != "0")
}

#' Acha a opcao correspondente a `valor` (id, texto exato ou trecho)
#' @noRd
resolver_filtro <- function(valor, campo, opcoes) {
  if (is.null(valor) || identical(valor, "")) return(NULL)
  op <- opcoes[opcoes$campo == campo, ]
  if (nrow(op) == 0) cli::cli_abort("Esta base n\u00e3o tem filtro por {.field {campo}}.")
  valor <- as.character(valor)
  if (grepl("^\\d+$", valor)) {
    achou <- op[op$id == valor, ]
  } else {
    alvo <- sem_acento(valor)
    textos <- sem_acento(op$texto)
    achou <- op[textos == alvo, ]
    if (nrow(achou) == 0) achou <- op[stringr::str_detect(textos, stringr::fixed(alvo)), ]
  }
  if (nrow(achou) == 0) {
    cli::cli_abort(c(
      "{.field {campo}} {.val {valor}} n\u00e3o encontrado.",
      i = "Veja as op\u00e7\u00f5es v\u00e1lidas com {.fn trf3_opcoes}."
    ))
  }
  # A lista do site tem textos repetidos (ex.: "5a Turma" com dois ids); como
  # o criterio enviado e o texto, sao equivalentes
  if (nrow(achou) > 1 && dplyr::n_distinct(achou$texto) == 1) achou <- achou[1, ]
  if (nrow(achou) > 1) {
    cli::cli_abort(c(
      "{.field {campo}} {.val {valor}} \u00e9 amb\u00edguo. Candidatos:",
      stats::setNames(utils::head(achou$texto, 10), rep("*", min(10, nrow(achou))))
    ))
  }
  achou
}

# O JS do site troca "1a. secao" por "1A" antes de montar o criterio de orgao
ajustar_orgao <- function(x) gsub("1a\\.\\s+se[c\u00e7][a\u00e3]o", "1A", x, ignore.case = TRUE)

#' Monta o corpo do POST para /Home/ResultadoTotais, replicando o que o
#' JavaScript `preencherHDDN()` do site faz antes de submeter
#' @noRd
montar_campos <- function(base, livre, numero, inicio, fim, tipo_data,
                          relator, classe, orgao, ementa, indexacao, legislacao) {
  campos <- list(
    chkAcordaos = "on",
    txtPesquisaLivre = livre,
    chkMostrarLista = "off",
    opcaoQtdePagina = as.character(POR_PAGINA),
    numero = numero,
    magistrado = relator$id %||% "0",
    data_inicial = if (is.na(inicio)) "" else formatar_data(inicio),
    data_final = if (is.na(fim)) "" else formatar_data(fim),
    data_tipo = if (tipo_data == "publicacao") "0" else "1",
    classe = classe$id %||% "0",
    numclasse = "",
    orgao = orgao$id %||% "0",
    ementa = ementa,
    indexacao = indexacao,
    legislacao = legislacao,
    hdnMagistrado = if (is.null(relator)) "" else sprintf("((%s)).rel.", relator$texto),
    hdnClasse = if (is.null(classe)) "" else sprintf("(%s).dclas.", classe$texto),
    hdnOrgao = if (is.null(orgao)) "" else sprintf("(%s).org.", ajustar_orgao(orgao$texto)),
    hdnOrgaoJef = if (is.null(orgao) || is.na(orgao$texto_jef)) "" else
      sprintf("(%s).org.", ajustar_orgao(orgao$texto_jef)),
    hdnLegislacao = ""
  )
  if (base == "monocraticas") {
    campos$in_juizado_trf3 <- "on"
    campos$in_juizado_recursal <- "on"
  }
  campos
}
