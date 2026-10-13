DATAJUD_URL <- "https://api-publica.datajud.cnj.jus.br/api_publica_trf3/_search"

# Chave publica divulgada em https://datajud-wiki.cnj.jus.br/api-publica/acesso
# (conferida em 13/10/2026). O CNJ pode troca-la a qualquer momento: nesse
# caso, defina a variavel de ambiente DATAJUD_KEY.
DATAJUD_CHAVE_PUBLICA <- "cDZHYzlZa0JadVREZDJCendQbXY6SkJlTzNjLV9TRENyQk1RdnFKZGRQdw=="

datajud_chave <- function() {
  k <- Sys.getenv("DATAJUD_KEY")
  if (nzchar(k)) k else DATAJUD_CHAVE_PUBLICA
}

#' Baixa metadados de processos do TRF3 na API publica do DataJud (CNJ)
#'
#' Consulta o indice `api_publica_trf3` e grava um JSON por processo. Um
#' mesmo numero pode trazer mais de um registro (1o grau, 2o grau, turma
#' recursal). A API do CNJ nao fica atras do Akamai, entao usa `httr2`
#' direto.
#'
#' @param processos Numeros CNJ, com ou sem pontuacao.
#' @param api_key Chave da API. Padrao: variavel de ambiente `DATAJUD_KEY`
#'   ou, na falta dela, a chave publica vigente.
#' @param sono Pausa, em segundos, entre requisicoes.
#' @param diretorio Pasta onde gravar `datajud_<cnj>.json`.
#'
#' @return Invisivel: vetor com os caminhos dos arquivos.
#' @export
#' @examples
#' \dontrun{
#' trf3_baixar_datajud("5014445-85.2023.4.03.6183", diretorio = "dados/datajud")
#' }
trf3_baixar_datajud <- function(processos, api_key = datajud_chave(), sono = 0.5,
                                diretorio = ".") {
  cnjs <- unique(stats::na.omit(cnj_limpar(processos)))
  if (length(cnjs) == 0) cli::cli_abort("Nenhum n\u00famero de processo v\u00e1lido.")
  dir.create(diretorio, showWarnings = FALSE, recursive = TRUE)

  req <- httr2::request(DATAJUD_URL) |>
    httr2::req_headers(Authorization = paste("APIKey", api_key)) |>
    httr2::req_user_agent("trf3juris (https://github.com/marcellofilgueiras)") |>
    httr2::req_timeout(60) |>
    httr2::req_retry(max_tries = 4, retry_on_failure = TRUE)

  arquivos <- file.path(diretorio, sprintf("datajud_%s.json", cnjs))
  faltam <- which(!file.exists(arquivos))
  if (length(faltam) < length(cnjs)) {
    cli::cli_alert_info("{length(cnjs) - length(faltam)} processo{?s} j\u00e1 baixado{?s}.")
  }

  purrr::walk(faltam, function(i) {
    corpo <- list(query = list(match = list(numeroProcesso = cnjs[i])), size = 10)
    resp <- tryCatch(
      httr2::req_perform(httr2::req_body_json(req, corpo)),
      httr2_http_401 = function(e) {
        cli::cli_abort(c(
          "DataJud recusou a chave (401).",
          i = "Confira a chave vigente em {.url https://datajud-wiki.cnj.jus.br/api-publica/acesso} e defina {.envvar DATAJUD_KEY}."
        ), parent = e)
      },
      error = function(e) {
        cli::cli_alert_warning("{cnjs[i]}: {conditionMessage(e)}")
        NULL
      }
    )
    if (!is.null(resp)) gravar(httr2::resp_body_string(resp, encoding = "UTF-8"), arquivos[i])
    if (sono > 0) Sys.sleep(sono)
  }, .progress = length(faltam) > 10)

  invisible(arquivos[file.exists(arquivos)])
}

ler_hits_datajud <- function(arquivo) {
  x <- jsonlite::fromJSON(arquivo, simplifyVector = FALSE)
  purrr::map(x$hits$hits, "_source")
}

listar_datajud <- function(arquivos, diretorio) {
  listar_arquivos(arquivos, diretorio, "^datajud_\\d{20}\\.json$")
}

nulo_na <- function(x, tipo = NA_character_) if (is.null(x)) tipo else x

#' Le os metadados baixados por trf3_baixar_datajud()
#'
#' @param arquivos Vetor de arquivos. Se `NULL`, le todos os
#'   `datajud_<cnj>.json` de `diretorio`.
#' @param diretorio Pasta com os arquivos.
#'
#' @return `trf3_ler_datajud()`: tibble com uma linha por processo e grau
#'   (`processo`, `grau`, `classe_codigo`, `classe`, `sistema`, `formato`,
#'   `orgao_julgador_codigo`, `orgao_julgador`, `municipio_ibge`,
#'   `data_ajuizamento`, `nivel_sigilo`, `assuntos_codigos`, `assuntos`,
#'   `n_movimentos`, `data_ultima_atualizacao`).
#' @export
trf3_ler_datajud <- function(arquivos = NULL, diretorio = ".") {
  arquivos <- listar_datajud(arquivos, diretorio)
  purrr::map(arquivos, function(arq) {
    purrr::map(ler_hits_datajud(arq), function(s) {
      assuntos <- purrr::map(s$assuntos, function(a) {
        list(codigo = nulo_na(a$codigo, NA_integer_), nome = nulo_na(a$nome))
      })
      tibble::tibble(
        processo = cnj_pontuar(s$numeroProcesso),
        grau = nulo_na(s$grau),
        classe_codigo = nulo_na(s$classe$codigo, NA_integer_),
        classe = nulo_na(s$classe$nome),
        sistema = nulo_na(s$sistema$nome),
        formato = nulo_na(s$formato$nome),
        orgao_julgador_codigo = nulo_na(s$orgaoJulgador$codigo, NA_integer_),
        orgao_julgador = nulo_na(s$orgaoJulgador$nome),
        municipio_ibge = nulo_na(s$orgaoJulgador$codigoMunicipioIBGE, NA_integer_),
        data_ajuizamento = lubridate::ymd_hms(nulo_na(s$dataAjuizamento), quiet = TRUE, truncated = 3),
        nivel_sigilo = nulo_na(s$nivelSigilo, NA_integer_),
        assuntos_codigos = paste(purrr::map_int(assuntos, "codigo"), collapse = ";"),
        assuntos = paste(stats::na.omit(purrr::map_chr(assuntos, "nome")), collapse = "; "),
        n_movimentos = length(s$movimentos),
        data_ultima_atualizacao = lubridate::ymd_hms(nulo_na(s$dataHoraUltimaAtualizacao), quiet = TRUE)
      )
    }) |> dplyr::bind_rows()
  }, .progress = length(arquivos) > 50) |>
    dplyr::bind_rows()
}

#' @rdname trf3_ler_datajud
#' @return `trf3_ler_movimentos()`: tibble com uma linha por movimento
#'   (`processo`, `grau`, `data_hora`, `codigo`, `movimento`,
#'   `complementos`, `orgao_julgador`), em ordem cronologica.
#' @export
trf3_ler_movimentos <- function(arquivos = NULL, diretorio = ".") {
  arquivos <- listar_datajud(arquivos, diretorio)
  purrr::map(arquivos, function(arq) {
    purrr::map(ler_hits_datajud(arq), function(s) {
      purrr::map(s$movimentos, function(m) {
        compl <- purrr::map_chr(m$complementosTabelados, function(cx) {
          paste0(nulo_na(cx$descricao), ": ", nulo_na(cx$nome, as.character(nulo_na(cx$valor))))
        })
        tibble::tibble(
          processo = cnj_pontuar(s$numeroProcesso),
          grau = nulo_na(s$grau),
          data_hora = lubridate::ymd_hms(nulo_na(m$dataHora), quiet = TRUE),
          codigo = nulo_na(m$codigo, NA_integer_),
          movimento = nulo_na(m$nome),
          complementos = if (length(compl)) paste(compl, collapse = "; ") else NA_character_,
          orgao_julgador = nulo_na(m$orgaoJulgador$nome)
        )
      }) |> dplyr::bind_rows()
    }) |> dplyr::bind_rows()
  }) |>
    dplyr::bind_rows() |>
    dplyr::arrange(.data$processo, .data$grau, .data$data_hora)
}
