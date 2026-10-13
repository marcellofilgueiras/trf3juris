TRF3_URL <- "https://web.trf3.jus.br"

url_abs <- function(caminho) {
  if (grepl("^https?://", caminho)) caminho else paste0(TRF3_URL, caminho)
}

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

# Datas ------------------------------------------------------------------

#' Converte "dd/mm/aaaa" (ou Date) em Date; "" vira NA
#' @noRd
validar_data <- function(x, nome = "data") {
  if (inherits(x, "Date")) return(x)
  if (is.null(x) || identical(x, "") || is.na(x)) return(as.Date(NA))
  if (!grepl("^\\d{2}/\\d{2}/\\d{4}$", x)) {
    cli::cli_abort("{.arg {nome}} deve estar no formato {.val dd/mm/aaaa}, recebi {.val {x}}.")
  }
  d <- as.Date(x, format = "%d/%m/%Y")
  if (is.na(d)) cli::cli_abort("{.arg {nome}} {.val {x}} n\u00e3o \u00e9 uma data v\u00e1lida.")
  d
}

formatar_data <- function(d) format(d, "%d/%m/%Y")

#' Divide um intervalo em janelas consecutivas
#'
#' @param inicio,fim Date.
#' @param por "mes", "semana", "dia", "ano" ou "nenhum".
#' @return tibble com colunas `inicio` e `fim` (Date).
#' @noRd
agrupar_datas <- function(inicio, fim, por = c("mes", "semana", "dia", "ano", "nenhum")) {
  por <- match.arg(por)
  if (is.na(inicio) || is.na(fim) || por == "nenhum") {
    return(tibble::tibble(inicio = inicio, fim = fim))
  }
  if (inicio > fim) cli::cli_abort("{.arg data_inicial} \u00e9 posterior a {.arg data_final}.")
  unidade <- c(mes = "month", semana = "week", dia = "day", ano = "year")[[por]]
  if (por %in% c("mes", "ano")) {
    # Janelas alinhadas ao calendario: 15/01-31/01, 01/02-28/02, ...
    proxima <- seq(lubridate::floor_date(inicio, unidade), by = unidade, length.out = 2)[2]
    inicios <- c(inicio, if (proxima <= fim) seq(proxima, fim, by = unidade))
  } else {
    inicios <- seq(inicio, fim, by = unidade)
  }
  tibble::tibble(inicio = inicios, fim = c(inicios[-1] - 1, fim))
}

#' Extrai a primeira data dd/mm/aaaa de um texto
#' @noRd
extrair_data <- function(x) {
  lubridate::dmy(stringr::str_extract(x, "\\d{2}/\\d{2}/\\d{4}"), quiet = TRUE)
}

# Numeracao CNJ ------------------------------------------------------------

#' Normaliza numeros de processo CNJ
#'
#' `cnj_limpar()` deixa so os 20 digitos; `cnj_pontuar()` devolve
#' NNNNNNN-DD.AAAA.J.TR.OOOO.
#'
#' @param x Vetor de numeros de processo, com ou sem pontuacao.
#' @export
#' @examples
#' cnj_pontuar("50144458520234036183")
#' cnj_limpar("5014445-85.2023.4.03.6183")
cnj_limpar <- function(x) {
  d <- stringr::str_remove_all(x, "\\D")
  ifelse(is.na(d) | d == "", NA_character_, stringr::str_pad(d, 20, "left", "0"))
}

#' @rdname cnj_limpar
#' @export
cnj_pontuar <- function(x) {
  d <- cnj_limpar(x)
  ifelse(
    is.na(d), NA_character_,
    paste0(substr(d, 1, 7), "-", substr(d, 8, 9), ".", substr(d, 10, 13), ".",
           substr(d, 14, 14), ".", substr(d, 15, 16), ".", substr(d, 17, 20))
  )
}

# Texto / HTML -----------------------------------------------------------

#' Texto de um no HTML preservando quebras de <br> e <p>
#' @noRd
texto_no <- function(no) {
  if (inherits(no, "xml_missing") || length(no) == 0) return(NA_character_)
  no <- xml2::read_html(as.character(no))
  xml2::xml_remove(xml2::xml_find_all(no, "//script|//style"))
  for (br in xml2::xml_find_all(no, "//br")) xml2::xml_add_sibling(br, "span", "\n")
  for (p in xml2::xml_find_all(no, "//p|//div|//li")) xml2::xml_add_child(p, "span", "\n")
  limpar_texto(xml2::xml_text(no))
}

limpar_texto <- function(x) {
  x <- stringr::str_replace_all(x, "\u00a0", " ")
  x <- stringr::str_replace_all(x, "[ \t\r\f\v]+", " ")
  x <- stringr::str_replace_all(x, " *\n *", "\n")
  x <- stringr::str_replace_all(x, "\n{3,}", "\n\n")
  x <- stringr::str_trim(x)
  ifelse(x == "", NA_character_, x)
}

texto_simples <- function(no) {
  if (inherits(no, "xml_missing")) return(NA_character_)
  limpar_texto(stringr::str_squish(xml2::xml_text(no)))
}

# Arquivos ---------------------------------------------------------------

gravar <- function(texto, arquivo) {
  dir.create(dirname(arquivo), showWarnings = FALSE, recursive = TRUE)
  writeBin(charToRaw(enc2utf8(texto)), arquivo)
  invisible(arquivo)
}

ler_utf8 <- function(arquivo) {
  xml2::read_html(arquivo, encoding = "UTF-8")
}

listar_arquivos <- function(arquivos, diretorio, padrao) {
  if (is.null(arquivos)) {
    arquivos <- list.files(diretorio, pattern = padrao, full.names = TRUE)
  }
  if (length(arquivos) == 0) cli::cli_abort("Nenhum arquivo encontrado em {.path {diretorio}}.")
  arquivos
}

hash_curto <- function(...) substr(rlang::hash(list(...)), 1, 8)

dormir <- function(sono) if (sono > 0) Sys.sleep(stats::runif(1, sono * 0.75, sono * 1.25))

sem_acento <- function(x) {
  tolower(iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT", sub = ""))
}
