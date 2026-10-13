#' Le as paginas de jurisprudencia baixadas por trf3_baixar_cjsg()
#'
#' Cada decisao (`li.acordao-retorno`) vira uma linha. Os campos sao
#' extraidos um a um: um campo ausente vira `NA` e nao descarta o
#' registro. Decisoes repetidas (janelas sobrepostas, reexecucoes) sao
#' removidas.
#'
#' @param arquivos Vetor de arquivos. Se `NULL`, le todos os
#'   `trf3_*_pNNNN.html` de `diretorio`.
#' @param diretorio Pasta com os arquivos.
#'
#' @return tibble com as colunas:
#'   * `arquivo`, `base`, `indice` (posicao na pesquisa);
#'   * `processo` (CNJ pontuado), `classe_sigla`, `classe`,
#'     `orgao_julgador`, `relator`;
#'   * `fonte_publicacao`, `data_publicacao`;
#'   * `tipo_data` ("Julgamento" ou "Decisao"), `data_decisao`;
#'   * `ementa` (nas monocraticas, o inicio da decisao);
#'   * `inteiro_teor` (monocraticas; `NA` em acordaos, cujo inteiro teor
#'     vem de [trf3_baixar_inteiro_teor()]);
#'   * `url_pje`.
#' @export
trf3_ler_cjsg <- function(arquivos = NULL, diretorio = ".") {
  arquivos <- listar_arquivos(arquivos, diretorio, "^trf3_.+_p\\d{4}\\.html$")
  ler <- purrr::possibly(ler_cjsg_arquivo, otherwise = NULL, quiet = FALSE)
  df <- purrr::map(arquivos, ler, .progress = length(arquivos) > 10) |>
    dplyr::bind_rows()
  if (nrow(df) == 0) return(df)
  dplyr::distinct(df, dplyr::across(-c("arquivo", "indice")), .keep_all = TRUE)
}

ler_cjsg_arquivo <- function(arquivo) {
  doc <- ler_utf8(arquivo)
  itens <- xml2::xml_find_all(doc, "//li[contains(@class, 'acordao-retorno')]")
  if (length(itens) == 0) return(NULL)
  base <- base_do_arquivo(arquivo, doc)
  purrr::map(itens, ler_item_cjsg) |>
    dplyr::bind_rows() |>
    tibble::add_column(arquivo = arquivo, base = base, .before = 1)
}

base_do_arquivo <- function(arquivo, doc) {
  b <- stringr::str_match(basename(arquivo), "^trf3_([a-z]+)_")[, 2]
  if (!is.na(b) && b %in% names(BASES)) return(b)
  titulo <- sem_acento(xml2::xml_text(xml2::xml_find_first(doc, "//span[@class='texto-titulo']")))
  if (grepl("monocrat", titulo)) "monocraticas"
  else if (grepl("recursa|jef|turma", titulo)) "recursais"
  else "acordaos"
}

`%|na|%` <- function(x, y) if (is.na(x)) y else x

#' Nas monocraticas o site nao preenche o orgao julgador, mas o cabecalho do
#' inteiro teor traz a linha ("9a Turma", "10a Turma Recursal da ...", etc.)
#' @noRd
orgao_do_cabecalho <- function(texto) {
  if (is.na(texto)) return(NA_character_)
  linhas <- utils::head(strsplit(texto, "\n", fixed = TRUE)[[1]], 6)
  padrao <- "(?i)^(\\d+\\S*\\s+(turma|se\u00e7\u00e3o)|\u00f3rg\u00e3o especial|vice-presid|presid\u00eancia|gab)"
  linhas[stringr::str_detect(linhas, padrao)][1]
}

ler_item_cjsg <- function(li) {
  achar <- function(xpath) xml2::xml_find_first(li, xpath)

  info <- xml2::xml_find_all(li, ".//div[contains(@class, 'informacoes-basicas')]/span")
  info <- stringr::str_squish(xml2::xml_text(info))
  pos <- function(i) if (length(info) >= i && nzchar(info[i])) info[i] else NA_character_

  # Data da decisao: "Julgamento: dd/mm/aaaa" (acordaos) ou "Decisao: ..." (monocraticas)
  rotulo_data <- "^(Julgamento|Decis\\S+)\\s*:"
  txt_decisao <- info[stringr::str_detect(info, rotulo_data)][1]

  publicacao <- texto_simples(achar(".//div[contains(@class, 'info-publicacao')]"))
  if (is.na(publicacao)) publicacao <- info[stringr::str_detect(info, "(?i)data:")][1]

  classe <- stringr::str_split_fixed(pos(1) %||% "", "\\s+-\\s+", 2)

  inteiro_teor <- achar(".//textarea[contains(@class, 'acordao-hidden')]")
  inteiro_teor <- if (inherits(inteiro_teor, "xml_missing")) NA_character_ else
    limpar_texto(xml2::xml_text(inteiro_teor))

  tibble::tibble(
    indice = as.integer(stringr::str_extract(texto_simples(achar(".//span[@class='contagem']")), "^\\d+")),
    processo = cnj_pontuar(texto_simples(achar(".//div[@class='processo']/span[@class='info']"))),
    classe_sigla = dplyr::na_if(classe[, 1], ""),
    classe = dplyr::na_if(classe[, 2], ""),
    orgao_julgador = pos(2) %|na|% orgao_do_cabecalho(inteiro_teor),
    relator = pos(3),
    fonte_publicacao = stringr::str_trim(stringr::str_remove(publicacao, "(?i)\\s*data\\s*:.*$")),
    data_publicacao = extrair_data(publicacao),
    tipo_data = stringr::str_extract(txt_decisao, "^[^:]+"),
    data_decisao = extrair_data(txt_decisao),
    ementa = texto_no(achar(".//span[contains(@class, 'info-ementa')]")),
    inteiro_teor = inteiro_teor,
    url_pje = xml2::xml_attr(achar(".//button[contains(@class, 'btn-consultar-processo')]"), "data-link")
  )
}
