#' Baixa o inteiro teor dos acordaos de processos do TRF3
#'
#' Para cada processo, consulta
#' `web.trf3.jus.br/acordaos/Acordao/PesquisarDocumento` (lista de acordaos
#' publicados) e baixa cada documento, PJe ou GEDPRO (legado), como HTML.
#' As decisoes monocraticas nao precisam desta etapa: o texto integral ja
#' vem na pesquisa (coluna `inteiro_teor` de [trf3_ler_cjsg()]).
#'
#' Arquivos gravados:
#' * `trf3_it_indice_<cnj>.html`: lista de documentos do processo;
#' * `trf3_it_<cnj>_<pje|gedpro>_<id>.html`: cada acordao.
#'
#' Arquivos ja existentes sao pulados.
#'
#' @param processos Numeros CNJ, com ou sem pontuacao.
#' @inheritParams trf3_baixar_cjsg
#' @return Invisivel: vetor com os caminhos dos acordaos.
#' @export
#' @examples
#' \dontrun{
#' trf3_baixar_inteiro_teor("0028967-30.2004.4.03.6100", diretorio = "dados/it")
#' }
trf3_baixar_inteiro_teor <- function(processos, motor = c("auto", "httr2", "chromote"),
                                     sono = 1, diretorio = ".") {
  motor <- match.arg(motor)
  cnjs <- unique(stats::na.omit(cnj_limpar(processos)))
  if (length(cnjs) == 0) cli::cli_abort("Nenhum n\u00famero de processo v\u00e1lido.")

  sessao <- NULL
  abrir_sessao <- function() {
    if (is.null(sessao)) sessao <<- trf3_sessao(motor)
    sessao
  }
  on.exit(if (!is.null(sessao)) sessao$fechar(), add = TRUE)

  baixar <- function(caminho, arquivo, rotulo) {
    if (file.exists(arquivo)) return(arquivo)
    s <- abrir_sessao()
    r <- com_tentativas(function() s$get(caminho), rotulo = rotulo)
    gravar(r$texto, arquivo)
    dormir(sono)
    arquivo
  }

  arquivos <- purrr::map(cnjs, function(cnj) {
    indice <- baixar(
      sprintf("/acordaos/Acordao/PesquisarDocumento?processo=%s", cnj),
      file.path(diretorio, sprintf("trf3_it_indice_%s.html", cnj)),
      rotulo = cnj
    )
    docs <- ler_indice_it(indice)
    purrr::pmap_chr(docs, function(processo, fonte, id_documento, data_documento, url) {
      baixar(url, file.path(diretorio, sprintf("trf3_it_%s_%s_%s.html", cnj, fonte, id_documento)),
             rotulo = paste(cnj, fonte, id_documento))
    })
  }, .progress = if (length(cnjs) > 1) "Baixando inteiro teor" else FALSE)
  invisible(unlist(arquivos))
}

#' Le a lista de acordaos de um processo (pagina PesquisarDocumento)
#' @noRd
ler_indice_it <- function(arquivo) {
  doc <- ler_utf8(arquivo)
  links <- xml2::xml_find_all(doc, "//a[contains(@href, 'BuscarDocumento')]")
  cnj <- stringr::str_extract(basename(arquivo), "\\d{20}")
  href <- xml2::xml_attr(links, "href")
  tibble::tibble(
    processo = rep(cnj_pontuar(cnj), length(links)),
    fonte = tolower(stringr::str_match(href, "BuscarDocumento([A-Za-z]+)/")[, 2]),
    id_documento = stringr::str_extract(href, "\\d+$"),
    data_documento = extrair_data(xml2::xml_text(links)),
    url = href
  )
}

#' Le os acordaos baixados por trf3_baixar_inteiro_teor()
#'
#' @param arquivos Vetor de arquivos. Se `NULL`, le todos os
#'   `trf3_it_<cnj>_<fonte>_<id>.html` de `diretorio`.
#' @param diretorio Pasta com os arquivos.
#'
#' @return tibble com `arquivo`, `processo`, `fonte` (pje/gedpro),
#'   `id_documento`, `data_documento` (data de disponibilizacao, lida do
#'   indice do processo quando ele esta na mesma pasta) e `texto`.
#' @export
trf3_ler_inteiro_teor <- function(arquivos = NULL, diretorio = ".") {
  arquivos <- listar_arquivos(arquivos, diretorio, "^trf3_it_\\d{20}_[a-z]+_\\d+\\.html$")
  partes <- stringr::str_match(basename(arquivos), "^trf3_it_(\\d{20})_([a-z]+)_(\\d+)\\.html$")

  ler <- purrr::possibly(ler_documento_it, otherwise = NA_character_, quiet = FALSE)
  df <- tibble::tibble(
    arquivo = arquivos,
    processo = cnj_pontuar(partes[, 2]),
    fonte = partes[, 3],
    id_documento = partes[, 4],
    texto = purrr::map_chr(arquivos, ler, .progress = length(arquivos) > 10)
  )

  indices <- file.path(dirname(arquivos), sprintf("trf3_it_indice_%s.html", partes[, 2]))
  indices <- unique(indices[file.exists(indices)])
  datas <- purrr::map(indices, purrr::possibly(ler_indice_it, NULL)) |> dplyr::bind_rows()
  if (nrow(datas) > 0) {
    datas <- dplyr::distinct(datas, .data$fonte, .data$id_documento, .data$data_documento)
    df <- dplyr::left_join(df, datas, by = c("fonte", "id_documento"))
  } else {
    df$data_documento <- as.Date(NA)
  }
  dplyr::relocate(df, "data_documento", .before = "texto")
}

ler_documento_it <- function(arquivo) {
  doc <- ler_utf8(arquivo)
  xml2::xml_remove(xml2::xml_find_all(doc, "//script|//style|//div[@id='sec-overlay']"))
  # PJe: conteudo do editor; GEDPRO: div#divGeral; senao, o body inteiro
  no <- xml2::xml_find_first(doc, "//div[@id='bdInnerContent'] | //div[@id='divGeral']")
  if (inherits(no, "xml_missing")) no <- xml2::xml_find_first(doc, "//body")
  # Cabecalhos em tabela (GEDPRO) saem como "RELATORA\n:\nNOME"
  stringr::str_replace_all(texto_no(no), "\\s*\\n+\\s*:\\s*\\n+\\s*", ": ")
}
