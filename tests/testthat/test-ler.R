fx <- function(padrao) list.files(test_path("fixtures"), padrao, full.names = TRUE)

test_that("trf3_ler_cjsg le monocraticas com inteiro teor inline", {
  df <- trf3_ler_cjsg(fx("^trf3_monocraticas_.*\\.html$"))
  expect_equal(nrow(df), 3)
  expect_equal(unique(df$base), "monocraticas")
  expect_equal(df$indice, 1:3)
  expect_true(all(grepl("^\\d{7}-\\d{2}\\.\\d{4}\\.4\\.03\\.\\d{4}$", df$processo)))
  expect_s3_class(df$data_decisao, "Date")
  expect_false(anyNA(df$data_decisao))
  expect_false(anyNA(df$data_publicacao))
  expect_false(anyNA(df$inteiro_teor))
  expect_false(anyNA(df$orgao_julgador)) # recuperado do cabecalho do inteiro teor
  expect_false(anyNA(df$classe_sigla))
  expect_true(all(grepl("^https://pje", df$url_pje)))
})

test_that("trf3_ler_cjsg le acordaos (sem inteiro teor inline)", {
  df <- trf3_ler_cjsg(fx("^trf3_acordaos_.*\\.html$"))
  expect_equal(nrow(df), 3)
  expect_equal(unique(df$base), "acordaos")
  expect_true(all(is.na(df$inteiro_teor)))
  expect_false(anyNA(df$ementa))
  expect_false(anyNA(df$orgao_julgador))
  expect_false(anyNA(df$relator))
  expect_true(all(df$tipo_data == "Julgamento"))
})

test_that("trf3_ler_cjsg remove duplicatas entre arquivos", {
  arq <- fx("^trf3_monocraticas_.*\\.html$")
  copia <- file.path(tempdir(), sub("_p0001", "_p0002", basename(arq)))
  file.copy(arq, copia, overwrite = TRUE)
  on.exit(unlink(copia))
  expect_equal(nrow(trf3_ler_cjsg(c(arq, copia))), 3)
})

test_that("ler_total e primeiro_indice leem a pagina", {
  txt <- trf3juris:::ler_texto(fx("^trf3_monocraticas_.*\\.html$"))
  expect_gt(trf3juris:::ler_total(txt), 3)
  expect_equal(trf3juris:::primeiro_indice(txt), 1L)
  expect_equal(trf3juris:::ler_total("<html>nada</html>"), 0L)
})

test_that("trf3_ler_inteiro_teor le PJe e GEDPRO e junta datas do indice", {
  df <- trf3_ler_inteiro_teor(diretorio = test_path("fixtures"))
  expect_equal(nrow(df), 2)
  expect_setequal(df$fonte, c("pje", "gedpro"))
  expect_equal(unique(df$processo), "0028967-30.2004.4.03.6100")
  expect_false(anyNA(df$data_documento))
  expect_true(all(nchar(df$texto) > 5000))
  expect_match(df$texto[df$fonte == "gedpro"], "RELATORA: Desembargadora")
  expect_match(df$texto[df$fonte == "pje"], "VOTO")
  expect_false(any(grepl("@media print", df$texto, fixed = TRUE)))
})

test_that("trf3_ler_datajud e trf3_ler_movimentos", {
  arq <- fx("^datajud_.*\\.json$")
  dj <- trf3_ler_datajud(arq)
  expect_equal(nrow(dj), 2)
  expect_setequal(dj$grau, c("G1", "G2"))
  expect_s3_class(dj$data_ajuizamento, "POSIXct")
  expect_false(anyNA(dj$data_ajuizamento))
  expect_equal(unique(dj$processo), "5014445-85.2023.4.03.6183")

  mv <- trf3_ler_movimentos(arq)
  expect_equal(nrow(mv), sum(dj$n_movimentos))
  expect_false(anyNA(mv$data_hora))
  expect_false(is.unsorted(mv$data_hora[mv$grau == "G1"]))
})
