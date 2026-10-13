test_that("cnj_limpar e cnj_pontuar normalizam numeros", {
  expect_equal(cnj_limpar("5014445-85.2023.4.03.6183"), "50144458520234036183")
  expect_equal(cnj_pontuar("50144458520234036183"), "5014445-85.2023.4.03.6183")
  expect_equal(cnj_pontuar("28967-30.2004.4.03.6100"), "0028967-30.2004.4.03.6100")
  expect_true(is.na(cnj_pontuar(NA)))
})

test_that("agrupar_datas fatia em janelas alinhadas ao calendario", {
  j <- trf3juris:::agrupar_datas(as.Date("2026-01-15"), as.Date("2026-03-10"), "mes")
  expect_equal(j$inicio, as.Date(c("2026-01-15", "2026-02-01", "2026-03-01")))
  expect_equal(j$fim, as.Date(c("2026-01-31", "2026-02-28", "2026-03-10")))

  j <- trf3juris:::agrupar_datas(as.Date("2026-01-01"), as.Date("2026-01-03"), "dia")
  expect_equal(nrow(j), 3)
  expect_equal(j$inicio, j$fim)

  j <- trf3juris:::agrupar_datas(as.Date("2026-01-10"), as.Date("2026-01-20"), "mes")
  expect_equal(nrow(j), 1)

  j <- trf3juris:::agrupar_datas(as.Date(NA), as.Date(NA), "mes")
  expect_equal(nrow(j), 1)
})

test_that("validar_data rejeita formatos errados", {
  expect_equal(trf3juris:::validar_data("01/09/2026"), as.Date("2026-09-01"))
  expect_true(is.na(trf3juris:::validar_data("")))
  expect_error(trf3juris:::validar_data("2026-09-01"))
  expect_error(trf3juris:::validar_data("31/02/2026"))
})

test_that("montar_campos replica o preenchimento dos hdn* do site", {
  relator <- tibble::tibble(campo = "magistrado", id = "7", texto = "FULANO DE TAL", texto_jef = NA)
  orgao <- tibble::tibble(campo = "orgao", id = "1", texto = "1a. Seção", texto_jef = "PRIMEIRA SEÇÃO")
  c <- trf3juris:::montar_campos("acordaos", "x", "", as.Date("2026-09-01"), as.Date("2026-09-05"),
                                 "julgamento", relator, NULL, orgao, "", "", "")
  expect_equal(c$magistrado, "7")
  expect_equal(c$hdnMagistrado, "((FULANO DE TAL)).rel.")
  expect_equal(c$hdnOrgao, "(1A).org.")
  expect_equal(c$data_inicial, "01/09/2026")
  expect_equal(c$data_tipo, "1")
  expect_null(c$in_juizado_trf3)

  m <- trf3juris:::montar_campos("monocraticas", "", "", as.Date(NA), as.Date(NA),
                                 "publicacao", NULL, NULL, NULL, "", "", "")
  expect_equal(m$in_juizado_trf3, "on")
  expect_equal(m$data_tipo, "0")
  expect_equal(m$hdnClasse, "")
})

test_that("resolver_filtro casa id, texto e trecho sem acento", {
  op <- tibble::tibble(
    campo = "magistrado", id = c("1", "2", "3"),
    texto = c("ANDRÉ NABARRETE", "ANDRÉ NEKATSCHALOW", "MARLI FERREIRA"),
    texto_jef = NA
  )
  expect_equal(trf3juris:::resolver_filtro("3", "magistrado", op)$texto, "MARLI FERREIRA")
  expect_equal(trf3juris:::resolver_filtro("nabarrete", "magistrado", op)$id, "1")
  expect_null(trf3juris:::resolver_filtro("", "magistrado", op))
  expect_error(trf3juris:::resolver_filtro("andre", "magistrado", op), "amb")
  expect_error(trf3juris:::resolver_filtro("ninguem", "magistrado", op), "encontrado")

  dup <- tibble::tibble(campo = "orgao", id = c("11", "12"), texto = "5ª Turma", texto_jef = "QUINTA TURMA")
  expect_equal(trf3juris:::resolver_filtro("5ª Turma", "orgao", dup)$id, "11")
})

test_that("estado_resposta detecta sessao expirada e bloqueio", {
  ok <- list(status = 200L, url = "https://x/ListaResumida/2?np=2", texto = "<html></html>")
  expect_equal(trf3juris:::estado_resposta(ok), "ok")
  exp <- list(status = 200L, url = "https://x/Home/SessaoExpirada", texto = "")
  expect_equal(trf3juris:::estado_resposta(exp), "expirada")
  blq <- list(status = 403L, url = "https://x", texto = "Access Denied")
  expect_equal(trf3juris:::estado_resposta(blq), "bloqueio")
})
