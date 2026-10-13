sessao_falsa <- function(get) {
  list(abrir = get, get = get, post_form = function(caminho, campos) get(caminho),
       fechar = function() invisible())
}

test_that("motor auto troca para chromote quando httr2 falha", {
  local_mocked_bindings(
    sessao_httr2 = function(...) sessao_falsa(function(caminho) stop("timeout")),
    sessao_chromote = function(...) sessao_falsa(function(caminho) {
      list(status = 200L, url = caminho, texto = "ok")
    })
  )
  s <- trf3_sessao("auto")
  expect_equal(s$motor(), "httr2")
  expect_message(r <- s$get("/x"), "chromote")
  expect_equal(r$texto, "ok")
  expect_equal(s$motor(), "chromote")
})

test_that("motor auto troca para chromote quando o Akamai bloqueia", {
  local_mocked_bindings(
    sessao_httr2 = function(...) sessao_falsa(function(caminho) {
      list(status = 403L, url = caminho, texto = "Access Denied")
    }),
    sessao_chromote = function(...) sessao_falsa(function(caminho) {
      list(status = 200L, url = caminho, texto = "ok")
    })
  )
  s <- trf3_sessao("auto")
  expect_message(s$post_form("/x", list(a = 1)), "Akamai")
  expect_equal(s$motor(), "chromote")
})

test_that("motor auto mantem httr2 quando funciona", {
  local_mocked_bindings(
    sessao_httr2 = function(...) sessao_falsa(function(caminho) {
      list(status = 200L, url = caminho, texto = "ok")
    }),
    sessao_chromote = function(...) stop("não deveria abrir o Chrome")
  )
  s <- trf3_sessao("auto")
  expect_equal(s$get("/x")$texto, "ok")
  expect_equal(s$motor(), "httr2")
})
