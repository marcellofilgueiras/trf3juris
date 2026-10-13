UA_PADRAO <- paste(
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36",
  "(KHTML, like Gecko) Chrome/140.0.0.0 Safari/537.36"
)

#' Abre uma sessao de navegacao no site do TRF3
#'
#' O site do TRF3 fica atras do Akamai Bot Manager, que derruba alguns
#' clientes HTTP ainda no handshake TLS (o resultado depende da biblioteca
#' TLS e da rede). Ha dois motores:
#'
#' * `"httr2"`: requisicoes HTTP diretas, rapidas e leves;
#' * `"chromote"`: um Chrome headless abre a pagina (gerando os cookies do
#'   Akamai) e as requisicoes sao feitas por `fetch()` dentro dela. E mais
#'   lento, mas passa onde o `httr2` e barrado. Exige Chrome/Chromium.
#'
#' O padrao, `"auto"`, comeca com `httr2` e muda sozinho para `chromote` se
#' a conexao falhar ou for bloqueada.
#'
#' As funcoes `trf3_baixar_*()` abrem e fecham a propria sessao; use esta
#' funcao apenas se quiser reaproveitar uma sessao entre chamadas.
#'
#' @param motor `"auto"` (padrao), `"httr2"` ou `"chromote"`.
#' @param timeout Tempo maximo por requisicao, em segundos.
#'
#' @return Objeto `trf3_sessao`: lista com as funcoes `abrir(caminho)`,
#'   `get(caminho)`, `post_form(caminho, campos)`, `fechar()` e
#'   `motor()`. Todas as requisicoes devolvem `list(status, url, texto)`.
#' @export
trf3_sessao <- function(motor = c("auto", "httr2", "chromote"), timeout = 120) {
  motor <- match.arg(motor)
  s <- switch(motor,
    auto = sessao_auto(timeout),
    chromote = sessao_chromote(timeout),
    httr2 = sessao_httr2(timeout)
  )
  if (is.null(s$motor)) s$motor <- function() motor
  structure(s, class = "trf3_sessao")
}

#' @export
print.trf3_sessao <- function(x, ...) {
  cli::cli_text("<trf3_sessao> motor {.val {x$motor()}}")
  invisible(x)
}

sessao_auto <- function(timeout) {
  atual <- sessao_httr2(min(timeout, 30), tentativas = 2)
  nome <- "httr2"

  trocar <- function(motivo) {
    cli::cli_alert_warning("httr2 n\u00e3o passou ({motivo}); mudando para o motor chromote.")
    atual$fechar()
    atual <<- sessao_chromote(timeout)
    nome <<- "chromote"
  }

  envolver <- function(metodo) {
    function(...) {
      if (nome == "httr2") {
        r <- tryCatch(atual[[metodo]](...), error = function(e) e)
        if (inherits(r, "error")) {
          trocar("falha de conex\u00e3o")
        } else if (estado_resposta(r) == "bloqueio") {
          trocar("bloqueio do Akamai")
        } else {
          return(r)
        }
      }
      atual[[metodo]](...)
    }
  }

  list(
    abrir = envolver("abrir"),
    get = envolver("get"),
    post_form = envolver("post_form"),
    fechar = function() atual$fechar(),
    motor = function() nome
  )
}

sessao_chromote <- function(timeout) {
  b <- chromote::ChromoteSession$new()
  # Sem isso o UA anuncia "HeadlessChrome", o que o Akamai pode barrar
  ua <- b$Runtime$evaluate("navigator.userAgent")$result$value
  b$Network$setUserAgentOverride(
    userAgent = sub("HeadlessChrome", "Chrome", ua),
    acceptLanguage = "pt-BR,pt;q=0.9"
  )
  na_origem <- FALSE

  avaliar <- function(js) {
    x <- b$Runtime$evaluate(js, awaitPromise = TRUE, returnByValue = TRUE, timeout_ = timeout)
    if (!is.null(x$exceptionDetails)) {
      cli::cli_abort("Erro no navegador: {x$exceptionDetails$exception$description %||% x$exceptionDetails$text}")
    }
    x$result$value
  }

  abrir <- function(caminho) {
    b$go_to(url_abs(caminho), timeout_ = timeout)
    Sys.sleep(1.5) # deixa o sensor do Akamai assentar os cookies
    na_origem <<- TRUE
    v <- avaliar("({url: location.href, texto: document.documentElement.outerHTML})")
    list(status = 200L, url = v$url, texto = v$texto)
  }

  buscar <- function(caminho, metodo, campos = NULL) {
    if (!na_origem) abrir("/jurisprudencia/")
    corpo <- if (is.null(campos)) {
      "null"
    } else {
      sprintf("new URLSearchParams(%s)", jsonlite::toJSON(lapply(campos, as.character), auto_unbox = TRUE))
    }
    js <- sprintf(
      "(async () => {
         const r = await fetch(%s, {method: %s, body: %s, credentials: 'include'});
         return {status: r.status, url: r.url, texto: await r.text()};
       })()",
      jsonlite::toJSON(url_abs(caminho), auto_unbox = TRUE),
      jsonlite::toJSON(metodo, auto_unbox = TRUE),
      corpo
    )
    v <- avaliar(js)
    list(status = as.integer(v$status), url = v$url, texto = v$texto)
  }

  list(
    abrir = abrir,
    get = function(caminho) buscar(caminho, "GET"),
    post_form = function(caminho, campos) buscar(caminho, "POST", campos),
    fechar = function() try(b$close(), silent = TRUE)
  )
}

sessao_httr2 <- function(timeout, tentativas = 3) {
  jar <- tempfile(fileext = ".cookies")

  requisicao <- function(caminho) {
    httr2::request(url_abs(caminho)) |>
      httr2::req_user_agent(UA_PADRAO) |>
      httr2::req_headers(
        Accept = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
        `Accept-Language` = "pt-BR,pt;q=0.9"
      ) |>
      httr2::req_cookie_preserve(jar) |>
      httr2::req_timeout(timeout) |>
      httr2::req_retry(max_tries = tentativas, retry_on_failure = TRUE) |>
      httr2::req_error(is_error = function(resp) FALSE)
  }

  executar <- function(req) {
    resp <- tryCatch(httr2::req_perform(req), error = function(e) {
      cli::cli_abort(c(
        "Falha ao acessar {.url {req$url}}.",
        i = "O TRF3 usa Akamai Bot Manager, que costuma bloquear clientes HTTP comuns.",
        i = "Tente {.code motor = \"chromote\"} (ou {.code \"auto\"})."
      ), parent = e)
    })
    list(
      status = httr2::resp_status(resp),
      url = httr2::resp_url(resp),
      texto = httr2::resp_body_string(resp, encoding = "UTF-8")
    )
  }

  list(
    abrir = function(caminho) executar(requisicao(caminho)),
    get = function(caminho) executar(requisicao(caminho)),
    post_form = function(caminho, campos) {
      executar(httr2::req_body_form(requisicao(caminho), !!!campos))
    },
    fechar = function() unlink(jar)
  )
}

#' Classifica uma resposta: "ok", "expirada", "bloqueio" ou "erro"
#' @noRd
estado_resposta <- function(r) {
  if (grepl("SessaoExpirada", r$url %||% "", fixed = TRUE)) return("expirada")
  if (r$status %in% c(403L, 429L) || grepl("Access Denied|sec-if-cpt-container", r$texto)) {
    return("bloqueio")
  }
  if (r$status >= 400L) return("erro")
  "ok"
}

#' Executa `f()` ate obter resposta "ok", com backoff exponencial
#' @noRd
com_tentativas <- function(f, tentativas = 3, rotulo = "requisi\u00e7\u00e3o", ao_falhar = NULL) {
  for (i in seq_len(tentativas)) {
    r <- tryCatch(f(), error = function(e) e)
    estado <- if (inherits(r, "error")) conditionMessage(r) else estado_resposta(r)
    if (identical(estado, "ok")) return(r)
    if (i < tentativas) {
      cli::cli_alert_warning("{rotulo}: {estado}; nova tentativa em {2^i}s.")
      Sys.sleep(2^i)
      if (!is.null(ao_falhar)) ao_falhar()
    }
  }
  cli::cli_abort("{rotulo}: falhou ap\u00f3s {tentativas} tentativas ({estado}).")
}
