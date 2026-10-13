# trf3juris

Pacote R para baixar e organizar a jurisprudência do **Tribunal Regional Federal da 3ª Região** a partir de <https://web.trf3.jus.br/jurisprudencia/>, com metadados processuais da **API pública do DataJud/CNJ**.

Toda função de coleta se divide em duas etapas:

| etapa | baixar (grava em disco) | ler (devolve tibble) |
|---|---|---|
| decisões (acórdãos, monocráticas, turmas recursais) | `trf3_baixar_cjsg()` | `trf3_ler_cjsg()` |
| inteiro teor dos acórdãos | `trf3_baixar_inteiro_teor()` | `trf3_ler_inteiro_teor()` |
| metadados dos processos (DataJud) | `trf3_baixar_datajud()` | `trf3_ler_datajud()`, `trf3_ler_movimentos()` |

Auxiliares: `trf3_opcoes()`, para listar relatores, classes e órgãos aceitos nos filtros; `trf3_sessao()`; `cnj_limpar()` e `cnj_pontuar()`.

## Instalação

```r
# install.packages("remotes")
remotes::install_local("trf3juris")
```

## Uso

```r
library(trf3juris)

# 1. Baixar: decisões monocráticas de setembro/2026, em janelas semanais
trf3_baixar_cjsg(
  base = "monocraticas",
  data_inicial = "01/09/2026", data_final = "30/09/2026",
  por = "semana",
  diretorio = "dados/mono"
)

# Acórdãos com pesquisa livre e filtro de relator (aceita trecho do nome, sem acento)
trf3_baixar_cjsg(
  "aposentadoria ADJ especial",
  base = "acordaos", relator = "nabarrete",
  data_inicial = "01/01/2026", data_final = "30/06/2026",
  diretorio = "dados/acordaos"
)

# 2. Ler
mono <- trf3_ler_cjsg(diretorio = "dados/mono")     # já traz o inteiro_teor
acor <- trf3_ler_cjsg(diretorio = "dados/acordaos") # traz a ementa

# Inteiro teor dos acórdãos (PJe e GEDPRO)
trf3_baixar_inteiro_teor(acor$processo, diretorio = "dados/it")
it <- trf3_ler_inteiro_teor(diretorio = "dados/it")

# 3. Metadados dos processos (DataJud/CNJ)
trf3_baixar_datajud(mono$processo, diretorio = "dados/datajud")
meta <- trf3_ler_datajud(diretorio = "dados/datajud")      # 1 linha por processo × grau
movs <- trf3_ler_movimentos(diretorio = "dados/datajud")   # 1 linha por movimento
```

### Colunas de `trf3_ler_cjsg()`

| coluna | conteúdo |
|---|---|
| `arquivo`, `base`, `indice` | arquivo de origem, base pesquisada e posição do resultado na pesquisa |
| `processo` | número CNJ pontuado |
| `classe_sigla`, `classe` | classe processual (sigla e nome) |
| `orgao_julgador`, `relator` | órgão julgador e relator |
| `fonte_publicacao`, `data_publicacao` | veículo e data de publicação |
| `tipo_data`, `data_decisao` | "Julgamento" ou "Decisão", e a data correspondente |
| `ementa` | ementa; nas monocráticas, o início da decisão |
| `inteiro_teor` | texto integral, só nas monocráticas |
| `url_pje` | link de consulta do processo no PJe |

## Como funciona

- **Retomável.** Os nomes dos arquivos são determinísticos: base, período, tipo de data, hash dos critérios e página. Se a mesma chamada for interrompida e rodada de novo, ela pula o que já está em disco. Quando a primeira página já existe, o total de resultados é lido dela, sem nova requisição.
- **Janelas de data.** O período é fatiado (`por = "mes"` por padrão) e cada janela vira uma pesquisa curta. A sessão do site guarda o estado da pesquisa no servidor, e janelas menores reduzem a chance de ela expirar.
- **Verificação de página.** Cada página baixada é conferida pelo índice do primeiro resultado. Se a sessão expirar ou desalinhar, o pacote refaz a pesquisa e tenta de novo, com backoff.
- **Akamai.** O site fica atrás do Akamai Bot Manager. No motor padrão, `motor = "auto"`, o pacote tenta primeiro o `httr2`, que é rápido. Se a conexão falhar ou for bloqueada, troca sozinho para o `chromote`: um Chrome headless que abre a página e faz as requisições por `fetch()` dentro dela. Isso exige Chrome ou Chromium instalado.
- **Parser por registro.** Cada decisão é lida campo a campo. Um campo ausente vira `NA` e não descarta o registro nem a página. Decisões repetidas, vindas de janelas sobrepostas ou de reexecuções, são removidas.
- **DataJud.** Usa a chave pública do CNJ, conferida em 13/10/2026. Se o CNJ trocar a chave, defina `Sys.setenv(DATAJUD_KEY = "...")` com a nova, publicada em <https://datajud-wiki.cnj.jus.br/api-publica/acesso>.

## web.trf3 × Jurisprudência Unificada do CJF

Também avaliamos <https://jurisprudencia.cjf.jus.br/unificada/index.xhtml> como fonte. Ambos foram testados ao vivo em outubro de 2026.

| | **web.trf3.jus.br/jurisprudencia** (usado) | **CJF Unificada** |
|---|---|---|
| Acórdãos do TRF3 | sim | sim, desde ~1990 |
| **Decisões monocráticas do TRF3** | **sim** (base própria, com inteiro teor inline) | **não**: só acórdãos do TRF3 |
| Turmas recursais / JEF | sim | parcial |
| Inteiro teor de acórdão | via `/acordaos/Acordao/BuscarDocumento{Pje,Gedpro}` | inline no campo "Decisão" (acórdãos PJe) |
| Filtros | relator, classe, órgão, data de julgamento ou de publicação, ementa, indexação, legislação | relator, classe e data de julgamento; o filtro por data de publicação não se mostrou confiável |
| Tecnologia | ASP.NET MVC, pesquisa guardada na sessão | JSF/PrimeFaces, com `ViewState` e ids `j_idtNN` que mudam a cada deploy |
| Anti-robô | Akamai Bot Manager, contornado pelo motor `auto`/`chromote` | nenhum por ora (há um script de reCAPTCHA carregado, mas inativo) |
| Peso | 50 decisões ≈ 1,5 MB (monocráticas, com texto integral) | 50 decisões ≈ 1,3 MB |

**Conclusão.** O site do TRF3 é a única fonte com as **decisões monocráticas**. Só entre 01 e 05/09/2026 foram cerca de 2.900. Ele também tem filtros mais completos. O CJF é uma boa alternativa para séries históricas de **acórdãos**, porque traz o texto integral sem um passo extra, mas não substitui o TRF3. Se o Akamai endurecer a ponto de barrar até o Chrome headless, o CJF fica como plano B para acórdãos.

## Testes

```r
testthat::test_local("trf3juris")   # offline, com páginas reais em tests/testthat/fixtures
```
