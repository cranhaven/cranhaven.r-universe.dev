
<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/nomesbr)](https://CRAN.R-project.org/package=nomesbr)
[![check](https://github.com/ipeadata-lab/nomesbr/actions/workflows/check.yaml/badge.svg)](https://github.com/ipeadata-lab/nomesbr/actions/workflows/check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/ipeadata-lab/nomesbr/graph/badge.svg)](https://app.codecov.io/gh/ipeadata-lab/nomesbr)
[![CRAN/METACRAN Total
downloads](https://cranlogs.r-pkg.org/badges/grand-total/nomesbr?color=blue)](https://CRAN.R-project.org/package=nomesbr)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
<!-- badges: end -->

## nomesbr

**nomesbr** é um pacote de R que limpa e simplifica nomes de pessoas
para auxiliar no pareamento de banco de dados na ausência de chaves
únicas não ambíguas. Detecta e corrige erros tipográficos mais comuns,
simplifica opcionalmente termos sujeitos eventualmente a omissão em
cadastros.

(R package for tidying and simplifying names. Created for aiding in
dataset pairing in the absence of unambiguous keys. It detects and
corrects common typos, optionally simplifies terms prone to omission in
records.)

## Instalação

A versão estável do pacote pode ser instalada com:

``` r
install.packages("nomesbr")
```

A versão em desenvolvimento pode ser instalada com o seguinte comando :

``` r
# install.packages("remotes")
remotes::install_github("ipeadata-lab/nomesbr")
```

## Utilização

O pacote **nomesbr** torna disponíveis funções para limpar e simplificar
nomes. `limpar_nomes()` e `simplifica_PARTICULAS_AGNOMES_PATENTES()`,
principais funções do pacote, foram criadas para seu uso em sequência
nessa ordem.

`limpar_nomes()` recebe como parâmetros d,um data.frame, e s, nome da
coluna com os nomes a processar. A função cria uma nova coluna, com
sufixo ’\_clean’, e gera novas colunas com informações dos tipos de
limpeza detectados como necessários e realizados.

`simplifica_PARTICULAS_AGNOMES_PATENTES()` recebe também, d e s (por
padrão para s, ‘nome_clean’) como parâmetros, e simplifica partículas
repetidas, agnomes e alguma patentes.

A informação (novas colunas) gerada pela função `limpar_nomes()` servem
como base para a função `tabular_problemas_em_nomes()` , que retorna uma
tabela resumo dos problemas detectados e ações tomadas no sentido da
limpeza de nomes.

## Nota <a href="https://www.ipea.gov.br"><img src="man/figures/ipea_logo.png" alt="Ipea" align="right" width="300"/></a>

**nomesbr** é desenvolvido por uma equipe de pesquisadores do Instituto
de Pesquisa Econômica Aplicada (Ipea).
