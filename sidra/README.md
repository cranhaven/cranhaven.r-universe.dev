Introdução ao pacote sidra
================
Rodrigo E. S. Borges

# sidra <img src='man/figures/logo.png' align="right" height="139" />

[![CRAN
status](https://www.r-pkg.org/badges/version/sidra)](https://CRAN.R-project.org/package=sidra)
[![check](https://github.com/rodrigoesborges/sidra/actions/workflows/check.yaml/badge.svg)](https://github.com/rodrigoesborges/sidra/actions/workflows/check.yaml)
[![codecov](https://codecov.io/gh/rodrigoesborges/sidra/graph/badge.svg?token=7B3AMAQYHS)](https://app.codecov.io/gh/rodrigoesborges/sidra)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

# Introdução

O pacote `sidra` fornece uma interface simples para acessar a API de
dados do SIDRA (Sistema IBGE de Recuperação Automática), permitindo que
você consulte dados do IBGE diretamente do R, **a partir da api rest
tornada disponível em
[servicodados.ibge.gov.br](https://servicodados.ibge.gov.br/api/docs/agregados?versao=3)**.

Este documento fornece uma introdução ao pacote e apresenta exemplos
básicos para ajudá-lo a começar.

# Instalação

O pacote foi submetido ao CRAN, uma vez aceito pode instalar a versão
CRAN com:

``` r
install.packages("sidra")
```

Para instalar o pacote diretamente do GitHub, utilize o código abaixo:

``` r
# Instalar remotes, se necessário
# install.packages("remotes")

# Instalar o pacote sidra
remotes::install_github("rodrigoesborges/sidra")
```

Após a instalação, carregue o pacote com:

library(sidra)

# Funções Principais

O pacote sidra contém diversas funções para acessar diferentes seções da
API SIDRA. Abaixo, uma descrição das funções principais.

1.  Função sidra()

Esta é a função principal do pacote, que permite fazer consultas gerais
à API SIDRA com diversos parâmetros. Use esta função para acessar dados
diretamente especificando a tabela, variáveis, classificadores, períodos
e níveis geográficos.

    sidra(tabela, classificador = "", filtro_cats = "", nivel = 1, filtro_niveis = "all", periodo = "all", variavel = "allxp", inicio = NULL, fim = NULL)
        tabela: Número da tabela desejada.
        classificador: Classificador a ser detalhado. O padrão retorna todos os classificadores disponíveis.
        filtro_cats: Define subconjunto do classificador.
        nivel: Define o nível geográfico, por exemplo, N1 para Brasil, N6 para Município.
        filtro_niveis: Define um subconjunto do nível especificado.
        periodo: Período dos dados; "all" para todos os períodos disponíveis.
        variavel: Variáveis a serem retornadas; "allxp" exclui variáveis calculadas pela SIDRA.
        inicio, fim: Início e fim do período desejado.

2.  Função tab_search() - Busca de Tabelas e Agregados

A função tab_search() permite buscar tabelas, agregados ou variáveis da
SIDRA que contenham o termo especificado. Esta função é útil quando você
deseja encontrar tabelas ou variáveis específicas sem conhecer o número
exato da tabela.

Essa função retorna uma lista de tabelas ou variáveis que possuem o
termo especificado na descrição. É útil para encontrar rapidamente as
tabelas que contêm os dados que você deseja consultar.

    tab_search(termo): Retorna uma lista de agregados ou variáveis que contêm o termo buscado.
        termo: Termo de busca em texto. A função pesquisa o termo em descrições de tabelas e variáveis.

A função retorna um data frame com três colunas:

    ID do Agregado/Tabela: Número identificador do agregado ou tabela.
    Descrição: Descrição do agregado ou variável contendo o termo buscado.
    Variável: Indica se o item retornado é uma variável (TRUE) ou uma tabela/agregado (FALSE).

3.  Funções para Fonte dos Dados (tab_fonte.R)

Essa função retorna a fonte dos dados, i.e. a Pesquisa primária fonte,
para uma tabela específica, permitindo entender a origem e
confiabilidade dos dados.

    tab_fonte(tabela): Retorna a fonte de dados para a tabela especificada.
        tabela: Número da tabela de interesse.

4.  Funções para Metadados da Tabela (tab_meta.R)

Essas funções fornecem metadados sobre uma tabela específica, oferecendo
informações detalhadas sobre o conteúdo da tabela.

    tab_meta(tabela): Retorna metadados para uma tabela específica.
        tabela: Número da tabela de interesse.

## Exemplos de Uso

Aqui estão exemplos de como usar essas funções para consultar dados
específicos na API SIDRA.

### Procurar por termos

Aqui está um exemplo de como usar tab_search() para buscar tabelas e
variáveis que contêm o termo “produção”.

``` r
# Carregar o pacote
library(sidra)

# Buscar tabelas e variáveis que contenham "produção" na descrição
resultados <- tab_search("produção")
print(resultados)
```

Esse comando retornará uma lista de tabelas e variáveis que contêm
“produção” na descrição. Essa funcionalidade é útil para explorar as
tabelas disponíveis na SIDRA quando você não conhece o número exato da
tabela ou variável que está procurando.

### Obter Metadados da Tabela

Para acessar os metadados de uma tabela específica, como a tabela 1612:

``` r
metadados <- tab_meta(1612)
print(metadados)
```

### Listar Variáveis Disponíveis

Para listar as variáveis disponíveis para uma tabela específica, como a
tabela 1612:

``` r
variaveis <- tab_vars(1612)
print(variaveis)
```

### Consultar Dados Específicos com sidra()

A função sidra() permite fazer consultas específicas de dados. Neste
exemplo, buscamos dados da tabela 1612, com o classificador 81, no nível
geográfico de Estados.

``` r
dados <- sidra(1612, classificador = 81, nivel = 3)
head(dados)
```

# Avisos e Dicas

Limites de consulta: Algumas consultas podem exceder o limite de 100.000
registros permitido pela API do IBGE. Nesse caso, por definição o pacote
busca dividir a consulta em requisições menores a partir de segmentação
dos períodos requisitados. Ainda que robusto, pode não funcionar para
todos os casos, pelo qual sugerimos faça a segmentação manualmente da
requisição se necessário.

Níveis e Classificadores: Ao utilizar filtro_niveis ou filtro_cats,
certifique-se de que eles tenham o mesmo tamanho do argumento nivel ou
classificador, respectivamente.

# Conclusão

O pacote sidra facilita a consulta aos dados do IBGE, possibilitando um
fluxo de trabalho mais ágil para análises de dados diretamente no R.
Para maiores informações, visite a documentação da API SIDRA e explore
as funções adicionais do pacote.

Para maiores detalhes, consulte a vinheta [Introdução ao
sidra](https://github.com/rodrigoesborges/sidra/blob/main/vignettes/sidra.Rmd)
.
