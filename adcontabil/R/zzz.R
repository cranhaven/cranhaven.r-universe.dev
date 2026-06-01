#' Converte texto contabil brasileiro para numero negativo quando necessario
#'
#' Esta funcao transforma strings representando valores contabeis no formato brasileiro
#' (com virgula decimal, ponto como separador de milhar e uso de parenteses para indicar valor negativo)
#' em valores numericos padrao do R.
#'
#' @param vetor Vetor de caracteres contendo os valores a serem convertidos.
#'
#' @return Vetor numerico com os valores convertidos.
#'
#' @keywords internal
#'
conv_br_numeric <- function(vetor) {
  vetor <- gsub("\\(", "-", vetor)      # troca "(" por "-"
  vetor <- gsub("\\)", "", vetor)       # remove ")"
  vetor <- gsub("\\.", "", vetor)       # remove ponto (milhar)
  vetor <- gsub(",", ".", vetor)        # troca virgula decimal por ponto
  as.numeric(vetor)                 # converte para numerico
}


#' Classifica contas contabeis de acordo com categorias predefinidas
#'
#' A funcao recebe o nome de uma conta e retorna sua categoria contabil conforme a lista `categorias`.
#'
#' @param conta Um vetor de caracteres com o(s) nome(s) da(s) conta(s) a classificar.
#'
#' @return Um vetor de caracteres com a categoria correspondente ou \code{NA} se nao classificada.
#'
#' @keywords internal
#'
classificar_conta_bp <- function(conta) {
  for (cat in names(categorias_bp)) {
    if (conta %in% categorias_bp[[cat]]) {
      return(cat)
    }
  }
  return(NA)  # Se a conta nao se encaixar, retorna NA
}


#' Lista de categorias contabeis utilizadas internamente
#'
#' Este objeto lista, em formato de `list`, as categorias contabeis utilizadas
#' para classificacao de contas em funcoes internas do pacote.
#'
#' As chaves representam os grupos (por exemplo, ACF = Ativo Circulante Financeiro),
#' e os vetores associados contem os nomes das contas que pertencem a cada grupo.
#'
#' @format Uma lista nomeada com vetores de caracteres.
#'
#' @keywords internal
#'
categorias_bp <- list(
  ACF = c("caixa e equivalentes de caixa", "aplicacoes financeiras", "titulos e valores mobiliarios"),
  ACO = c("contas a receber de clientes", "adiantamento a fornecedores", "estoques",
          "tributos a recuperar", "outros ativos circulantes"),
  PCF = c("emprestimos e financiamentos", "debentures", "operacoes de credito"),
  PCO = c("fornecedores", "obrigacoes trabalhistas e previdenciarias",
          "tributos a pagar", "adiantamento de clientes", "outras obrigacoes circulantes"),
  ANC = c("ativo nao circulante"),
  PNC = c("passivo nao circulante"),
  PL  = c("patrimonio liquido", "capital social", "reservas de lucros",
          "prejuizos acumulados", "ajustes de avaliacao patrimonial")
)


#' Normaliza texto removendo acentos e cedilha
#'
#' Função interna utilizada nos processos de padronização textual.
#' Não deve ser chamada diretamente pelo usuário final.
#'
#' @param x Vetor de caracteres a ser normalizado.
#' @return Vetor de caracteres normalizado.
#' @keywords internal
#'
normalizar_texto <- function(x) {
  x |>
    stringi::stri_trans_nfc() |> # normaliza Unicode
    tolower() |>                 # converte para minusculas
    stringi::stri_trans_general("NFD; [:Nonspacing Mark:] Remove; NFC")
}


# Declara variáveis globais para evitar NOTEs no R CMD check
utils::globalVariables(c("Categoria", "Conta"))


#' Lista de categorias de contas utilizadas na Demonstração do Resultado do Exercício (DRE)
#'
#' Este objeto contém a estrutura de categorias aplicada para classificação de contas
#' referentes à Demonstração do Resultado do Exercício. Cada elemento da lista representa
#' uma categoria da DRE e contém um vetor de caracteres com os nomes das contas que
#' pertencem a essa classificação.
#'
#' As denominações refletem os grupos contábeis usuais, tais como:
#' * Receita Bruta
#' * Deduções e Impostos sobre Vendas
#' * Receita Líquida
#' * Custo das Vendas
#' * Lucro Bruto
#' * Despesas Operacionais
#' * Resultado Financeiro
#' * Resultado Antes do Imposto de Renda
#' * Resultado Líquido do Exercício
#'
#' @format Uma lista nomeada, onde cada elemento é um vetor de caracteres contendo os
#'         nomes das contas associadas à respectiva categoria.
#'
#' @details
#' Esta lista é utilizada internamente por funções de classificação automatizada,
#' servindo como referência padronizada para identificação dos agrupamentos de contas.
#' A precisão da classificação depende diretamente da aderência dos nomes das contas
#' aos termos definidos neste objeto.
#'
#' @keywords internal
#'
#'
categorias_dre <- list(
  RECEITA_BRUTA = c("receita bruta de vendas", "receita operacional bruta", "vendas de mercadorias", "vendas de produtos"),
  DEDUCOES = c("devolucoes de vendas", "abatimentos", "impostos sobre vendas", "pis", "cofins", "icms"),
  RECEITA_LIQUIDA = c("receita liquida de vendas", "receita operacional liquida"),
  CUSTO_VENDAS = c("custo das mercadorias vendidas", "custo dos produtos vendidos", "custo dos servicos prestados"),
  LUCRO_BRUTO = c("lucro bruto"),
  DESPESAS_OPERACIONAIS = c("despesas com vendas", "despesas administrativas", "despesas gerais e administrativas"),
  OUTRAS_RECEITAS = c("outras receitas operacionais", "outras receitas"),
  OUTRAS_DESPESAS = c("outras despesas operacionais", "outras despesas"),
  RESULTADO_FINANCEIRO = c("receitas financeiras", "despesas financeiras", "resultado financeiro liquido"),
  RESULTADO_ANTES_IR = c("resultado antes do imposto de renda", "lucro antes do imposto de renda", "lucro antes dos tributos"),
  IMPOSTO_RENDA = c("imposto de renda", "contribuicao social sobre o lucro", "csll"),
  RESULTADO_LIQUIDO = c("lucro liquido", "prejuizo liquido", "lucro liquido do exercicio")
)


#' Classifica contas específicas da Demonstração do Resultado do Exercício (DRE)
#'
#' Esta função recebe o nome de uma conta e identifica sua categoria dentro das
#' classes previstas para a Demonstração do Resultado do Exercício. A classificação
#' é realizada com base na lista interna `categorias_dre`, que associa cada categoria
#' aos respectivos nomes de contas normalmente encontrados em demonstrações contábeis.
#'
#' O objetivo principal é padronizar a identificação das contas, facilitando etapas
#' subsequentes de análise, agrupamento, sumarização e composição de indicadores.
#'
#' @param conta Um vetor de caracteres contendo o nome da conta a ser classificada.
#'
#' @return Um vetor de caracteres indicando a categoria da DRE correspondente ao valor
#'         informado. Caso o nome não seja reconhecido entre as categorias mapeadas,
#'         retorna \code{NA}.
#'
#' @details
#' A função realiza uma busca direta por correspondência exata entre o valor de
#' \code{conta} e os elementos registrados na lista `categorias_dre`. Assim,
#' recomenda-se que os nomes das contas sejam previamente normalizados, removendo
#' acentos, padronizando letras minúsculas e eliminando variações ortográficas,
#' a fim de maximizar a precisão da classificação.
#'
#' @keywords internal
#'
classificar_conta_dre <- function(conta) {
  for (cat in names(categorias_dre)) {
    if (conta %in% categorias_dre[[cat]]) {
      return(cat)
    }
  }
  return(NA)
}
