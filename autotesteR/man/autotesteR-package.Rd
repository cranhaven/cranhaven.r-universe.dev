\name{autotesteR-package}
\alias{autotesteR-package}
\alias{autotesteR}
\docType{package}
\title{
Pacote autotesteR: Testes estatísticos automatizados com foco em acessibilidade e comunicação visual
}
\description{
O pacote \code{autotesteR} oferece uma interface amigável para a realização de testes estatísticos comuns, com foco na apresentação gráfica dos resultados, checagem automática de pressupostos e uso da língua portuguesa.
}
\details{
A proposta do \code{autotesteR} é simplificar a aplicação de testes estatísticos frequentemente usados na pesquisa científica — como o teste t, ANOVA, Mann-Whitney, correlação de Pearson, entre outros —, aliando visualizações claras, diagnósticos automáticos e recomendações baseadas nas características dos dados.

As funções atualmente disponíveis incluem:

\itemize{
  \item \code{\link{pre.teste}} — sugere automaticamente o teste mais apropriado para os dados;
  \item \code{\link{teste.t}} — teste t de Student para dois grupos independentes;
  \item \code{\link{teste.tpareado}} — teste t pareado para medidas repetidas;
  \item \code{\link{teste.u}} — teste de Mann-Whitney para comparação não paramétrica entre dois grupos;
  \item \code{\link{teste.anova}} — ANOVA de uma via para mais de dois grupos;
  \item \code{\link{teste.kruskal}} — teste de Kruskal-Wallis (alternativa não paramétrica à ANOVA);
  \item \code{\link{teste.qui}} — teste do qui-quadrado para tabelas de contingência;
  \item \code{\link{teste.fisher}} — teste exato de Fisher para tabelas 2x2;
  \item \code{\link{teste.correlacao}} — teste de correlação de Pearson ou Spearman.
}

Todas as funções estão documentadas em português, aceitam objetos do tipo \code{data.frame} e fórmulas como \code{variavel ~ grupo}, além de retornarem resumos claros e gráficos integrados.
}
\author{
Luiz Fernando Cardoso Garcia \email{luiz.cardoso@ufpr.br}
}
\seealso{
\code{\link{pre.teste}},
\code{\link{teste.t}},
\code{\link{teste.tpareado}},
\code{\link{teste.u}},
\code{\link{teste.anova}},
\code{\link{teste.kruskal}},
\code{\link{teste.qui}},
\code{\link{teste.fisher}},
\code{\link{teste.correlacao}}
}
\examples{
# Acesse a função principal para testes automáticos
g1 <- c(1, 2, 3)
g2 <- c(4, 5, 6)
pre.teste(g1, g2)
}
