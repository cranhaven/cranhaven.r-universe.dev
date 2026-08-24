# autotesteR (legacy)

<!-- badges: start -->
<!-- badges: end -->

**autotesteR** é um pacote R voltado para simplificar a realização dos principais testes estatísticos usados na área de biociências, com funções amigáveis que geram gráficos automáticos e explicações claras, facilitando a vida de pesquisadores e estudantes brasileiros.

> [!NOTE]
> Este pacote não está mais em desenvolvimento ativo.  
> O desenvolvimento do projeto continuou em `autotestR`, uma versão reformulada e internacionalizada do pacote, com foco em uma interface em inglês e expansão das funcionalidades.

## Instalação

Para novas instalações e desenvolvimento ativo, utilize o pacote `autotestR`.

# Funcionalidades principais

- Teste t (independente e pareado)
- Teste de Mann-Whitney (U de Wilcoxon)
- Teste t múltiplo (Teste t ou Mann-Whitney)
- Teste qui-quadrado e teste exato de Fisher
- ANOVA com pós-teste de Tukey HSD
- Kruskal Wallis com pós-teste de Dunn
- Teste de correlação de Pearson, Spearman e Kendall com gráfico automático
- Função diagnóstica que sugere o teste estatístico mais adequado
- Gráficos intuitivos já integrados nas funções


### Uso básico

```r
library(autotesteR)

# Teste t independente
grupo1 <- rnorm(30, 10, 2)
grupo2 <- rnorm(30, 12, 2)
teste.t(grupo1, grupo2)

# Teste qui-quadrado
var1 <- sample(c("A", "B"), 100, replace = TRUE)
var2 <- sample(c("Sim", "Não"), 100, replace = TRUE)
teste.qui(var1, var2)

# Teste múltiplo (Teste t ou Mann-Whitney)
df <- data.frame(
controle   = rnorm(30, 10),
tratamento = rnorm(30, 12),
teste1     = rnorm(30, 11),
teste2     = rnorm(30, 15)
)
teste.tmulti(df)

# ANOVA com pós-teste
g1 <- rnorm(20, 5)
g2 <- rnorm(20, 7)
g3 <- rnorm(20, 6)
teste.anova(g1, g2, g3)

# Teste de correlação
x <- rnorm(30)
y <- x + rnorm(30, 0, 1)
teste.correlacao(x, y)
```


# Contato

Se tiver dúvidas, sugestões ou quiser contribuir, abra uma issue ou envie um pull request no repositório GitHub.

Obrigado por usar o autotestR!
