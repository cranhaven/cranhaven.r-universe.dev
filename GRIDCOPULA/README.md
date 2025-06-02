# GRIDCOPULA
Proyecto sobre GRIDCOPULA


## Instalar el paquete 

```R
devtools::install_github("rogelio-sg/GRIDCOPULA@prueba",
                         auth_token = "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")
```

## Ejemplo 

```R
library(GRIDCOPULA)
library(copBasic)

U <- simCOP(n = 1000, cop = CLcop, para = 5)
gc <- estimate.gridCopula(U = U, k = 7, m = 8)

measures.grid(gc) #Calcular todas las medidas
measures.grid(gc, measures = c("rho","tau","mi")) #Calcular solo rho, tau e información mutua
```

