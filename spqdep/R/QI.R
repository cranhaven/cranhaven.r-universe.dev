QI <- function(map1=map1,map2=map2){
R <- length(map1)

# Transformar factores en numeros
levels(map1) <- as.character(1:length(levels(map1)))
map1 <- as.numeric(map1)
levels(map2) <- as.character(1:length(levels(map2)))
map2 <- as.numeric(map2)

k=max(map1)
map12=10*map1+map2
## Obtencion de los simbolos (12,13,..,21,23,..,31,32,34,...) y sus probab
symb <- numeric()
h <- 0
for (i in 1:k){
  for (j in 1:k){
    if (i!=j){
      h <- h+1
      symb[h] <- 10*i+j}
  }
}

## Simbolizo la serie
nusi=length(symb)
nsk <- numeric()
for (s in 1:nusi){
  nsk[s] <- sum(map12==symb[s])
}

# La frecuencia de los simbolos
nsx <- table(map1)
nsy <- table(map2)
lns <- log((nsk>0)*nsk)
lnsX <- log((nsx>0)*nsx)
lnsY <- log((nsy>0)*nsy)

hXY <- -sum((nsk/R)*lns)
hX  <- -sum((nsx/R)*lnsX)
hY  <- -sum((nsy/R)*lnsY)
QI  <- hXY-hX-hY
}
