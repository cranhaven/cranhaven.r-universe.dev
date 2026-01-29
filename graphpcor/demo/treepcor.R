## for details see
## https://link.springer.com/article/10.1007/s10260-025-00788-y

library(graphpcor)

if(FALSE) {
    
### examples of what is not allowed
    treepcor(p1 ~ p2)
    treepcor(p1 ~ c2)
    
    treepcor(
        p1 ~ c1 + c2,
        p2 ~ c3)
    
    treepcor(
        p1 ~ c1 + c2,
        p2 ~ p1 + c2 + c3)
    
    treepcor(
        p1 ~ c1 + c2,
        p2 ~ p3 + c2 + c3)
    
    treepcor(
        p1 ~ p2 + c1 + c2,
        p2 ~ c2 + c3)

}

### allowed cases

## 3 children and 1 parent
g1 <- treepcor(p1 ~ c1 + c2 - c3)

g1

dim(g1)

summary(g1)

plot(g1)

prec(g1)

(q1 <- prec(g1, theta = c(0)))

v1 <- chol2inv(chol(q1))

v1

cov2cor(v1)

vcov(g1)
vcov(g1, theta = 0)
vcov(g1, theta = -1)
vcov(g1, theta = 1)

cov2cor(vcov(g1))
cov2cor(vcov(g1, theta = -1))
cov2cor(vcov(g1, theta = 1))

## 4 children and 2 parent
g2 <- treepcor(
    p1 ~ p2 + c1 + c2,
    p2 ~ c3 - c4)
g2
dim(g2)
summary(g2)

plot(g2)

prec(g2)
prec(g2, theta = c(0, 0))
prec(g2, theta = c(-1, 1))

cov2cor(solve(prec(g2, theta = c(0,0))))
vcov(g2, theta = c(0,0))
vcov(g2, theta = c(log(4:1), 0,0))

vcov(g2)

g2

## 4 children and 2 parent (notice the signs)
g3 <- treepcor(
    p1 ~ -p2 + c1 + c2,
    p2 ~ -c3 + c4)
g3
dim(g3)
summary(g3)

summary(g2)
summary(g3)

par(mfrow = c(1, 2), mar = c(0,0,0,0))
plot(g2)
plot(g3)

prec(g2)
prec(g3)

prec(g2, theta = c(0, 0))
prec(g3, theta = c(0, 0))

vcov(g2, theta = c(0, 0))
vcov(g3, theta = c(0, 0))

g3


drop1(g2)
drop1(g3) 

prec(g3)
prec(drop1(g3))

n3 <- dim(g3)[1]
all.equal(
    solve(prec(g2, theta = c(0, 0)))[1:n3, 1:n3],
    solve(prec(g3, theta = c(0, 0)))[1:n3, 1:n3]
)

vcov(g2, theta = c(0,0))
vcov(g3, theta = c(0,0))
