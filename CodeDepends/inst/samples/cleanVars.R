x = 1:10
y = 10 + 3*x + rnorm(length(x))
z = 3

d = data(x = x, y = y)

hist(x)

fit = lm(y ~ x, d)
plot(fit)
summary(fit)
coef(fit)




