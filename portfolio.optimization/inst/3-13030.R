### 1. Data

# Use daily-based (crude) weekly return data from the S&P 100 in 2017
# This scenario set contains 30 stocks with the highest average trading 
# volume over 2017
data(sp100w17av30s)

### 2. Default Portfolio Optimization Model

model <- optimal.portfolio(scenario.set)
model <- active.extension(model, 130, 30)

# 2a. ES/CVaR 130/30
cvar13030 <- optimal.portfolio(objective(model, "expected.shortfall"))

# 2b. MAD 130/30
mad13030 <- optimal.portfolio(objective(model, "mad"))

# 3. Plot comparison
barplot(matrix(c(x(cvar13030), x(mad13030)), nrow=2, byrow=TRUE), 
        las=3, names.arg=colnames(scenario.set), beside=TRUE,
        legend=c("CVaR (95%) 130/30", "MAD 130/30"))
