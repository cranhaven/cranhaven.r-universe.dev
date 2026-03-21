### 1. Data

# Use daily-based (crude) weekly return data from the S&P 100 in 2017
# This scenario set contains 30 stocks with the highest average trading 
# volume over 2017
data(sp100w17av30s)

### 2. Calculate optimal portfolios with different risk measures

# 2a. Markowitz
markowitz <- model <- optimal.portfolio(scenario.set)

# 2b. Expected Shortfall/CVaR with alpha=95% and alpha=90%
cvar95 <- optimal.portfolio(objective(model, "expected.shortfall"))
cvar90 <- optimal.portfolio(alpha(cvar95, 0.1))

# 2c. Mean Absolute Deviation (MAD)
mad <- optimal.portfolio(objective(model, "mad"))

### 3. Plot Comparison

compare <- matrix(c(x(markowitz), x(mad), x(cvar95), x(cvar90)), 
                  nrow=model$assets, byrow=FALSE)
barplot(t(compare), beside=TRUE, las=3, names.arg=colnames(scenario.set), 
        legend=c("Markowitz", "MAD", "CVaR (95%)", "CVaR (90%)"))

### 4. Add upper bounds (0.15) and repeat optimizations

markowitz <- model <- optimal.portfolio(upper.bound(portfolio.model(scenario.set), 0.15))
cvar95 <- optimal.portfolio(objective(model, "expected.shortfall"))
cvar90 <- optimal.portfolio(alpha(cvar95, 0.1))
mad <- optimal.portfolio(objective(model, "mad"))

compare <- matrix(c(x(markowitz), x(mad), x(cvar95), x(cvar90)), 
                  nrow=model$assets, byrow=FALSE)
barplot(t(compare), beside=TRUE, las=3, names.arg=colnames(scenario.set), 
        legend=c("Markowitz", "MAD", "CVaR (95%)", "CVaR (90%)"))
