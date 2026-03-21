### 1. Data

# Use daily-based (crude) weekly return data from the S&P 100 in 2017
# This scenario set contains 30 stocks with the highest average trading 
# volume over 2017
data(sp100w17av30s)

### 2. Optimization with default parameters (Long-only Markowitz)

# One-Liner
model <- optimal.portfolio(scenario.set)

# Plot the result
barplot(x(model), las=3, names.arg=colnames(scenario.set))

### 3. Repeat optimization but explicitely create a model

model <- portfolio.model(scenario.set)
markowitz <- optimal.portfolio(model)

### 4. Compute a long-only MAD portfolio

# We do so by changing one parameter of the model, i.e. the specification
# of the objective function and repeat the optimization

mad <- optimal.portfolio(objective(model, "mad"))

### 5. Plot portfolio differences between Markowitz and MAD

barplot(matrix(c(x(markowitz), x(mad)), nrow=2, byrow=TRUE), 
        beside=TRUE, las=3, names.arg=colnames(scenario.set), 
        legend=c("Markowitz", "MAD"))
