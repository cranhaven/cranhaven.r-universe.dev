## 0.3

### Features

- Add more unit tests
- Add R6 documentation
- Generalize the RandomForest model to classification and regression
- Update vignettes

### Bugfixes

- Fix ranger package to version 0.11.2
- Terminal nodes were wrongly calculated
- Fix Armadillo deprecation warnings


## 0.22

### Bugfixes

- Weighted distance calculation was wrongly calculated, namely: $abs(sum(x-y))$ to $sum(abs(x-y))$. This has an effect on all linear models.