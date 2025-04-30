#### Data format

The only requirement for the data is to be in `.csv` (or `.txt`) format and two have the following structure:

- The columns should include headers.
- The remaining columns should contain the cell distributions of dicentrics `CX`, where `X` can `0, 1, 2, 3`...

An example of how the data should be formatted can be seen below.

```
C0 ,C1,C2,C3,C4,C5
104,72,15,2 ,0 ,0
```

The rest of columns will be calculated automatically after clicking <button class="btn btn-default action-button inputs-button shiny-bound-input small-action-button"  type="button">Calculate parameters</button>:

- `N` is the total number of cells.
- `X` is the number of aberrations.
- `Fp` is the observed yield, and `Fp_err` is its standard error.
- `DI` is the dispersion index $\sigma^{2}/y$.
- `u` is the $u$-value, which for a Poisson distribution should be unity.
- `Xc` translocation frequency to be substracted from the observed yield.
- `Fg` is the full genome yield, and `Fg_err` is its standard error.

Note that `Fg` already takes into account the correction applied by the selected confounders.
