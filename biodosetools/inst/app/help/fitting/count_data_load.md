#### Data format

The only requirement for the data is to be in `.csv` (or `.txt`) format and two have the following structure:

- The columns should include headers.
- The first column should be named `D` and contain the doses in Gy.
- The remaining columns should contain the cell distributions of dicentrics `CX`, where `X` can `0, 1, 2, 3`...

An example of how the data should be formatted can be seen below.

```
D   ,C0  ,C1,C2,C3,C4,C5
0   ,4992,8 ,0 ,0 ,0 ,0
0.1 ,4988,14,0 ,0 ,0 ,0
0.25,1987,20,1 ,0 ,0 ,0
0.5 ,1947,55,0 ,0 ,0 ,0
0.75,1736,92,4 ,0 ,0 ,0
1   ,1064,99,5 ,0 ,0 ,0
1.5 ,474 ,76,12,0 ,0 ,0
2   ,251 ,63,17,2 ,0 ,0
3   ,104 ,72,15,2 ,0 ,0
4   ,35  ,41,21,4 ,2 ,0
5   ,11  ,19,11,9 ,6 ,3
```

The rest of columns will be calculated automatically after clicking <button class="btn btn-default action-button inputs-button shiny-bound-input small-action-button"  type="button">Calculate parameters</button>:

- `N` is the total number of cells.
- `X` is the number of aberrations.
- `DI` is the dispersion index $\sigma^{2}/y$.
- `u` is the $u$-value, which for a Poisson distribution should be unity.
