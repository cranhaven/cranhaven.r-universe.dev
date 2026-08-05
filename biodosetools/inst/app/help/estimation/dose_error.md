The following methods for error calculation of the dose, $D$, can be selected:

- **Merkle's method**: considering both curve calibration and measurement errors.

 By default, a 83% confidence interval (CI) is used for both curve calibration and measurement errors. This results in a total CI of 95%.
In case no variance-covariance matrix is provided, the measurement errors CI will be adjusted accordingly to have a resulting total CI of 95%.

- **Delta method**: derived from propagation of error, expresses the variance on $D$ in terms of the variances and co-variances of $C$, $\alpha$, $\beta$ and $\lambda$.
