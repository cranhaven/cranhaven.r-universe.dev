The characteristic limits, i.e. the decision threshold and the detection limit can be calculated based on the assumption that the counts are poisson distributed. The calculations are performed as described in ISO 19238. The user can choose the type I error rate $\alpha$ (false positive rate) and the type II error rate $\beta$ (false negative rate).

If possible, the user should choose `Number of dicentrics and cells` and provide the number of dicentrics and cells of the control data. This accounts for the uncertainty of the control data as well as for the uncertainty of the case data. If only the mean number of dicentrics per cell is available, the user should choose `Dicentrics per cell`. This does not account for the uncertainty of the control data and should not be used for relatively low cell numbers.

The user can either choose manually the cell number of a hypothetical case or the calculation is performed for cell numbers of $N=20, 50, 100, 200, 500, 1000$ by default.

Moreover, the user can upload a curve as a .rds file or manually in order to calculate the `Minimum resolvable dose (Gy)` and the `Dose at detection limit (Gy)`.

The output can be interpreted in the following way:

- Decision threshold: If the observed number of dicentrics exceeds the decision threshold, this means that there is evidence to refute the null hypothesis (H0) of no significant difference between the background and observed numbers
of dicentrics (i.e. the probability for wrongly rejecting H0 is less than $\alpha/2$). If an appropriate calibration curve is available, the calculation of observed absorbed dose shall then be carried out and reported. If not, then the detection limit should be reported.

- Detection limit: If the true number of dicentrics is higher than the detection limit, the probability of observing a lower number of dicentrics than the decision threshold (or the probability to infer false negative conclusions) is lower than the chosen type II error rate $β$.

