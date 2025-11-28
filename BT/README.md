# BT : (Adaptive) Boosting Tree for Poisson distributed response variable.

## Acknowledgements

The author thanks Julien Trufin and Michel Denuit for their support during the project.

The idea behind (A)BT is akin to the Gradient Boosting Methods. 
This package is therefore inspired by the `gbm3` one, originally written by Greg Ridgeway <greg.ridgeway@gmail.com> (for more details, please refer to the following [url](https://github.com/gbm-developers/gbm3)).
Some of the developed codes are then pretty similar.

## References

This package is based on the original idea proposed by M. Denuit, D. Hainaut and J. Trufin. We refer the user to the following
books and articles:

* M. Denuit, D. Hainaut and J. Trufin (2019). [**Effective Statistical Learning Methods for Actuaries |: GLMs and Extensions**](https://link.springer.com/book/10.1007/978-3-030-25820-7), *Springer Actuarial*.
* M. Denuit, D. Hainaut and J. Trufin (2019). [**Effective Statistical Learning Methods for Actuaries ||: Tree-Based Methods and Extensions**](https://link.springer.com/book/10.1007/978-3-030-57556-4), *Springer Actuarial*.
* M. Denuit, D. Hainaut and J. Trufin (2019). [**Effective Statistical Learning Methods for Actuaries |||: Neural Networks and Extensions**](https://link.springer.com/book/10.1007/978-3-030-25827-6), *Springer Actuarial*.
* M. Denuit, D. Hainaut and J. Trufin (2022). **Response versus gradient boosting trees, GLMs and neural networks under Tweedie loss and log-link**. 
Accepted for publication in *Scandinavian Actuarial Journal*.
* M. Denuit, J. Huyghe and J. Trufin (2022). **Boosting cost-complexity pruned trees on Tweedie responses: The ABT machine for insurance ratemaking**.
Paper submitted for publication.
* M. Denuit, J. Trufin and T. Verdebout (2022). **Boosting on the responses with Tweedie loss functions**. Paper submitted for publication.

## Package builder and maintainer

This package has been written and is currently maintained by Gireg Willame <gireg.willame@gmail.com>.
All remarks/suggestions/improvements are warmly welcome.

## Details and installation

Non-production releases (bug fixes, mostly) will be released via the GitHub
release workflow. To install from GitHub, first install `devtools` from CRAN:

```r
install.packages("devtools")
```

Then install the `BT` package from GitHub:

```r
library("devtools")
install_github("GiregWillame/BT")
```
