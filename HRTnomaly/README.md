# Historical, Relational, and Tail Anomaly-Detection Algorithms

Authors: <img src="man/figures/logo.svg" align="right" alt="HRTnomaly logo" style="width:166px; height:192px; padding:2px;" />
[Luca Sartore](mailto://luca.sartore@usda.gov)[<img alt="ORCID iD" src="https://cran.r-project.org/web/orcid.svg" width="16px" height="16px" style="width:16px; height:16px; margin-left:4px; margin-right:4px; vertical-align:middle">](https://orcid.org/0000-0002-0446-1328),
[Lu Chen](mailto://lu.chen@usda.gov)[<img alt="ORCID iD" src="https://cran.r-project.org/web/orcid.svg" width="16px" height="16px" style="width:16px; height:16px; margin-left:4px; margin-right:4px; vertical-align:middle">](https://orcid.org/0000-0003-3387-3484),
[Justin van Wart](mailto://justin.vanwart@usda.gov),
[Andrew Dau](mailto://andrew.dau@usda.gov)[<img alt="ORCID iD" src="https://cran.r-project.org/web/orcid.svg" width="16px" height="16px" style="width:16px; height:16px; margin-left:4px; margin-right:4px; vertical-align:middle">](https://orcid.org/0009-0008-9482-5316), and
[Valbona Bejleri](mailto://valbona.bejleri@usda.gov)[<img alt="ORCID iD" src="https://cran.r-project.org/web/orcid.svg" width="16px" height="16px" style="width:16px; height:16px; margin-left:4px; margin-right:4px; vertical-align:middle">](https://orcid.org/0000-0001-9828-968X)

Maintainer: [Luca Sartore](mailto://drwolf85@gmail.com)

[![](https://www.r-pkg.org/badges/version/HRTnomaly)](https://CRAN.R-project.org/package=HRTnomaly)
[![License: AGPL-3.0](https://img.shields.io/badge/License-AGPL3-088800.svg)](https://www.gnu.org/licenses/agpl-3.0.en.html)
[![DOI](https://zenodo.org/badge/doi/10.6339/24-JDS1136.svg)](https://www.researchgate.net/profile/Luca-Sartore-2/publication/382981292_Identifying_Anomalous_Data_Entries_in_Repeated_Surveys/links/66d4cb242390e50b2c268732/Identifying-Anomalous-Data-Entries-in-Repeated-Surveys.pdf)
[![DOI](https://zenodo.org/badge/doi/10.3390/stats7040073.svg)](https://www.researchgate.net/profile/Luca-Sartore-2/publication/385085550_Empirical_Inferences_Under_Bayesian_Framework_to_Identify_Cellwise_Outliers/links/671501df09ba2d0c760eab89/Empirical-Inferences-Under-Bayesian-Framework-to-Identify-Cellwise-Outliers.pdf)
[![CRAN RStudio mirror downloads](https://cranlogs.r-pkg.org/badges/HRTnomaly)](https://www.r-pkg.org/pkg/HRTnomaly)
[![Total Downloads from CRAN RStudio mirror](https://cranlogs.r-pkg.org/badges/grand-total/HRTnomaly?color=orange)](https://CRAN.R-project.org/package=HRTnomaly)
[![Mentioned in Awesome Official Statistics ](https://awesome.re/mentioned-badge.svg)](https://github.com/SNStatComp/awesome-official-statistics-software)

*This research was supported by the U.S. Department of Agriculture, National Agriculture Statistics Service. The findings and conclusions in this publication are those of the authors and should not be construed to represent any official USDA, or US Government determination or policy.*

## Features of the package

The presence of outliers in a dataset can substantially bias the results of statistical analyses. To correct for outliers, micro edits are manually performed on all records. A set of constraints and decision rules is typically used to aid the editing process. However, straightforward decision rules might overlook anomalies arising from disruption of linear relationships.

Computationally efficient methods are provided to identify historical, tail, and relational anomalies at the data-entry level (Sartore et al., 2024b). A score statistic is developed for each anomaly type, using a distribution-free approach motivated by the Bienaymé-Chebyshev's inequality, and fuzzy logic is used to detect cellwise outliers resulting from different types of anomalies. Each data entry is individually scored and individual scores are combined into a final score to determine anomalous entries. In contrast to fuzzy logic, Bayesian bootstrap and a Bayesian test based on empirical likelihoods are also provided as studied by Sartore et al. (2024a). These algorithms allow for a more nuanced approach to outlier detection, as it can identify outliers at data-entry level which are not obviously distinct from the rest of the data.

## Example

```{R}
# Load the package
library(HRTnomaly)
# Load the toy dataset
data(toy)
# Detect cellwise outliers
res <- cellwise(toy, contamination = 0.05, epochs = 10L)
# Check the results with ground truth data
class_check(res$outlier, res$anomaly_flag != "")
```

## References

Agostinelli C, Leung A, Yohai VJ, Zamar RH (2015). Robust estimation of multivariate location and scatter in the presence of cellwise and casewise contamination. *Test*, 24(3): 441-461.

Alqallaf F, Van Aelst S, Yohai VJ, Zamar RH (2009). Propagation of outliers in multivariate data. *The Annals of Statistics*, 311-331.

Bienaymé IJ (1867). Considérations à l'appui de la découverte de Laplace sur la loi de probabilité dans la méthode des moindres carrés. *Journal de Mathématiques Pures et Appliquées*, 2(12): 158-176.

Chepulis MA, Shevlyakov G (2020). On outlier detection with the Chebyshev type inequalities. *Journal of the Belarusian State University. Mathematics and Informatics*, 3: 28-35.

Filzmoser P, Gregorich M (2020). Multivariate outlier detection in applied data analysis: Global, local, compositional and cellwise outliers. *Mathematical Geosciences*, 52(8): 1049-1066.

Gupta MM, Qi J (1991). Theory of t-norms and fuzzy inference methods. *Fuzzy Sets and Systems*, 40(3): 431-450.

Huber PJ, Ronchetti EM (1981). *Robust statistics*. John Wiley & Sons, New York.

O'Gorman TJ (1994). The effect of cosmic rays on the soft error rate of a DRAM at ground level. *IEEE Transactions on Electron Devices*, 41(4): 553-557.

Rousseeuw PJ, Van den Bossche W (2018). Detecting deviating data cells. *Technometrics*, 60(2): 135-145.

Sandqvist AP (2016). Identifizierung von Ausreissern in eindimensionalen gewichteten Umfragedaten. *KOF Analysen*, 2016(2): 45-56.

Sartore L, Chen L, Bejleri V (2024). Empirical Inferences Under Bayesian Framework to Identify Cellwise Outliers. *Stats*, **7**: 1244-1258.

Sartore L, Chen L, van Wart J, Dau A, Bejleri V (2024). Identifying Anomalous Data Entries in Repeated Surveys. *Journal of Data Science*, **22**(3): 436-455.

Tchebichef P (1867). Des valeurs moyennes. *Journal de Mathématiques Pures et Appliquées*, 2(12): 177-184.
