
The method used to calculate doses after an exposure to mixed field irradiation (gamma plus neutrons) is the analytical method (Fornalski 2014; Pacyniak et al. 2015). This method was proposed to automate the iterative algorithm used for calculating the absorbed doses in mixed radiation fields and exclude the propagation of uncertainty effect. 
The uncertainties associated with the estimated doses are calculated using the delta method.

This method assumes additivity. It means, that the dicentrics are formed independently by the neutron and gamma components.
$$
Y_{n+\gamma}(D_n,D_\gamma) = Y_0 + \alpha_n D_n + \alpha_\gamma D_\gamma + \beta_\gamma D_\gamma^2
$$
The ratio r of gamma (𝐷𝛾) to neutron (𝐷𝑛) absorbed doses should be given after considering the type of accident (i.e., critical or a nuclear detonation) and the distance and possible shielding structures between the source and the position of the victim.

#### Data Entry
Curve data can be entered in two ways:
 - Manually: Input the data directly into the system using the provided tables.
 - Uploading a File: Upload a .rds file containing the curve data.

Depending on the available case data, it can be entered in one of the following formats:
 - Distribution Table: A table with the distribution of dicentrics (e.g., C0, C1, C2, C3, C4, etc.).
 - Total Counts: Only the total number of cells and dicentrics.

#### Requirements
To perform the calculations, the following requirements are necessary:
 - Gamma curve
 - Neutron curve
 - Case data (suports multiple cases)
 - Ratio of gamma to neutron dose. 
