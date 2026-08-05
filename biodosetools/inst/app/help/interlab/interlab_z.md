#### Z-Score

the deviation of each laboratory’s reported frequency (or estimated dose) from the robust average (or the physical dose) respectively is calculated using (Di Giorgio et al., 2011):

$$
z= \frac{x_i- x_{\text{ref}}}{\sqrt{s^{*2} + u_{\text{ref}}^{2}}}
$$
Where:
- $x_i$ is the estimated dose or the reported value for the frequency of observed aberrations.
- $x_{\text{ref}}$ is the reference dose value or the robust average for frequency (then $x_{\text{ref}}$  = $x^{*}$).
- $s^{*}$ is the robust standard deviation.
- $u_{\text{ref}}$ is the standard uncertainty of $x_{\text{ref}}$. 

To obtain the robust standard deviation (s*) and the robust average for frequency (x*) three algorithms are available (González et al., 2023): 

- the Algorithm A that applies the hubers() function from the MASS R-project package (Venables and Ripley 2002); 

- the Algorithm B (Rousseeuw and Verboven 2002; Szewczak and Bondarzewski 2016), also named as the logistic M-estimator in section D.1.4.2 of the ISO 13528;

- the Q/Hampel algorithm, described in detail in the section C5 of the ISO/IEC 13528 and in the German DIN 38402-45.


##### Recommendations

The Algorithm B is recommended for estimation in very small samples in the ISO 13258 standard. This algorithm should be considered as a candidate to estimate the standard deviation in interlaboratory comparisons (ILCs) when the proportion of identical values is less than the half of the results.

The results of González et al., 2023 support the use of Algorithm B over A to estimate the dispersion parameter in the case of regional comparisons with a small number of experienced laboratories. 

The Q/Hampel algorithm seems like a notable candidate to be applied for mean parameter estimation in the ILCs by dicentric assay (González et al., 2023).  

Without the presence of atypical values, the Algorithm A is the closest to the arithmetic.

