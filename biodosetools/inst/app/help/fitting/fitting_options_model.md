#### Fitting model

Normally the Automatic option should be used. 

- If there is overdispersion on the fitting, the quasi-Poission model will be used.
- Otherwise a Poisson model assuming equidispersion will be used.

With the automatic option, when the model dispersion index obtained in the fitting process is equal or lower than 1, a Poisson model is used. For values higher than 1, quasi-Poisson model is used instead.
