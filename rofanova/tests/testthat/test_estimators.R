
# test estimators ----------------------------------------------------------

library(rofanova)
 data_out<-simulate_data(scenario="one-way")
X_fdata<-data_out$X_fdata
fusem_ow<-fusem(X_fdata)
data_out<-simulate_data(scenario="one-way surface")
label=data_out$label
X_fdata<-data_out$X_fdata
fusem_tw<-fusem(X_fdata)

data_out<-simulate_data(scenario="one-way")
X_fdata<-data_out$X_fdata
funmad_ow<-funmad(X_fdata)
data_out<-simulate_data(scenario="one-way surface")
label=data_out$label
X_fdata<-data_out$X_fdata
funmad_tw<-funmad(X_fdata)
