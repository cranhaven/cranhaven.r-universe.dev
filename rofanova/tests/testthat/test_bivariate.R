

# Test one-way surface ----------------------------------------------------


data_out<-simulate_data(scenario="one-way surface")
label_1=data_out$label_1
X_fdata<-data_out$X_fdata

cores=1
B=3
# RoFanova one-way
per_list_median<-rofanova(X_fdata,label_1,B = B,family="median",cores=cores)
pvalue_median_vec<-per_list_median$pval_vec
per_list_huber<-rofanova(X_fdata,label_1,B = B,family="huber",cores=cores)
pvalue_huber_vec<-per_list_huber$pval_vec
per_list_bisquare<-rofanova(X_fdata,label_1,B = B,family="bisquare",cores=cores)
pvalue_bisquare_vec<-per_list_bisquare$pval_vec
per_list_hampel<-rofanova(X_fdata,label_1,B = B,family="hampel",cores=cores)
pvalue_hampel_vec<-per_list_hampel$pval_vec
per_list_optimal<-rofanova(X_fdata,label_1,B = B,family="optimal",cores=cores)
pvalue_optimal_vec<-per_list_optimal$pval_vec



# Test two-way surface ----------------------------------------------------

data_out<-simulate_data(scenario="two-way surface")
label_1=data_out$label_1
label_2=data_out$label_2
X_fdata<-data_out$X_fdata


cores=1
B=3
# RoFanova two-way
per_list_median<-rofanova(X_fdata,label_1,label_2,B = B,family="median",cores=cores)
pvalue_median_vec<-per_list_median$pval_vec
per_list_huber<-rofanova(X_fdata,label_1,label_2,B = B,family="huber",cores=cores)
pvalue_huber_vec<-per_list_huber$pval_vec
per_list_bisquare<-rofanova(X_fdata,label_1,label_2,B = B,family="bisquare",cores=cores)
pvalue_bisquare_vec<-per_list_bisquare$pval_vec
per_list_hampel<-rofanova(X_fdata,label_1,label_2,B = B,family="hampel",cores=cores)
pvalue_hampel_vec<-per_list_hampel$pval_vec
per_list_optimal<-rofanova(X_fdata,label_1,label_2,B = B,family="optimal",cores=cores)
pvalue_optimal_vec<-per_list_optimal$pval_vec



