#Generate small dataset
# set.seed(1)
# data <- data.frame(entrytime = gen_arriv_times(psi = 1, t = 40))
# data$survtime <- gen_surv_times(invchaz = function(t) inv_chaz_exp(t, lambda = 0.05, mu = log(10)), data = data)
# data$censorid <- rep(1, nrow(data))
# #Use cgr_helper to compare with CGRCUSONE
# asd <- cgr_helper(data = data, ctime = 50, coxphmod = NULL, cbaseh = function(t) chaz_exp(t, lambda = 0.05))
# asd5 <- cgrcusum(data = data, cbaseh = function(t) chaz_exp(t, lambda = 0.05))
# asd6 <- cgrcusum(data = data, ctimes = seq(0, 42, by = 0.5), cbaseh = function(t) chaz_exp(t, lambda = 0.05))

# varsanalysis <- c("age", "sex", "BMI")
# exprfit <- as.formula(paste("Surv(survtime, censorid) ~" ,paste(varsanalysis, collapse='+')))
# coxmodt <- coxph(exprfit, data= surgerydat)
#
#
#
#
# cgrctest <- cgrcusum(data = subset(surgerydat, Hosp_num == 14), coxphmod = coxmodt, pb = TRUE)
# cgrc2 <-cgrcusum(data = subset(surgerydat, Hosp_num == 14), ctimes = seq(1, 600, by =1), coxphmod = coxmodt, cbaseh = function(t) chaz_exp(t, lambda = 0.01), pb = TRUE)
#
#
# surdat <- surgerydat
# surdat$Si <- surdat$entrytime
# surdat$Xi <- surdat$survtime
# surdat$Ti <- surdat$Si + surdat$Xi
# surdat$Status <- surdat$censorid
#
# kbc <- bkcusum(data = subset(surgerydat, Hosp_num == 14), coxphmod = coxmodt, theta = log(2), cbaseh = function(t) chaz_exp(t, lambda = 0.01), pb = TRUE)
# kbctest <- bkcusum(data = subset(surgerydat, Hosp_num == 14), ctimes = seq(1, 600, by = 1), coxphmod = coxmodt, theta = log(2), cbaseh = function(t) chaz_exp(t, lambda = 0.01), pb = TRUE)
# ctctest <- CTCUSUM(dat = subset(surdat, Hosp_num == 14), coxmod = coxmodt, theta = log(2), cbaseh = function(t) chaz_exp(t, lambda = 0.01))
# plot.CTCUSUM(ctctest)  #original
# plot(kbc)              #new
#
#
# cgrc <- cgrcusum(data = subset(surgerydat, Hosp_num == 14), coxphmod = coxmodt, cbaseh = function(t) chaz_exp(t, lambda = 0.01), pb = TRUE)
# cgrc2 <- cgrcusum(data = ffdat, coxphmod = coxmodt, cbaseh = function(t) chaz_exp(t, lambda = 0.01), pb = TRUE)
# cgrmax <- CGRCUSMAX(dat = subset(surdat, Hosp_num == 14), coxmod = coxmodt, cbaseh = function(t) chaz_exp(t, lambda = 0.01))
# plot.CGR(cgrmax)
# plot(cgrc)


#data2 <- data
#data2$Si <- data2$entrytime
#data2$Xi <- data2$survtime
#data2$Ti <- data2$Si + data2$Xi
#data2$Status <- rep(1, nrow(data2))
#asd2 <- CGRCUSONE(dat = data2, coxmod = NULL, cbaseh =  function(t) chaz_exp(t, lambda = 0.05), timepoint = 50)
#asd3 <- CGRCUSMAX(dat = data2, coxmod = NULL, cbaseh =  function(t) chaz_exp(t, lambda = 0.05), stoptime = 50)
