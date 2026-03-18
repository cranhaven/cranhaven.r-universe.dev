library(ICEinfer)
# input the fluoxpin data of Sacristan et al. (2000).
data(fluoxpin)
# Effectiveness = respond, Cost = cost, trtm = flxpin where
# flxpin = 1 ==> fluoxetine plus pindolol and flxpin = 0 ==> fluoxetine plus placebo

cat("\n Display of Lambda => Shadow Price Summary Statistics...\n")
ICEscale(fluoxpin, flxpin, respond, cost)
ICEscale(fluoxpin, flxpin, respond, cost, lambda=100000)

cat("\nBootstrap ICE Uncertainty calculations with R=25000 can be lengthy...\n")
fpunc <- ICEuncrt(fluoxpin, flxpin, respond, cost, lambda=100000, R=5000)
fpunc

cat("\nDisplay the Bootstrap ICE Uncertainty Distribution...\n")
plot(fpunc)

fpwdg <- ICEwedge(fpunc)
fpwdg
plot(fpwdg)

cat("\nComputing VAGR Acceptability and ALICE Curves...\n")
fpacc <- ICEalice(fpwdg)
plot(fpacc, show="VAGR")
plot(fpacc, show="Alice")

cat("\nColor Interior of Confidence Wedge with LINEAR Economic Preferences...\n")
fpcol <- ICEcolor(fpwdg, gamma=1)
plot(fpcol, show="RBOW")
plot(fpcol, show="Hist")

cat("\nIncrease Lambda and Recolor Confidence Wedge with NON-Linear Preferences...\n")
fpcol <- ICEcolor(fpwdg, lfact=10)
plot(fpcol, show="RBOW")
plot(fpcol, show="Hist")
