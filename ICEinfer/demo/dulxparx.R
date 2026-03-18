library(ICEinfer)
# input the dulxparx data of Obenchain et al. (2000).
data(dulxparx)
# Effectiveness = idb, Cost = ru, trtm = dulx where
# dulx = 1 ==> Duloxetine and dulx = 0 ==> Paroxetine
#
# Display of Lambda => Shadow Price Summary Statistics...
ICEscale(dulxparx, dulx, idb, ru)
ICEscale(dulxparx, dulx, idb, ru, lambda=0.26)

# Bootstrap ICE Uncertainty calculations with R=25000 can be time consuming...
dpunc <- ICEuncrt(dulxparx, dulx, idb, ru, lambda=0.26, R=5000)
dpunc

# Display the Bootstrap ICE Uncertainty Distribution...
plot(dpunc)

dpwdg <- ICEwedge(dpunc)
dpwdg
plot(dpwdg)

# Compute VAGR Acceptability and ALICE Curves...
dpacc <- ICEalice(dpwdg)
plot(dpacc, show="VAGR")
plot(dpacc, show="Alice")

# Color Interior of Confidence Wedge with LINEAR Economic Preferences...
dpcol <- ICEcolor(dpwdg, gamma=1)
plot(dpcol, show="RBOW")
plot(dpcol, show="Hist")

# Increase Lambda and Recolor Confidence Wedge with NON-Linear Preferences...
dpcol <- ICEcolor(dpwdg, lfact=10)
plot(dpcol, show="RBOW")
plot(dpcol, show="Hist")
