# BOSSreg v0.2.0
In this update, we I
* added argument `maxstep` to stop FS and BOSS at a specified step size
* extended the estimation of hdf to the scenario where n<=p
* modified function `boss` to account for n<=p
* modified function `cv.boss` to account for n<=p (only validates subset with sizes up to min(n - n/n.folds, maxstep))
