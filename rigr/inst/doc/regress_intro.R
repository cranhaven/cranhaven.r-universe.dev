## -----------------------------------------------------------------------------
## Preparing our R session
library(rigr)
data(mri)
regress("mean", atrophy ~ age + sex + race, data = mri)

## -----------------------------------------------------------------------------
regress("odds", diabetes ~ sex, data = mri)

## -----------------------------------------------------------------------------
regress("rate", yrsquit ~ age, data = mri)

## -----------------------------------------------------------------------------
library(survival)
regress("hazard", Surv(obstime, death)~age, data=mri)

## -----------------------------------------------------------------------------
regress("geometric mean", packyrs ~ age, data = mri)

## -----------------------------------------------------------------------------
regress("geometric mean", packyrs ~ age, data = mri, replaceZeroes = 1)

## -----------------------------------------------------------------------------
regress("mean", atrophy ~ dummy(sex, reference = "Male"), data = mri)
regress("mean", atrophy ~ dummy(sex, reference = "Female"), data = mri)

## -----------------------------------------------------------------------------
regress("mean", atrophy ~ polynomial(age, degree = 2), data = mri)

## -----------------------------------------------------------------------------
regress("mean", atrophy ~ polynomial(age, degree = 2, center = 65), data = mri)

## -----------------------------------------------------------------------------
regress("mean", atrophy ~ age + sex + U("Smoking variables" = ~packyrs + yrsquit), data = mri)

## -----------------------------------------------------------------------------
mod_rigr <- regress("mean", atrophy ~ age + sex, data = mri)
mod_rigr

## -----------------------------------------------------------------------------
mod_combo <- c(0, -10, 1)
lincom(mod_rigr, mod_combo)

## -----------------------------------------------------------------------------
lincom(mod_rigr, mod_combo, null.hypoth = -1)

