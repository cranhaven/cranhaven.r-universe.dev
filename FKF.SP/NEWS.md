# FKF.SP 0.3.4

Hotfix - correctly coercing variable `d_t` from an array of `double` to an array of `int` within fkfs_sp.c
Warnings - wrapping definitions in `utils.h` with #ifndef for additional compatibility
Hotfix - removing rprintf statement that snuck into production, deployed compiled code (oops)

# FKF.SP 0.3.3

Upgraded compiled C functions to be compatible with R 4.4.1. Split C functions into separate files to make them more readable.

# FKF.SP 0.3.2

Added 'mathjaxr' to the NAMESPACE, updated Roxygen

# FKF.SP 0.3.1

Minor bug fixes to the "init.c" and "fkf_SP" files, as per the "additional issues" guidelines of CRAN.

The compiled C code: "fkfs_SP" has been added, which combines Kalman filtering and smoothing into a singular function.

The "smoothing" logical argument has been added to the 'fkf.SP' function.

Minor documentation changes to reflect the additional argument.


# FKF.SP 0.3.0

More outputs are now returned from the 'fkf.SP' function when 'verbose' is set to true. These are 'yt', 'Tt', 'Zt', 'Ftinv', 'vt', 'Kt'

The 'fks.SP' function has been added, which is a Kalman smoothing implementation written in compiled C code and using the solution described in the textbook of Durbin and Koopman (2001): "Time Series Analysis by State Space Methods".

The vignette of the package has been updated with an example of Kalman smoothing.

# FKF.SP 0.2.0

The 'fkf.SP' function now includes the 'verbose' argument, allowing for filtered values to be returned as a list object.

The default setting for the 'verbose' argument is FALSE, resulting in no changes to existing applications of this function.

# FKF.SP 0.1.3

- USE_FC_LEN_T now adopted, consistent with its upcoming obligatory usage

# FKF.SP 0.1.2

- Minor name changes to the vignette
- Deprecated data from the 'NFCP' package used in vignette has been updated

# FKF.SP 0.1.1

- Minor fixes in error reporting of 'fkf.SP'.
- Example 4 of the vignette added
- Now suggests the 'NFCP' package for example 4
- Minor documentation edits
- 'fkf.SP' no longer returns a 'warning' when NA's are returned


# FKF.SP 0.1.0

- Release of FKF.SP
