# NEWS

## TRMF 0.2.0

### Enhancements

   - Added an option to scale the columns of Xm and store the scaling factors in an additional factor Z.  This
      makes the task of choosing temporal regularization parameters easier and much more stable.
  
   - Allows for a full normal prior for the columns of Fm
   
   - Add an initialization option in train() and retrain()
   
   
### Changes

 - TRMF_es() was removed.It was unclear what this was actually doing.
 
 - TRMF_coefficients() was renamed to TRMF_columns.


## TRMF 0.1.5

### Enhancements

  - Added retrain() function to allow for warm starts and restarts.
  - Setting numit = 0 in train() now initializes the computation, but doesn't do any iterations


### Bug fixes

  - Fixed a bug where weights for regression with factorization were incorrectly included in the calculation.
  

## TRMF 0.1.2

### Enhancements

  - Updated some of the internal linear algebra for 2-5x speedup
  
  - TRMF_summary now prints a weighted R2 for weighted data.


### Bug fixes

  - Fixed a bug in the component function which only returned the Xm component.
  
  - Fixed a bug in NormalizeMatrix which returns NA's in some cases.
  
### Changes

  - Renamed TRMF_coefficients() to TRMF_columns() to make less confusing. TRMF_coefficients() will be deprecated future releases.
