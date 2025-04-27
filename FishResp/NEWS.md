# FishResp v1.1.2
Improving quality and performance of the import.meas function 

### Bug fixes:
* head & tail error fix: removing first or last phase(s) if its 
  length was significantly reduced (i.e. <90% of expected length). 
  This bug fix ensures that the dataset does not include shortened 
  or incomplete measurement phases, which caused errors
* delete time duplications and add empty lines for missing seconds.
  This bug fix addresses the issue of duplicate timestamps and
  missing seconds in the imported dataset
* remove any measurement phase with only one value to avoid errors 
  in the correct.meas function

### Notes:
* clarity and structure of the README section was improved
* Contributors were moved from the author list to the contributor 
  page of the FishResp website: https://fishresp.org/people/

------------------------------------------------------------------

# FishResp v1.1.1
Time-related issues have been fixed offering more flexibility for the 
analysis of multinight measurements and M->F and M->W phase transitions.

### Major changes:
* add stop.meas.date and stop.meas.date arguments to import.meas()
* add the meas.to.flush argument to the import.meas() function

### Bug fixes:
* fix the warning related to AM/PM after updating R core
* time index is modified for each meas.phase in import.meas()
* buffer is added to import.meas() to avoid tail error
* fix code for appending time index to meas phases

------------------------------------------------------------------

# FishResp v1.1.0

### Major changes:
* add the new function 'prepare.data'
* add an example data for 'prepare.data', data contributor: Erik Gill.
* add the new function 'pyroscience.pumpresp'
* add an example data for 'pyroscience.pumpresp', data contributor: Jenni Prokkola.
* fill the 'README.md' file.
* add the 'NEWS.md' file for tracking changes

### Bug fixes:
* solve an issue with comma separator in raw data
* fix a bug with NA and 9999 in slope extraction
* fix a bug with empty lines in the end of a file with raw data

### Notes:
This and following versions of the R package will be developed using Git and hosted on GitHub (in addition to CRAN):
<https://github.com/embedded-sergey/FishResp-Rpackage>

Thanks to Erik Gill and Jenni Prokkola for data contributions.

------------------------------------------------------------

# FishResp v1.0.3 [LTS]
This version was described in the academic publication:
Morozov et al. (2019) <https://doi.org/10.1093/conphys/coz003>
