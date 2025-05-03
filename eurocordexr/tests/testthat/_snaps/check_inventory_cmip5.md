# simple checks

    Code
      check_inventory_cmip5(get_inventory_cmip5(fs::path(tmpdir, "testdata-cmip5",
        "basic")))
    Output
      Checks performed: 
      ------------------------------------------------------
      ------------------------------------------------------
      Multiple time frequencies detected: Amon day 
      ------------------------------------------------------
      ------------------------------------------------------
      Multiple ensembles in 1 cases: 
         variable       gcm experiment timefreq     N              ensembles
           <char>    <char>     <char>   <char> <int>                 <char>
      1:       pr ACCESS1-0 historical     Amon     3 r1i1p1, r2i1p1, r3i1p1
      ------------------------------------------------------
      ------------------------------------------------------
      Following model runs do not have complete periods: 
         variable timefreq        gcm experiment ensemble nn_files date_start
           <char>   <char>     <char>     <char>   <char>    <int>     <Date>
      1:       pr     Amon  CMCC-CESM historical   r1i1p1        2 1855-01-01
      2:       pr      day   CNRM-CM5 historical   r1i1p1        4 1990-01-01
      3:       pr      day   CNRM-CM5      rcp26   r1i1p1        2 2006-01-01
      4:      tas     Amon   CMCC-CMS      rcp85   r1i1p1        2 2070-01-01
      5:   tasmax      day HadGEM2-ES      rcp85   r1i1p1        2 2279-12-01
           date_end total_simulation_years period_contiguous
             <Date>                  <int>            <lgcl>
      1: 1879-12-01                     10             FALSE
      2: 2005-12-31                     16              TRUE
      3: 2025-12-31                     10             FALSE
      4: 2100-12-01                     21             FALSE
      5: 2299-12-30                     12             FALSE
      ------------------------------------------------------
      ------------------------------------------------------
      Finished checks. 
      ------------------------------------------------------
      ------------------------------------------------------

---

    Code
      check_inventory_cmip5(get_inventory_cmip5(fs::path(tmpdir, "testdata-cmip5",
        "mult-ens")))
    Output
      Checks performed: 
      ------------------------------------------------------
      ------------------------------------------------------
      No multiple time frequencies. 
      ------------------------------------------------------
      ------------------------------------------------------
      Multiple ensembles in 2 cases: 
         variable       gcm experiment timefreq     N              ensembles
           <char>    <char>     <char>   <char> <int>                 <char>
      1:      tas ACCESS1-0 historical     Amon     3 r1i1p1, r2i1p1, r3i1p1
      2:      tas ACCESS1-3 historical     Amon     3 r1i1p1, r2i1p1, r3i1p1
      ------------------------------------------------------
      ------------------------------------------------------
      All historical and rcp simulations have complete periods.
      ------------------------------------------------------
      ------------------------------------------------------
      Finished checks. 
      ------------------------------------------------------
      ------------------------------------------------------

---

    Code
      check_inventory_cmip5(get_inventory_cmip5(fs::path(tmpdir, "testdata-cmip5",
        "incomplete-period")))
    Output
      Checks performed: 
      ------------------------------------------------------
      ------------------------------------------------------
      Multiple time frequencies detected: Amon day 
      ------------------------------------------------------
      ------------------------------------------------------
      No multiple ensembles. 
      ------------------------------------------------------
      ------------------------------------------------------
      Following model runs do not have complete periods: 
         variable timefreq        gcm experiment ensemble nn_files date_start
           <char>   <char>     <char>     <char>   <char>    <int>     <Date>
      1:       pr      day   CNRM-CM5 historical   r1i1p1       31 1850-01-01
      2:       pr      day   CNRM-CM5      rcp26   r1i1p1       18 2011-01-01
      3:   tasmin      day HadGEM2-ES      rcp26   r1i1p1       11 2005-12-01
           date_end total_simulation_years period_contiguous
             <Date>                  <int>            <lgcl>
      1: 2004-12-31                    155              TRUE
      2: 2100-12-31                     90              TRUE
      3: 2139-11-30                    112             FALSE
      ------------------------------------------------------
      ------------------------------------------------------
      Finished checks. 
      ------------------------------------------------------
      ------------------------------------------------------

---

    Code
      check_inventory_cmip5(get_inventory_cmip5(fs::path(tmpdir, "testdata-cmip5")),
      check_hist = TRUE)
    Output
      Checks performed: 
      ------------------------------------------------------
      ------------------------------------------------------
      Multiple time frequencies detected: Amon day 
      ------------------------------------------------------
      ------------------------------------------------------
      Multiple ensembles in 3 cases: 
         variable       gcm experiment timefreq     N              ensembles
           <char>    <char>     <char>   <char> <int>                 <char>
      1:       pr ACCESS1-0 historical     Amon     3 r1i1p1, r2i1p1, r3i1p1
      2:      tas ACCESS1-0 historical     Amon     3 r1i1p1, r2i1p1, r3i1p1
      3:      tas ACCESS1-3 historical     Amon     3 r1i1p1, r2i1p1, r3i1p1
      ------------------------------------------------------
      ------------------------------------------------------
      Following model runs do not have complete periods: 
         variable timefreq        gcm experiment ensemble nn_files date_start
           <char>   <char>     <char>     <char>   <char>    <int>     <Date>
      1:       pr     Amon  CMCC-CESM historical   r1i1p1        2 1855-01-01
      2:      tas     Amon   CMCC-CMS      rcp85   r1i1p1        2 2070-01-01
      3:   tasmax      day HadGEM2-ES      rcp85   r1i1p1        2 2279-12-01
      4:   tasmin      day HadGEM2-ES      rcp26   r1i1p1       11 2005-12-01
           date_end total_simulation_years period_contiguous
             <Date>                  <int>            <lgcl>
      1: 1879-12-01                     10             FALSE
      2: 2100-12-01                     21             FALSE
      3: 2299-12-30                     12             FALSE
      4: 2139-11-30                    112             FALSE
      ------------------------------------------------------
      ------------------------------------------------------
      Following scenario model runs do not have a corresponding historical run: 
         variable timefreq        gcm experiment ensemble date_start   date_end
           <char>   <char>     <char>     <char>   <char>     <Date>     <Date>
      1:      tas     Amon   CMCC-CMS      rcp85   r1i1p1 2070-01-01 2100-12-01
      2:   tasmax      day HadGEM2-ES      rcp85   r1i1p1 2279-12-01 2299-12-30
      3:   tasmin      day HadGEM2-ES      rcp26   r1i1p1 2005-12-01 2139-11-30
      3 variables not shown: nn_files <int>, total_simulation_years <int>, period_contiguous <lgcl>
      ------------------------------------------------------
      ------------------------------------------------------
      Finished checks. 
      ------------------------------------------------------
      ------------------------------------------------------

