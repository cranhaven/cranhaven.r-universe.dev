# simple checks

    Code
      check_inventory(get_inventory(fs::path(tmpdir, "testdata", "mixed-vars")))
    Output
      Checks performed: 
      ------------------------------------------------------
      ------------------------------------------------------
      No multiple time frequencies. 
      ------------------------------------------------------
      ------------------------------------------------------
      No multiple domains. 
      ------------------------------------------------------
      ------------------------------------------------------
      No multiple ensembles. 
      ------------------------------------------------------
      ------------------------------------------------------
      No multiple downscale realisations 
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
      check_inventory(get_inventory(fs::path(tmpdir, "testdata", "dup-ens")))
    Output
      Checks performed: 
      ------------------------------------------------------
      ------------------------------------------------------
      No multiple time frequencies. 
      ------------------------------------------------------
      ------------------------------------------------------
      No multiple domains. 
      ------------------------------------------------------
      ------------------------------------------------------
      Multiple ensembles in 1 cases: 
         variable domain              gcm                institute_rcm experiment
           <char> <char>           <char>                       <char>     <char>
      1:       pr EUR-11 MPI-M-MPI-ESM-LR CLMcom-ETH-COSMO-crCLIM-v1-1 historical
         downscale_realisation timefreq     N      ensembles
                        <char>   <char> <int>         <char>
      1:                    v1      day     2 r1i1p1, r2i1p1
      ------------------------------------------------------
      ------------------------------------------------------
      No multiple downscale realisations 
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
      check_inventory(get_inventory(fs::path(tmpdir, "testdata", "incomplete-period")))
    Output
      Checks performed: 
      ------------------------------------------------------
      ------------------------------------------------------
      No multiple time frequencies. 
      ------------------------------------------------------
      ------------------------------------------------------
      No multiple domains. 
      ------------------------------------------------------
      ------------------------------------------------------
      No multiple ensembles. 
      ------------------------------------------------------
      ------------------------------------------------------
      No multiple downscale realisations 
      ------------------------------------------------------
      ------------------------------------------------------
      Following model runs do not have complete periods: 
         variable domain                   gcm institute_rcm experiment ensemble
           <char> <char>                <char>        <char>     <char>   <char>
      1:       pr EUR-11 CNRM-CERFACS-CNRM-CM5 CNRM-ALADIN63 historical   r1i1p1
         downscale_realisation timefreq nn_files date_start   date_end
                        <char>   <char>    <int>     <Date>     <Date>
      1:                    v2      day        5 1951-01-01 1985-12-31
         total_simulation_years period_contiguous
                          <int>            <lgcl>
      1:                     25             FALSE
      ------------------------------------------------------
      ------------------------------------------------------
      Finished checks. 
      ------------------------------------------------------
      ------------------------------------------------------

