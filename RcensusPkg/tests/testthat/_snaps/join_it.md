# join_it()

    Code
      dc_fips <- usmap::fips(state = "dc")
      dc_B19013_dt <- RcensusPkg::get_vintage_data(dataset = "acs/acs5", vintage = 2020,
        vars = "B19013_001E", region = "tract", regionin = paste0("state:", dc_fips))
      output_dir <- withr::local_tempdir()
      if (!dir.exists(output_dir)) {
        dir.create(output_dir)
      }
      dc_tracts_sf <- RcensusPkg::tiger_tracts_sf(state = dc_fips, output_dir = output_dir,
        general = TRUE, delete_files = FALSE)
      head(RcensusPkg::join_it(df_1 = dc_B19013_dt, df_2 = dc_tracts_sf, key_1 = "GEOID",
        key_2 = "GEOID", return_sf = TRUE))
    Output
      Simple feature collection with 6 features and 18 fields
      Geometry type: POLYGON
      Dimension:     XY
      Bounding box:  xmin: -77.08258 ymin: 38.89178 xmax: -77.05008 ymax: 38.93187
      Geodetic CRS:  NAD83
                                                                 NAME B19013_001E
      1 Census Tract 1.01, District of Columbia, District of Columbia      187839
      2 Census Tract 1.02, District of Columbia, District of Columbia      184167
      3 Census Tract 2.01, District of Columbia, District of Columbia  -666666666
      4 Census Tract 2.02, District of Columbia, District of Columbia      164261
      5    Census Tract 3, District of Columbia, District of Columbia      156483
      6    Census Tract 4, District of Columbia, District of Columbia      153397
        state county  tract       GEOID STATEFP COUNTYFP TRACTCE             AFFGEOID
      1    11    001 000101 11001000101      11      001  000101 1400000US11001000101
      2    11    001 000102 11001000102      11      001  000102 1400000US11001000102
      3    11    001 000201 11001000201      11      001  000201 1400000US11001000201
      4    11    001 000202 11001000202      11      001  000202 1400000US11001000202
      5    11    001 000300 11001000300      11      001  000300 1400000US11001000300
      6    11    001 000400 11001000400      11      001  000400 1400000US11001000400
        i.NAME          NAMELSAD STUSPS           NAMELSADCO           STATE_NAME
      1   1.01 Census Tract 1.01     DC District of Columbia District of Columbia
      2   1.02 Census Tract 1.02     DC District of Columbia District of Columbia
      3   2.01 Census Tract 2.01     DC District of Columbia District of Columbia
      4   2.02 Census Tract 2.02     DC District of Columbia District of Columbia
      5      3    Census Tract 3     DC District of Columbia District of Columbia
      6      4    Census Tract 4     DC District of Columbia District of Columbia
        LSAD   ALAND AWATER                       geometry
      1   CT  199776   5261 POLYGON ((-77.05714 38.9105...
      2   CT 1706484 516665 POLYGON ((-77.06871 38.9010...
      3   CT  505004      0 POLYGON ((-77.07902 38.9126...
      4   CT  776435 439661 POLYGON ((-77.07941 38.9056...
      5   CT 1042157   2305 POLYGON ((-77.08257 38.9215...
      6   CT 1541239     69 POLYGON ((-77.07319 38.9275...

