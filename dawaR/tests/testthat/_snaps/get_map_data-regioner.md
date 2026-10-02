# get_map_data() fetches data properly and provides `regioner` in the proper format with cache = TRUE

    Code
      get_map_data(type = "regioner", cache = TRUE)
    Message
      > Getting data on `regioner`. This usually takes 10.24s.
      Fetching data from the API. This will take some time.
      Reading data to `st`.
      Converting map data to `sf` object
    Output
      Simple feature collection with 5 features and 9 fields
      Geometry type: MULTIPOLYGON
      Dimension:     XY
      Bounding box:  xmin: 8.07251 ymin: 54.55908 xmax: 15.1974 ymax: 57.75257
      Geodetic CRS:  WGS 84
        dagi_id kode               navn nuts2              ændret          geo_ændret
      1  389098 1081 Region Nordjylland  DK05 2026-01-01 22:02:26 2026-01-01 22:02:26
      2  389101 1082 Region Midtjylland  DK04 2025-07-11 21:05:02 2025-07-11 21:05:02
      3  389102 1083  Region Syddanmark  DK03 2026-03-13 22:03:41 2026-03-13 22:03:41
      4  389099 1084 Region Hovedstaden  DK01 2026-03-27 22:17:36 2026-03-27 22:17:36
      5  389100 1085    Region Sjælland  DK02 2026-03-27 22:17:36 2026-03-27 22:17:36
        geo_version visueltcenter_x visueltcenter_y                       geometry
      1          34       10.112829        57.30716 MULTIPOLYGON (((10.6136 57....
      2          47        9.598478        56.23406 MULTIPOLYGON (((9.06695 56....
      3          35        9.098327        55.32959 MULTIPOLYGON (((9.229857 55...
      4          37       12.280106        55.97838 MULTIPOLYGON (((12.31075 56...
      5          31       11.572433        55.42949 MULTIPOLYGON (((11.27753 56...

---

    Code
      get_map_data(type = "regioner")
    Message
      > Using cached response.
      Change this behaviour by setting cache = FALSE
    Output
      Simple feature collection with 5 features and 9 fields
      Geometry type: MULTIPOLYGON
      Dimension:     XY
      Bounding box:  xmin: 8.07251 ymin: 54.55908 xmax: 15.1974 ymax: 57.75257
      Geodetic CRS:  WGS 84
        dagi_id kode               navn nuts2              ændret          geo_ændret
      1  389098 1081 Region Nordjylland  DK05 2026-01-01 22:02:26 2026-01-01 22:02:26
      2  389101 1082 Region Midtjylland  DK04 2025-07-11 21:05:02 2025-07-11 21:05:02
      3  389102 1083  Region Syddanmark  DK03 2026-03-13 22:03:41 2026-03-13 22:03:41
      4  389099 1084 Region Hovedstaden  DK01 2026-03-27 22:17:36 2026-03-27 22:17:36
      5  389100 1085    Region Sjælland  DK02 2026-03-27 22:17:36 2026-03-27 22:17:36
        geo_version visueltcenter_x visueltcenter_y                       geometry
      1          34       10.112829        57.30716 MULTIPOLYGON (((10.6136 57....
      2          47        9.598478        56.23406 MULTIPOLYGON (((9.06695 56....
      3          35        9.098327        55.32959 MULTIPOLYGON (((9.229857 55...
      4          37       12.280106        55.97838 MULTIPOLYGON (((12.31075 56...
      5          31       11.572433        55.42949 MULTIPOLYGON (((11.27753 56...

---

    Code
      get_map_data(type = "regioner", cache = FALSE)
    Message
      > Getting data on `regioner`. This usually takes 10.24s.
      Fetching data from the API. This will take some time.
      Reading data to `st`.
      Converting map data to `sf` object
    Output
      Simple feature collection with 5 features and 9 fields
      Geometry type: MULTIPOLYGON
      Dimension:     XY
      Bounding box:  xmin: 8.07251 ymin: 54.55908 xmax: 15.1974 ymax: 57.75257
      Geodetic CRS:  WGS 84
        dagi_id kode               navn nuts2              ændret          geo_ændret
      1  389098 1081 Region Nordjylland  DK05 2026-01-01 22:02:26 2026-01-01 22:02:26
      2  389101 1082 Region Midtjylland  DK04 2025-07-11 21:05:02 2025-07-11 21:05:02
      3  389102 1083  Region Syddanmark  DK03 2026-03-13 22:03:41 2026-03-13 22:03:41
      4  389099 1084 Region Hovedstaden  DK01 2026-03-27 22:17:36 2026-03-27 22:17:36
      5  389100 1085    Region Sjælland  DK02 2026-03-27 22:17:36 2026-03-27 22:17:36
        geo_version visueltcenter_x visueltcenter_y                       geometry
      1          34       10.112829        57.30716 MULTIPOLYGON (((10.6136 57....
      2          47        9.598478        56.23406 MULTIPOLYGON (((9.06695 56....
      3          35        9.098327        55.32959 MULTIPOLYGON (((9.229857 55...
      4          37       12.280106        55.97838 MULTIPOLYGON (((12.31075 56...
      5          31       11.572433        55.42949 MULTIPOLYGON (((11.27753 56...

---

    Code
      get_map_data(type = "regioner")
    Message
      > Using cached response.
      Change this behaviour by setting cache = FALSE
    Output
      Simple feature collection with 5 features and 9 fields
      Geometry type: MULTIPOLYGON
      Dimension:     XY
      Bounding box:  xmin: 8.07251 ymin: 54.55908 xmax: 15.1974 ymax: 57.75257
      Geodetic CRS:  WGS 84
        dagi_id kode               navn nuts2              ændret          geo_ændret
      1  389098 1081 Region Nordjylland  DK05 2026-01-01 22:02:26 2026-01-01 22:02:26
      2  389101 1082 Region Midtjylland  DK04 2025-07-11 21:05:02 2025-07-11 21:05:02
      3  389102 1083  Region Syddanmark  DK03 2026-03-13 22:03:41 2026-03-13 22:03:41
      4  389099 1084 Region Hovedstaden  DK01 2026-03-27 22:17:36 2026-03-27 22:17:36
      5  389100 1085    Region Sjælland  DK02 2026-03-27 22:17:36 2026-03-27 22:17:36
        geo_version visueltcenter_x visueltcenter_y                       geometry
      1          34       10.112829        57.30716 MULTIPOLYGON (((10.6136 57....
      2          47        9.598478        56.23406 MULTIPOLYGON (((9.06695 56....
      3          35        9.098327        55.32959 MULTIPOLYGON (((9.229857 55...
      4          37       12.280106        55.97838 MULTIPOLYGON (((12.31075 56...
      5          31       11.572433        55.42949 MULTIPOLYGON (((11.27753 56...

---

    Code
      get_map_data(type = "regioner")
    Message
      > Using cached response.
      Change this behaviour by setting cache = FALSE
    Output
      Simple feature collection with 5 features and 9 fields
      Geometry type: MULTIPOLYGON
      Dimension:     XY
      Bounding box:  xmin: 8.07251 ymin: 54.55908 xmax: 15.1974 ymax: 57.75257
      Geodetic CRS:  WGS 84
        dagi_id kode               navn nuts2              ændret          geo_ændret
      1  389098 1081 Region Nordjylland  DK05 2026-01-01 22:02:26 2026-01-01 22:02:26
      2  389101 1082 Region Midtjylland  DK04 2025-07-11 21:05:02 2025-07-11 21:05:02
      3  389102 1083  Region Syddanmark  DK03 2026-03-13 22:03:41 2026-03-13 22:03:41
      4  389099 1084 Region Hovedstaden  DK01 2026-03-27 22:17:36 2026-03-27 22:17:36
      5  389100 1085    Region Sjælland  DK02 2026-03-27 22:17:36 2026-03-27 22:17:36
        geo_version visueltcenter_x visueltcenter_y                       geometry
      1          34       10.112829        57.30716 MULTIPOLYGON (((10.6136 57....
      2          47        9.598478        56.23406 MULTIPOLYGON (((9.06695 56....
      3          35        9.098327        55.32959 MULTIPOLYGON (((9.229857 55...
      4          37       12.280106        55.97838 MULTIPOLYGON (((12.31075 56...
      5          31       11.572433        55.42949 MULTIPOLYGON (((11.27753 56...

