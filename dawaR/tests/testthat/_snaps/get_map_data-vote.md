# get_map_data() fetches data properly and provides `afstemningsomraader` in the proper format with cache = TRUE

    Code
      get_map_data(type = "afstemningsomraader", cache = TRUE)
    Message
      > Getting data on `afstemningsomraader`. This usually takes 18.77s.
      Fetching data from the API. This will take some time.
      Reading data to `st`.
      Converting map data to `sf` object
    Output
      Simple feature collection with 1314 features and 23 fields
      Geometry type: MULTIPOLYGON
      Dimension:     XY
      Bounding box:  xmin: 8.07251 ymin: 54.55908 xmax: 15.1974 ymax: 57.75257
      Geodetic CRS:  WGS 84
      First 10 features:
         dagi_id nummer            navn      afstemningsstednavn
      1   707732      1     1. Østerbro              Idrætshuset
      2   711932      2         1. Nord         Strandvejsskolen
      3   704936      3          1. Syd                  Remisen
      4   706160      5         1. Vest       Nørre Fælled Skole
      5   710802      6     1. Nordvest         Kildevældsskolen
      6   711204      7 2. Sundbyvester        Sundby Idrætspark
      7   706166      8         2. Nord      Amager Fælled Skole
      8   706562      9      2. Ørestad            Ørestad Skole
      9   712286     10         2. Vest Skolen på Islands Brygge
      10  711926     11     3. Indre By             Rådhushallen
                     afstemningsstedadresseid
      1  0a3f507a-8121-32b8-e044-0003ba298018
      2  0a3f507a-f1b6-32b8-e044-0003ba298018
      3  0a3f507a-4851-32b8-e044-0003ba298018
      4  0a3f507a-457f-32b8-e044-0003ba298018
      5  0a3f507a-4387-32b8-e044-0003ba298018
      6  0a3f507a-6784-32b8-e044-0003ba298018
      7  0a3f507b-05f2-32b8-e044-0003ba298018
      8  20d7372c-7d2b-4ee3-a55c-54da765f5d84
      9  0a3f507a-3e64-32b8-e044-0003ba298018
      10 0a3f507a-ec01-32b8-e044-0003ba298018
                    afstemningsstedadressebetegnelse afstemningssted_adgangspunkt_x
      1  Gunnar Nu Hansens Plads 7, 2100 København Ø                       12.57485
      2                Sionsgade 1, 2100 København Ø                       12.57921
      3            Blegdamsvej 132, 2100 København Ø                       12.57679
      4       Biskop Krags Vænge 7, 2100 København Ø                       12.55815
      5            Bellmansgade 5A, 2100 København Ø                       12.56941
      6             Englandsvej 61, 2300 København S                       12.59996
      7            Sundholmsvej 2A, 2300 København S                       12.59499
      8     Arne Jacobsens Allé 21, 2300 København S                       12.58144
      9            Artillerivej 57, 2300 København S                       12.58333
      10           Rådhuspladsen 1, 1550 København V                       12.56958
         afstemningssted_adgangspunkt_y kommunekode kommunenavn regionskode
      1                        55.70477        0101   København        1084
      2                        55.71124        0101   København        1084
      3                        55.69868        0101   København        1084
      4                        55.70536        0101   København        1084
      5                        55.71540        0101   København        1084
      6                        55.65023        0101   København        1084
      7                        55.66245        0101   København        1084
      8                        55.63111        0101   København        1084
      9                        55.66292        0101   København        1084
      10                       55.67563        0101   København        1084
                regionsnavn opstillingskredsnummer opstillingskredsnavn
      1  Region Hovedstaden                      1             Østerbro
      2  Region Hovedstaden                      1             Østerbro
      3  Region Hovedstaden                      1             Østerbro
      4  Region Hovedstaden                      1             Østerbro
      5  Region Hovedstaden                      1             Østerbro
      6  Region Hovedstaden                      2         Sundbyvester
      7  Region Hovedstaden                      2         Sundbyvester
      8  Region Hovedstaden                      2         Sundbyvester
      9  Region Hovedstaden                      2         Sundbyvester
      10 Region Hovedstaden                      3             Indre By
         storkredsnummer storkredsnavn valglandsdelsbogstav valglandsdelsnavn
      1                1     København                    A       Hovedstaden
      2                1     København                    A       Hovedstaden
      3                1     København                    A       Hovedstaden
      4                1     København                    A       Hovedstaden
      5                1     København                    A       Hovedstaden
      6                1     København                    A       Hovedstaden
      7                1     København                    A       Hovedstaden
      8                1     København                    A       Hovedstaden
      9                1     København                    A       Hovedstaden
      10               1     København                    A       Hovedstaden
                      ændret          geo_ændret geo_version visueltcenter_x
      1  2025-08-21 21:04:39 2025-08-21 21:04:39           9        12.57252
      2  2025-08-21 21:04:39 2025-08-21 21:04:39           7        12.58267
      3  2025-08-21 21:04:39 2025-08-21 21:04:39           7        12.56874
      4  2025-08-21 21:04:39 2025-08-21 21:04:39           8        12.56014
      5  2025-08-21 21:04:39 2025-08-21 21:04:39           8        12.56856
      6  2025-08-21 21:04:39 2025-08-21 21:04:39           8        12.60509
      7  2025-08-21 21:04:39 2025-08-21 21:04:39           7        12.59394
      8  2026-03-27 22:17:36 2026-03-27 22:17:36          12        12.55095
      9  2025-08-21 21:04:39 2025-08-21 21:04:39           9        12.56800
      10 2025-08-21 21:04:39 2025-08-21 21:04:39          10        12.56623
         visueltcenter_y                       geometry
      1         55.70528 MULTIPOLYGON (((12.56228 55...
      2         55.71087 MULTIPOLYGON (((12.58745 55...
      3         55.69809 MULTIPOLYGON (((12.56136 55...
      4         55.70791 MULTIPOLYGON (((12.55314 55...
      5         55.71552 MULTIPOLYGON (((12.55859 55...
      6         55.65088 MULTIPOLYGON (((12.59358 55...
      7         55.66577 MULTIPOLYGON (((12.58392 55...
      8         55.62567 MULTIPOLYGON (((12.58604 55...
      9         55.64696 MULTIPOLYGON (((12.55133 55...
      10        55.68159 MULTIPOLYGON (((12.55518 55...

---

    Code
      get_map_data(type = "afstemningsomraader", cache = FALSE)
    Message
      > Getting data on `afstemningsomraader`. This usually takes 18.77s.
      Fetching data from the API. This will take some time.
      Reading data to `st`.
      Converting map data to `sf` object
    Output
      Simple feature collection with 1314 features and 23 fields
      Geometry type: MULTIPOLYGON
      Dimension:     XY
      Bounding box:  xmin: 8.07251 ymin: 54.55908 xmax: 15.1974 ymax: 57.75257
      Geodetic CRS:  WGS 84
      First 10 features:
         dagi_id nummer            navn      afstemningsstednavn
      1   707732      1     1. Østerbro              Idrætshuset
      2   711932      2         1. Nord         Strandvejsskolen
      3   704936      3          1. Syd                  Remisen
      4   706160      5         1. Vest       Nørre Fælled Skole
      5   710802      6     1. Nordvest         Kildevældsskolen
      6   711204      7 2. Sundbyvester        Sundby Idrætspark
      7   706166      8         2. Nord      Amager Fælled Skole
      8   706562      9      2. Ørestad            Ørestad Skole
      9   712286     10         2. Vest Skolen på Islands Brygge
      10  711926     11     3. Indre By             Rådhushallen
                     afstemningsstedadresseid
      1  0a3f507a-8121-32b8-e044-0003ba298018
      2  0a3f507a-f1b6-32b8-e044-0003ba298018
      3  0a3f507a-4851-32b8-e044-0003ba298018
      4  0a3f507a-457f-32b8-e044-0003ba298018
      5  0a3f507a-4387-32b8-e044-0003ba298018
      6  0a3f507a-6784-32b8-e044-0003ba298018
      7  0a3f507b-05f2-32b8-e044-0003ba298018
      8  20d7372c-7d2b-4ee3-a55c-54da765f5d84
      9  0a3f507a-3e64-32b8-e044-0003ba298018
      10 0a3f507a-ec01-32b8-e044-0003ba298018
                    afstemningsstedadressebetegnelse afstemningssted_adgangspunkt_x
      1  Gunnar Nu Hansens Plads 7, 2100 København Ø                       12.57485
      2                Sionsgade 1, 2100 København Ø                       12.57921
      3            Blegdamsvej 132, 2100 København Ø                       12.57679
      4       Biskop Krags Vænge 7, 2100 København Ø                       12.55815
      5            Bellmansgade 5A, 2100 København Ø                       12.56941
      6             Englandsvej 61, 2300 København S                       12.59996
      7            Sundholmsvej 2A, 2300 København S                       12.59499
      8     Arne Jacobsens Allé 21, 2300 København S                       12.58144
      9            Artillerivej 57, 2300 København S                       12.58333
      10           Rådhuspladsen 1, 1550 København V                       12.56958
         afstemningssted_adgangspunkt_y kommunekode kommunenavn regionskode
      1                        55.70477        0101   København        1084
      2                        55.71124        0101   København        1084
      3                        55.69868        0101   København        1084
      4                        55.70536        0101   København        1084
      5                        55.71540        0101   København        1084
      6                        55.65023        0101   København        1084
      7                        55.66245        0101   København        1084
      8                        55.63111        0101   København        1084
      9                        55.66292        0101   København        1084
      10                       55.67563        0101   København        1084
                regionsnavn opstillingskredsnummer opstillingskredsnavn
      1  Region Hovedstaden                      1             Østerbro
      2  Region Hovedstaden                      1             Østerbro
      3  Region Hovedstaden                      1             Østerbro
      4  Region Hovedstaden                      1             Østerbro
      5  Region Hovedstaden                      1             Østerbro
      6  Region Hovedstaden                      2         Sundbyvester
      7  Region Hovedstaden                      2         Sundbyvester
      8  Region Hovedstaden                      2         Sundbyvester
      9  Region Hovedstaden                      2         Sundbyvester
      10 Region Hovedstaden                      3             Indre By
         storkredsnummer storkredsnavn valglandsdelsbogstav valglandsdelsnavn
      1                1     København                    A       Hovedstaden
      2                1     København                    A       Hovedstaden
      3                1     København                    A       Hovedstaden
      4                1     København                    A       Hovedstaden
      5                1     København                    A       Hovedstaden
      6                1     København                    A       Hovedstaden
      7                1     København                    A       Hovedstaden
      8                1     København                    A       Hovedstaden
      9                1     København                    A       Hovedstaden
      10               1     København                    A       Hovedstaden
                      ændret          geo_ændret geo_version visueltcenter_x
      1  2025-08-21 21:04:39 2025-08-21 21:04:39           9        12.57252
      2  2025-08-21 21:04:39 2025-08-21 21:04:39           7        12.58267
      3  2025-08-21 21:04:39 2025-08-21 21:04:39           7        12.56874
      4  2025-08-21 21:04:39 2025-08-21 21:04:39           8        12.56014
      5  2025-08-21 21:04:39 2025-08-21 21:04:39           8        12.56856
      6  2025-08-21 21:04:39 2025-08-21 21:04:39           8        12.60509
      7  2025-08-21 21:04:39 2025-08-21 21:04:39           7        12.59394
      8  2026-03-27 22:17:36 2026-03-27 22:17:36          12        12.55095
      9  2025-08-21 21:04:39 2025-08-21 21:04:39           9        12.56800
      10 2025-08-21 21:04:39 2025-08-21 21:04:39          10        12.56623
         visueltcenter_y                       geometry
      1         55.70528 MULTIPOLYGON (((12.56228 55...
      2         55.71087 MULTIPOLYGON (((12.58745 55...
      3         55.69809 MULTIPOLYGON (((12.56136 55...
      4         55.70791 MULTIPOLYGON (((12.55314 55...
      5         55.71552 MULTIPOLYGON (((12.55859 55...
      6         55.65088 MULTIPOLYGON (((12.59358 55...
      7         55.66577 MULTIPOLYGON (((12.58392 55...
      8         55.62567 MULTIPOLYGON (((12.58604 55...
      9         55.64696 MULTIPOLYGON (((12.55133 55...
      10        55.68159 MULTIPOLYGON (((12.55518 55...

---

    Code
      get_map_data(type = "afstemningsomraader")
    Message
      > Using cached response.
      Change this behaviour by setting cache = FALSE
    Output
      Simple feature collection with 1314 features and 23 fields
      Geometry type: MULTIPOLYGON
      Dimension:     XY
      Bounding box:  xmin: 8.07251 ymin: 54.55908 xmax: 15.1974 ymax: 57.75257
      Geodetic CRS:  WGS 84
      First 10 features:
         dagi_id nummer            navn      afstemningsstednavn
      1   707732      1     1. Østerbro              Idrætshuset
      2   711932      2         1. Nord         Strandvejsskolen
      3   704936      3          1. Syd                  Remisen
      4   706160      5         1. Vest       Nørre Fælled Skole
      5   710802      6     1. Nordvest         Kildevældsskolen
      6   711204      7 2. Sundbyvester        Sundby Idrætspark
      7   706166      8         2. Nord      Amager Fælled Skole
      8   706562      9      2. Ørestad            Ørestad Skole
      9   712286     10         2. Vest Skolen på Islands Brygge
      10  711926     11     3. Indre By             Rådhushallen
                     afstemningsstedadresseid
      1  0a3f507a-8121-32b8-e044-0003ba298018
      2  0a3f507a-f1b6-32b8-e044-0003ba298018
      3  0a3f507a-4851-32b8-e044-0003ba298018
      4  0a3f507a-457f-32b8-e044-0003ba298018
      5  0a3f507a-4387-32b8-e044-0003ba298018
      6  0a3f507a-6784-32b8-e044-0003ba298018
      7  0a3f507b-05f2-32b8-e044-0003ba298018
      8  20d7372c-7d2b-4ee3-a55c-54da765f5d84
      9  0a3f507a-3e64-32b8-e044-0003ba298018
      10 0a3f507a-ec01-32b8-e044-0003ba298018
                    afstemningsstedadressebetegnelse afstemningssted_adgangspunkt_x
      1  Gunnar Nu Hansens Plads 7, 2100 København Ø                       12.57485
      2                Sionsgade 1, 2100 København Ø                       12.57921
      3            Blegdamsvej 132, 2100 København Ø                       12.57679
      4       Biskop Krags Vænge 7, 2100 København Ø                       12.55815
      5            Bellmansgade 5A, 2100 København Ø                       12.56941
      6             Englandsvej 61, 2300 København S                       12.59996
      7            Sundholmsvej 2A, 2300 København S                       12.59499
      8     Arne Jacobsens Allé 21, 2300 København S                       12.58144
      9            Artillerivej 57, 2300 København S                       12.58333
      10           Rådhuspladsen 1, 1550 København V                       12.56958
         afstemningssted_adgangspunkt_y kommunekode kommunenavn regionskode
      1                        55.70477        0101   København        1084
      2                        55.71124        0101   København        1084
      3                        55.69868        0101   København        1084
      4                        55.70536        0101   København        1084
      5                        55.71540        0101   København        1084
      6                        55.65023        0101   København        1084
      7                        55.66245        0101   København        1084
      8                        55.63111        0101   København        1084
      9                        55.66292        0101   København        1084
      10                       55.67563        0101   København        1084
                regionsnavn opstillingskredsnummer opstillingskredsnavn
      1  Region Hovedstaden                      1             Østerbro
      2  Region Hovedstaden                      1             Østerbro
      3  Region Hovedstaden                      1             Østerbro
      4  Region Hovedstaden                      1             Østerbro
      5  Region Hovedstaden                      1             Østerbro
      6  Region Hovedstaden                      2         Sundbyvester
      7  Region Hovedstaden                      2         Sundbyvester
      8  Region Hovedstaden                      2         Sundbyvester
      9  Region Hovedstaden                      2         Sundbyvester
      10 Region Hovedstaden                      3             Indre By
         storkredsnummer storkredsnavn valglandsdelsbogstav valglandsdelsnavn
      1                1     København                    A       Hovedstaden
      2                1     København                    A       Hovedstaden
      3                1     København                    A       Hovedstaden
      4                1     København                    A       Hovedstaden
      5                1     København                    A       Hovedstaden
      6                1     København                    A       Hovedstaden
      7                1     København                    A       Hovedstaden
      8                1     København                    A       Hovedstaden
      9                1     København                    A       Hovedstaden
      10               1     København                    A       Hovedstaden
                      ændret          geo_ændret geo_version visueltcenter_x
      1  2025-08-21 21:04:39 2025-08-21 21:04:39           9        12.57252
      2  2025-08-21 21:04:39 2025-08-21 21:04:39           7        12.58267
      3  2025-08-21 21:04:39 2025-08-21 21:04:39           7        12.56874
      4  2025-08-21 21:04:39 2025-08-21 21:04:39           8        12.56014
      5  2025-08-21 21:04:39 2025-08-21 21:04:39           8        12.56856
      6  2025-08-21 21:04:39 2025-08-21 21:04:39           8        12.60509
      7  2025-08-21 21:04:39 2025-08-21 21:04:39           7        12.59394
      8  2026-03-27 22:17:36 2026-03-27 22:17:36          12        12.55095
      9  2025-08-21 21:04:39 2025-08-21 21:04:39           9        12.56800
      10 2025-08-21 21:04:39 2025-08-21 21:04:39          10        12.56623
         visueltcenter_y                       geometry
      1         55.70528 MULTIPOLYGON (((12.56228 55...
      2         55.71087 MULTIPOLYGON (((12.58745 55...
      3         55.69809 MULTIPOLYGON (((12.56136 55...
      4         55.70791 MULTIPOLYGON (((12.55314 55...
      5         55.71552 MULTIPOLYGON (((12.55859 55...
      6         55.65088 MULTIPOLYGON (((12.59358 55...
      7         55.66577 MULTIPOLYGON (((12.58392 55...
      8         55.62567 MULTIPOLYGON (((12.58604 55...
      9         55.64696 MULTIPOLYGON (((12.55133 55...
      10        55.68159 MULTIPOLYGON (((12.55518 55...

---

    Code
      get_map_data(type = "afstemningsomraader")
    Message
      > Using cached response.
      Change this behaviour by setting cache = FALSE
    Output
      Simple feature collection with 1314 features and 23 fields
      Geometry type: MULTIPOLYGON
      Dimension:     XY
      Bounding box:  xmin: 8.07251 ymin: 54.55908 xmax: 15.1974 ymax: 57.75257
      Geodetic CRS:  WGS 84
      First 10 features:
         dagi_id nummer            navn      afstemningsstednavn
      1   707732      1     1. Østerbro              Idrætshuset
      2   711932      2         1. Nord         Strandvejsskolen
      3   704936      3          1. Syd                  Remisen
      4   706160      5         1. Vest       Nørre Fælled Skole
      5   710802      6     1. Nordvest         Kildevældsskolen
      6   711204      7 2. Sundbyvester        Sundby Idrætspark
      7   706166      8         2. Nord      Amager Fælled Skole
      8   706562      9      2. Ørestad            Ørestad Skole
      9   712286     10         2. Vest Skolen på Islands Brygge
      10  711926     11     3. Indre By             Rådhushallen
                     afstemningsstedadresseid
      1  0a3f507a-8121-32b8-e044-0003ba298018
      2  0a3f507a-f1b6-32b8-e044-0003ba298018
      3  0a3f507a-4851-32b8-e044-0003ba298018
      4  0a3f507a-457f-32b8-e044-0003ba298018
      5  0a3f507a-4387-32b8-e044-0003ba298018
      6  0a3f507a-6784-32b8-e044-0003ba298018
      7  0a3f507b-05f2-32b8-e044-0003ba298018
      8  20d7372c-7d2b-4ee3-a55c-54da765f5d84
      9  0a3f507a-3e64-32b8-e044-0003ba298018
      10 0a3f507a-ec01-32b8-e044-0003ba298018
                    afstemningsstedadressebetegnelse afstemningssted_adgangspunkt_x
      1  Gunnar Nu Hansens Plads 7, 2100 København Ø                       12.57485
      2                Sionsgade 1, 2100 København Ø                       12.57921
      3            Blegdamsvej 132, 2100 København Ø                       12.57679
      4       Biskop Krags Vænge 7, 2100 København Ø                       12.55815
      5            Bellmansgade 5A, 2100 København Ø                       12.56941
      6             Englandsvej 61, 2300 København S                       12.59996
      7            Sundholmsvej 2A, 2300 København S                       12.59499
      8     Arne Jacobsens Allé 21, 2300 København S                       12.58144
      9            Artillerivej 57, 2300 København S                       12.58333
      10           Rådhuspladsen 1, 1550 København V                       12.56958
         afstemningssted_adgangspunkt_y kommunekode kommunenavn regionskode
      1                        55.70477        0101   København        1084
      2                        55.71124        0101   København        1084
      3                        55.69868        0101   København        1084
      4                        55.70536        0101   København        1084
      5                        55.71540        0101   København        1084
      6                        55.65023        0101   København        1084
      7                        55.66245        0101   København        1084
      8                        55.63111        0101   København        1084
      9                        55.66292        0101   København        1084
      10                       55.67563        0101   København        1084
                regionsnavn opstillingskredsnummer opstillingskredsnavn
      1  Region Hovedstaden                      1             Østerbro
      2  Region Hovedstaden                      1             Østerbro
      3  Region Hovedstaden                      1             Østerbro
      4  Region Hovedstaden                      1             Østerbro
      5  Region Hovedstaden                      1             Østerbro
      6  Region Hovedstaden                      2         Sundbyvester
      7  Region Hovedstaden                      2         Sundbyvester
      8  Region Hovedstaden                      2         Sundbyvester
      9  Region Hovedstaden                      2         Sundbyvester
      10 Region Hovedstaden                      3             Indre By
         storkredsnummer storkredsnavn valglandsdelsbogstav valglandsdelsnavn
      1                1     København                    A       Hovedstaden
      2                1     København                    A       Hovedstaden
      3                1     København                    A       Hovedstaden
      4                1     København                    A       Hovedstaden
      5                1     København                    A       Hovedstaden
      6                1     København                    A       Hovedstaden
      7                1     København                    A       Hovedstaden
      8                1     København                    A       Hovedstaden
      9                1     København                    A       Hovedstaden
      10               1     København                    A       Hovedstaden
                      ændret          geo_ændret geo_version visueltcenter_x
      1  2025-08-21 21:04:39 2025-08-21 21:04:39           9        12.57252
      2  2025-08-21 21:04:39 2025-08-21 21:04:39           7        12.58267
      3  2025-08-21 21:04:39 2025-08-21 21:04:39           7        12.56874
      4  2025-08-21 21:04:39 2025-08-21 21:04:39           8        12.56014
      5  2025-08-21 21:04:39 2025-08-21 21:04:39           8        12.56856
      6  2025-08-21 21:04:39 2025-08-21 21:04:39           8        12.60509
      7  2025-08-21 21:04:39 2025-08-21 21:04:39           7        12.59394
      8  2026-03-27 22:17:36 2026-03-27 22:17:36          12        12.55095
      9  2025-08-21 21:04:39 2025-08-21 21:04:39           9        12.56800
      10 2025-08-21 21:04:39 2025-08-21 21:04:39          10        12.56623
         visueltcenter_y                       geometry
      1         55.70528 MULTIPOLYGON (((12.56228 55...
      2         55.71087 MULTIPOLYGON (((12.58745 55...
      3         55.69809 MULTIPOLYGON (((12.56136 55...
      4         55.70791 MULTIPOLYGON (((12.55314 55...
      5         55.71552 MULTIPOLYGON (((12.55859 55...
      6         55.65088 MULTIPOLYGON (((12.59358 55...
      7         55.66577 MULTIPOLYGON (((12.58392 55...
      8         55.62567 MULTIPOLYGON (((12.58604 55...
      9         55.64696 MULTIPOLYGON (((12.55133 55...
      10        55.68159 MULTIPOLYGON (((12.55518 55...

---

    Code
      get_map_data(type = "afstemningsomraader")
    Message
      > Using cached response.
      Change this behaviour by setting cache = FALSE
    Output
      Simple feature collection with 1314 features and 23 fields
      Geometry type: MULTIPOLYGON
      Dimension:     XY
      Bounding box:  xmin: 8.07251 ymin: 54.55908 xmax: 15.1974 ymax: 57.75257
      Geodetic CRS:  WGS 84
      First 10 features:
         dagi_id nummer            navn      afstemningsstednavn
      1   707732      1     1. Østerbro              Idrætshuset
      2   711932      2         1. Nord         Strandvejsskolen
      3   704936      3          1. Syd                  Remisen
      4   706160      5         1. Vest       Nørre Fælled Skole
      5   710802      6     1. Nordvest         Kildevældsskolen
      6   711204      7 2. Sundbyvester        Sundby Idrætspark
      7   706166      8         2. Nord      Amager Fælled Skole
      8   706562      9      2. Ørestad            Ørestad Skole
      9   712286     10         2. Vest Skolen på Islands Brygge
      10  711926     11     3. Indre By             Rådhushallen
                     afstemningsstedadresseid
      1  0a3f507a-8121-32b8-e044-0003ba298018
      2  0a3f507a-f1b6-32b8-e044-0003ba298018
      3  0a3f507a-4851-32b8-e044-0003ba298018
      4  0a3f507a-457f-32b8-e044-0003ba298018
      5  0a3f507a-4387-32b8-e044-0003ba298018
      6  0a3f507a-6784-32b8-e044-0003ba298018
      7  0a3f507b-05f2-32b8-e044-0003ba298018
      8  20d7372c-7d2b-4ee3-a55c-54da765f5d84
      9  0a3f507a-3e64-32b8-e044-0003ba298018
      10 0a3f507a-ec01-32b8-e044-0003ba298018
                    afstemningsstedadressebetegnelse afstemningssted_adgangspunkt_x
      1  Gunnar Nu Hansens Plads 7, 2100 København Ø                       12.57485
      2                Sionsgade 1, 2100 København Ø                       12.57921
      3            Blegdamsvej 132, 2100 København Ø                       12.57679
      4       Biskop Krags Vænge 7, 2100 København Ø                       12.55815
      5            Bellmansgade 5A, 2100 København Ø                       12.56941
      6             Englandsvej 61, 2300 København S                       12.59996
      7            Sundholmsvej 2A, 2300 København S                       12.59499
      8     Arne Jacobsens Allé 21, 2300 København S                       12.58144
      9            Artillerivej 57, 2300 København S                       12.58333
      10           Rådhuspladsen 1, 1550 København V                       12.56958
         afstemningssted_adgangspunkt_y kommunekode kommunenavn regionskode
      1                        55.70477        0101   København        1084
      2                        55.71124        0101   København        1084
      3                        55.69868        0101   København        1084
      4                        55.70536        0101   København        1084
      5                        55.71540        0101   København        1084
      6                        55.65023        0101   København        1084
      7                        55.66245        0101   København        1084
      8                        55.63111        0101   København        1084
      9                        55.66292        0101   København        1084
      10                       55.67563        0101   København        1084
                regionsnavn opstillingskredsnummer opstillingskredsnavn
      1  Region Hovedstaden                      1             Østerbro
      2  Region Hovedstaden                      1             Østerbro
      3  Region Hovedstaden                      1             Østerbro
      4  Region Hovedstaden                      1             Østerbro
      5  Region Hovedstaden                      1             Østerbro
      6  Region Hovedstaden                      2         Sundbyvester
      7  Region Hovedstaden                      2         Sundbyvester
      8  Region Hovedstaden                      2         Sundbyvester
      9  Region Hovedstaden                      2         Sundbyvester
      10 Region Hovedstaden                      3             Indre By
         storkredsnummer storkredsnavn valglandsdelsbogstav valglandsdelsnavn
      1                1     København                    A       Hovedstaden
      2                1     København                    A       Hovedstaden
      3                1     København                    A       Hovedstaden
      4                1     København                    A       Hovedstaden
      5                1     København                    A       Hovedstaden
      6                1     København                    A       Hovedstaden
      7                1     København                    A       Hovedstaden
      8                1     København                    A       Hovedstaden
      9                1     København                    A       Hovedstaden
      10               1     København                    A       Hovedstaden
                      ændret          geo_ændret geo_version visueltcenter_x
      1  2025-08-21 21:04:39 2025-08-21 21:04:39           9        12.57252
      2  2025-08-21 21:04:39 2025-08-21 21:04:39           7        12.58267
      3  2025-08-21 21:04:39 2025-08-21 21:04:39           7        12.56874
      4  2025-08-21 21:04:39 2025-08-21 21:04:39           8        12.56014
      5  2025-08-21 21:04:39 2025-08-21 21:04:39           8        12.56856
      6  2025-08-21 21:04:39 2025-08-21 21:04:39           8        12.60509
      7  2025-08-21 21:04:39 2025-08-21 21:04:39           7        12.59394
      8  2026-03-27 22:17:36 2026-03-27 22:17:36          12        12.55095
      9  2025-08-21 21:04:39 2025-08-21 21:04:39           9        12.56800
      10 2025-08-21 21:04:39 2025-08-21 21:04:39          10        12.56623
         visueltcenter_y                       geometry
      1         55.70528 MULTIPOLYGON (((12.56228 55...
      2         55.71087 MULTIPOLYGON (((12.58745 55...
      3         55.69809 MULTIPOLYGON (((12.56136 55...
      4         55.70791 MULTIPOLYGON (((12.55314 55...
      5         55.71552 MULTIPOLYGON (((12.55859 55...
      6         55.65088 MULTIPOLYGON (((12.59358 55...
      7         55.66577 MULTIPOLYGON (((12.58392 55...
      8         55.62567 MULTIPOLYGON (((12.58604 55...
      9         55.64696 MULTIPOLYGON (((12.55133 55...
      10        55.68159 MULTIPOLYGON (((12.55518 55...

