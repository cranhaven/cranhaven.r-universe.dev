# calc_selection_weights()

    {
      "type": "list",
      "attributes": {
        "class": {
          "type": "character",
          "attributes": {},
          "value": ["tbl_df", "tbl", "data.frame"]
        },
        "row.names": {
          "type": "integer",
          "attributes": {},
          "value": [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27]
        },
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["file_name", "type", "path", "aru_id", "manufacturer", "model", "aru_type", "site_id", "tz_offset", "date_time", "date", "longitude", "latitude", "tz", "t2sr", "t2ss", "doy", "psel_by", "psel_min", "psel_doy", "psel", "psel_scaled", "psel_std", "psel_normalized"]
        }
      },
      "value": [
        {
          "type": "character",
          "attributes": {},
          "value": ["P01_1_20200503T052000-0400_ARU.wav", "P02_1_20200504T052500_ARU.wav", "P02_1_20200505T073000_ARU.wav", "P03_1_20200506T100000-0400_ARU.wav", "P06_1_20200509T052000-0400_ARU.wav", "P07_1_20200509T052500_ARU.wav", "P07_1_20200510T073000_ARU.wav", "P08_1_20200511T100000-0400_ARU.wav", "P09_1_20200511T050000_ARU.wav", "P01_1_20200503T052000-0400_ARU.wav", "P02_1_20200504T052500_ARU.wav", "P02_1_20200505T073000_ARU.wav", "P03_1_20200506T100000-0400_ARU.wav", "P06_1_20200509T052000-0400_ARU.wav", "P07_1_20200509T052500_ARU.wav", "P07_1_20200510T073000_ARU.wav", "P08_1_20200511T100000-0400_ARU.wav", "P09_1_20200511T050000_ARU.wav", "P01_1_20200503T052000-0400_ARU.wav", "P02_1_20200504T052500_ARU.wav", "P02_1_20200505T073000_ARU.wav", "P03_1_20200506T100000-0400_ARU.wav", "P06_1_20200509T052000-0400_ARU.wav", "P07_1_20200509T052500_ARU.wav", "P07_1_20200510T073000_ARU.wav", "P08_1_20200511T100000-0400_ARU.wav", "P09_1_20200511T050000_ARU.wav"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["wav", "wav", "wav", "wav", "wav", "wav", "wav", "wav", "wav", "wav", "wav", "wav", "wav", "wav", "wav", "wav", "wav", "wav", "wav", "wav", "wav", "wav", "wav", "wav", "wav", "wav", "wav"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["a_BARLT10962_P01_1/P01_1_20200503T052000-0400_ARU.wav", "a_S4A01234_P02_1/P02_1_20200504T052500_ARU.wav", "a_S4A01234_P02_1/P02_1_20200505T073000_ARU.wav", "a_BARLT10962_P03_1/P03_1_20200506T100000-0400_ARU.wav", "a_BARLT10962_P06_1/P06_1_20200509T052000-0400_ARU.wav", "a_S4A01234_P07_1/P07_1_20200509T052500_ARU.wav", "a_S4A01234_P07_1/P07_1_20200510T073000_ARU.wav", "a_BARLT10962_P08_1/P08_1_20200511T100000-0400_ARU.wav", "a_S4A02222_P09_1/P09_1_20200511T050000_ARU.wav", "j_BARLT10962_P01_1/P01_1_20200503T052000-0400_ARU.wav", "j_S4A01234_P02_1/P02_1_20200504T052500_ARU.wav", "j_S4A01234_P02_1/P02_1_20200505T073000_ARU.wav", "j_BARLT10962_P03_1/P03_1_20200506T100000-0400_ARU.wav", "j_BARLT10962_P06_1/P06_1_20200509T052000-0400_ARU.wav", "j_S4A01234_P07_1/P07_1_20200509T052500_ARU.wav", "j_S4A01234_P07_1/P07_1_20200510T073000_ARU.wav", "j_BARLT10962_P08_1/P08_1_20200511T100000-0400_ARU.wav", "j_S4A02222_P09_1/P09_1_20200511T050000_ARU.wav", "o_BARLT10962_P01_1/P01_1_20200503T052000-0400_ARU.wav", "o_S4A01234_P02_1/P02_1_20200504T052500_ARU.wav", "o_S4A01234_P02_1/P02_1_20200505T073000_ARU.wav", "o_BARLT10962_P03_1/P03_1_20200506T100000-0400_ARU.wav", "o_BARLT10962_P06_1/P06_1_20200509T052000-0400_ARU.wav", "o_S4A01234_P07_1/P07_1_20200509T052500_ARU.wav", "o_S4A01234_P07_1/P07_1_20200510T073000_ARU.wav", "o_BARLT10962_P08_1/P08_1_20200511T100000-0400_ARU.wav", "o_S4A02222_P09_1/P09_1_20200511T050000_ARU.wav"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["BARLT10962", "S4A01234", "S4A01234", "BARLT10962", "BARLT10962", "S4A01234", "S4A01234", "BARLT10962", "S4A02222", "BARLT10962", "S4A01234", "S4A01234", "BARLT10962", "BARLT10962", "S4A01234", "S4A01234", "BARLT10962", "S4A02222", "BARLT10962", "S4A01234", "S4A01234", "BARLT10962", "BARLT10962", "S4A01234", "S4A01234", "BARLT10962", "S4A02222"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["Frontier Labs", "Wildlife Acoustics", "Wildlife Acoustics", "Frontier Labs", "Frontier Labs", "Wildlife Acoustics", "Wildlife Acoustics", "Frontier Labs", "Wildlife Acoustics", "Frontier Labs", "Wildlife Acoustics", "Wildlife Acoustics", "Frontier Labs", "Frontier Labs", "Wildlife Acoustics", "Wildlife Acoustics", "Frontier Labs", "Wildlife Acoustics", "Frontier Labs", "Wildlife Acoustics", "Wildlife Acoustics", "Frontier Labs", "Frontier Labs", "Wildlife Acoustics", "Wildlife Acoustics", "Frontier Labs", "Wildlife Acoustics"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["BAR-LT", "Song Meter 4", "Song Meter 4", "BAR-LT", "BAR-LT", "Song Meter 4", "Song Meter 4", "BAR-LT", "Song Meter 4", "BAR-LT", "Song Meter 4", "Song Meter 4", "BAR-LT", "BAR-LT", "Song Meter 4", "Song Meter 4", "BAR-LT", "Song Meter 4", "BAR-LT", "Song Meter 4", "Song Meter 4", "BAR-LT", "BAR-LT", "Song Meter 4", "Song Meter 4", "BAR-LT", "Song Meter 4"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["BARLT", "SongMeter", "SongMeter", "BARLT", "BARLT", "SongMeter", "SongMeter", "BARLT", "SongMeter", "BARLT", "SongMeter", "SongMeter", "BARLT", "BARLT", "SongMeter", "SongMeter", "BARLT", "SongMeter", "BARLT", "SongMeter", "SongMeter", "BARLT", "BARLT", "SongMeter", "SongMeter", "BARLT", "SongMeter"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["P01_1", "P02_1", "P02_1", "P03_1", "P06_1", "P07_1", "P07_1", "P08_1", "P09_1", "P01_1", "P02_1", "P02_1", "P03_1", "P06_1", "P07_1", "P07_1", "P08_1", "P09_1", "P01_1", "P02_1", "P02_1", "P03_1", "P06_1", "P07_1", "P07_1", "P08_1", "P09_1"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["-0400", null, null, "-0400", "-0400", null, null, "-0400", null, "-0400", null, null, "-0400", "-0400", null, null, "-0400", null, "-0400", null, null, "-0400", "-0400", null, null, "-0400", null]
        },
        {
          "type": "double",
          "attributes": {
            "tzone": {
              "type": "character",
              "attributes": {},
              "value": ["UTC"]
            },
            "class": {
              "type": "character",
              "attributes": {},
              "value": ["POSIXct", "POSIXt"]
            }
          },
          "value": [1588483200, 1588569900, 1588663800, 1588759200, 1589001600, 1589001900, 1589095800, 1589191200, 1589173200, 1588483200, 1588569900, 1588663800, 1588759200, 1589001600, 1589001900, 1589095800, 1589191200, 1589173200, 1588483200, 1588569900, 1588663800, 1588759200, 1589001600, 1589001900, 1589095800, 1589191200, 1589173200]
        },
        {
          "type": "double",
          "attributes": {
            "class": {
              "type": "character",
              "attributes": {},
              "value": ["Date"]
            }
          },
          "value": [18385, 18386, 18387, 18388, 18391, 18391, 18392, 18393, 18393, 18385, 18386, 18387, 18388, 18391, 18391, 18392, 18393, 18393, 18385, 18386, 18387, 18388, 18391, 18391, 18392, 18393, 18393]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [-85.03, -87.45, -87.45, -90.38, -90.08, -86.03, -86.03, -84.45, -91.38, -85.03, -87.45, -87.45, -90.38, -90.08, -86.03, -86.03, -84.45, -91.38, -85.03, -87.45, -87.45, -90.38, -90.08, -86.03, -86.03, -84.45, -91.38]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [50.01, 52.68, 52.68, 48.99, 52, 50.45, 50.45, 48.999, 45, 50.01, 52.68, 52.68, 48.99, 52, 50.45, 50.45, 48.999, 45, 50.01, 52.68, 52.68, 48.99, 52, 50.45, 50.45, 48.999, 45]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["America/Toronto", "America/Toronto", "America/Toronto", "America/Toronto", "America/Winnipeg", "America/Toronto", "America/Toronto", "America/Toronto", "America/Chicago", "America/Toronto", "America/Toronto", "America/Toronto", "America/Toronto", "America/Winnipeg", "America/Toronto", "America/Toronto", "America/Toronto", "America/Chicago", "America/Toronto", "America/Toronto", "America/Toronto", "America/Toronto", "America/Winnipeg", "America/Toronto", "America/Toronto", "America/Toronto", "America/Chicago"]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [-53.21666667, -47.25, 79.61666667, 207.13333333, 3.58333333, -40.93333333, 85.61666667, 238.31666667, -41.88333333, -53.21666667, -47.25, 79.61666667, 207.13333333, 3.58333333, -40.93333333, 85.61666667, 238.31666667, -41.88333333, -53.21666667, -47.25, 79.61666667, 207.13333333, 3.58333333, -40.93333333, 85.61666667, 238.31666667, -41.88333333]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [498.41666667, 483.41666667, 606.68333333, -685.88333333, 521.93333333, 488.75, 612.23333333, -669.31666667, 516.65, 498.41666667, 483.41666667, 606.68333333, -685.88333333, 521.93333333, 488.75, 612.23333333, -669.31666667, 516.65, 498.41666667, 483.41666667, 606.68333333, -685.88333333, 521.93333333, 488.75, 612.23333333, -669.31666667, 516.65]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [124, 125, 126, 127, 130, 130, 131, 132, 132, 124, 125, 126, 127, 130, 130, 131, 132, 132, 124, 125, 126, 127, 130, 130, 131, 132, 132]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["t2sr", "t2sr", "t2sr", "t2sr", "t2sr", "t2sr", "t2sr", "t2sr", "t2sr", "t2sr", "t2sr", "t2sr", "t2sr", "t2sr", "t2sr", "t2sr", "t2sr", "t2sr", "t2sr", "t2sr", "t2sr", "t2sr", "t2sr", "t2sr", "t2sr", "t2sr", "t2sr"]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [-0.53599721, -0.52402648, -0.48126855, -0.84075194, -0.45852417, -0.51295356, -0.48919915, -0.98957549, -0.5147367, -0.53599721, -0.52402648, -0.48126855, -0.84075194, -0.45852417, -0.51295356, -0.48919915, -0.98957549, -0.5147367, -0.53599721, -0.52402648, -0.48126855, -0.84075194, -0.45852417, -0.51295356, -0.48919915, -0.98957549, -0.5147367]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [-0.93517202, -0.92000393, -0.90525141, -0.89091445, -0.85039697, -0.85039697, -0.83772227, -0.82546313, -0.82546313, -0.93517202, -0.92000393, -0.90525141, -0.89091445, -0.85039697, -0.85039697, -0.83772227, -0.82546313, -0.82546313, -0.93517202, -0.92000393, -0.90525141, -0.89091445, -0.85039697, -0.85039697, -0.83772227, -0.82546313, -0.82546313]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [0.22965681, 0.23597476, 0.24994361, 0.17698923, 0.27011131, 0.25580227, 0.26529273, 0.16283162, 0.26179335, 0.22965681, 0.23597476, 0.24994361, 0.17698923, 0.27011131, 0.25580227, 0.26529273, 0.16283162, 0.26179335, 0.22965681, 0.23597476, 0.24994361, 0.17698923, 0.27011131, 0.25580227, 0.26529273, 0.16283162, 0.26179335]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [0.85023024, 0.87362044, 0.92533557, 0.65524553, 1, 0.94702537, 0.98216075, 0.60283154, 0.96920542, 0.85023024, 0.87362044, 0.92533557, 0.65524553, 1, 0.94702537, 0.98216075, 0.60283154, 0.96920542, 0.85023024, 0.87362044, 0.92533557, 0.65524553, 1, 0.94702537, 0.98216075, 0.60283154, 0.96920542]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [1, 0.94411202, 1, 1, 1, 0.96422644, 1, 1, 1, 1, 0.94411202, 1, 1, 1, 0.96422644, 1, 1, 1, 1, 0.94411202, 1, 1, 1, 0.96422644, 1, 1, 1]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [0.001, 0.001, 1, 0.001, 0.001, 0.001, 1, 0.001, 0.001, 0.001, 0.001, 1, 0.001, 0.001, 0.001, 1, 0.001, 0.001, 0.001, 0.001, 1, 0.001, 0.001, 0.001, 1, 0.001, 0.001]
        }
      ]
    }

# sample_recordings()

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["sites_legacy", "sites_base", "sites_over", "sites_near", "design"]
        },
        "class": {
          "type": "character",
          "attributes": {},
          "value": ["sp_design"]
        }
      },
      "value": [
        {
          "type": "NULL"
        },
        {
          "type": "list",
          "attributes": {
            "names": {
              "type": "character",
              "attributes": {},
              "value": ["siteID", "siteuse", "replsite", "lon_WGS84", "lat_WGS84", "stratum", "wgt", "ip", "caty", "aux", "file_name", "type", "path", "aru_id", "manufacturer", "model", "aru_type", "site_id", "tz_offset", "date_time", "date", "longitude", "latitude", "tz", "t2ss", "psel_by", "psel_min", "psel_doy", "psel", "psel_scaled", "psel_std", "psel_normalized", "geometry"]
            },
            "row.names": {
              "type": "integer",
              "attributes": {},
              "value": [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12]
            },
            "class": {
              "type": "character",
              "attributes": {},
              "value": ["sf", "data.frame"]
            },
            "sf_column": {
              "type": "character",
              "attributes": {},
              "value": ["geometry"]
            },
            "agr": {
              "type": "integer",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["siteID", "siteuse", "replsite", "lon_WGS84", "lat_WGS84", "stratum", "wgt", "ip", "caty", "aux", "file_name", "type", "path", "aru_id", "manufacturer", "model", "aru_type", "site_id", "tz_offset", "date_time", "date", "longitude", "latitude", "tz", "t2ss", "psel_by", "psel_min", "psel_doy", "psel", "psel_scaled", "psel_std", "psel_normalized"]
                },
                "levels": {
                  "type": "character",
                  "attributes": {},
                  "value": ["constant", "aggregate", "identity"]
                },
                "class": {
                  "type": "character",
                  "attributes": {},
                  "value": ["factor"]
                }
              },
              "value": ["NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA"]
            }
          },
          "value": [
            {
              "type": "character",
              "attributes": {},
              "value": ["sample-01", "sample-02", "sample-03", "sample-04", "sample-05", "sample-06", "sample-07", "sample-08", "sample-09", "sample-10", "sample-11", "sample-12"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["Base", "Base", "Base", "Base", "Base", "Base", "Base", "Base", "Base", "Base", "Base", "Base"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["None", "None", "None", "None", "None", "None", "None", "None", "None", "None", "None", "None"]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [0.00118578, 0.00116781, 0.00118578, 0.00112289, 0.00116781, 0.00111391, 0.00118578, 0.00117679, 0.00114086, 0.00113188, 0.00116781, 0.00118578]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [-0.00037878, 0.00003241, 0.00215526, -0.00042731, 0.00003241, -0.00048128, -0.00037878, 0.00077429, 0.00187325, 0.00072003, -0.00037019, 0.00215526]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["None", "None", "None", "None", "None", "None", "None", "None", "None", "None", "None", "None"]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [2.22708462, 2.22708462, 2.22708462, 2.35891989, 2.22708462, 2.22708462, 2.22708462, 2.22708462, 2.22708462, 2.22708462, 2.3097112, 2.22708462]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [0.44901752, 0.44901752, 0.44901752, 0.42392283, 0.44901752, 0.44901752, 0.44901752, 0.44901752, 0.44901752, 0.44901752, 0.43295456, 0.44901752]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["None", "None", "None", "None", "None", "None", "None", "None", "None", "None", "None", "None"]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [1, 1, 1, 0.94411202, 1, 1, 1, 1, 1, 1, 0.96422644, 1]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["P09_1_20200511T050000_ARU.wav", "P06_1_20200509T052000-0400_ARU.wav", "P08_1_20200511T100000-0400_ARU.wav", "P02_1_20200504T052500_ARU.wav", "P06_1_20200509T052000-0400_ARU.wav", "P01_1_20200503T052000-0400_ARU.wav", "P09_1_20200511T050000_ARU.wav", "P07_1_20200510T073000_ARU.wav", "P03_1_20200506T100000-0400_ARU.wav", "P02_1_20200505T073000_ARU.wav", "P07_1_20200509T052500_ARU.wav", "P08_1_20200511T100000-0400_ARU.wav"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["wav", "wav", "wav", "wav", "wav", "wav", "wav", "wav", "wav", "wav", "wav", "wav"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["a_S4A02222_P09_1/P09_1_20200511T050000_ARU.wav", "a_BARLT10962_P06_1/P06_1_20200509T052000-0400_ARU.wav", "o_BARLT10962_P08_1/P08_1_20200511T100000-0400_ARU.wav", "o_S4A01234_P02_1/P02_1_20200504T052500_ARU.wav", "o_BARLT10962_P06_1/P06_1_20200509T052000-0400_ARU.wav", "o_BARLT10962_P01_1/P01_1_20200503T052000-0400_ARU.wav", "o_S4A02222_P09_1/P09_1_20200511T050000_ARU.wav", "o_S4A01234_P07_1/P07_1_20200510T073000_ARU.wav", "j_BARLT10962_P03_1/P03_1_20200506T100000-0400_ARU.wav", "a_S4A01234_P02_1/P02_1_20200505T073000_ARU.wav", "o_S4A01234_P07_1/P07_1_20200509T052500_ARU.wav", "a_BARLT10962_P08_1/P08_1_20200511T100000-0400_ARU.wav"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["S4A02222", "BARLT10962", "BARLT10962", "S4A01234", "BARLT10962", "BARLT10962", "S4A02222", "S4A01234", "BARLT10962", "S4A01234", "S4A01234", "BARLT10962"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["Wildlife Acoustics", "Frontier Labs", "Frontier Labs", "Wildlife Acoustics", "Frontier Labs", "Frontier Labs", "Wildlife Acoustics", "Wildlife Acoustics", "Frontier Labs", "Wildlife Acoustics", "Wildlife Acoustics", "Frontier Labs"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["Song Meter 4", "BAR-LT", "BAR-LT", "Song Meter 4", "BAR-LT", "BAR-LT", "Song Meter 4", "Song Meter 4", "BAR-LT", "Song Meter 4", "Song Meter 4", "BAR-LT"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["SongMeter", "BARLT", "BARLT", "SongMeter", "BARLT", "BARLT", "SongMeter", "SongMeter", "BARLT", "SongMeter", "SongMeter", "BARLT"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["P09_1", "P06_1", "P08_1", "P02_1", "P06_1", "P01_1", "P09_1", "P07_1", "P03_1", "P02_1", "P07_1", "P08_1"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": [null, "-0400", "-0400", null, "-0400", "-0400", null, null, "-0400", null, null, "-0400"]
            },
            {
              "type": "double",
              "attributes": {
                "class": {
                  "type": "character",
                  "attributes": {},
                  "value": ["POSIXct", "POSIXt"]
                },
                "tzone": {
                  "type": "character",
                  "attributes": {},
                  "value": ["UTC"]
                }
              },
              "value": [1589173200, 1589001600, 1589191200, 1588569900, 1589001600, 1588483200, 1589173200, 1589095800, 1588759200, 1588663800, 1589001900, 1589191200]
            },
            {
              "type": "double",
              "attributes": {
                "class": {
                  "type": "character",
                  "attributes": {},
                  "value": ["Date"]
                }
              },
              "value": [18393, 18391, 18393, 18386, 18391, 18385, 18393, 18392, 18388, 18387, 18391, 18393]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [-91.38, -90.08, -84.45, -87.45, -90.08, -85.03, -91.38, -86.03, -90.38, -87.45, -86.03, -84.45]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [45, 52, 48.999, 52.68, 52, 50.01, 45, 50.45, 48.99, 52.68, 50.45, 48.999]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["America/Chicago", "America/Winnipeg", "America/Toronto", "America/Toronto", "America/Winnipeg", "America/Toronto", "America/Chicago", "America/Toronto", "America/Toronto", "America/Toronto", "America/Toronto", "America/Toronto"]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [516.65, 521.93333333, -669.31666667, 483.41666667, 521.93333333, 498.41666667, 516.65, 612.23333333, -685.88333333, 606.68333333, 488.75, -669.31666667]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["t2sr", "t2sr", "t2sr", "t2sr", "t2sr", "t2sr", "t2sr", "t2sr", "t2sr", "t2sr", "t2sr", "t2sr"]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [-0.5147367, -0.45852417, -0.98957549, -0.52402648, -0.45852417, -0.53599721, -0.5147367, -0.48919915, -0.84075194, -0.48126855, -0.51295356, -0.98957549]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [-0.82546313, -0.85039697, -0.82546313, -0.92000393, -0.85039697, -0.93517202, -0.82546313, -0.83772227, -0.89091445, -0.90525141, -0.85039697, -0.82546313]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [0.26179335, 0.27011131, 0.16283162, 0.23597476, 0.27011131, 0.22965681, 0.26179335, 0.26529273, 0.17698923, 0.24994361, 0.25580227, 0.16283162]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [0.96920542, 1, 0.60283154, 0.87362044, 1, 0.85023024, 0.96920542, 0.98216075, 0.65524553, 0.92533557, 0.94702537, 0.60283154]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [1, 1, 1, 0.94411202, 1, 1, 1, 1, 1, 1, 0.96422644, 1]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 1, 0.001, 1, 0.001, 0.001]
            },
            {
              "type": "list",
              "attributes": {
                "class": {
                  "type": "character",
                  "attributes": {},
                  "value": ["sfc_POINT", "sfc"]
                },
                "precision": {
                  "type": "double",
                  "attributes": {},
                  "value": [0]
                },
                "bbox": {
                  "type": "double",
                  "attributes": {
                    "names": {
                      "type": "character",
                      "attributes": {},
                      "value": ["xmin", "ymin", "xmax", "ymax"]
                    },
                    "class": {
                      "type": "character",
                      "attributes": {},
                      "value": ["bbox"]
                    }
                  },
                  "value": [124, -53.21666667, 132, 238.31666667]
                },
                "crs": {
                  "type": "list",
                  "attributes": {
                    "names": {
                      "type": "character",
                      "attributes": {},
                      "value": ["input", "wkt"]
                    },
                    "class": {
                      "type": "character",
                      "attributes": {},
                      "value": ["crs"]
                    }
                  },
                  "value": [
                    {
                      "type": "character",
                      "attributes": {},
                      "value": ["EPSG:3395"]
                    },
                    {
                      "type": "character",
                      "attributes": {},
                      "value": ["PROJCRS[\"WGS 84 / World Mercator\",\n    BASEGEOGCRS[\"WGS 84\",\n        ENSEMBLE[\"World Geodetic System 1984 ensemble\",\n            MEMBER[\"World Geodetic System 1984 (Transit)\"],\n            MEMBER[\"World Geodetic System 1984 (G730)\"],\n            MEMBER[\"World Geodetic System 1984 (G873)\"],\n            MEMBER[\"World Geodetic System 1984 (G1150)\"],\n            MEMBER[\"World Geodetic System 1984 (G1674)\"],\n            MEMBER[\"World Geodetic System 1984 (G1762)\"],\n            MEMBER[\"World Geodetic System 1984 (G2139)\"],\n            MEMBER[\"World Geodetic System 1984 (G2296)\"],\n            ELLIPSOID[\"WGS 84\",6378137,298.257223563,\n                LENGTHUNIT[\"metre\",1]],\n            ENSEMBLEACCURACY[2.0]],\n        PRIMEM[\"Greenwich\",0,\n            ANGLEUNIT[\"degree\",0.0174532925199433]],\n        ID[\"EPSG\",4326]],\n    CONVERSION[\"World Mercator\",\n        METHOD[\"Mercator (variant A)\",\n            ID[\"EPSG\",9804]],\n        PARAMETER[\"Latitude of natural origin\",0,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8801]],\n        PARAMETER[\"Longitude of natural origin\",0,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8802]],\n        PARAMETER[\"Scale factor at natural origin\",1,\n            SCALEUNIT[\"unity\",1],\n            ID[\"EPSG\",8805]],\n        PARAMETER[\"False easting\",0,\n            LENGTHUNIT[\"metre\",1],\n            ID[\"EPSG\",8806]],\n        PARAMETER[\"False northing\",0,\n            LENGTHUNIT[\"metre\",1],\n            ID[\"EPSG\",8807]]],\n    CS[Cartesian,2],\n        AXIS[\"(E)\",east,\n            ORDER[1],\n            LENGTHUNIT[\"metre\",1]],\n        AXIS[\"(N)\",north,\n            ORDER[2],\n            LENGTHUNIT[\"metre\",1]],\n    USAGE[\n        SCOPE[\"Very small scale conformal mapping.\"],\n        AREA[\"World between 80°S and 84°N.\"],\n        BBOX[-80,-180,84,180]],\n    ID[\"EPSG\",3395]]"]
                    }
                  ]
                },
                "n_empty": {
                  "type": "integer",
                  "attributes": {},
                  "value": [0]
                }
              },
              "value": [
                {
                  "type": "double",
                  "attributes": {
                    "class": {
                      "type": "character",
                      "attributes": {},
                      "value": ["XY", "POINT", "sfg"]
                    }
                  },
                  "value": [132, -41.88333333]
                },
                {
                  "type": "double",
                  "attributes": {
                    "class": {
                      "type": "character",
                      "attributes": {},
                      "value": ["XY", "POINT", "sfg"]
                    }
                  },
                  "value": [130, 3.58333333]
                },
                {
                  "type": "double",
                  "attributes": {
                    "class": {
                      "type": "character",
                      "attributes": {},
                      "value": ["XY", "POINT", "sfg"]
                    }
                  },
                  "value": [132, 238.31666667]
                },
                {
                  "type": "double",
                  "attributes": {
                    "class": {
                      "type": "character",
                      "attributes": {},
                      "value": ["XY", "POINT", "sfg"]
                    }
                  },
                  "value": [125, -47.25]
                },
                {
                  "type": "double",
                  "attributes": {
                    "class": {
                      "type": "character",
                      "attributes": {},
                      "value": ["XY", "POINT", "sfg"]
                    }
                  },
                  "value": [130, 3.58333333]
                },
                {
                  "type": "double",
                  "attributes": {
                    "class": {
                      "type": "character",
                      "attributes": {},
                      "value": ["XY", "POINT", "sfg"]
                    }
                  },
                  "value": [124, -53.21666667]
                },
                {
                  "type": "double",
                  "attributes": {
                    "class": {
                      "type": "character",
                      "attributes": {},
                      "value": ["XY", "POINT", "sfg"]
                    }
                  },
                  "value": [132, -41.88333333]
                },
                {
                  "type": "double",
                  "attributes": {
                    "class": {
                      "type": "character",
                      "attributes": {},
                      "value": ["XY", "POINT", "sfg"]
                    }
                  },
                  "value": [131, 85.61666667]
                },
                {
                  "type": "double",
                  "attributes": {
                    "class": {
                      "type": "character",
                      "attributes": {},
                      "value": ["XY", "POINT", "sfg"]
                    }
                  },
                  "value": [127, 207.13333333]
                },
                {
                  "type": "double",
                  "attributes": {
                    "class": {
                      "type": "character",
                      "attributes": {},
                      "value": ["XY", "POINT", "sfg"]
                    }
                  },
                  "value": [126, 79.61666667]
                },
                {
                  "type": "double",
                  "attributes": {
                    "class": {
                      "type": "character",
                      "attributes": {},
                      "value": ["XY", "POINT", "sfg"]
                    }
                  },
                  "value": [130, -40.93333333]
                },
                {
                  "type": "double",
                  "attributes": {
                    "class": {
                      "type": "character",
                      "attributes": {},
                      "value": ["XY", "POINT", "sfg"]
                    }
                  },
                  "value": [132, 238.31666667]
                }
              ]
            }
          ]
        },
        {
          "type": "list",
          "attributes": {
            "names": {
              "type": "character",
              "attributes": {},
              "value": ["siteID", "siteuse", "replsite", "lon_WGS84", "lat_WGS84", "stratum", "wgt", "ip", "caty", "aux", "file_name", "type", "path", "aru_id", "manufacturer", "model", "aru_type", "site_id", "tz_offset", "date_time", "date", "longitude", "latitude", "tz", "t2ss", "psel_by", "psel_min", "psel_doy", "psel", "psel_scaled", "psel_std", "psel_normalized", "geometry"]
            },
            "row.names": {
              "type": "integer",
              "attributes": {},
              "value": [1, 2]
            },
            "class": {
              "type": "character",
              "attributes": {},
              "value": ["sf", "data.frame"]
            },
            "sf_column": {
              "type": "character",
              "attributes": {},
              "value": ["geometry"]
            },
            "agr": {
              "type": "integer",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["siteID", "siteuse", "replsite", "lon_WGS84", "lat_WGS84", "stratum", "wgt", "ip", "caty", "aux", "file_name", "type", "path", "aru_id", "manufacturer", "model", "aru_type", "site_id", "tz_offset", "date_time", "date", "longitude", "latitude", "tz", "t2ss", "psel_by", "psel_min", "psel_doy", "psel", "psel_scaled", "psel_std", "psel_normalized"]
                },
                "levels": {
                  "type": "character",
                  "attributes": {},
                  "value": ["constant", "aggregate", "identity"]
                },
                "class": {
                  "type": "character",
                  "attributes": {},
                  "value": ["factor"]
                }
              },
              "value": ["NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA"]
            }
          },
          "value": [
            {
              "type": "character",
              "attributes": {},
              "value": ["sample-13", "sample-14"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["Over", "Over"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["Next", "Next"]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [0.00112289, 0.00113188]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [-0.00042731, 0.00072003]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["None", "None"]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [2.35891989, 2.22708462]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [0.42392283, 0.44901752]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["None", "None"]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [0.94411202, 1]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["P02_1_20200504T052500_ARU.wav", "P02_1_20200505T073000_ARU.wav"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["wav", "wav"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["a_S4A01234_P02_1/P02_1_20200504T052500_ARU.wav", "j_S4A01234_P02_1/P02_1_20200505T073000_ARU.wav"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["S4A01234", "S4A01234"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["Wildlife Acoustics", "Wildlife Acoustics"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["Song Meter 4", "Song Meter 4"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["SongMeter", "SongMeter"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["P02_1", "P02_1"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": [null, null]
            },
            {
              "type": "double",
              "attributes": {
                "class": {
                  "type": "character",
                  "attributes": {},
                  "value": ["POSIXct", "POSIXt"]
                },
                "tzone": {
                  "type": "character",
                  "attributes": {},
                  "value": ["UTC"]
                }
              },
              "value": [1588569900, 1588663800]
            },
            {
              "type": "double",
              "attributes": {
                "class": {
                  "type": "character",
                  "attributes": {},
                  "value": ["Date"]
                }
              },
              "value": [18386, 18387]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [-87.45, -87.45]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [52.68, 52.68]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["America/Toronto", "America/Toronto"]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [483.41666667, 606.68333333]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["t2sr", "t2sr"]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [-0.52402648, -0.48126855]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [-0.92000393, -0.90525141]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [0.23597476, 0.24994361]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [0.87362044, 0.92533557]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [0.94411202, 1]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [0.001, 1]
            },
            {
              "type": "list",
              "attributes": {
                "class": {
                  "type": "character",
                  "attributes": {},
                  "value": ["sfc_POINT", "sfc"]
                },
                "precision": {
                  "type": "double",
                  "attributes": {},
                  "value": [0]
                },
                "bbox": {
                  "type": "double",
                  "attributes": {
                    "names": {
                      "type": "character",
                      "attributes": {},
                      "value": ["xmin", "ymin", "xmax", "ymax"]
                    },
                    "class": {
                      "type": "character",
                      "attributes": {},
                      "value": ["bbox"]
                    }
                  },
                  "value": [125, -47.25, 126, 79.61666667]
                },
                "crs": {
                  "type": "list",
                  "attributes": {
                    "names": {
                      "type": "character",
                      "attributes": {},
                      "value": ["input", "wkt"]
                    },
                    "class": {
                      "type": "character",
                      "attributes": {},
                      "value": ["crs"]
                    }
                  },
                  "value": [
                    {
                      "type": "character",
                      "attributes": {},
                      "value": ["EPSG:3395"]
                    },
                    {
                      "type": "character",
                      "attributes": {},
                      "value": ["PROJCRS[\"WGS 84 / World Mercator\",\n    BASEGEOGCRS[\"WGS 84\",\n        ENSEMBLE[\"World Geodetic System 1984 ensemble\",\n            MEMBER[\"World Geodetic System 1984 (Transit)\"],\n            MEMBER[\"World Geodetic System 1984 (G730)\"],\n            MEMBER[\"World Geodetic System 1984 (G873)\"],\n            MEMBER[\"World Geodetic System 1984 (G1150)\"],\n            MEMBER[\"World Geodetic System 1984 (G1674)\"],\n            MEMBER[\"World Geodetic System 1984 (G1762)\"],\n            MEMBER[\"World Geodetic System 1984 (G2139)\"],\n            MEMBER[\"World Geodetic System 1984 (G2296)\"],\n            ELLIPSOID[\"WGS 84\",6378137,298.257223563,\n                LENGTHUNIT[\"metre\",1]],\n            ENSEMBLEACCURACY[2.0]],\n        PRIMEM[\"Greenwich\",0,\n            ANGLEUNIT[\"degree\",0.0174532925199433]],\n        ID[\"EPSG\",4326]],\n    CONVERSION[\"World Mercator\",\n        METHOD[\"Mercator (variant A)\",\n            ID[\"EPSG\",9804]],\n        PARAMETER[\"Latitude of natural origin\",0,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8801]],\n        PARAMETER[\"Longitude of natural origin\",0,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8802]],\n        PARAMETER[\"Scale factor at natural origin\",1,\n            SCALEUNIT[\"unity\",1],\n            ID[\"EPSG\",8805]],\n        PARAMETER[\"False easting\",0,\n            LENGTHUNIT[\"metre\",1],\n            ID[\"EPSG\",8806]],\n        PARAMETER[\"False northing\",0,\n            LENGTHUNIT[\"metre\",1],\n            ID[\"EPSG\",8807]]],\n    CS[Cartesian,2],\n        AXIS[\"(E)\",east,\n            ORDER[1],\n            LENGTHUNIT[\"metre\",1]],\n        AXIS[\"(N)\",north,\n            ORDER[2],\n            LENGTHUNIT[\"metre\",1]],\n    USAGE[\n        SCOPE[\"Very small scale conformal mapping.\"],\n        AREA[\"World between 80°S and 84°N.\"],\n        BBOX[-80,-180,84,180]],\n    ID[\"EPSG\",3395]]"]
                    }
                  ]
                },
                "n_empty": {
                  "type": "integer",
                  "attributes": {},
                  "value": [0]
                }
              },
              "value": [
                {
                  "type": "double",
                  "attributes": {
                    "class": {
                      "type": "character",
                      "attributes": {},
                      "value": ["XY", "POINT", "sfg"]
                    }
                  },
                  "value": [125, -47.25]
                },
                {
                  "type": "double",
                  "attributes": {
                    "class": {
                      "type": "character",
                      "attributes": {},
                      "value": ["XY", "POINT", "sfg"]
                    }
                  },
                  "value": [126, 79.61666667]
                }
              ]
            }
          ]
        },
        {
          "type": "NULL"
        },
        {
          "type": "list",
          "attributes": {
            "names": {
              "type": "character",
              "attributes": {},
              "value": ["call", "stratum_var", "stratum", "n_base", "seltype", "caty_var", "caty_n", "aux_var", "legacy", "mindis", "n_over", "n_near"]
            }
          },
          "value": [
            {
              "type": "language",
              "attributes": {},
              "value": ["spsurvey::grts(sframe = meta_weights_sf, n_base = n, stratum_var = name_site_id, ", "    aux_var = col_sel_weights, n_over = n_os, DesignID = \"sample\")"]
            },
            {
              "type": "NULL"
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["None"]
            },
            {
              "type": "double",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["None"]
                }
              },
              "value": [12]
            },
            {
              "type": "character",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["None"]
                }
              },
              "value": ["proportional"]
            },
            {
              "type": "NULL"
            },
            {
              "type": "list",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["None"]
                }
              },
              "value": [
                {
                  "type": "NULL"
                }
              ]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["psel_std"]
            },
            {
              "type": "logical",
              "attributes": {},
              "value": [false]
            },
            {
              "type": "NULL"
            },
            {
              "type": "list",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["None"]
                }
              },
              "value": [
                {
                  "type": "double",
                  "attributes": {},
                  "value": [2]
                }
              ]
            },
            {
              "type": "NULL"
            }
          ]
        }
      ]
    }

---

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["sites_legacy", "sites_base", "sites_over", "sites_near", "design"]
        },
        "class": {
          "type": "character",
          "attributes": {},
          "value": ["sp_design"]
        }
      },
      "value": [
        {
          "type": "NULL"
        },
        {
          "type": "list",
          "attributes": {
            "names": {
              "type": "character",
              "attributes": {},
              "value": ["siteID", "siteuse", "replsite", "lon_WGS84", "lat_WGS84", "stratum", "wgt", "ip", "caty", "aux", "file_name", "type", "path", "aru_id", "manufacturer", "model", "aru_type", "site_id", "tz_offset", "date_time", "date", "longitude", "latitude", "tz", "t2ss", "psel_by", "psel_min", "psel_doy", "psel", "psel_scaled", "psel_std", "psel_normalized", "geometry"]
            },
            "row.names": {
              "type": "integer",
              "attributes": {},
              "value": [1, 2, 3, 4, 5, 6, 7, 8, 9]
            },
            "class": {
              "type": "character",
              "attributes": {},
              "value": ["sf", "data.frame"]
            },
            "sf_column": {
              "type": "character",
              "attributes": {},
              "value": ["geometry"]
            },
            "agr": {
              "type": "integer",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["siteID", "siteuse", "replsite", "lon_WGS84", "lat_WGS84", "stratum", "wgt", "ip", "caty", "aux", "file_name", "type", "path", "aru_id", "manufacturer", "model", "aru_type", "site_id", "tz_offset", "date_time", "date", "longitude", "latitude", "tz", "t2ss", "psel_by", "psel_min", "psel_doy", "psel", "psel_scaled", "psel_std", "psel_normalized"]
                },
                "levels": {
                  "type": "character",
                  "attributes": {},
                  "value": ["constant", "aggregate", "identity"]
                },
                "class": {
                  "type": "character",
                  "attributes": {},
                  "value": ["factor"]
                }
              },
              "value": ["NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA"]
            }
          },
          "value": [
            {
              "type": "character",
              "attributes": {},
              "value": ["sample-01", "sample-02", "sample-03", "sample-04", "sample-05", "sample-06", "sample-07", "sample-08", "sample-09"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["Base", "Base", "Base", "Base", "Base", "Base", "Base", "Base", "Base"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["None", "None", "None", "None", "None", "None", "None", "None", "None"]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [0.00111391, 0.00111391, 0.00112289, 0.00112289, 0.00113188, 0.00113188, 0.00112289, 0.00114086, 0.00114086]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [-0.00048128, -0.00048128, -0.00042731, -0.00042731, 0.00072003, 0.00072003, -0.00042731, 0.00187325, 0.00187325]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["P01_1", "P01_1", "P02_1", "P02_1", "P02_1", "P02_1", "P02_1", "P03_1", "P03_1"]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [1.5, 1.5, 1.2, 1.2, 1.2, 1.2, 1.2, 1.5, 1.5]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [0.66666667, 0.66666667, 0.83333333, 0.83333333, 0.83333333, 0.83333333, 0.83333333, 0.66666667, 0.66666667]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["None", "None", "None", "None", "None", "None", "None", "None", "None"]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [1, 1, 0.94411202, 0.94411202, 1, 1, 0.94411202, 1, 1]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["P01_1_20200503T052000-0400_ARU.wav", "P01_1_20200503T052000-0400_ARU.wav", "P02_1_20200504T052500_ARU.wav", "P02_1_20200504T052500_ARU.wav", "P02_1_20200505T073000_ARU.wav", "P02_1_20200505T073000_ARU.wav", "P02_1_20200504T052500_ARU.wav", "P03_1_20200506T100000-0400_ARU.wav", "P03_1_20200506T100000-0400_ARU.wav"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["wav", "wav", "wav", "wav", "wav", "wav", "wav", "wav", "wav"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["a_BARLT10962_P01_1/P01_1_20200503T052000-0400_ARU.wav", "o_BARLT10962_P01_1/P01_1_20200503T052000-0400_ARU.wav", "a_S4A01234_P02_1/P02_1_20200504T052500_ARU.wav", "j_S4A01234_P02_1/P02_1_20200504T052500_ARU.wav", "a_S4A01234_P02_1/P02_1_20200505T073000_ARU.wav", "j_S4A01234_P02_1/P02_1_20200505T073000_ARU.wav", "o_S4A01234_P02_1/P02_1_20200504T052500_ARU.wav", "a_BARLT10962_P03_1/P03_1_20200506T100000-0400_ARU.wav", "o_BARLT10962_P03_1/P03_1_20200506T100000-0400_ARU.wav"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["BARLT10962", "BARLT10962", "S4A01234", "S4A01234", "S4A01234", "S4A01234", "S4A01234", "BARLT10962", "BARLT10962"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["Frontier Labs", "Frontier Labs", "Wildlife Acoustics", "Wildlife Acoustics", "Wildlife Acoustics", "Wildlife Acoustics", "Wildlife Acoustics", "Frontier Labs", "Frontier Labs"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["BAR-LT", "BAR-LT", "Song Meter 4", "Song Meter 4", "Song Meter 4", "Song Meter 4", "Song Meter 4", "BAR-LT", "BAR-LT"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["BARLT", "BARLT", "SongMeter", "SongMeter", "SongMeter", "SongMeter", "SongMeter", "BARLT", "BARLT"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["P01_1", "P01_1", "P02_1", "P02_1", "P02_1", "P02_1", "P02_1", "P03_1", "P03_1"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["-0400", "-0400", null, null, null, null, null, "-0400", "-0400"]
            },
            {
              "type": "double",
              "attributes": {
                "class": {
                  "type": "character",
                  "attributes": {},
                  "value": ["POSIXct", "POSIXt"]
                },
                "tzone": {
                  "type": "character",
                  "attributes": {},
                  "value": ["UTC"]
                }
              },
              "value": [1588483200, 1588483200, 1588569900, 1588569900, 1588663800, 1588663800, 1588569900, 1588759200, 1588759200]
            },
            {
              "type": "double",
              "attributes": {
                "class": {
                  "type": "character",
                  "attributes": {},
                  "value": ["Date"]
                }
              },
              "value": [18385, 18385, 18386, 18386, 18387, 18387, 18386, 18388, 18388]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [-85.03, -85.03, -87.45, -87.45, -87.45, -87.45, -87.45, -90.38, -90.38]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [50.01, 50.01, 52.68, 52.68, 52.68, 52.68, 52.68, 48.99, 48.99]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["America/Toronto", "America/Toronto", "America/Toronto", "America/Toronto", "America/Toronto", "America/Toronto", "America/Toronto", "America/Toronto", "America/Toronto"]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [498.41666667, 498.41666667, 483.41666667, 483.41666667, 606.68333333, 606.68333333, 483.41666667, -685.88333333, -685.88333333]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["t2sr", "t2sr", "t2sr", "t2sr", "t2sr", "t2sr", "t2sr", "t2sr", "t2sr"]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [-0.53599721, -0.53599721, -0.52402648, -0.52402648, -0.48126855, -0.48126855, -0.52402648, -0.84075194, -0.84075194]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [-0.93517202, -0.93517202, -0.92000393, -0.92000393, -0.90525141, -0.90525141, -0.92000393, -0.89091445, -0.89091445]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [0.22965681, 0.22965681, 0.23597476, 0.23597476, 0.24994361, 0.24994361, 0.23597476, 0.17698923, 0.17698923]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [0.85023024, 0.85023024, 0.87362044, 0.87362044, 0.92533557, 0.92533557, 0.87362044, 0.65524553, 0.65524553]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [1, 1, 0.94411202, 0.94411202, 1, 1, 0.94411202, 1, 1]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [0.001, 0.001, 0.001, 0.001, 1, 1, 0.001, 0.001, 0.001]
            },
            {
              "type": "list",
              "attributes": {
                "class": {
                  "type": "character",
                  "attributes": {},
                  "value": ["sfc_POINT", "sfc"]
                },
                "precision": {
                  "type": "double",
                  "attributes": {},
                  "value": [0]
                },
                "bbox": {
                  "type": "double",
                  "attributes": {
                    "names": {
                      "type": "character",
                      "attributes": {},
                      "value": ["xmin", "ymin", "xmax", "ymax"]
                    },
                    "class": {
                      "type": "character",
                      "attributes": {},
                      "value": ["bbox"]
                    }
                  },
                  "value": [124, -53.21666667, 127, 207.13333333]
                },
                "crs": {
                  "type": "list",
                  "attributes": {
                    "names": {
                      "type": "character",
                      "attributes": {},
                      "value": ["input", "wkt"]
                    },
                    "class": {
                      "type": "character",
                      "attributes": {},
                      "value": ["crs"]
                    }
                  },
                  "value": [
                    {
                      "type": "character",
                      "attributes": {},
                      "value": ["EPSG:3395"]
                    },
                    {
                      "type": "character",
                      "attributes": {},
                      "value": ["PROJCRS[\"WGS 84 / World Mercator\",\n    BASEGEOGCRS[\"WGS 84\",\n        ENSEMBLE[\"World Geodetic System 1984 ensemble\",\n            MEMBER[\"World Geodetic System 1984 (Transit)\"],\n            MEMBER[\"World Geodetic System 1984 (G730)\"],\n            MEMBER[\"World Geodetic System 1984 (G873)\"],\n            MEMBER[\"World Geodetic System 1984 (G1150)\"],\n            MEMBER[\"World Geodetic System 1984 (G1674)\"],\n            MEMBER[\"World Geodetic System 1984 (G1762)\"],\n            MEMBER[\"World Geodetic System 1984 (G2139)\"],\n            MEMBER[\"World Geodetic System 1984 (G2296)\"],\n            ELLIPSOID[\"WGS 84\",6378137,298.257223563,\n                LENGTHUNIT[\"metre\",1]],\n            ENSEMBLEACCURACY[2.0]],\n        PRIMEM[\"Greenwich\",0,\n            ANGLEUNIT[\"degree\",0.0174532925199433]],\n        ID[\"EPSG\",4326]],\n    CONVERSION[\"World Mercator\",\n        METHOD[\"Mercator (variant A)\",\n            ID[\"EPSG\",9804]],\n        PARAMETER[\"Latitude of natural origin\",0,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8801]],\n        PARAMETER[\"Longitude of natural origin\",0,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8802]],\n        PARAMETER[\"Scale factor at natural origin\",1,\n            SCALEUNIT[\"unity\",1],\n            ID[\"EPSG\",8805]],\n        PARAMETER[\"False easting\",0,\n            LENGTHUNIT[\"metre\",1],\n            ID[\"EPSG\",8806]],\n        PARAMETER[\"False northing\",0,\n            LENGTHUNIT[\"metre\",1],\n            ID[\"EPSG\",8807]]],\n    CS[Cartesian,2],\n        AXIS[\"(E)\",east,\n            ORDER[1],\n            LENGTHUNIT[\"metre\",1]],\n        AXIS[\"(N)\",north,\n            ORDER[2],\n            LENGTHUNIT[\"metre\",1]],\n    USAGE[\n        SCOPE[\"Very small scale conformal mapping.\"],\n        AREA[\"World between 80°S and 84°N.\"],\n        BBOX[-80,-180,84,180]],\n    ID[\"EPSG\",3395]]"]
                    }
                  ]
                },
                "n_empty": {
                  "type": "integer",
                  "attributes": {},
                  "value": [0]
                }
              },
              "value": [
                {
                  "type": "double",
                  "attributes": {
                    "class": {
                      "type": "character",
                      "attributes": {},
                      "value": ["XY", "POINT", "sfg"]
                    }
                  },
                  "value": [124, -53.21666667]
                },
                {
                  "type": "double",
                  "attributes": {
                    "class": {
                      "type": "character",
                      "attributes": {},
                      "value": ["XY", "POINT", "sfg"]
                    }
                  },
                  "value": [124, -53.21666667]
                },
                {
                  "type": "double",
                  "attributes": {
                    "class": {
                      "type": "character",
                      "attributes": {},
                      "value": ["XY", "POINT", "sfg"]
                    }
                  },
                  "value": [125, -47.25]
                },
                {
                  "type": "double",
                  "attributes": {
                    "class": {
                      "type": "character",
                      "attributes": {},
                      "value": ["XY", "POINT", "sfg"]
                    }
                  },
                  "value": [125, -47.25]
                },
                {
                  "type": "double",
                  "attributes": {
                    "class": {
                      "type": "character",
                      "attributes": {},
                      "value": ["XY", "POINT", "sfg"]
                    }
                  },
                  "value": [126, 79.61666667]
                },
                {
                  "type": "double",
                  "attributes": {
                    "class": {
                      "type": "character",
                      "attributes": {},
                      "value": ["XY", "POINT", "sfg"]
                    }
                  },
                  "value": [126, 79.61666667]
                },
                {
                  "type": "double",
                  "attributes": {
                    "class": {
                      "type": "character",
                      "attributes": {},
                      "value": ["XY", "POINT", "sfg"]
                    }
                  },
                  "value": [125, -47.25]
                },
                {
                  "type": "double",
                  "attributes": {
                    "class": {
                      "type": "character",
                      "attributes": {},
                      "value": ["XY", "POINT", "sfg"]
                    }
                  },
                  "value": [127, 207.13333333]
                },
                {
                  "type": "double",
                  "attributes": {
                    "class": {
                      "type": "character",
                      "attributes": {},
                      "value": ["XY", "POINT", "sfg"]
                    }
                  },
                  "value": [127, 207.13333333]
                }
              ]
            }
          ]
        },
        {
          "type": "list",
          "attributes": {
            "names": {
              "type": "character",
              "attributes": {},
              "value": ["siteID", "siteuse", "replsite", "lon_WGS84", "lat_WGS84", "stratum", "wgt", "ip", "caty", "aux", "file_name", "type", "path", "aru_id", "manufacturer", "model", "aru_type", "site_id", "tz_offset", "date_time", "date", "longitude", "latitude", "tz", "t2ss", "psel_by", "psel_min", "psel_doy", "psel", "psel_scaled", "psel_std", "psel_normalized", "geometry"]
            },
            "row.names": {
              "type": "integer",
              "attributes": {},
              "value": [1]
            },
            "class": {
              "type": "character",
              "attributes": {},
              "value": ["sf", "data.frame"]
            },
            "sf_column": {
              "type": "character",
              "attributes": {},
              "value": ["geometry"]
            },
            "agr": {
              "type": "integer",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["siteID", "siteuse", "replsite", "lon_WGS84", "lat_WGS84", "stratum", "wgt", "ip", "caty", "aux", "file_name", "type", "path", "aru_id", "manufacturer", "model", "aru_type", "site_id", "tz_offset", "date_time", "date", "longitude", "latitude", "tz", "t2ss", "psel_by", "psel_min", "psel_doy", "psel", "psel_scaled", "psel_std", "psel_normalized"]
                },
                "levels": {
                  "type": "character",
                  "attributes": {},
                  "value": ["constant", "aggregate", "identity"]
                },
                "class": {
                  "type": "character",
                  "attributes": {},
                  "value": ["factor"]
                }
              },
              "value": ["NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA"]
            }
          },
          "value": [
            {
              "type": "character",
              "attributes": {},
              "value": ["sample-10"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["Over"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["Next"]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [0.00113188]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [0.00072003]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["P02_1"]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [1.2]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [0.83333333]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["None"]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [1]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["P02_1_20200505T073000_ARU.wav"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["wav"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["o_S4A01234_P02_1/P02_1_20200505T073000_ARU.wav"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["S4A01234"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["Wildlife Acoustics"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["Song Meter 4"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["SongMeter"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["P02_1"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": [null]
            },
            {
              "type": "double",
              "attributes": {
                "class": {
                  "type": "character",
                  "attributes": {},
                  "value": ["POSIXct", "POSIXt"]
                },
                "tzone": {
                  "type": "character",
                  "attributes": {},
                  "value": ["UTC"]
                }
              },
              "value": [1588663800]
            },
            {
              "type": "double",
              "attributes": {
                "class": {
                  "type": "character",
                  "attributes": {},
                  "value": ["Date"]
                }
              },
              "value": [18387]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [-87.45]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [52.68]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["America/Toronto"]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [606.68333333]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["t2sr"]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [-0.48126855]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [-0.90525141]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [0.24994361]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [0.92533557]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [1]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [1]
            },
            {
              "type": "list",
              "attributes": {
                "class": {
                  "type": "character",
                  "attributes": {},
                  "value": ["sfc_POINT", "sfc"]
                },
                "precision": {
                  "type": "double",
                  "attributes": {},
                  "value": [0]
                },
                "bbox": {
                  "type": "double",
                  "attributes": {
                    "names": {
                      "type": "character",
                      "attributes": {},
                      "value": ["xmin", "ymin", "xmax", "ymax"]
                    },
                    "class": {
                      "type": "character",
                      "attributes": {},
                      "value": ["bbox"]
                    }
                  },
                  "value": [126, 79.61666667, 126, 79.61666667]
                },
                "crs": {
                  "type": "list",
                  "attributes": {
                    "names": {
                      "type": "character",
                      "attributes": {},
                      "value": ["input", "wkt"]
                    },
                    "class": {
                      "type": "character",
                      "attributes": {},
                      "value": ["crs"]
                    }
                  },
                  "value": [
                    {
                      "type": "character",
                      "attributes": {},
                      "value": ["EPSG:3395"]
                    },
                    {
                      "type": "character",
                      "attributes": {},
                      "value": ["PROJCRS[\"WGS 84 / World Mercator\",\n    BASEGEOGCRS[\"WGS 84\",\n        ENSEMBLE[\"World Geodetic System 1984 ensemble\",\n            MEMBER[\"World Geodetic System 1984 (Transit)\"],\n            MEMBER[\"World Geodetic System 1984 (G730)\"],\n            MEMBER[\"World Geodetic System 1984 (G873)\"],\n            MEMBER[\"World Geodetic System 1984 (G1150)\"],\n            MEMBER[\"World Geodetic System 1984 (G1674)\"],\n            MEMBER[\"World Geodetic System 1984 (G1762)\"],\n            MEMBER[\"World Geodetic System 1984 (G2139)\"],\n            MEMBER[\"World Geodetic System 1984 (G2296)\"],\n            ELLIPSOID[\"WGS 84\",6378137,298.257223563,\n                LENGTHUNIT[\"metre\",1]],\n            ENSEMBLEACCURACY[2.0]],\n        PRIMEM[\"Greenwich\",0,\n            ANGLEUNIT[\"degree\",0.0174532925199433]],\n        ID[\"EPSG\",4326]],\n    CONVERSION[\"World Mercator\",\n        METHOD[\"Mercator (variant A)\",\n            ID[\"EPSG\",9804]],\n        PARAMETER[\"Latitude of natural origin\",0,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8801]],\n        PARAMETER[\"Longitude of natural origin\",0,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8802]],\n        PARAMETER[\"Scale factor at natural origin\",1,\n            SCALEUNIT[\"unity\",1],\n            ID[\"EPSG\",8805]],\n        PARAMETER[\"False easting\",0,\n            LENGTHUNIT[\"metre\",1],\n            ID[\"EPSG\",8806]],\n        PARAMETER[\"False northing\",0,\n            LENGTHUNIT[\"metre\",1],\n            ID[\"EPSG\",8807]]],\n    CS[Cartesian,2],\n        AXIS[\"(E)\",east,\n            ORDER[1],\n            LENGTHUNIT[\"metre\",1]],\n        AXIS[\"(N)\",north,\n            ORDER[2],\n            LENGTHUNIT[\"metre\",1]],\n    USAGE[\n        SCOPE[\"Very small scale conformal mapping.\"],\n        AREA[\"World between 80°S and 84°N.\"],\n        BBOX[-80,-180,84,180]],\n    ID[\"EPSG\",3395]]"]
                    }
                  ]
                },
                "n_empty": {
                  "type": "integer",
                  "attributes": {},
                  "value": [0]
                }
              },
              "value": [
                {
                  "type": "double",
                  "attributes": {
                    "class": {
                      "type": "character",
                      "attributes": {},
                      "value": ["XY", "POINT", "sfg"]
                    }
                  },
                  "value": [126, 79.61666667]
                }
              ]
            }
          ]
        },
        {
          "type": "NULL"
        },
        {
          "type": "list",
          "attributes": {
            "names": {
              "type": "character",
              "attributes": {},
              "value": ["call", "stratum_var", "stratum", "n_base", "seltype", "caty_var", "caty_n", "aux_var", "legacy", "mindis", "n_over", "n_near"]
            }
          },
          "value": [
            {
              "type": "language",
              "attributes": {},
              "value": ["spsurvey::grts(sframe = meta_weights_sf, n_base = n, stratum_var = name_site_id, ", "    aux_var = col_sel_weights, n_over = n_os, DesignID = \"sample\")"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["site_id"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["P01_1", "P02_1", "P03_1"]
            },
            {
              "type": "list",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["P01_1", "P02_1", "P03_1"]
                }
              },
              "value": [
                {
                  "type": "double",
                  "attributes": {},
                  "value": [2]
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": [5]
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": [2]
                }
              ]
            },
            {
              "type": "character",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["P01_1", "P02_1", "P03_1"]
                }
              },
              "value": ["proportional", "proportional", "proportional"]
            },
            {
              "type": "NULL"
            },
            {
              "type": "list",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["P01_1", "P02_1", "P03_1"]
                }
              },
              "value": [
                {
                  "type": "NULL"
                },
                {
                  "type": "NULL"
                },
                {
                  "type": "NULL"
                }
              ]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["psel_std"]
            },
            {
              "type": "logical",
              "attributes": {},
              "value": [false]
            },
            {
              "type": "NULL"
            },
            {
              "type": "list",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["P01_1", "P02_1", "P03_1"]
                }
              },
              "value": [
                {
                  "type": "NULL"
                },
                {
                  "type": "double",
                  "attributes": {},
                  "value": [1]
                },
                {
                  "type": "NULL"
                }
              ]
            },
            {
              "type": "NULL"
            }
          ]
        }
      ]
    }

