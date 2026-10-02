# Test regioner default

    Code
      dawa("regioner")
    Message
      > Requesting `regioner` from DAWA
      i Accessing <https://api.dataforsyningen.dk/regioner>
    Output
      [[1]]
      [[1]]$ændret
      [1] "2024-10-04T21:02:54.978Z"
      
      [[1]]$geo_version
      [1] 32
      
      [[1]]$geo_ændret
      [1] "2024-10-04T21:02:54.978Z"
      
      [[1]]$bbox
      [[1]]$bbox[[1]]
      [1] 8.189517
      
      [[1]]$bbox[[2]]
      [1] 56.53455
      
      [[1]]$bbox[[3]]
      [1] 11.22599
      
      [[1]]$bbox[[4]]
      [1] 57.76025
      
      
      [[1]]$visueltcenter
      [[1]]$visueltcenter[[1]]
      [1] 10.11283
      
      [[1]]$visueltcenter[[2]]
      [1] 57.30716
      
      
      [[1]]$dagi_id
      [1] "389098"
      
      [[1]]$kode
      [1] "1081"
      
      [[1]]$navn
      [1] "Region Nordjylland"
      
      [[1]]$nuts2
      [1] "DK05"
      
      [[1]]$href
      [1] "https://api.dataforsyningen.dk/regioner/1081"
      
      
      [[2]]
      [[2]]$ændret
      [1] "2025-07-11T21:05:02.274Z"
      
      [[2]]$geo_version
      [1] 47
      
      [[2]]$geo_ændret
      [1] "2025-07-11T21:05:02.274Z"
      
      [[2]]$bbox
      [[2]]$bbox[[1]]
      [1] 8.078876
      
      [[2]]$bbox[[2]]
      [1] 55.64438
      
      [[2]]$bbox[[3]]
      [1] 11.66419
      
      [[2]]$bbox[[4]]
      [1] 56.84326
      
      
      [[2]]$visueltcenter
      [[2]]$visueltcenter[[1]]
      [1] 9.598478
      
      [[2]]$visueltcenter[[2]]
      [1] 56.23406
      
      
      [[2]]$dagi_id
      [1] "389101"
      
      [[2]]$kode
      [1] "1082"
      
      [[2]]$navn
      [1] "Region Midtjylland"
      
      [[2]]$nuts2
      [1] "DK04"
      
      [[2]]$href
      [1] "https://api.dataforsyningen.dk/regioner/1082"
      
      
      [[3]]
      [[3]]$ændret
      [1] "2025-05-14T21:02:41.891Z"
      
      [[3]]$geo_version
      [1] 34
      
      [[3]]$geo_ændret
      [1] "2025-05-14T21:02:41.891Z"
      
      [[3]]$bbox
      [[3]]$bbox[[1]]
      [1] 8.063203
      
      [[3]]$bbox[[2]]
      [1] 54.71828
      
      [[3]]$bbox[[3]]
      [1] 10.99555
      
      [[3]]$bbox[[4]]
      [1] 55.95325
      
      
      [[3]]$visueltcenter
      [[3]]$visueltcenter[[1]]
      [1] 9.028461
      
      [[3]]$visueltcenter[[2]]
      [1] 55.56317
      
      
      [[3]]$dagi_id
      [1] "389102"
      
      [[3]]$kode
      [1] "1083"
      
      [[3]]$navn
      [1] "Region Syddanmark"
      
      [[3]]$nuts2
      [1] "DK03"
      
      [[3]]$href
      [1] "https://api.dataforsyningen.dk/regioner/1083"
      
      
      [[4]]
      [[4]]$ændret
      [1] "2024-10-04T21:02:54.978Z"
      
      [[4]]$geo_version
      [1] 31
      
      [[4]]$geo_ændret
      [1] "2024-10-04T21:02:54.978Z"
      
      [[4]]$bbox
      [[4]]$bbox[[1]]
      [1] 11.60212
      
      [[4]]$bbox[[2]]
      [1] 54.98355
      
      [[4]]$bbox[[3]]
      [1] 15.31831
      
      [[4]]$bbox[[4]]
      [1] 56.2052
      
      
      [[4]]$visueltcenter
      [[4]]$visueltcenter[[1]]
      [1] 12.27937
      
      [[4]]$visueltcenter[[2]]
      [1] 55.97239
      
      
      [[4]]$dagi_id
      [1] "389099"
      
      [[4]]$kode
      [1] "1084"
      
      [[4]]$navn
      [1] "Region Hovedstaden"
      
      [[4]]$nuts2
      [1] "DK01"
      
      [[4]]$href
      [1] "https://api.dataforsyningen.dk/regioner/1084"
      
      
      [[5]]
      [[5]]$ændret
      [1] "2025-05-14T21:02:41.891Z"
      
      [[5]]$geo_version
      [1] 30
      
      [[5]]$geo_ændret
      [1] "2025-05-14T21:02:41.891Z"
      
      [[5]]$bbox
      [[5]]$bbox[[1]]
      [1] 10.81481
      
      [[5]]$bbox[[2]]
      [1] 54.54441
      
      [[5]]$bbox[[3]]
      [1] 12.64552
      
      [[5]]$bbox[[4]]
      [1] 56.01731
      
      
      [[5]]$visueltcenter
      [[5]]$visueltcenter[[1]]
      [1] 11.62132
      
      [[5]]$visueltcenter[[2]]
      [1] 55.43979
      
      
      [[5]]$dagi_id
      [1] "389100"
      
      [[5]]$kode
      [1] "1085"
      
      [[5]]$navn
      [1] "Region Sjælland"
      
      [[5]]$nuts2
      [1] "DK02"
      
      [[5]]$href
      [1] "https://api.dataforsyningen.dk/regioner/1085"
      
      

# Test regioner no cache

    Code
      dawa("regioner", cache = FALSE)
    Message
      > Requesting `regioner` from DAWA
      i Accessing <https://api.dataforsyningen.dk/regioner?cache=no-cache>
    Output
      [[1]]
      [[1]]$ændret
      [1] "2024-10-04T21:02:54.978Z"
      
      [[1]]$geo_version
      [1] 32
      
      [[1]]$geo_ændret
      [1] "2024-10-04T21:02:54.978Z"
      
      [[1]]$bbox
      [[1]]$bbox[[1]]
      [1] 8.189517
      
      [[1]]$bbox[[2]]
      [1] 56.53455
      
      [[1]]$bbox[[3]]
      [1] 11.22599
      
      [[1]]$bbox[[4]]
      [1] 57.76025
      
      
      [[1]]$visueltcenter
      [[1]]$visueltcenter[[1]]
      [1] 10.11283
      
      [[1]]$visueltcenter[[2]]
      [1] 57.30716
      
      
      [[1]]$dagi_id
      [1] "389098"
      
      [[1]]$kode
      [1] "1081"
      
      [[1]]$navn
      [1] "Region Nordjylland"
      
      [[1]]$nuts2
      [1] "DK05"
      
      [[1]]$href
      [1] "https://api.dataforsyningen.dk/regioner/1081"
      
      
      [[2]]
      [[2]]$ændret
      [1] "2025-07-11T21:05:02.274Z"
      
      [[2]]$geo_version
      [1] 47
      
      [[2]]$geo_ændret
      [1] "2025-07-11T21:05:02.274Z"
      
      [[2]]$bbox
      [[2]]$bbox[[1]]
      [1] 8.078876
      
      [[2]]$bbox[[2]]
      [1] 55.64438
      
      [[2]]$bbox[[3]]
      [1] 11.66419
      
      [[2]]$bbox[[4]]
      [1] 56.84326
      
      
      [[2]]$visueltcenter
      [[2]]$visueltcenter[[1]]
      [1] 9.598478
      
      [[2]]$visueltcenter[[2]]
      [1] 56.23406
      
      
      [[2]]$dagi_id
      [1] "389101"
      
      [[2]]$kode
      [1] "1082"
      
      [[2]]$navn
      [1] "Region Midtjylland"
      
      [[2]]$nuts2
      [1] "DK04"
      
      [[2]]$href
      [1] "https://api.dataforsyningen.dk/regioner/1082"
      
      
      [[3]]
      [[3]]$ændret
      [1] "2025-05-14T21:02:41.891Z"
      
      [[3]]$geo_version
      [1] 34
      
      [[3]]$geo_ændret
      [1] "2025-05-14T21:02:41.891Z"
      
      [[3]]$bbox
      [[3]]$bbox[[1]]
      [1] 8.063203
      
      [[3]]$bbox[[2]]
      [1] 54.71828
      
      [[3]]$bbox[[3]]
      [1] 10.99555
      
      [[3]]$bbox[[4]]
      [1] 55.95325
      
      
      [[3]]$visueltcenter
      [[3]]$visueltcenter[[1]]
      [1] 9.028461
      
      [[3]]$visueltcenter[[2]]
      [1] 55.56317
      
      
      [[3]]$dagi_id
      [1] "389102"
      
      [[3]]$kode
      [1] "1083"
      
      [[3]]$navn
      [1] "Region Syddanmark"
      
      [[3]]$nuts2
      [1] "DK03"
      
      [[3]]$href
      [1] "https://api.dataforsyningen.dk/regioner/1083"
      
      
      [[4]]
      [[4]]$ændret
      [1] "2024-10-04T21:02:54.978Z"
      
      [[4]]$geo_version
      [1] 31
      
      [[4]]$geo_ændret
      [1] "2024-10-04T21:02:54.978Z"
      
      [[4]]$bbox
      [[4]]$bbox[[1]]
      [1] 11.60212
      
      [[4]]$bbox[[2]]
      [1] 54.98355
      
      [[4]]$bbox[[3]]
      [1] 15.31831
      
      [[4]]$bbox[[4]]
      [1] 56.2052
      
      
      [[4]]$visueltcenter
      [[4]]$visueltcenter[[1]]
      [1] 12.27937
      
      [[4]]$visueltcenter[[2]]
      [1] 55.97239
      
      
      [[4]]$dagi_id
      [1] "389099"
      
      [[4]]$kode
      [1] "1084"
      
      [[4]]$navn
      [1] "Region Hovedstaden"
      
      [[4]]$nuts2
      [1] "DK01"
      
      [[4]]$href
      [1] "https://api.dataforsyningen.dk/regioner/1084"
      
      
      [[5]]
      [[5]]$ændret
      [1] "2025-05-14T21:02:41.891Z"
      
      [[5]]$geo_version
      [1] 30
      
      [[5]]$geo_ændret
      [1] "2025-05-14T21:02:41.891Z"
      
      [[5]]$bbox
      [[5]]$bbox[[1]]
      [1] 10.81481
      
      [[5]]$bbox[[2]]
      [1] 54.54441
      
      [[5]]$bbox[[3]]
      [1] 12.64552
      
      [[5]]$bbox[[4]]
      [1] 56.01731
      
      
      [[5]]$visueltcenter
      [[5]]$visueltcenter[[1]]
      [1] 11.62132
      
      [[5]]$visueltcenter[[2]]
      [1] 55.43979
      
      
      [[5]]$dagi_id
      [1] "389100"
      
      [[5]]$kode
      [1] "1085"
      
      [[5]]$navn
      [1] "Region Sjælland"
      
      [[5]]$nuts2
      [1] "DK02"
      
      [[5]]$href
      [1] "https://api.dataforsyningen.dk/regioner/1085"
      
      

