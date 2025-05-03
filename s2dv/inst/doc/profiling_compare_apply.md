This document records the profiling tests of those functions using apply() and Apply() 
depending on 'ncores'. The comparison is among apply(), Apply() with one core, and Apply() with two cores. Two different data sizes are tested. The testing package is "peakRAM".


- Ano()  
For small data, apply() is better than Apply() both in time and memory usage. However, if 
the data size is larger, apply() requires more memory even if it saves time still. Using
2 cores can save memory usage but time is even longer. 

   - small data  
```r
  set.seed(1)
  dat1 <- array(rnorm(10000), c(dat = 10, member = 30, sdate = 400, ftime = 10))
  set.seed(2)
  clim1 <- array(rnorm(10000), c(dat = 10, member = 30, sdate = 400))
  pryr::object_size(dat1)
  9.6 MB

  # (1) apply
 Elapsed_Time_sec Total_RAM_Used_MiB  Peak_RAM_Used_MiB
            0.016                9.1               68.6

  # (2) Apply, 1 core
 Elapsed_Time_sec Total_RAM_Used_MiB  Peak_RAM_Used_MiB
            0.039                9.1               82.4
 
  # (3) Apply, 2 cores
 Elapsed_Time_sec Total_RAM_Used_MiB  Peak_RAM_Used_MiB
            0.117                9.1               50.4
```

   - large data
```r
  set.seed(1)
  dat2 <- array(rnorm(10000), c(dat = 10, member = 30, sdate = 400, ftime = 10, lon = 150))
  set.seed(2)
  clim2 <- array(rnorm(10000), c(dat = 10, member = 30, sdate = 400))
  pryr::object_size(dat2)
  1.44GB

  # (1) apply                          
 Elapsed_Time_sec Total_RAM_Used_MiB  Peak_RAM_Used_MiB
            6.368             1373.3               6004

  # (2) Apply, 1 core
 Elapsed_Time_sec Total_RAM_Used_MiB  Peak_RAM_Used_MiB
           15.211             1373.3             5844.3

  # (3) Apply, 2 cores
 Elapsed_Time_sec Total_RAM_Used_MiB  Peak_RAM_Used_MiB
           20.193             1373.3             4718.9
```

- Trend  
Because the returned value is list, apply() is not suitable for Trend(). For small data,
2 cores is twice faster than 1 core. The peak RAM is around 6-7x of data. For larger data,
1 core is a bit faster than 2 cores. The peak RAM is around 4-5x of data.
   - small data
```r
  set.seed(1)
  dat1 <- array(rnorm(10000), c(dat = 10, member = 30, sdate = 40, ftime = 100))
  pryr::object_size(dat1)
  9.6 MB

  # (1) Apply, 1 core
 Elapsed_Time_sec Total_RAM_Used_MiB  Peak_RAM_Used_MiB
           21.324                9.8               56.4

  # (2) Apply, 2 cores
 Elapsed_Time_sec Total_RAM_Used_MiB  Peak_RAM_Used_MiB
           11.327                9.8               63.1

  # (3) Apply, 4 cores

```

   - large data
```r
  set.seed(1)
  dat2 <- array(rnorm(10000), c(dat = 10, member = 10, sdate = 400, ftime = 1000, lon = 4))
  pryr::object_size(dat2)
  1.28GB

  # (1) Apply, 1 core
 Elapsed_Time_sec Total_RAM_Used_MiB  Peak_RAM_Used_MiB
          602.273             1230.4             6004.3

  # (2) Apply, 2 cores
 Elapsed_Time_sec Total_RAM_Used_MiB  Peak_RAM_Used_MiB
          632.638             1229.8             5979.2

```



