## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, include=FALSE-----------------------------------------------------
library(photon)
options(photon_movers = FALSE)

## ----public_api, eval=FALSE---------------------------------------------------
# new_photon()
# #> <photon>
# #>   Type   : remote
# #>   Server : https://photon.komoot.io/

## ----geocode, eval=FALSE------------------------------------------------------
# cities1 <- geocode(c("Sanaa", "Caracas"), osm_tag = ":city")
# cities1
# #> Simple feature collection with 2 features and 12 fields
# #> Geometry type: POINT
# #> Dimension:     XY
# #> Bounding box:  xmin: -66.9146 ymin: 10.50609 xmax: 44.20588 ymax: 15.35386
# #> Geodetic CRS:  WGS 84
# #> # A tibble: 2 × 13
# #>     idx osm_type    osm_id osm_key osm_value type   countrycode name  country state county extent            geometry
# #>   <int> <chr>        <int> <chr>   <chr>     <chr>  <chr>       <chr> <chr>   <chr> <chr>  <list>         <POINT [°]>
# #> 1     1 N        258013552 place   city      distr… YE          Sana… Yemen   Aman… At Ta… <lgl>  (44.20588 15.35386)
# #> 2     2 R         11219583 place   city      city   VE          Cara… Venezu… Capi… Munic… <dbl>  (-66.9146 10.50609)

## ----reverse, eval=FALSE------------------------------------------------------
# cities2 <- reverse(cities1, osm_tag = ":city")
# cities2
# #> Simple feature collection with 2 features and 12 fields
# #> Geometry type: POINT
# #> Dimension:     XY
# #> Bounding box:  xmin: -66.9146 ymin: 10.50609 xmax: 44.20588 ymax: 15.35386
# #> Geodetic CRS:  WGS 84
# #> # A tibble: 2 × 13
# #>    idx osm_type    osm_id osm_key osm_value type   countrycode name  country state county extent            geometry
# #>  <int> <chr>        <int> <chr>   <chr>     <chr>  <chr>       <chr> <chr>   <chr> <chr>  <list>         <POINT [°]>
# #>1     1 N        258013552 place   city      distr… YE          Sana… Yemen   Aman… At Ta… <lgl>  (44.20588 15.35386)
# #>2     2 R         11219583 place   city      city   VE          Cara… Venezu… Capi… Munic… <dbl>  (-66.9146 10.50609)

## ----compare, eval=FALSE------------------------------------------------------
# all.equal(cities1, cities2)
# #> [1] TRUE

## -----------------------------------------------------------------------------
place <- data.frame(
  city = "Moscow",
  state = "Minnesota",
  countrycode = "USA"
)
structured(place)

## ----java, eval=FALSE---------------------------------------------------------
# # pak::pkg_install("rJavaEnv")
# library(rJavaEnv)
# 
# # Consent to downloading Java
# rje_consent()
# #> Consent for using rJavaEnv has already been provided.
# 
# # Install and use Corretto JDK 24
# use_java(24)
# 
# # Check if installation was successful
# java_check_version_cmd()
# #> JAVA_HOME: /root/.cache/R/rJavaEnv/installed/linux/x64/24
# #> Java path: /root/.cache/R/rJavaEnv/installed/linux/x64/24/bin/java
# #> Java version: "openjdk version \"24.0.1\" 2025-04-15 OpenJDK Runtime Environment Corretto-24.0.1.9.1 (build
# #> 24.0.1+9-FR) OpenJDK 64-Bit Server VM Corretto-24.0.1.9.1 (build 24.0.1+9-FR, mixed mode, sharing)"
# #> [1] "24"

## ----local_photon, eval=FALSE-------------------------------------------------
# path <- file.path(tempdir(), "photon")
# photon <- new_photon(path, region = "Andorra")
# #> ℹ openjdk version "24.0.1" 2025-04-15
# #> ℹ OpenJDK Runtime Environment Corretto-24.0.1.9.1 (build 24.0.1+9-FR)
# #> ℹ OpenJDK 64-Bit Server VM Corretto-24.0.1.9.1 (build 24.0.1+9-FR, mixed mode, sharing)
# #> ✔ Successfully downloaded OpenSearch photon 1.0.0. [17.9s]
# #> ✔ Successfully downloaded search index. [854ms]
# #> • Version: 1.0.0
# #> • Coverage: Andorra
# #> • Time: 2026-02-27

## ----start, eval=FALSE--------------------------------------------------------
# #> photon$start()
# #> Cluster Name: photon
# #> Base Path:    /tmp/RtmpLQIqmy/photon/./photon_data
# #> Num Of Node:  1
# #> Node Name:      Node 1
# #> HTTP Port:      9201
# #> Data Directory: /tmp/RtmpLQIqmy/photon/./photon_data/node_1/data
# #> Log Directory:  /tmp/RtmpLQIqmy/photon/./photon_data/node_1/logs
# #> [2026-02-27T14:29:54,384][INFO ][o.o.n.Node               ] version[3.4.0], pid[19752], build[unknown/unknown/unknown], OS[Windows 11/10.0/amd64], JVM[Oracle Corporation/Java HotSpot(TM) 64-Bit Server VM/22/22+36-2370]
# #> [2026-02-27T14:29:54,385][INFO ][o.o.n.Node               ] JVM home [/root/.cache/R/rJavaEnv/installed/linux/x64/24]
# #> [2026-02-27T14:29:54,389][INFO ][o.o.n.Node               ] JVM arguments []
# #> [2026-02-27T14:29:54,395][INFO ][o.o.p.PluginsService     ] no modules loaded
# #> [2026-02-27T14:29:54,396][INFO ][o.o.p.PluginsService     ] loaded plugin [org.opensearch.analysis.common.CommonAnalysisModulePlugin]
# #> [2026-02-27T14:29:54,397][INFO ][o.o.p.PluginsService     ] loaded plugin [org.opensearch.geo.GeoModulePlugin]
# #> [2026-02-27T14:29:54,397][INFO ][o.o.p.PluginsService     ] loaded plugin [org.opensearch.transport.Netty4ModulePlugin]
# #> [2026-02-27T14:29:54,438][INFO ][o.o.e.NodeEnvironment    ] using [1] data paths, mounts [[/ (/dev/sdd)]], net usable_space [103.5gb], net total_space [783.6gb], types [NTFS]
# #> [2026-02-27T14:29:54,443][INFO ][o.o.e.NodeEnvironment    ] heap size [3.9gb], compressed ordinary object pointers [true]
# #> [2026-02-27T14:29:54,536][WARN ][o.a.l.i.v.VectorizationProvider] Java vector incubator module is not readable. For optimal vector performance, pass '--add-modules jdk.incubator.vector' to enable Vector API.
# #> [2026-02-27T14:29:54,624][INFO ][o.o.n.Node               ] node name [Node 1], node ID [FYcghCrFQyi8ZcOrpZkWmA], cluster name [photon], roles [data, cluster_manager]
# #> [2026-02-27T14:29:54,664][INFO ][o.o.e.ExtensionsManager  ] ExtensionsManager initialized
# #> [2026-02-27T14:29:54,911][INFO ][o.o.i.MergeSchedulerConfig] Updating autoThrottle for index _na_ from [false] to [true]
# #> [2026-02-27T14:29:54,911][INFO ][o.o.i.MergeSchedulerConfig] Updating maxThreadCount from [0] to [4] and maxMergeCount from [0] to [9] for index _na_.
# #> [2026-02-27T14:29:54,911][INFO ][o.o.i.MergeSchedulerConfig] Initialized index _na_ with maxMergeCount=9, maxThreadCount=4, autoThrottleEnabled=true
# #> [2026-02-27T14:29:55,087][INFO ][o.o.i.MergeSchedulerConfig] Updating autoThrottle for index _na_ from [false] to [true]
# #> [2026-02-27T14:29:55,087][INFO ][o.o.i.MergeSchedulerConfig] Updating maxThreadCount from [0] to [4] and maxMergeCount from [0] to [9] for index _na_.
# #> [2026-02-27T14:29:55,087][INFO ][o.o.i.MergeSchedulerConfig] Initialized index _na_ with maxMergeCount=9, maxThreadCount=4, autoThrottleEnabled=true
# #> [2026-02-27T14:29:56,578][INFO ][o.o.t.NettyAllocator     ] creating NettyAllocator with the following configs: [name=opensearch_configured, chunk_size=512kb, suggested_max_allocation_size=512kb, factors={opensearch.unsafe.use_netty_default_chunk_and_page_size=false, g1gc_enabled=true, g1gc_region_size=2mb}]
# #> ✔ Photon is now running. [6.2s]
# 
# photon$proc
# #> PROCESS 'java', running, pid 1896.

## ----is_ready, eval=FALSE-----------------------------------------------------
# photon$is_ready()
# #> [1] TRUE

## ----benchmark1, eval=FALSE---------------------------------------------------
# # offline geocoding
# bench::mark(with_photon(photon, geocode("Monte Carlo")), iterations = 25)$median
# #> [1] 30.2ms

## ----benchmark2, eval=FALSE---------------------------------------------------
# # online geocoding
# photon_pub <- new_photon(mount = FALSE)
# bench::mark(with_photon(photon_pub, geocode("Monte Carlo")), iterations = 25)$median
# #> [1] 334ms

## ----stop, eval=FALSE---------------------------------------------------------
# photon$stop()

## ----purge, eval=FALSE--------------------------------------------------------
# photon$purge()
# #> ℹ Purging an instance kills the photon process and removes the photon directory.
# #> Continue? (y/N/Cancel) y

