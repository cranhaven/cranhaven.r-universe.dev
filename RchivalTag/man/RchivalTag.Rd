\name{RchivalTag}
\alias{RchivalTag}
\docType{package}
\title{
RchivalTag - Analyzing and Interactive Visualization of Archival Tagging Data
}
\description{
\code{RchivalTag} provides a set of functions to analyze and visualize different data products from Archival Tags (Supported Models include amongst others: MiniPAT, sPAT, mk10, mk9 from \href{https://wildlifecomputers.com/}{Wildlife Computers} as well as LOTEK PSAT Models \href{https://www.lotek.com/products/psat-series/}{LOTEK}. Models from other Manufactorers might be supported as well.

\itemize{
  \item{"(Depth) time series data"} (See \link{plot_TS}, \link{plot_DepthTS},  \link{dy_DepthTS}, \link{empty.plot_TS})
  \item{"Time-at-Depth (TaD) and Time-at-Temperature (TaT) fequencies"} (See \link{ts2histos}, \link{merge_histos}, \link{hist_tad} & \link{hist_tat})
  \item{"Depth Temperature profiles (time series data)"} (See \link{plot_DepthTempTS}, \link{plot_DepthTempTS_resampled}, \link{plot_DepthTempTS_resampled_PDT}, \link{interpolate_TempDepthProfiles}, \link{get_thermalstrat} & \link{image_TempDepthProfiles})
  \item{"PDT (PAT-style Depth Temperature profiles) data"} (See \link{read_PDT}, \link{interpolate_TempDepthProfiles}, \link{get_thermalstrat} & \link{image_TempDepthProfiles})
  \item{"visualization of geolocation estimates"} (See: \link{ggplot_geopos}, \link{ggplotly_geopos})
  }
  }

\details{
\bold{TaD-/TaT-histogram data}\cr
- The package allows to read and calculate standard summary data products (TaD-/TaT-profiles, see above) from recovered or transmitted time series data sets as well as to merge and visualize such summary data products from different tag setups/tagging programs. For more information on these data products, please see: Wildlife Computers (2016).

\bold{Depth time series data}\cr
- data visualization, optionally highlighting daytime differences (dawn, day, dusk, night).

\bold{Depth-temperature time series data}\cr
- data visualization and examination of the thermal stratification of the water column (i.e. thermocline depth, gradient and stratification index), based on previously interpolated. The paper by Bauer et al. (2015) is highly recommended in this context. 

\bold{Depth-temperature time series data}\cr
- data visualization and examination of the thermal stratification of the water column (i.e. thermocline depth, gradient and stratification index), based on previously interpolated. The paper by Bauer et al. (2015) is highly recommended in this context. 

\bold{interactive geolocation vizualiation}\cr
- data visualization via \href{https://ggplot2.tidyverse.org/}{ggplot2} and \href{https://plotly.com/r/}{plot_ly} based on \href{https://cran.r-project.org/package=oceanmap}{oceanmap} standard maps.


\bold{Compatibility}\cr
So far, the package is mainly adapted for archival tagging data from \href{https://wildlifecomputers.com/}{Wildlife Computers}, but can also be applied to data from other tag manufacturers (e.g. see \link{ts2histos} in order to calculate TaD & TaT-frequencies from time series data). Function examples are based on the transmitted data sets of a miniPAT-tag from the \href{https://anr.fr/Project-ANR-14-ACHN-0002}{BLUEMED-project}, funded by the French National Research Agency (ANR; \url{https://anr.fr}).
}

%\bold{Getting Started}\cr
%}


\references{
Bauer, R., F. Forget and JM. Fromentin (2015) Optimizing PAT data transmission: assessing the accuracy of temperature summary data to estimate environmental conditions. Fisheries Oceanography, 24(6): 533-539, \doi{10.1111/fog.12127}

Bauer, R., JM. Fromentin, H. Demarcq and S. Bonhommeau (2017) Habitat use, vertical and horizontal behaviour of Atlantic bluefin tuna (Thunnus thynnus) in the Northwestern Mediterranean Sea in relation to oceanographic conditions. Deep-Sea Research Part II: Topical Studies in Oceanography, 141: 248-261, \doi{10.1016/j.dsr2.2017.04.006}

Bauer, R., F. Forget, JM. Fromentin and M. Capello (2020) Surfacing and vertical behaviour of Atlantic bluefin tuna (*Thunnus thynnus*) in the Mediterranean Sea: implications for aerial surveys. ICES Journal of Marine Science, 77(5): 1979-1991, \doi{10.1093/icesjms/fsaa083}

Wildlife Computers (2016) MiniPAT-User-Guide, 4 April 2016, 26 pp. \url{https://static.wildlifecomputers.com/MiniPAT-User-Guide1.pdf}
}

\author{
Robert K. Bauer
}

