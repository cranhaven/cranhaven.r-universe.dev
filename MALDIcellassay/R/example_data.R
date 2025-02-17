#' Blank2022spec
#' @description
#' MALDI mass spectrometry data of EOC cells treated with different concentrations of SAHA.
#' It is used to demonstrate the usage of \code{MALDIcellassay}.
#' @details
#' The concentrations include: 0, 0.04, 0.12, 0.37, 1.11, 3.33, 10 and 30 uM of SAHA at 4 replicates each.
#' The original spectra were trimmed to 400-900 Da mass-range to keep the file size small.
#' @format A list of MALDIquant::MassSpectrum-objects named with the respective concentration.
#' @references Blank, M., Enzlein, T. & Hopf, C. LPS-induced lipid alterations in microglia revealed by MALDI mass spectrometry-based cell fingerprinting in neuroinflammation studies. Sci Rep 12, 2908 (2022). https://doi.org/10.1038/s41598-022-06894-1
#' @usage data(Blank2022spec)
"Blank2022spec"

#' Blank2022peaks
#' @description
#' Peaks from MALDI mass spectrometry data of EOC cells treated with different concentrations of SAHA.
#' It is used to demonstrate the usage of \code{MALDIcellassay}.
#' @details
#' The concentrations include: 0, 0.04, 0.12, 0.37, 1.11, 3.33, 10 and 30 uM of SAHA at 4 replicates each.
#' The original spectra were trimmed to 400-900 Da mass-range to keep the file size small.
#' The peaks are the result of applying `MALDIquant::detectPeaks` to `Blank2022spec` with arguments `SNR = 3, method = "SuperSmoother"`.
#' @format A list of MALDIquant::MassPeaks-objects named with the respective concentration.
#' @references Blank, M., Enzlein, T. & Hopf, C. LPS-induced lipid alterations in microglia revealed by MALDI mass spectrometry-based cell fingerprinting in neuroinflammation studies. Sci Rep 12, 2908 (2022). https://doi.org/10.1038/s41598-022-06894-1
#' @usage data(Blank2022peaks)
"Blank2022peaks"


#' Blank2022intmat
#' @description
#' Intensity matrix from MALDI mass spectrometry data of EOC cells treated with different concentrations of SAHA.
#' It is used to demonstrate the usage of \code{MALDIcellassay}.
#' @details
#' The concentrations include: 0, 0.04, 0.12, 0.37, 1.11, 3.33, 10 and 30 uM of SAHA at 4 replicates each.
#' The original spectra were trimmed to 400-900 Da mass-range to keep the file size small.
#' The peaks are the result of `MALDIquant::intensityMatrix(Blank2022peaks, Blank2022spec)`
#' @format Matrix with concentrations of original spectra as rownames and m/z-values as colnames.
#' @references Blank, M., Enzlein, T. & Hopf, C. LPS-induced lipid alterations in microglia revealed by MALDI mass spectrometry-based cell fingerprinting in neuroinflammation studies. Sci Rep 12, 2908 (2022). https://doi.org/10.1038/s41598-022-06894-1
#' @usage data(Blank2022intmat)
"Blank2022intmat"


#' Blank2022res
#' @description
#' Object of class MALDIcellassay from MALDI mass spectrometry data of EOC cells treated with different concentrations of SAHA.
#' It is used to demonstrate the usage of \code{MALDIcellassay}.
#' @details
#' The concentrations include: 0, 0.04, 0.12, 0.37, 1.11, 3.33, 10 and 30 uM of SAHA at 4 replicates each.
#' The original spectra were trimmed to 400-900 Da mass-range to keep the file size small.
#' The peaks are the result of 
#' `fitCurve(spec = Blank2022spec,
#'           SinglePointRecal = TRUE, 
#'           normMz = 760.585, 
#'           alignTol = 0.1, 
#'           normTol = 0.1)`
#' @format Matrix with concentrations of original spectra as rownames and m/z-values as colnames.
#' @references Blank, M., Enzlein, T. & Hopf, C. LPS-induced lipid alterations in microglia revealed by MALDI mass spectrometry-based cell fingerprinting in neuroinflammation studies. Sci Rep 12, 2908 (2022). https://doi.org/10.1038/s41598-022-06894-1
#' @usage data(Blank2022res)
"Blank2022res"