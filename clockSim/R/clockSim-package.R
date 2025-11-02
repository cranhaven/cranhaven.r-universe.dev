#' @keywords internal
"_PACKAGE"

#' clockSim: Streamlined Simulation of Circadian Gene Networks
#'
#' This package provides preconfigured circadian clock gene network simulation 
#' models based on the Odin simulation engine.
#' 
#' The circadian clock is a foundational model for studying negative-positive 
#' feedback mechanisms in biology. The molecular clock—a cell-autonomous 
#' transcriptional-translational feedback loop—is one of the most 
#' well-characterized gene network oscillators.
#' 
#' Despite widespread interest in molecular clock research, 
#' numerical simulation of these systems remains inaccessible to many, 
#' if not most, wet-lab biologists. This gap has hindered the generation of 
#' novel hypotheses and efficient exploration of the clock’s parameter space.
#' 
#' 'clockSim' addresses this challenge by providing a low-friction workflow 
#' for simulating molecular clocks, making in silico exploration accessible 
#' to a broader audience.
#' 
#' Please refer to the package vignettes on workflows provided by 'clockSim'. 
#' 
#' The currently implemented models are based on the classical Leloup-Goldbeter 
#' PER-TIM feedback loop. 
#' Refer to Leloup and Goldbeter (1998) <doi:10.1177/074873098128999934>.
#'
#' @name clockSim
#' @useDynLib clockSim
#' @importFrom odin odin
#' @importFrom dde difeq
NULL

## usethis namespace: start
## usethis namespace: end
NULL
