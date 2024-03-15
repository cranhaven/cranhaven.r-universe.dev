#' Toolkit for analysis of genomic data
#'
#' 'misha' package is intended to help users to efficiently analyze genomic
#' data achieved from various experiments.
#'
#' For a complete list of help resources, use \code{library(help = "misha")}.
#'
#' The following options are available for the package. Use 'options' function
#' to alter the value of the options.
#'
#' \tabular{lll}{ NAME \tab DEFAULT \tab DESCRIPTION \cr gmax.data.size \tab
#' 10000000 \tab Maximal number of data (intervals, ...) in large data sets
#' stored \cr \tab \tab in memory. Prevents excessive memory usage by various
#' functions \cr \tab \tab such as 'gextract', 'gscreen', etc. \cr
#' gbig.intervals.size \tab 1000000 \tab Minimal number of intervals in a big
#' intervals set format \cr gmax.mem.usage \tab 10000000 \tab Maximal memory
#' consumption of all child processes in KB before the limiting algorithm is
#' invoked. \cr gmax.processes \tab 16 \tab Maximal number of processes for
#' multitasking \cr gmax.processes2core \tab 2 \tab Maximal number of processes
#' per CPU core for multitasking \cr gmin.scope4process \tab 10000 \tab Minimal
#' scope range (for 2D: surface) assigned to a \cr \tab \tab process in
#' multitasking mode. \cr gbuf.size \tab 1000 \tab Size of track expression
#' values buffer. \cr gtrack.chunk.size \tab 100000 \tab Chunk size in bytes of
#' a 2D track. If '0' chunk size is unlimited. \cr gtrack.num.chunks \tab 0
#' \tab Maximal number of 2D track chunks simultaneously stored in \cr \tab
#' \tab memory.\cr }
#'
#' More information about the options can be found in 'User manual' of the
#' package.
#'
#'
#' @name misha-package
#' @importFrom utils read.csv
#' @importFrom stats qnorm
#' @useDynLib misha garrays_import gbins_quantiles gbins_summary gbintransform gchain2interv gcheck_iterator gcheck_vtrack C_gcis_decay C_gcompute_strands_autocorr gcreate_pwm_energy_multitask gcreate_pwm_energy gcreate_test_computer2d_track C_gextract gextract_multitask gfind_neighbors gfind_neighbors gfind_tracks_n_intervals gget_tracks_attrs gintervals_chrom_sizes gintervals_import_genes gintervals_quantiles gintervals_quantiles_multitask gintervals_stats gintervals_stats gintervals_summary gintervcanonic gintervdiff ginterv_intersectband gintervintersect gintervs_liftover gintervsort gintervunion giterator_intervals gmapply gmapply gmapply_multitask C_gpartition C_gquantiles gquantiles_multitask grbind C_gsample C_gscreen gscreen_multitask C_gsegment gseqimport gseqread gset_tracks_attrs gsmooth gtrack_2d_import gtrack_bintransform gtrackconvert gtrack_create_meta gtrackcreate_multitask gtrack_create_sparse gtrack_create_track2d gtrackcreate gtrackdist gtrackdist_multitask gtrack_import_contacts gtrackimport_mappedseq gtrackimportwig gtrackinfo gtrack_intervals_load gtrack_liftover gtrack_modify gtracksummary gtracksummary_multitask C_gwilcox
#' @aliases misha-package misha
#' @docType package
#' @keywords package
NULL
