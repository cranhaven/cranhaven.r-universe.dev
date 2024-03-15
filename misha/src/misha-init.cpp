#include <R.h>
#include <R_ext/Rdynload.h>
#include <Rinternals.h>
#include <stdlib.h>  // for NULL

extern "C" {
    extern SEXP garrayextract(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
    extern SEXP garrays_import(SEXP, SEXP, SEXP, SEXP);
    extern SEXP gbins_quantiles(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
    extern SEXP gbins_summary(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
    extern SEXP gbintransform(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
    extern SEXP gchain2interv(SEXP, SEXP);
    extern SEXP gcheck_iterator(SEXP, SEXP);
    extern SEXP gcheck_vtrack(SEXP, SEXP);
    extern SEXP C_gcis_decay(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
    extern SEXP C_gcompute_strands_autocorr(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
    // extern SEXP gcreate_arrays_track(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
    extern SEXP gcreate_pwm_energy_multitask(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
    extern SEXP gcreate_pwm_energy(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
    extern SEXP gcreate_test_computer2d_track(SEXP, SEXP, SEXP, SEXP, SEXP);
    extern SEXP C_gextract(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
    extern SEXP gextract_multitask(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
    extern SEXP gfind_neighbors(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
    extern SEXP gfind_tracks_n_intervals(SEXP, SEXP);
    extern SEXP gget_tracks_attrs(SEXP, SEXP, SEXP);
    extern SEXP gintervals_chrom_sizes(SEXP, SEXP);
    extern SEXP gintervals_import_genes(SEXP, SEXP, SEXP, SEXP);
    extern SEXP gintervals_quantiles(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
    extern SEXP gintervals_quantiles_multitask(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
    extern SEXP gintervals_stats(SEXP, SEXP);
    extern SEXP gintervals_summary(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
    extern SEXP gintervcanonic(SEXP, SEXP, SEXP);
    extern SEXP gintervdiff(SEXP, SEXP, SEXP);
    extern SEXP ginterv_intersectband(SEXP, SEXP, SEXP, SEXP);
    extern SEXP gintervintersect(SEXP, SEXP, SEXP);
    extern SEXP gintervs_liftover(SEXP, SEXP, SEXP);
    extern SEXP gintervsort(SEXP, SEXP);
    extern SEXP gintervunion(SEXP, SEXP, SEXP);
    extern SEXP giterator_intervals(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
    extern SEXP gmapply(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
    extern SEXP gmapply_multitask(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
    extern SEXP C_gpartition(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
    extern SEXP C_gquantiles(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
    extern SEXP gquantiles_multitask(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
    extern SEXP grbind(SEXP, SEXP);
    extern SEXP C_gsample(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
    extern SEXP C_gscreen(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
    extern SEXP gscreen_multitask(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
    extern SEXP C_gsegment(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
    extern SEXP gseqimport(SEXP, SEXP, SEXP);
    extern SEXP gseqread(SEXP, SEXP);
    extern SEXP gset_tracks_attrs(SEXP, SEXP, SEXP, SEXP);
    extern SEXP gsmooth(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
    extern SEXP gtrack_2d_import(SEXP, SEXP, SEXP);
    extern SEXP gtrack_bintransform(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
    extern SEXP gtrackconvert(SEXP, SEXP, SEXP);
    extern SEXP gtrack_create_meta(SEXP, SEXP);
    extern SEXP gtrackcreate_multitask(SEXP, SEXP, SEXP, SEXP, SEXP);
    extern SEXP gtrack_create_sparse(SEXP, SEXP, SEXP, SEXP);
    extern SEXP gtrack_create_track2d(SEXP, SEXP, SEXP, SEXP);
    extern SEXP gtrackcreate(SEXP, SEXP, SEXP, SEXP, SEXP);
    extern SEXP gtrackdist(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
    extern SEXP gtrackdist_multitask(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
    extern SEXP gtrack_import_contacts(SEXP, SEXP, SEXP, SEXP, SEXP);
    extern SEXP gtrackimport_mappedseq(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
    extern SEXP gtrackimportwig(SEXP, SEXP, SEXP, SEXP, SEXP);
    extern SEXP gtrackinfo(SEXP, SEXP);
    extern SEXP gtrack_intervals_load(SEXP, SEXP, SEXP, SEXP, SEXP);
    extern SEXP gtrack_liftover(SEXP, SEXP, SEXP, SEXP);
    extern SEXP gtrack_modify(SEXP, SEXP, SEXP, SEXP, SEXP);
    extern SEXP gtracksummary(SEXP, SEXP, SEXP, SEXP, SEXP);
    extern SEXP gtracksummary_multitask(SEXP, SEXP, SEXP, SEXP, SEXP);
    extern SEXP C_gwilcox(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
}

static const R_CallMethodDef CallEntries[] = {
    {"garrayextract", (DL_FUNC)&garrayextract, 7},
    {"garrays_import", (DL_FUNC)&garrays_import, 4},
    {"gbins_quantiles", (DL_FUNC)&gbins_quantiles, 8},
    {"gbins_summary", (DL_FUNC)&gbins_summary, 7},
    {"gbintransform", (DL_FUNC)&gbintransform, 10},
    {"gchain2interv", (DL_FUNC)&gchain2interv, 2},
    {"gcheck_iterator", (DL_FUNC)&gcheck_iterator, 2},
    {"gcheck_vtrack", (DL_FUNC)&gcheck_vtrack, 2},
    {"C_gcis_decay", (DL_FUNC)&C_gcis_decay, 9},
    {"C_gcompute_strands_autocorr", (DL_FUNC)&C_gcompute_strands_autocorr, 8},
    // {"gcreate_arrays_track", (DL_FUNC)&gcreate_arrays_track, 7},
    {"gcreate_pwm_energy_multitask", (DL_FUNC)&gcreate_pwm_energy_multitask, 6},
    {"gcreate_pwm_energy", (DL_FUNC)&gcreate_pwm_energy, 6},
    {"gcreate_test_computer2d_track", (DL_FUNC)&gcreate_test_computer2d_track, 5},
    {"C_gextract", (DL_FUNC)&C_gextract, 8},
    {"gextract_multitask", (DL_FUNC)&gextract_multitask, 8},
    {"gfind_neighbors", (DL_FUNC)&gfind_neighbors, 12},
    {"gfind_tracks_n_intervals", (DL_FUNC)&gfind_tracks_n_intervals, 2},
    {"gget_tracks_attrs", (DL_FUNC)&gget_tracks_attrs, 3},
    {"gintervals_chrom_sizes", (DL_FUNC)&gintervals_chrom_sizes, 2},
    {"gintervals_import_genes", (DL_FUNC)&gintervals_import_genes, 4},
    {"gintervals_quantiles", (DL_FUNC)&gintervals_quantiles, 7},
    {"gintervals_quantiles_multitask", (DL_FUNC)&gintervals_quantiles_multitask, 7},
    {"gintervals_stats", (DL_FUNC)&gintervals_stats, 2},
    {"gintervals_summary", (DL_FUNC)&gintervals_summary, 6},
    {"gintervcanonic", (DL_FUNC)&gintervcanonic, 3},
    {"gintervdiff", (DL_FUNC)&gintervdiff, 3},
    {"ginterv_intersectband", (DL_FUNC)&ginterv_intersectband, 4},
    {"gintervintersect", (DL_FUNC)&gintervintersect, 3},
    {"gintervs_liftover", (DL_FUNC)&gintervs_liftover, 3},
    {"gintervsort", (DL_FUNC)&gintervsort, 2},
    {"gintervunion", (DL_FUNC)&gintervunion, 3},
    {"giterator_intervals", (DL_FUNC)&giterator_intervals, 6},
    {"gmapply", (DL_FUNC)&gmapply, 8},
    {"gmapply_multitask", (DL_FUNC)&gmapply_multitask, 8},
    {"C_gpartition", (DL_FUNC)&C_gpartition, 8},
    {"C_gquantiles", (DL_FUNC)&C_gquantiles, 6},
    {"gquantiles_multitask", (DL_FUNC)&gquantiles_multitask, 6},
    {"grbind", (DL_FUNC)&grbind, 2},
    {"C_gsample", (DL_FUNC)&C_gsample, 6},
    {"C_gscreen", (DL_FUNC)&C_gscreen, 6},
    {"gscreen_multitask", (DL_FUNC)&gscreen_multitask, 6},
    {"C_gsegment", (DL_FUNC)&C_gsegment, 8},
    {"gseqimport", (DL_FUNC)&gseqimport, 3},
    {"gseqread", (DL_FUNC)&gseqread, 2},
    {"gset_tracks_attrs", (DL_FUNC)&gset_tracks_attrs, 4},
    {"gsmooth", (DL_FUNC)&gsmooth, 8},
    {"gtrack_2d_import", (DL_FUNC)&gtrack_2d_import, 3},
    {"gtrack_bintransform", (DL_FUNC)&gtrack_bintransform, 9},
    {"gtrackconvert", (DL_FUNC)&gtrackconvert, 3},
    {"gtrack_create_meta", (DL_FUNC)&gtrack_create_meta, 2},
    {"gtrackcreate_multitask", (DL_FUNC)&gtrackcreate_multitask, 5},
    {"gtrack_create_sparse", (DL_FUNC)&gtrack_create_sparse, 4},
    {"gtrack_create_track2d", (DL_FUNC)&gtrack_create_track2d, 4},
    {"gtrackcreate", (DL_FUNC)&gtrackcreate, 5},
    {"gtrackdist", (DL_FUNC)&gtrackdist, 7},
    {"gtrackdist_multitask", (DL_FUNC)&gtrackdist_multitask, 7},
    {"gtrack_import_contacts", (DL_FUNC)&gtrack_import_contacts, 5},
    {"gtrackimport_mappedseq", (DL_FUNC)&gtrackimport_mappedseq, 7},
    {"gtrackimportwig", (DL_FUNC)&gtrackimportwig, 5},
    {"gtrackinfo", (DL_FUNC)&gtrackinfo, 2},
    {"gtrack_intervals_load", (DL_FUNC)&gtrack_intervals_load, 5},
    {"gtrack_liftover", (DL_FUNC)&gtrack_liftover, 4},
    {"gtrack_modify", (DL_FUNC)&gtrack_modify, 5},
    {"gtracksummary", (DL_FUNC)&gtracksummary, 5},
    {"gtracksummary_multitask", (DL_FUNC)&gtracksummary_multitask, 5},
    {"C_gwilcox", (DL_FUNC)&C_gwilcox, 10},
    {NULL, NULL, 0}
};

void R_init_misha(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
