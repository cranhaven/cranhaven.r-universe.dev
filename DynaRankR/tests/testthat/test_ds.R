# ##### Test equivalence of Davids scores in DynaRankR and other packages
# library(DynaRankR)
# library(testthat)
# 
# context('DS agreement')
# 
# edges <- C.crocuta.female$interactions[C.crocuta.female$interactions$period == 1989,]
# ids <- C.crocuta.female$contestants[C.crocuta.female$contestants$period == 1989,'id']
# 
# DynaRankR_ds <- informed_ds(contestants = C.crocuta.female$contestants[C.crocuta.female$contestants$period == 1989,],
#                               convention = 'none', interactions = edges)
# 
# ## Works but has EloRating has unsupported package dependency
# # EloRating_DS <- EloRating::DS(edgelist_to_matrix(edges[,c(1:2)], ids), 'Dij')
# # 
# # 
# # test_that('Davids scores agrees with other packages', {
# #   testthat::expect_equal(EloRating_DS$normDS, DynaRankR_ds$score)
# # })
