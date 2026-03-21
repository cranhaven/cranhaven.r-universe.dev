# ##### Test equivalence of Elo-rating in DynaRankR and other packages
# library(DynaRankR)
# library(testthat)
# 
# context('Elo rating agreement')
# edges <- C.crocuta.female$interactions[C.crocuta.female$interactions$period == 1989,]
# ids <- C.crocuta.female$contestants[C.crocuta.female$contestants$period == 1989,'id']
# 
# DynaRankR_elo <- informed_elo(contestants = C.crocuta.female$contestants[C.crocuta.female$contestants$period == 1989,],
#              convention = 'none', K = 200, lambda = 100, interactions = edges)
# 
# aniDom_elo <- aniDom::elo_scores(winners = edges$winner, losers = edges$loser, identities = ids,
#                    sigmoid.param = 1/100, K = 200, init.score = 0,randomise = FALSE)
# 
# aniDom_elo <- sort(aniDom_elo, decreasing = TRUE)
# 
# test_that('Elo agrees with other packages', {
#   testthat::expect_equal(aniDom_elo, DynaRankR_elo$score)
# })
# 
