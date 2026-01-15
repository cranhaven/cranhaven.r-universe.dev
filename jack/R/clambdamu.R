.alsMatrixFromPartition <- function(lambda) {
  lambdap <- conjugate(lambda)
  do.call(
    rbind,
    lapply(seq_along(lambda), function(i) {
      lambda_i <- lambda[i]
      j_ <- seq_len(lambda_i)
      cbind(
        lambda_i - j_,
        lambdap[j_] - i + 1L,
        deparse.level = 0L
      )
    })
  )
}

.alcsFromMatrix <- function(alsMatrix) {
  pairs <- apply(alsMatrix, 1L, toString)
  tbl <- table(factor(pairs))
  rownames(alsMatrix) <- pairs
  colnames(alsMatrix) <- c("i", "j")
  cbind(
    alsMatrix[names(tbl), , drop = FALSE],
    count = tbl,
    deparse.level = 0L
  )
}

.alcsFromPartition <- function(lambda) {
  .alcsFromMatrix(.alsMatrixFromPartition(lambda))
}
# alMapFromPairs :: Seq (Int, Int) -> Map (Int, Int) Int
# alMapFromPairs als =
#   foldl' (\i al -> DM.insertWith (+) al 1 i) DM.empty als
#
# alMap :: Seq Int -> Map (Int, Int) Int
# alMap lambda = alMapFromPairs als
#   where
#     lambda' = _dualPartition' lambda
#     zs = S.zip lambda (S.fromList [1 .. S.length lambda])
#     zs' = S.zip lambda' (S.fromList [1 .. S.length lambda'])
#     als =
#        foldl'
#           (
#             \sq (m, i) ->
#               sq ><
#                 fmap (\(m', j) -> (m - j, m'- i + 1)) (S.take m zs')
#           )
#            S.empty zs
#
# poly_from_assoc :: (Eq a, AlgRing.C a) => ((Int, Int), Int) -> Spray a
# poly_from_assoc ((a, l), c) =
#   (HM.fromList
#     [
#       (Powers S.empty 0, AlgRing.one)
#     , (Powers (S.fromList [a, l]) 2, AlgAdd.negate AlgRing.one)
#     ]) ^**^ c
.poly_from_alc <- function(alc) {
  qspray <- new(
    "qspray",
    powers = list(integer(0L), alc[c(1L, 2L)]),
    coeffs = c("1", "-1")
  )
  qspray^(alc[3L])
}
# poly_from_assocs :: (Eq a, AlgRing.C a) => [((Int, Int), Int)] -> Spray a
# poly_from_assocs assocs = productOfSprays (map poly_from_assoc assocs)
.poly_from_alcs <- function(alcs) {
  Reduce(
    `*`,
    apply(alcs, 1L, .poly_from_alc, simplify = FALSE)
  )
}
# clambda :: (Eq a, AlgRing.C a) => Seq Int -> Spray a
# clambda lambda =
#   poly_from_assocs (DM.assocs (alMap lambda))
.clambda <- function(lambda) {
  if(length(lambda) == 0L) {
    qone()
  } else {
    .poly_from_alcs(.alcsFromPartition(lambda))
  }
}
# assocsFromMaps ::
#   Map (Int, Int) Int -> Map (Int, Int) Int
#   -> ([((Int, Int), Int)], [((Int, Int), Int)])
# assocsFromMaps num_map den_map =
#   both DM.assocs
#     (
#       DM.differenceWith f num_map den_map
#     , DM.differenceWith f den_map num_map
#     )
#   where
#     f k1 k2 = if k1 > k2 then Just (k1 - k2) else Nothing
.clambdamuMatrices <- function(lambda, mu) {
  matrix1 <- .alsMatrixFromPartition(lambda)
  if(length(mu) == 0L) {
    matrix2 <- matrix(NA_integer_, nrow = 0L, ncol = 2L)
  } else {
    matrix2 <- .alsMatrixFromPartition(mu)
  }
  simplifyTheTwoMatrices(
    matrix1,
    matrix2
  )
}
# clambdamuAssocs ::
#   Seq Int -> Seq Int -> ([((Int, Int), Int)], [((Int, Int), Int)])
# clambdamuAssocs lambda mu = assocsFromMaps num_map den_map
#   where
#     num_map = alMap lambda
#     den_map = alMap mu
.clambdamu <- function(lambda, mu) {
  if(length(mu) == 0L) {
    return(.clambda(lambda))
  }
  alcsMatrices <- .clambdamuMatrices(lambda, mu)
  matrix1 <- alcsMatrices[[1L]]
  if(nrow(matrix1) >= 1L) {
    num <- .poly_from_alcs(matrix1)
  } else {
    num <- qone()
  }
  matrix2 <- alcsMatrices[[2L]]
  if(nrow(matrix2) >= 1L) {
    den <- .poly_from_alcs(matrix2)
  } else {
    den <- qone()
  }
  num / den
}
# clambdamu :: (Eq a, AlgField.C a) => Seq Int -> Seq Int -> RatioOfSprays a
# clambdamu lambda mu = num %//% den
#   where
#     assocs = clambdamuAssocs lambda mu
#     (num, den) = both poly_from_assocs assocs
