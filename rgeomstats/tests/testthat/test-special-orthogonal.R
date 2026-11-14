# Setup -------------------------------------------------------------------

V <- cbind(
  c(sqrt(2) / 2, -sqrt(2) / 2, 0),
  c(sqrt(2) / 2,  sqrt(2) / 2, 0),
  c(0, 0, 1)
)
A <- V %*% diag(1:3) %*% t(V)
B <- V %*% diag(4:6) %*% t(V)
Id <- diag(1, 3)
Ze <- diag(0, 3)
S1 <- array(dim = c(2, 3, 3))
S1[1, , ] <- A
S1[2, , ] <- B
S2 <- array(dim = c(3, 3, 3))
S2[1, , ] <- A
S2[2, , ] <- B
S2[3, , ] <- Id

# Constructors ------------------------------------------------------------

test_that("SpecialOrthogonal constructors work", {
  so2_vector <- SpecialOrthogonal(n = 2, point_type = "vector")
  expect_true("SpecialOrthogonal" %in% class(so2_vector))
  expect_true("SpecialOrthogonal2Vectors" %in% class(so2_vector))
  expect_equal(so2_vector$default_point_type, "vector")
  so3_vector <- SpecialOrthogonal(n = 3, point_type = "vector")
  expect_true("SpecialOrthogonal" %in% class(so3_vector))
  expect_true("SpecialOrthogonal3Vectors" %in% class(so3_vector))
  expect_equal(so3_vector$default_point_type, "vector")
  so3 <- SpecialOrthogonal(n = 3)
  expect_true("SpecialOrthogonal" %in% class(so3))
  expect_true("SpecialOrthogonalMatrices" %in% class(so3))
  expect_equal(so3$default_point_type, "matrix")
})

# SpecialOrthogonalMatrices -----------------------------------------------

test_that("SpecialOrthogonalMatrices method belongs() works", {
  so3 <- SpecialOrthogonal(n = 3)
  expect_true(so3$belongs(Id))
})

test_that("SpecialOrthogonalMatrices method intrinsic_to_extrinsic_coords() works", {
  so3 <- SpecialOrthogonal(n = 3)
  expect_snapshot(so3$intrinsic_to_extrinsic_coords(Id), error = TRUE)
})

test_that("SpecialOrthogonalMatrices method extrinsic_to_intrinsic_coords() works", {
  so3 <- SpecialOrthogonal(n = 3)
  expect_snapshot(so3$extrinsic_to_intrinsic_coords(Id), error = TRUE)
})

test_that("SpecialOrthogonalMatrices method projection() works", {
  so3 <- SpecialOrthogonal(n = 3)
  expect_equal(so3$projection(Id), Id)
})

test_that("SpecialOrthogonalMatrices method exp() works", {
  skip("Needs bug fix in gs")
  so3 <- SpecialOrthogonal(n = 3)
  expect_equal(so3$exp(Ze), Id)
})

test_that("SpecialOrthogonalMatrices method log() works", {
  so3 <- SpecialOrthogonal(n = 3)
  expect_equal(so3$log(Id), Ze)
})

test_that("SpecialOrthogonalMatrices method get_identity() works", {
  so3 <- SpecialOrthogonal(n = 3)
  expect_equal(so3$get_identity(), Id)
})

test_that("SpecialOrthogonalMatrices method lie_bracket() works", {
  so3 <- SpecialOrthogonal(n = 3)
  expect_equal(so3$lie_bracket(Ze, Id), Ze)
})

test_that("SpecialOrthogonalMatrices method tangent_translation_map() works", {
  so3 <- SpecialOrthogonal(n = 3)
  tangent_map <- so3$tangent_translation_map(Id)
  expect_equal(tangent_map(Id), Id)
})

test_that("SpecialOrthogonalMatrices method compose() works", {
  so3 <- SpecialOrthogonal(n = 3)
  expect_equal(so3$compose(Id, Id), Id)
})

test_that("SpecialOrthogonalMatrices method inverse() works", {
  so3 <- SpecialOrthogonal(n = 3)
  expect_equal(so3$inverse(Id), Id)
})

# SpecialOrthogonal2Vectors -----------------------------------------------

test_that("SpecialOrthogonal2Vectors method rotation_vector_from_matrix() works", {
  so2 <- SpecialOrthogonal(n = 2, point_type = "vector")
  expect_equal(so2$rotation_vector_from_matrix(diag(1, 2)), array(0))
})

test_that("SpecialOrthogonal2Vectors method matrix_from_rotation_vector() works", {
  so2 <- SpecialOrthogonal(n = 2, point_type = "vector")
  expect_equal(so2$matrix_from_rotation_vector(array(0)), diag(1, 2))
})

test_that("SpecialOrthogonal2Vectors method random_uniform() works", {
  reticulate::py_set_seed(1234)
  so2 <- SpecialOrthogonal(n = 2, point_type = "vector")
  expect_snapshot(so2$random_uniform())
})

test_that("SpecialOrthogonal2Vectors method projection() works", {
  so2 <- SpecialOrthogonal(n = 2, point_type = "vector")
  expect_equal(so2$projection(diag(1, 2)), diag(1, 2))
})

test_that("SpecialOrthogonal2Vectors method skew_matrix_from_vector() works", {
  so2 <- SpecialOrthogonal(n = 2, point_type = "vector")
  expect_equal(so2$skew_matrix_from_vector(array(0)), diag(0, 2))
})

test_that("SpecialOrthogonal2Vectors method vector_from_skew_matrix() works", {
  so2 <- SpecialOrthogonal(n = 2, point_type = "vector")
  expect_equal(so2$vector_from_skew_matrix(diag(0, 2)), array(0))
})

test_that("SpecialOrthogonal2Vectors method regularize_tangent_vec_at_identity() works", {
  so2 <- SpecialOrthogonal(n = 2, point_type = "vector")
  expect_equal(so2$regularize_tangent_vec_at_identity(array(0)), array(0))
})

test_that("SpecialOrthogonal2Vectors method regularize_tangent_vec() works", {
  so2 <- SpecialOrthogonal(n = 2, point_type = "vector")
  expect_equal(so2$regularize_tangent_vec(array(0), array(1)), array(0))
})

# SpecialOrthogonal3Vectors -----------------------------------------------

test_that("SpecialOrthogonal3Vectors method rotation_vector_from_matrix() works", {
  so3 <- SpecialOrthogonal(n = 3, point_type = "vector")
  expect_equal(so3$rotation_vector_from_matrix(diag(1, 3)), array(rep(0, 3)))
})

test_that("SpecialOrthogonal3Vectors method matrix_from_rotation_vector() works", {
  so3 <- SpecialOrthogonal(n = 3, point_type = "vector")
  expect_equal(so3$matrix_from_rotation_vector(rep(0, 3)), diag(1, 3))
})

test_that("SpecialOrthogonal3Vectors method quaternion_from_matrix() works", {
  so3 <- SpecialOrthogonal(n = 3, point_type = "vector")
  expect_equal(so3$quaternion_from_matrix(diag(1, 3)), array(c(1, rep(0, 3))))
})

test_that("SpecialOrthogonal3Vectors method quaternion_from_matrix() works", {
  so3 <- SpecialOrthogonal(n = 3, point_type = "vector")
  expect_equal(so3$quaternion_from_rotation_vector(array(rep(0, 3))), array(c(1, rep(0, 3))))
})

test_that("SpecialOrthogonal3Vectors method rotation_vector_from_quaternion() works", {
  so3 <- SpecialOrthogonal(n = 3, point_type = "vector")
  expect_equal(so3$rotation_vector_from_quaternion(array(c(1, rep(0, 3)))), array(rep(0, 3)))
})

test_that("SpecialOrthogonal3Vectors method matrix_from_quaternion() works", {
  so3 <- SpecialOrthogonal(n = 3, point_type = "vector")
  expect_equal(so3$matrix_from_quaternion(c(1, rep(0, 3))), diag(1, 3))
})

test_that("SpecialOrthogonal3Vectors method matrix_from_tait_bryan_angles() works", {
  so3 <- SpecialOrthogonal(n = 3, point_type = "vector")
  expect_equal(so3$matrix_from_tait_bryan_angles(rep(0, 3)), diag(1, 3))
})

test_that("SpecialOrthogonal3Vectors method tait_bryan_angles_from_matrix() works", {
  so3 <- SpecialOrthogonal(n = 3, point_type = "vector")
  expect_equal(so3$tait_bryan_angles_from_matrix(diag(1, 3)), array(rep(0, 3)))
})

test_that("SpecialOrthogonal3Vectors method quaternion_from_tait_bryan_angles() works", {
  so3 <- SpecialOrthogonal(n = 3, point_type = "vector")
  expect_equal(so3$quaternion_from_tait_bryan_angles(array(rep(0, 3))), array(c(1, rep(0, 3))))
})

test_that("SpecialOrthogonal3Vectors method rotation_vector_from_tait_bryan_angles() works", {
  so3 <- SpecialOrthogonal(n = 3, point_type = "vector")
  expect_equal(so3$rotation_vector_from_tait_bryan_angles(array(rep(0, 3))), array(rep(0, 3)))
})

test_that("SpecialOrthogonal3Vectors method tait_bryan_angles_from_quaternion() works", {
  so3 <- SpecialOrthogonal(n = 3, point_type = "vector")
  expect_equal(so3$tait_bryan_angles_from_quaternion(array(c(1, rep(0, 3)))), array(rep(0, 3)))
})

test_that("SpecialOrthogonal3Vectors method tait_bryan_angles_from_rotation_vector() works", {
  so3 <- SpecialOrthogonal(n = 3, point_type = "vector")
  expect_equal(so3$tait_bryan_angles_from_rotation_vector(array(rep(0, 3))), array(rep(0, 3)))
})

test_that("SpecialOrthogonal3Vectors method random_uniform() works", {
  so3 <- SpecialOrthogonal(n = 3, point_type = "vector")
  expect_snapshot(so3$random_uniform())
})

test_that("SpecialOrthogonal3Vectors method projection() works", {
  so3 <- SpecialOrthogonal(n = 3, point_type = "vector")
  expect_equal(so3$projection(diag(1, 3)), diag(1, 3))
})

test_that("SpecialOrthogonal3Vectors method skew_matrix_from_vector() works", {
  so3 <- SpecialOrthogonal(n = 3, point_type = "vector")
  expect_equal(so3$skew_matrix_from_vector(rep(0, 3)), diag(0, 3))
})

test_that("SpecialOrthogonal3Vectors method vector_from_skew_matrix() works", {
  so3 <- SpecialOrthogonal(n = 3, point_type = "vector")
  expect_equal(so3$vector_from_skew_matrix(diag(0, 3)), array(rep(0, 3)))
})

test_that("SpecialOrthogonal3Vectors method regularize_tangent_vec_at_identity() works", {
  so3 <- SpecialOrthogonal(n = 3, point_type = "vector")
  expect_equal(so3$regularize_tangent_vec_at_identity(array(rep(0, 3))), array(rep(0, 3)))
})

test_that("SpecialOrthogonal3Vectors method regularize_tangent_vec() works", {
  so3 <- SpecialOrthogonal(n = 3, point_type = "vector")
  expect_equal(so3$regularize_tangent_vec(rep(0, 3), rep(0, 3)), array(rep(0, 3)))
})

test_that("SpecialOrthogonal3Vectors method exp() works", {
  so3 <- SpecialOrthogonal(n = 3, point_type = "vector")
  expect_equal(so3$exp(rep(0, 3)), array(rep(0, 3)))
})

test_that("SpecialOrthogonal3Vectors method exp_from_identity() works", {
  so3 <- SpecialOrthogonal(n = 3, point_type = "vector")
  expect_equal(so3$exp_from_identity(rep(0, 3)), array(rep(0, 3)))
})

test_that("SpecialOrthogonal3Vectors method exp_not_from_identity() works", {
  so3 <- SpecialOrthogonal(n = 3, point_type = "vector")
  expect_equal(so3$exp_not_from_identity(rep(0, 3), rep(0, 3)), array(rep(0, 3)))
})

test_that("SpecialOrthogonal3Vectors method log() works", {
  so3 <- SpecialOrthogonal(n = 3, point_type = "vector")
  expect_equal(so3$log(rep(0, 3)), array(rep(0, 3)))
})

test_that("SpecialOrthogonal3Vectors method log_from_identity() works", {
  so3 <- SpecialOrthogonal(n = 3, point_type = "vector")
  expect_equal(so3$log_from_identity(rep(0, 3)), array(rep(0, 3)))
})

test_that("SpecialOrthogonal3Vectors method log_not_from_identity() works", {
  so3 <- SpecialOrthogonal(n = 3, point_type = "vector")
  expect_equal(so3$log_not_from_identity(rep(0, 3), rep(0, 3)), array(rep(0, 3)))
})

test_that("SpecialOrthogonal3Vectors method get_identity() works", {
  so3 <- SpecialOrthogonal(n = 3, point_type = "vector")
  expect_equal(so3$get_identity(), array(rep(0, 3)))
})

test_that("SpecialOrthogonal3Vectors method lie_bracket() works", {
  so3 <- SpecialOrthogonal(n = 3, point_type = "vector")
  expect_equal(so3$lie_bracket(Ze, Ze), Ze)
})

test_that("SpecialOrthogonal3Vectors method tangent_translation_map() works", {
  so3 <- SpecialOrthogonal(n = 3, point_type = "vector")
  fn <- so3$tangent_translation_map(rep(0, 3))
  expect_equal(fn(rep(0, 3)), array(rep(0, 3)))
})

test_that("SpecialOrthogonal3Vectors method compose() works", {
  so3 <- SpecialOrthogonal(n = 3, point_type = "vector")
  expect_equal(so3$compose(rep(0, 3), rep(0, 3)), array(rep(0, 3)))
})

test_that("SpecialOrthogonal3Vectors method jacobian_translation() works", {
  so3 <- SpecialOrthogonal(n = 3, point_type = "vector")
  expect_equal(so3$jacobian_translation(rep(0, 3)), diag(1, 3))
})

test_that("SpecialOrthogonal3Vectors method inverse() works", {
  so3 <- SpecialOrthogonal(n = 3, point_type = "vector")
  expect_equal(so3$inverse(rep(0, 3)), array(rep(0, 3)))
})

test_that("SpecialOrthogonal3Vectors method add_metric() works", {
  so3 <- SpecialOrthogonal(n = 3, point_type = "vector")
  expect_equal(so3$metrics, list())
  m <- SPDMetricAffine$new(n = 3)
  so3$add_metric(m)
  expect_true(inherits(get_r6_class(so3$metrics[[1]]), "SPDMetricAffine"))
})
