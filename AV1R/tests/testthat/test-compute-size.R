test_that(".compute_output_size returns no scaling for large input", {
  result <- AV1R:::.compute_output_size(1920, 1080)
  expect_equal(result$width, 1920L)
  expect_equal(result$height, 1080L)
  expect_false(result$scaled)
})

test_that(".compute_output_size scales up small images to meet minimum", {
  result <- AV1R:::.compute_output_size(64, 64)
  expect_true(result$scaled)
  expect_gte(result$width, 320L)
  expect_gte(result$height, 128L)
})

test_that(".compute_output_size applies tiff_scale multiplier", {
  result <- AV1R:::.compute_output_size(400, 300, tiff_scale = 2)
  expect_true(result$scaled)
  expect_equal(result$width, 800L)
  expect_equal(result$height, 600L)
})

test_that(".compute_output_size ignores tiff_scale = 1", {
  result <- AV1R:::.compute_output_size(400, 300, tiff_scale = 1)
  expect_false(result$scaled)
  expect_equal(result$width, 400L)
  expect_equal(result$height, 296L)  # 300 rounded down to multiple of 8
})

test_that(".compute_output_size handles tiff_scale as bounding box", {
  result <- AV1R:::.compute_output_size(100, 50, tiff_scale = c(800, 400))
  expect_true(result$scaled)
  # ratio = min(800/100, 400/50) = min(8, 8) = 8, mult = 8
  expect_equal(result$width, 800L)
  expect_equal(result$height, 400L)
})

test_that(".compute_output_size rounds to multiple of 8", {
  # 100 * 4 = 400 (already multiple of 8), 75 * 4 = 300, 300 %% 8 = 4 -> 296
  result <- AV1R:::.compute_output_size(100, 75, tiff_scale = 4)
  expect_equal(result$width %% 8L, 0L)
  expect_equal(result$height %% 8L, 0L)
})

test_that(".compute_output_size respects custom min_w and min_h", {
  result <- AV1R:::.compute_output_size(50, 50, min_w = 200L, min_h = 200L)
  # mult=4 -> 200x200, already multiple of 8
  expect_equal(result$width, 200L)
  expect_equal(result$height, 200L)
  expect_true(result$scaled)
})

test_that(".crf_to_vulkan_crf maps calibration knots exactly", {
  expect_equal(AV1R:::.crf_to_vulkan_crf(0),   1L)
  expect_equal(AV1R:::.crf_to_vulkan_crf(1),   2L)
  expect_equal(AV1R:::.crf_to_vulkan_crf(3),   2L)
  expect_equal(AV1R:::.crf_to_vulkan_crf(5),   3L)
  expect_equal(AV1R:::.crf_to_vulkan_crf(8),   5L)
  expect_equal(AV1R:::.crf_to_vulkan_crf(10),  7L)
  expect_equal(AV1R:::.crf_to_vulkan_crf(15), 11L)
  expect_equal(AV1R:::.crf_to_vulkan_crf(20), 15L)
  expect_equal(AV1R:::.crf_to_vulkan_crf(30), 30L)
  expect_equal(AV1R:::.crf_to_vulkan_crf(40), 40L)
  expect_equal(AV1R:::.crf_to_vulkan_crf(50), 50L)
  expect_equal(AV1R:::.crf_to_vulkan_crf(63), 63L)
})

test_that(".crf_to_vulkan_crf converges to identity at high CRF", {
  for (crf in 30:63) {
    expect_equal(AV1R:::.crf_to_vulkan_crf(crf), crf)
  }
})

test_that(".crf_to_vulkan_crf errors on out-of-range input", {
  expect_error(AV1R:::.crf_to_vulkan_crf(-1))
  expect_error(AV1R:::.crf_to_vulkan_crf(64))
})

test_that(".crf_to_vulkan_crf is monotonically increasing", {
  crfs <- vapply(0:63, AV1R:::.crf_to_vulkan_crf, integer(1))
  expect_true(all(diff(crfs) >= 0))
})

test_that(".ensure_min_size delegates to .compute_output_size", {
  r1 <- AV1R:::.ensure_min_size(64, 64)
  r2 <- AV1R:::.compute_output_size(64, 64, tiff_scale = NULL)
  expect_equal(r1, r2)
})

test_that(".crf_to_vaapi_qp maps calibration knots exactly", {
  expect_equal(AV1R:::.crf_to_vaapi_qp(0),   1L)
  expect_equal(AV1R:::.crf_to_vaapi_qp(1),  10L)
  expect_equal(AV1R:::.crf_to_vaapi_qp(5),  15L)
  expect_equal(AV1R:::.crf_to_vaapi_qp(10), 35L)
  expect_equal(AV1R:::.crf_to_vaapi_qp(20), 70L)
  expect_equal(AV1R:::.crf_to_vaapi_qp(30), 110L)
  expect_equal(AV1R:::.crf_to_vaapi_qp(40), 150L)
  expect_equal(AV1R:::.crf_to_vaapi_qp(50), 190L)
  expect_equal(AV1R:::.crf_to_vaapi_qp(63), 255L)
})

test_that(".crf_to_vaapi_qp interpolates between knots", {
  qp15 <- AV1R:::.crf_to_vaapi_qp(15)
  # Between CRF 10 (QP 35) and CRF 20 (QP 70) — midpoint ~53
  expect_gt(qp15, 35L)
  expect_lt(qp15, 70L)
})

test_that(".crf_to_vaapi_qp errors on out-of-range input", {
  expect_error(AV1R:::.crf_to_vaapi_qp(-1))
  expect_error(AV1R:::.crf_to_vaapi_qp(64))
})

test_that(".crf_to_vaapi_qp is monotonically increasing", {
  qps <- vapply(0:63, AV1R:::.crf_to_vaapi_qp, integer(1))
  expect_true(all(diff(qps) >= 0))
})

test_that(".ensure_min_size with custom minimums", {
  result <- AV1R:::.ensure_min_size(100, 100, min_w = 500L, min_h = 500L)
  # After rounding down to multiple of 8, may be slightly below min
  # but the multiplier should bring it close
  expect_true(result$scaled)
  expect_gte(result$width, 488L)   # 500 - 8 (worst case rounding)
  expect_gte(result$height, 488L)
})
