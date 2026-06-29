# Pipe Operation
worm_download()$Ds |>
    as_worm_tensor() -> object

# k=3
worm_membership(object, k = 3) -> object_k3

expect_equal(is(object_k3@membership_tensor), "Tensor")
expect_equal(object_k3@k, 3)

# k=4
worm_membership(object, k = 6) -> object_k6

expect_equal(is(object_k6@membership_tensor), "Tensor")
expect_equal(object_k6@k, 6)
