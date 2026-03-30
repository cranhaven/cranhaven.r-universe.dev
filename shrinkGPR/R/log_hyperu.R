robust_hyperu <- function(a, b, z) {

  device <- z$device

  # Create object to store results
  res <- torch_full_like(z, NA_real_, device = device)
  small_z <- abs(z) < 1e-9

  # Approximation for small z and b > 2
  res[small_z & b > 2] <- torch_exp(torch_lgamma(b - 1) - torch_lgamma(a) + (1 - b) * torch_log(z[small_z & b > 2]))

  # Approximation for small z and b = 2
  res[small_z & b == 2] <- torch_exp(-torch_lgamma(a) - torch_log(z[small_z & b == 2]))

  # Approximation for small z and 1 < b < 2
  res[small_z & b < 2 & b > 1] <- torch_exp(torch_lgamma(b - 1) - torch_lgamma(a) + (1 - b) * torch_log(z[small_z & b < 2 & b > 1])) +
    torch_exp(-torch_lgamma(1 - b) - torch_lgamma(a - b + 1))

  # Approximation for small z and b = 1
  res[small_z & b == 1] <- -torch_exp(-torch_lgamma(a) - torch_log(z[small_z & b == 1] + torch_digamma(a) + 0.57721566490153286060651209008240243))

  # Approximation for small z and 0 < b < 1
  res[small_z & b < 1 & b > 0] <- torch_exp(torch_lgamma(1 - b) - torch_lgamma(a - b + 1))

  # Approximation for small z and b = 0
  res[small_z & b == 0] <- torch_exp(-torch_lgamma(a + 1))

  # Approximation for small z and b < 0
  res[small_z & b < 0] <- torch_exp(torch_lgamma(1 - b) - torch_lgamma(a - b + 1))

  # Fill up all where no special case applies
  res[res$isnan()] <- torch_tensor(hyperg_U(as.array(a$cpu()),
                                           as.array(b$cpu()),
                                           as.array(z[res$isnan()]$cpu())), device = device)

  return(res)
}

log_hyperu <- autograd_function(
  forward = function(ctx, a, b, x) {

    u_res <- robust_hyperu(a, b, x)
    result <- torch_log(u_res)

    ctx$save_for_backward(a = a, b = b, x = x, u_res = u_res)

    return(result)
  },
  backward = function(ctx, grad_output) {
    # Retrieve saved variables
    s <- ctx$saved_variables
    a <- s$a
    b <- s$b
    x <- s$x
    u_res <- s$u_res

    # Mark 'a' and 'b' as non-differentiable
    ctx$mark_non_differentiable(a)
    ctx$mark_non_differentiable(b)

    # Compute grad_x using differentiable Torch operations
    grad_x <- grad_output * (-a * torch_div(robust_hyperu(a + 1, b + 1, x), u_res))

    # Return a named list of gradients
    grads <- list(
      a = NULL,  # Since 'a' does not require a gradient
      b = NULL,  # Since 'b' does not require a gradient
      x = grad_x
    )

    return(grads)
  }
)


