#' Distributional IV Model Fit Function
#'
#' This function fits a joint distributional IV model to the provided data. It allows for the tuning of
#' several parameters related to model complexity and model training. The function is not meant to
#' be exported but can be used within the package or for internal testing purposes.
#'
#' @inheritParams div
#'
#' @return A list containing the trained DIV model and a matrix of loss values.
#'
#' @keywords internal

divfit <- function(Z, X, Y, W,
                   epsx_dim = epsx_dim, epsy_dim = epsy_dim, epsh_dim = epsh_dim,
                   hidden_dim = hidden_dim, num_layer = num_layer,
                   num_epochs = num_epochs, lr = lr, beta = beta, silent = silent) {

  # Determine the device to use
  device <- use_device()

  # Check input parameters
  assert_count(epsh_dim)
  assert_count(epsx_dim)
  assert_count(epsy_dim)
  assert_count(hidden_dim)
  assert_count(num_layer)
  assert_count(num_epochs)
  assert_numeric(lr, len = 1, any.missing = FALSE, lower = 0)
  assert_logical(silent)
  assert_numeric(beta, lower = 0.01, upper = 1.99)

  # Set input & output dimensions for NNs
  in_dim_g <- dim(Z)[2] + ifelse(is.null(W), 0, dim(W)[2])
  out_dim_g <- dim(X)[2]
  in_dim_f <- dim(X)[2] + ifelse(is.null(W), 0, dim(W)[2])
  out_dim_f <- dim(Y)[2]

  noise_g_dim <- epsx_dim + epsh_dim
  noise_f_dim <- epsy_dim + epsh_dim

  # Generator for g
  gen_g <- nn_model(in_dim = in_dim_g, noise_dim = noise_g_dim,
                    hidden_dim = hidden_dim, out_dim = out_dim_g, num_layer = num_layer)
  gen_g$to(device = device)
  gen_g$train()

  # Generator for f
  gen_f <- nn_model(in_dim = in_dim_f, noise_dim = noise_f_dim,
                    hidden_dim = hidden_dim, out_dim = out_dim_f, num_layer = num_layer)
  gen_f$to(device = device)
  gen_f$train()

  # Set up the optimizers for both generators
  params <- c(gen_g$parameters, gen_f$parameters)
  optim_gen <- optim_adam(params, lr = lr)

  Z <- torch_tensor(Z, device = device)
  X <- torch_tensor(X, device = device)
  Y <- torch_tensor(Y, device = device)
  if (!is.null(W)) {W <- torch_tensor(W, device = device)}

  x0_in_loss <- torch_cat(list(X, Y), dim = 2)$to(device = device) # cbind
  n <- dim(X)[1]

  loss_vec <- matrix(nrow = num_epochs, ncol = 3)
  colnames(loss_vec) <- c("Energy loss", "E(||U-Uhat||)", "E(||Uhat-Uhat'||)")
  print_at <- pmax(1, floor(seq(1, num_epochs, length = 11)))

  # Training loop
  for (epoch in 1:num_epochs) {
    optim_gen$zero_grad()

    # Gaussian noise
    eps_x1 <- torch_randn(n, epsx_dim)$to(device = device)
    eps_y1 <- torch_randn(n, epsy_dim)$to(device = device)
    eps_h1 <- torch_randn(n, epsh_dim)$to(device = device)

    eps_x2 <- torch_randn(n, epsx_dim)$to(device = device)
    eps_y2 <- torch_randn(n, epsy_dim)$to(device = device)
    eps_h2 <- torch_randn(n, epsh_dim)$to(device = device)

    # Data for training g
    in_g1 <- torch_cat(list(Z$to(device = device), eps_x1, eps_h1), dim = 2)
    in_g2 <- torch_cat(list(Z$to(device = device), eps_x2, eps_h2), dim = 2)
    if (!is.null(W)) {
      in_g1 <- torch_cat(list(in_g1, W), dim = 2)
      in_g2 <- torch_cat(list(in_g2, W), dim = 2)
    }

    # Generator for g
    gen_X1 <- gen_g(in_g1)
    gen_X2 <- gen_g(in_g2)

    # Data for training f, X sampled from generative model
    in_f1 <- torch_cat(list(gen_X1, eps_y1, eps_h1), dim = 2)
    in_f2 <- torch_cat(list(gen_X2, eps_y2, eps_h2), dim = 2)
    if (!is.null(W)) {
      in_f1 <- torch_cat(list(in_f1, W), dim = 2)
      in_f2 <- torch_cat(list(in_f2, W), dim = 2)
    }

    # Generator for f
    gen_Y1 <- gen_f(in_f1)
    gen_Y2 <- gen_f(in_f2)

    gen_XY1 <- torch_cat(list(gen_X1, gen_Y1), dim = 2)
    gen_XY2 <- torch_cat(list(gen_X2, gen_Y2), dim = 2)

    if(beta == 1){
      loss_fct <- energyloss(x0 = x0_in_loss, x = gen_XY1, xp = gen_XY2, verbose = TRUE)
    } else{
      loss_fct <- energylossbeta(x0 = x0_in_loss, x = gen_XY1, xp = gen_XY2,
                                 beta = beta, verbose = TRUE)
    }

    loss_vec[epoch, ] <- signif(c(sapply(loss_fct, as.numeric)), 3)

    loss_fct[[1]]$backward()
    # Optimize the generator's parameters
    optim_gen$step()

    # Show training loss and out-of-sample loss
    if (!silent) {
      cat("\r ", round(100 * epoch / num_epochs), "% complete, epoch: ", epoch)
      if (epoch %in% print_at) {
        cat("\n")
        print(loss_vec[epoch, ])
      }
    }
  }

  gen_f$eval()
  gen_g$eval()

  # needed for predict.DIV()
  DIV_f <- function(x, w = NULL) {
    noise_y <- torch_randn(nrow(x), epsy_dim)$to(device = device)
    noise_h <- torch_randn(nrow(x), epsh_dim)$to(device = device)
    if (!is.null(W)) {
      input_f <- torch_cat(list(x$to(device = device), noise_h, noise_y,
                                w$to(device = device)), dim = 2)
    }
    else {
      input_f <- torch_cat(list(x$to(device = device), noise_h, noise_y), dim = 2)
    }
    return(as.matrix(gen_f(input_f), ncol = out_dim_f))
  }

  DIV_g <- function(z, w = NULL) {
    noise_x <- torch_randn(nrow(z), epsx_dim)$to(device = device)
    noise_h <- torch_randn(nrow(z), epsh_dim)$to(device = device)
    if (!is.null(W)) {
      input_g <- torch_cat(list(z$to(device = device), noise_h, noise_x,
                                w$to(device = device)), dim = 2)
    }
    else {
      input_g <- torch_cat(list(z$to(device = device), noise_h, noise_x), dim = 2)
    }
    return(as.matrix(gen_g(input_g), ncol = out_dim_g))
  }

  return(list(DIV_f = DIV_f,
              DIV_g = DIV_g,
              loss_vec = loss_vec))
}
