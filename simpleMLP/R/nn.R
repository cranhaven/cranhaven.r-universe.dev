#' Initialize network
#'
#' Initialize 3 layer fully connected neural network, also known as multilayer
#' perceptron, setting biases to 0 and using the Xavier initialization method
#' for weights.
#'
#' @param num_inputs dimension of inputs
#' @param num_hidden_1 dimension of first hidden layer
#' @param num_hidden_2 dimension of second hidden layer
#' @param num_outputs dimension of output
#'
#' @return list containing weight and bias matrices in each layer of the network
#' @export
#'
#' @examples
#' mlp_model <- init_nn(784, 100, 50, 10)
init_nn <- function(num_inputs, num_hidden_1, num_hidden_2, num_outputs) {
  w1 <- matrix(stats::rnorm(num_inputs * num_hidden_1, 0, 1 / num_inputs), nrow=num_inputs)
  w2 <- matrix(stats::rnorm(num_hidden_1 * num_hidden_2, 0, 1 / num_hidden_1), nrow=num_hidden_1)
  w3 <- matrix(stats::rnorm(num_hidden_2 * num_outputs, 0, 1 / num_hidden_2), nrow=num_hidden_2)
  b1 <- rep(0, num_hidden_1)
  b2 <- rep(0, num_hidden_2)
  b3 <- rep(0, num_outputs)
  model <- list("w1" = w1, "w2" = w2, "w3" = w3, "b1" = b1, "b2" = b2, "b3" = b3)
  return(model)
}

affine <- function(x, w, b) {
  # The function arguments must be matrix or vector format
  x <- as.matrix(as.data.frame(x))
  w <- as.matrix(as.data.frame(w))
  b <- unlist(b)

  y <- x %*% w
  y <- y + b[col(y)]
  return(y)
}

affine_back <- function(grad_y, x, w) {
  # The function arguments must be matrix or vector format
  grad_y <- as.matrix(as.data.frame(grad_y))
  x <- as.matrix(as.data.frame(x))
  w <- as.matrix(as.data.frame(w))

  grad_x <- grad_y %*% t(w)
  grad_w <- t(x) %*% grad_y
  grad_b <- rep(1, nrow(grad_y)) %*% grad_y
  return(list("grad_x" = grad_x, "grad_w" = grad_w, "grad_b" = grad_b))
}

relu <- function(x) {
  x <- as.matrix(as.data.frame(x))
  x[x < 0] <- 0
  return(x)
}

relu_back <- function(grad_y, x) {
  grad_y <- as.matrix(as.data.frame(grad_y))
  x <- as.matrix(as.data.frame(x))

  grad_x <- grad_y
  grad_x[x < 0] <- 0
  return(grad_x)
}

softmax <- function(y) {
  y <- as.matrix(as.data.frame(y))
  prediction <- exp(y) / apply(y, 1, function(x) sum(exp(x)))
  return(prediction)
}

#' Forward propagation
#'
#' Runs a forward pass through the network.
#'
#' @param model list of all the weights and biases
#' @param x input to the network
#'
#' @return list of all intermediate values
forwardprop <- function(model, x) {
  z1 <- affine(x, model["w1"], model["b1"])
  h1 <- relu(z1)
  z2 <- affine(h1, model["w2"], model["b2"])
  h2 <- relu(z2)
  y <- affine(h2, model["w3"], model["b3"])
  forward_pass <- list("x" = x, "z1" = z1, "h1" = h1, "z2" = z2, "h2" = h2, "y" = y)
  return(forward_pass)
}

#' Backpropagation
#'
#' Runs a backwards pass through the network.
#'
#' @param model list of all the weights and biases
#' @param error gradients to the output of the network
#' @param forward_pass intermediate values from the forward pass
#'
#' @return list of derivatives after the backwards pass
backprop <- function(model, error, forward_pass) {
  affine3 <- affine_back(error, forward_pass["h2"], model["w3"])
  grad_z2 <- relu_back(affine3["grad_x"], forward_pass["z2"])

  affine2 <- affine_back(grad_z2, forward_pass["h1"], model["w2"])
  grad_z1 <- relu_back(affine2["grad_x"], forward_pass["z1"])

  affine1 <- affine_back(grad_z1, forward_pass["x"], model["w1"])

  back_pass <- list("dw1" = affine1["grad_w"], "db1" = affine1["grad_b"],
                    "dw2" = affine2["grad_w"], "db2" = affine2["grad_b"],
                    "dw3" = affine3["grad_w"], "db3" = affine3["grad_b"])
  return(back_pass)
}

#' Update Model
#'
#' Updates the model using derivatives from a backward pass.
#'
#' @param model list of all the weights and biases
#' @param back_pass derivatives from a backwards pass through the network
#' @param alpha learning rate
#'
#' @return updated list of the weights and biases
update <- function(model, back_pass, alpha) {
  update_model <- list(
    "w1" = as.matrix(as.data.frame(model["w1"])) - alpha * as.matrix(as.data.frame(back_pass["dw1"])),
    "w2" = as.matrix(as.data.frame(model["w2"])) - alpha * as.matrix(as.data.frame(back_pass["dw2"])),
    "w3" = as.matrix(as.data.frame(model["w3"])) - alpha * as.matrix(as.data.frame(back_pass["dw3"])),

    "b1" = unlist(model["b1"]) - alpha * unlist(back_pass["db1"]),
    "b2" = unlist(model["b2"]) - alpha * unlist(back_pass["db2"]),
    "b3" = unlist(model["b3"]) - alpha * unlist(back_pass["db3"])
  )
  return (update_model)
}
