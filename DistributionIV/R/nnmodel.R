#' Define a Stochastic Generative Neural Network Model with Noise at Input Layer
#'
#' This function defines a generative neural network model for a certain architecture
#' and adds noise to the input layer.
#'
#' @param in_dim Integer. Input dimension.
#' @param noise_dim Integer. Dimension of the noise to inject.
#' @param hidden_dim Integer. Number of neurons in the hidden layers (default: 100).
#' @param out_dim Integer. Output dimension.
#' @param num_layer Integer. Number of layers in the model (default: 3).
#'
#' @return A generative neural network model with intermediate noise injection.
#'
#' @keywords internal

nn_model <- function(in_dim, noise_dim, hidden_dim = 100, out_dim, num_layer = 3) {
  if (num_layer <= 2) {
    gen_model <- nn_sequential(
      nn_linear(in_dim + noise_dim, hidden_dim),
      nn_elu(),
      # nn_sigmoid(),
      nn_batch_norm1d(hidden_dim),
      # nn_dropout(dropout),
      nn_linear(hidden_dim, out_dim)
    )
  } else {
    hid <- nn_sequential(
      nn_linear(hidden_dim, hidden_dim),
      nn_elu(),
      # nn_sigmoid(),
      nn_batch_norm1d(hidden_dim)
    )
    if (num_layer > 3) {
      for (lay in 3:num_layer) {
        hid <- nn_sequential(
          hid,
          nn_sequential(
            nn_linear(hidden_dim, hidden_dim),
            nn_elu(),
            # nn_sigmoid(),
            nn_batch_norm1d(hidden_dim)
          )
        )
      }
    }
    gen_model <- nn_sequential(
      nn_sequential(
        nn_linear(in_dim + noise_dim, hidden_dim),
        nn_elu(),
        # nn_sigmoid(),
        nn_batch_norm1d(hidden_dim)
        # nn_dropout(dropout)
      ),
      hid,
      nn_linear(hidden_dim, out_dim)
    )
  }
  return(gen_model)
}
