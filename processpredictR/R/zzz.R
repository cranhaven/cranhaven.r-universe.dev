# global reference to scipy (will be initialized in .onLoad)
# scipy <- NULL
# tf <- NULL
# layers <- NULL
# activations <- NULL

# python 'scipy' module I want to use in my package
keras <- NULL

.onLoad <- function(libname, pkgname) {
  # delay load foo module (will only be loaded when accessed via $)

  keras <<- keras::implementation()}

#.onLoad <- function(libname, pkgname) {

  # create layer TransformerBlock
  # TransformerBlock <- keras::new_layer_class(
  #   classname = "TransformerBlock",
  #   initialize = function(self, embed_dim, num_heads, ff_dim, rate = 0.1) {
  #     super$initialize()
  #
  #     self$att <- keras::layer_multi_head_attention(num_heads=num_heads, key_dim=embed_dim)
  #     self$ffn <- keras::keras_model_sequential() %>%
  #       layer_dense(ff_dim, activation="relu") %>%
  #       layer_dense(embed_dim)
  #
  #     self$layernorm_a <- keras::layer_layer_normalization(epsilon=1e-6)  #LayerNormalization(epsilon=1e-6)
  #     self$layernorm_b <- keras::layer_layer_normalization(epsilon=1e-6) # OF layer_layer_normalization
  #     self$dropout_a <- keras::layer_dropout(rate=0.1) #layers.Dropout(rate)
  #     self$dropout_b <- keras::layer_dropout(rate=0.1)
  #   },
  #   call = function(self, inputs, training) {
  #     attn_output <- self$att(inputs, inputs)
  #     attn_output <- self$dropout_a(attn_output, training=training)
  #     out_a <- self$layernorm_a(inputs + attn_output)
  #     ffn_output <- self$ffn(out_a)
  #     ffn_output <- self$dropout_b(ffn_output, training=training)
  #     return(self$layernorm_b(out_a + ffn_output))
  #   }
  # )
  # assign("TransformerBlock", TransformerBlock, envir = globalenv())
  #
  # # create layer TokenAndPositionEmbedding
  # TokenAndPositionEmbedding <- keras::new_layer_class(
  #   classname = "TokenAndPositionEmbedding",
  #   initialize = function(self, maxlen, vocab_size, embed_dim) {
  #     super$initialize()
  #
  #     self$token_emb <- keras::layer_embedding(input_dim = vocab_size, output_dim = embed_dim) #layers.Embedding(input_dim=vocab_size, output_dim=embed_dim)
  #     self$pos_emb <- keras::layer_embedding(input_dim = maxlen, output_dim = embed_dim) #layers.Embedding(input_dim=maxlen, output_dim=embed_dim)
  #
  #   },
  #   call = function(self, x) {
  #     maxlen <- tf$shape(x)[-1]  #tf.shape(x)[-1] NA, NULL, -1 is all the same
  #     positions <- tf$range(start=0, limit=maxlen, delta=1)
  #     positions <- self$pos_emb(positions)
  #     x <- self$token_emb(x)
  #     return(x + positions)
  #   }
  # )
  # assign("TokenAndPositionEmbedding", TokenAndPositionEmbedding, envir = globalenv())


  # reticulate::configure_environment(pkgname)
  #
  # # use superassignment to update global reference to scipy
  # scipy <<- reticulate::import("scipy", delay_load = TRUE)
  #
  # tf <<- reticulate::import("tensorflow", delay_load = TRUE)
  # layers <<- reticulate::import("keras", delay_load = TRUE)$layers
  # activations <<- reticulate::import("keras", delay_load = TRUE)$activations
#}



# tf <- import("tensorflow")
# layers <- import("keras")$layers
# activations <- import("keras")$activations
