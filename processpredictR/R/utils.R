


#' Utils
#'
#' @param examples a preprocessed dataset returned by prepare_examples_dt().
#'
#'
get_vocabulary <- function(examples) {
  attr(examples, "vocabulary")
}

get_task <- function(examples) {
  attr(examples, "task")
}

hot_encode_feats <- function(examples) {
  mapping <- attr(examples, "mapping")
  features <- attr(examples, "features")

  # categorical features original and new hot-encoded features' names
  feats <- examples %>% as_tibble() %>% select(features)
  cat_features <- feats %>% select(where(is.factor))

  if (length(cat_features) > 0) {
    cat_features %>%
      data.table::as.data.table() %>%
      mltools::one_hot(cols = names(cat_features)) %>% names -> names_hotencoded_features

    output <- examples %>%
      data.table::as.data.table() %>%
      mltools::one_hot(cols = names(cat_features), dropCols = F) %>%
      as_tibble()
  }

  else {
    names_hotencoded_features <- NULL
    output <- examples
  }
  # names numeric features
  names_num_features <- feats %>% select(where(is.double)) %>% names
  if (length(names_num_features) == 0) names_num_features <- NULL


  class(output) <- c("ppred_examples_df", class(output))
  attr(output, "task") <- attr(examples, "task")
  attr(output, "y_var") <- attr(examples, "y_var")
  attr(output, "max_case_length") <- attr(examples, "max_case_length")
  attr(output, "vocab_size") <- attr(examples, "vocab_size")
  attr(output, "num_outputs") <- attr(examples, "num_outputs")


  attr(output, "numeric_features") <- names_num_features
  attr(output, "features") <- names_num_features %>% append(names_hotencoded_features)
  attr(output, "hot_encoded_categorical_features") <- names_hotencoded_features
  #attr(output, "number_features") <- names_num_features %>% append(names_hotencoded_features) %>% length

  attr(output, "mapping") <- mapping
  attr(output, "vocabulary") <- attr(examples, "vocabulary")

  return(output)
}

TransformerBlock <- function() {
  super <- NULL
  self <- NULL
  keras::new_layer_class(
    classname = "TransformerBlock",
    initialize = function(self, embed_dim, num_heads, ff_dim, rate = 0.1) {
      super$initialize()

      self$att <- keras::layer_multi_head_attention(num_heads=num_heads, key_dim=embed_dim)
      self$ffn <- keras::keras_model_sequential() %>%
        layer_dense(ff_dim, activation="relu") %>%
        layer_dense(embed_dim)

      self$layernorm_a <- keras::layer_layer_normalization(epsilon=1e-6)  #LayerNormalization(epsilon=1e-6)
      self$layernorm_b <- keras::layer_layer_normalization(epsilon=1e-6) # OF layer_layer_normalization
      self$dropout_a <- keras::layer_dropout(rate=0.1) #layers.Dropout(rate)
      self$dropout_b <- keras::layer_dropout(rate=0.1)
    },
    call = function(self, inputs, training) {
      attn_output <- self$att(inputs, inputs)
      attn_output <- self$dropout_a(attn_output, training=training)
      out_a <- self$layernorm_a(inputs + attn_output)
      ffn_output <- self$ffn(out_a)
      ffn_output <- self$dropout_b(ffn_output, training=training)
      return(self$layernorm_b(out_a + ffn_output))
    }
  )
}


# create layer TokenAndPositionEmbedding
TokenAndPositionEmbedding  <- function() {
  super <- NULL
  self <- NULL
  keras::new_layer_class(
    classname = "TokenAndPositionEmbedding",
    initialize = function(self, maxlen, vocab_size, embed_dim, ...) {
      super$initialize()
      self$maxlen <- maxlen
      self$vocab_size <- vocab_size
      self$embed_dim <- embed_dim

      self$token_emb <- keras::layer_embedding(input_dim = vocab_size, output_dim = embed_dim) #layers.Embedding(input_dim=vocab_size, output_dim=embed_dim)
      self$pos_emb <- keras::layer_embedding(input_dim = maxlen, output_dim = embed_dim) #layers.Embedding(input_dim=maxlen, output_dim=embed_dim)

    },
    call = function(self, x) {
      maxlen <- tf$shape(x)[-1]  #tf.shape(x)[-1] NA, NULL, -1 is all the same
      positions <- tf$range(start=0, limit=maxlen, delta=1)
      positions <- self$pos_emb(positions)
      x <- self$token_emb(x)
      return(x + positions)
    },

    get_config = function() {
      # base_config <- super(TokenAndPositionEmbedding, self)$get_config()
      # list(maxlen = self$maxlen,
      #      vocab_size = self$vocab_size,
      #      embed_dim = self$embed_dim)

      # CUSTOM OBJECTS section (https://tensorflow.rstudio.com/guides/keras/serialization_and_saving)
      config <- super()$get_config()
      config$maxlen <- self$maxlen
      config$vocab_size <- self$vocab_size
      config$embed_dim <- self$embed_dim
      config
    }
  )
}

