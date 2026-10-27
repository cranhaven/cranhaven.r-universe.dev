#' Sequence encoders with a joint multi-label head
#'
#' Three encoders, all fitted the same way and all ending the same way: one encoder maps a unit's
#' representation to an embedding and a single linear layer maps that embedding to one output per
#' variable, so every variable is predicted together from a shared representation. Pooling strength
#' across variables is what makes the rarer ones learnable at all at these sample sizes.
#'
#' `mlp()` flattens the channels and builds in no temporal geometry. It is what separates the effect
#' of the model class from the effect of the representation: where it matches a penalised
#' regression, the difference a convolutional encoder makes is the convolution rather than the
#' network.
#'
#' `cnn()` is blocks of a one-dimensional convolution, batch normalisation, a rectified linear
#' activation and max pooling, then global average pooling. Pooling becomes an identity once a
#' sequence is shorter than its kernel, so the same stack runs at every grain of a ladder,
#' including one bin per year.
#'
#' `rescnn()` adds dilated residual blocks with squeeze-excitation channel gates, whose stage
#' dilations widen the receptive field toward the seasonal scale without widening the kernel, and
#' concatenates global average with global maximum pooling so extremes reach the head beside the
#' level.
#'
#' The three constructors carry architecture. How that architecture is trained is
#' [train_control()], which the run supplies; a setting named in `...` here overrides the run's
#' control for this learner alone. What the head is trained toward is the response head's: its
#' `loss` is the training objective and its `activation` the output transform, so a head registered
#' with a squared-error loss and an identity activation trains the same encoders on a continuous
#' response.
#'
#' Every channel is standardised by its own centre and scale, computed over every unit and bin of
#' the fitting units, so a static predictor appended as a channel sits on the same footing as a
#' reading whatever its units are. The channels of [calendar_channels()] are the exception and are
#' read at their own amplitude: they lie on the unit circle already, and being identical across
#' units they reach a network as a bias, whose step under an adaptive optimiser grows with the scale
#' of the input carrying it. Standardised, the four calendar channels of an hourly record move the
#' fully connected encoder's first layer 1.4 times as far per step, and on the Schrankogel record
#' that costs it 0.044 TSS (`inst/reproduce/README.md`).
#'
#' A fitted encoder holds its weights as plain arrays and rebuilds the network when it predicts, so
#' a fit saved with [saveRDS()] predicts after [readRDS()] in a fresh session.
#'
#' @inheritParams elasticnet
#' @param channels Channel width of each stage.
#' @param hidden Hidden layer widths, for the fully connected encoder.
#' @param kernel Convolution kernel width.
#' @param dilations Dilation of each stage, cycled if shorter than `channels`.
#' @param blocks_per_stage Residual blocks in each stage.
#' @param dropout Dropout rate.
#' @param ... Training settings for this learner, named as in [train_control()].
#'
#' @return A [learner()].
#'
#' @examples
#' cnn(epochs = 5)
#'
#' @name torch_learners
NULL

#' @rdname torch_learners
#' @export
mlp <- function(data = NULL, hidden = c(512L, 256L), dropout = 0.3, ...) {
  .torch_learner("mlp", data, "tabular",
                 list(hidden = as.integer(hidden), dropout = dropout),
                 .given_control(list(...), "the mlp learner"))
}

#' @rdname torch_learners
#' @export
cnn <- function(data = NULL, channels = c(16L, 32L, 64L, 128L), kernel = 7L, dropout = 0.3, ...) {
  .torch_learner("cnn", data, "sequence",
                 list(channels = as.integer(channels), kernel = as.integer(kernel),
                      dropout = dropout),
                 .given_control(list(...), "the cnn learner"))
}

#' @rdname torch_learners
#' @export
rescnn <- function(data = NULL, channels = c(32L, 64L, 128L, 256L), blocks_per_stage = 2L,
                   kernel = 7L, dilations = c(1L, 2L, 4L, 8L), dropout = 0.3, ...) {
  .torch_learner("rescnn", data, "sequence",
                 list(channels = as.integer(channels),
                      blocks_per_stage = as.integer(blocks_per_stage),
                      kernel = as.integer(kernel), dilations = as.integer(dilations),
                      dropout = dropout),
                 .given_control(list(...), "the rescnn learner"))
}

# One learner factory for every encoder: the architecture is a module builder and nothing else, so
# the training recipe, the standardiser, the objective and the early stopping have one definition
# and cannot drift between architectures. The builder is named rather than passed, because a
# fitted encoder stores that name and looks the builder up when it rebuilds the network.
.torch_learner <- function(name, data, reads, arch, control) {
  learner(
    name = name,
    data = data, reads = reads, multi = "joint", control = control,
    needs = "torch",
    params = arch,
    fit = function(x, y, control, head, group = NULL, ...) {
      given <- list(...)
      unknown <- setdiff(names(given), c(names(arch), .control_names()))
      if (length(unknown)) {
        stop("the ", name, " learner has no setting called ",
             paste(sort(unknown, method = "radix"), collapse = ", "), ".", call. = FALSE)
      }
      .torch_fit(x, y, name,
                 utils::modifyList(arch, given[intersect(names(given), names(arch))]),
                 .resolve_control(control,
                                  .given_control(given[intersect(names(given),
                                                                 .control_names())],
                                                 paste("the", name, "learner"))),
                 head, group)
    },
    predict = function(model, x) .torch_predict(model, x)
  )
}

# ---- the objective -------------------------------------------------------------------------

# The response head names its loss and its activation, and the trainer looks both up here. Each
# loss takes the head's case weights for the cells of the batch, so a rare response weighs what
# the head says it weighs; an activation is applied at prediction and nowhere else.
.torch_losses <- list(
  binary_cross_entropy = function(torch) {
    function(out, target, weight) {
      torch$nnf_binary_cross_entropy_with_logits(out, target, weight = weight)
    }
  },
  squared_error = function(torch) {
    function(out, target, weight) (weight * (out - target)$pow(2))$mean()
  }
)

.torch_activations <- list(
  sigmoid = function(torch, out) torch$torch_sigmoid(out),
  identity = function(torch, out) out
)

.torch_objective <- function(head) {
  if (!is.character(head$loss) || !head$loss %in% names(.torch_losses)) {
    stop("the encoders do not train under the ", .describe(head$loss), " loss. They know ",
         paste(names(.torch_losses), collapse = " and "), ".", call. = FALSE)
  }
  if (!is.character(head$activation) || !head$activation %in% names(.torch_activations)) {
    stop("the encoders have no ", .describe(head$activation), " activation. They know ",
         paste(names(.torch_activations), collapse = " and "), ".", call. = FALSE)
  }
  list(loss = .torch_losses[[head$loss]](.torch()), activation = head$activation)
}

# ---- the training recipe -------------------------------------------------------------------

.torch_fit <- function(x, y, module, arch, cfg, head, group = NULL) {
  torch <- .torch()
  device <- .torch_device(cfg$device)
  objective <- .torch_objective(head)

  scaler <- .channel_scaler(x)
  m <- .scale_channels(.to_nchw(x), scaler)

  n <- dim(m)[1L]
  old <- .seed_state()
  on.exit(.restore_seed(old), add = TRUE)
  # torch's generators are seeded for the fit and put back with R's, so a fit leaves every
  # stream where it found it and what runs after it draws what it would have drawn anyway.
  torch_state <- .torch_rng_state(torch, device)
  on.exit(.restore_torch_rng(torch, device, torch_state), add = TRUE)
  set.seed(cfg$seed)
  torch$torch_manual_seed(cfg$seed)

  val <- .validation_split(y, cfg$val_frac, group)
  fit_idx <- setdiff(seq_len(n), val)

  xt <- torch$torch_tensor(m, dtype = torch$torch_float())$to(device = device)
  yt <- torch$torch_tensor(y, dtype = torch$torch_float())$to(device = device)
  # The weights are the head's, read off the fitting units alone and applied to every unit: the
  # validation units are held out of the count a rare response's weight is made from, as they are
  # held out of the fit, and the loss the early stopping reads on them is weighted as the loss the
  # fit minimises, so the epoch it keeps is the one the fit's own objective prefers.
  weights <- .head_weights(head, y, fitting = seq_len(n) %in% fit_idx)
  wt <- torch$torch_tensor(weights, dtype = torch$torch_float())$to(device = device)
  loss_fn <- objective$loss

  shape <- c(dim(m)[2L], dim(m)[3L], ncol(y))
  net <- .torch_build(module, shape, arch)
  net$to(device = device)
  opt <- torch$optim_adamw(net$parameters, lr = cfg$learning_rate,
                           weight_decay = cfg$weight_decay)
  sched <- torch$lr_cosine_annealing(opt, T_max = cfg$epochs)

  best <- list(loss = Inf, state = NULL)
  bad <- 0L
  swa_from <- if (cfg$swa) max(1L, as.integer(cfg$swa_start * cfg$epochs)) else cfg$epochs + 1L
  average <- list(state = NULL, n = 0L)
  for (epoch in seq_len(cfg$epochs)) {
    net$train()
    for (b in .batches(fit_idx, cfg$batch_size, shuffle = TRUE)) {
      idx <- .index(torch, b, device)
      opt$zero_grad()
      loss <- loss_fn(net(xt[idx, , ]), yt[idx, ], wt[idx, ])
      loss$backward()
      opt$step()
    }
    if (epoch < swa_from) {
      sched$step()
    } else {
      average <- .accumulate(net, average)
      next
    }
    if (!length(val)) {
      next
    }
    vloss <- .torch_loss(torch, net, loss_fn, xt, yt, wt, val, cfg$batch_size, device)
    if (vloss < best$loss - 1e-4) {
      best <- list(loss = vloss, state = .snapshot(net))
      bad <- 0L
    } else {
      bad <- bad + 1L
      if (bad >= cfg$early_stopping) {
        break
      }
    }
  }
  if (average$n > 0L) {
    net$load_state_dict(average$state)
    net$to(device = device)
    .refresh_batchnorm(torch, net, xt, fit_idx, cfg$batch_size, device)
  } else if (!is.null(best$state)) {
    net$load_state_dict(best$state)
    net$to(device = device)
  }
  net$eval()
  # The weights leave here as plain arrays rather than as a live network, so the fit is an R object
  # through and through: it serialises, it copies and it predicts in a session that has never seen
  # the network that produced it.
  structure(list(module = module, arch = arch, shape = shape, state = .state_arrays(net),
                 scaler = scaler, device = cfg$device, activation = objective$activation,
                 channels = dimnames(x)[[3L]], bins = dim(x)[2L], batch_size = cfg$batch_size),
            class = "timesift_torch")
}

.torch_predict <- function(model, x) {
  torch <- .torch()
  device <- .torch_device(model$device)
  net <- .torch_restore(torch, model, device)
  activation <- .torch_activations[[model$activation]]
  m <- .scale_channels(.to_nchw(x), model$scaler)
  xt <- torch$torch_tensor(m, dtype = torch$torch_float())$to(device = device)
  out <- vector("list", 0L)
  torch$with_no_grad({
    for (b in .batches(seq_len(dim(m)[1L]), model$batch_size, shuffle = FALSE)) {
      idx <- .index(torch, b, device)
      out[[length(out) + 1L]] <- as.matrix(activation(torch, net(xt[idx, , ]))$to(device = "cpu"))
    }
  })
  do.call(rbind, out)
}

.torch_loss <- function(torch, net, loss_fn, xt, yt, wt, idx_all, batch_size, device) {
  net$eval()
  total <- 0
  torch$with_no_grad({
    for (b in .batches(idx_all, batch_size, shuffle = FALSE)) {
      idx <- .index(torch, b, device)
      total <- total +
        as.numeric(loss_fn(net(xt[idx, , ]), yt[idx, ], wt[idx, ])$to(device = "cpu")) *
        length(b)
    }
  })
  total / length(idx_all)
}

# The inner validation set, `val_frac` of the units drawn by a plain random permutation. Under a
# grouping the draw is over the groups, and the rows of a drawn group are held out together: a
# unit the outer folds kept whole is not split across the fit and the loss it stops on. A draw
# stratified on the response total was tried and dropped for this: on the Schrankogel weekly arm
# the plain permutation reads 0.0047 AUC above it, five seeds each, p = 0.019
# (`inst/reproduce/README.md`). A single rare response can still draw no presence into the
# validation set here, where the stratified draw guaranteed one.
.validation_split <- function(y, val_frac, group = NULL) {
  key <- if (is.null(group)) seq_len(nrow(y)) else match(group, unique(group))
  n <- max(key)
  n_val <- max(1L, as.integer(round(val_frac * n)))
  if (val_frac <= 0 || n - n_val < 2L) {
    return(integer(0))
  }
  drawn <- sample.int(n)[seq_len(n_val)]
  sort(which(key %in% drawn), method = "radix")
}

# A copy of the weights as they stand. Detaching a tensor on the CPU returns the storage the
# optimiser goes on updating, so without the clone a snapshot would follow the live weights.
.snapshot <- function(net) {
  lapply(net$state_dict(), function(p) p$detach()$cpu()$clone())
}

# A running mean of the weights over the tail epochs. Averaging flattens the minimum the optimiser
# settled in, which is a lever on generalisation orthogonal to the architecture and to averaging
# several fitted models at prediction time.
.accumulate <- function(net, average) {
  state <- .snapshot(net)
  if (average$n == 0L) {
    return(list(state = state, n = 1L))
  }
  n <- average$n + 1L
  for (k in names(state)) {
    # Only the floating-point entries are weights. A batch-normalisation module also carries an
    # integer count of the batches it has seen, and a running mean of that is not a number.
    if (state[[k]]$dtype == .torch()$torch_float()) {
      average$state[[k]] <- average$state[[k]] + (state[[k]] - average$state[[k]]) / n
    } else {
      average$state[[k]] <- state[[k]]
    }
  }
  list(state = average$state, n = n)
}

# Batch normalisation carries running statistics that belong to the weights that produced them, so
# an average of weights needs its own pass over the fitting units before it predicts anything. The
# statistics are reset and recomputed as the plain mean over the batches of that pass, weighting
# the k-th batch by 1/k, rather than folded into whatever the last epoch left behind.
.refresh_batchnorm <- function(torch, net, xt, fit_idx, batch_size, device) {
  norms <- Filter(function(mod) inherits(mod, "nn_batch_norm_"), net$modules)
  if (!length(norms)) {
    return(invisible(net))
  }
  momentum <- lapply(norms, function(mod) mod$momentum)
  for (mod in norms) {
    mod$reset_running_stats()
  }
  net$train()
  torch$with_no_grad({
    k <- 0L
    for (b in .batches(fit_idx, batch_size, shuffle = FALSE)) {
      k <- k + 1L
      for (mod in norms) {
        mod$momentum <- 1 / k
      }
      net(xt[.index(torch, b, device), , ])
    }
  })
  for (i in seq_along(norms)) {
    norms[[i]]$momentum <- momentum[[i]]
  }
  invisible(net)
}

# As few batches of at most `size` rows as the rows divide into, of as equal a length as they can
# be, which is what NumPy's array_split gives the Python side. Cutting fixed-length batches instead
# leaves a remainder, and a remainder of one row has no variance for batch normalisation to
# standardise by: the layer's running statistics take a NaN and every prediction after it is NaN.
# Fewer than two rows per batch is refused for the same reason, so a handful of rows is one batch.
.batches <- function(idx, size, shuffle) {
  if (shuffle) {
    idx <- sample(idx)
  }
  n <- length(idx)
  k <- max(1L, min(as.integer(ceiling(n / size)), n %/% 2L))
  rest <- n %% k
  split(idx, rep(seq_len(k), rep(n %/% k, k) + c(rep(1L, rest), rep(0L, k - rest))))
}

.index <- function(torch, b, device) {
  torch$torch_tensor(as.integer(b), dtype = torch$torch_long())$to(device = device)
}

# torch wants (unit, channel, bin); the representation is (unit, bin, channel).
.to_nchw <- function(x) {
  aperm(array(as.numeric(x), dim = dim(x)), c(1L, 3L, 2L))
}

# One centre and one scale per channel, over every unit and bin of the units handed in, and the
# identity on a channel the representation marks as a position.
.channel_scaler <- function(x) {
  scaler <- .scaler(matrix(as.numeric(x), ncol = dim(x)[3L]))
  position <- dimnames(x)[[3L]] %in% (attr(x, "position") %||% character(0))
  scaler$centre[position] <- 0
  scaler$scale[position] <- 1
  scaler
}

.scale_channels <- function(m, scaler) {
  sweep(sweep(m, 2L, scaler$centre, "-"), 2L, scaler$scale, "/")
}

.torch_rng_state <- function(torch, device) {
  list(cpu = torch$torch_get_rng_state(),
       cuda = if (identical(device, "cuda")) torch$cuda_get_rng_state() else NULL)
}

.restore_torch_rng <- function(torch, device, state) {
  torch$torch_set_rng_state(state$cpu)
  if (!is.null(state$cuda)) {
    torch$cuda_set_rng_state(state$cuda)
  }
  invisible(TRUE)
}

.torch <- function() {
  if (!requireNamespace("torch", quietly = TRUE)) {
    stop("this learner needs torch. Install it with install.packages(\"torch\").", call. = FALSE)
  }
  asNamespace("torch")
}

# ---- the weights as arrays -----------------------------------------------------------------

.torch_build <- function(module, shape, arch) {
  .torch_module(module)(in_ch = shape[1L], in_len = shape[2L], n_out = shape[3L], arch = arch)
}

.state_arrays <- function(net) {
  torch <- .torch()
  lapply(net$state_dict(), function(p) {
    p <- p$detach()$cpu()
    floating <- p$dtype == torch$torch_float()
    list(values = as.array(p), shape = as.integer(p$shape), floating = floating)
  })
}

.state_tensors <- function(torch, state) {
  lapply(state, function(entry) {
    torch$torch_tensor(entry$values,
                       dtype = if (entry$floating) torch$torch_float() else torch$torch_long())$
      reshape(entry$shape)
  })
}

# The network a fitted encoder is, built from its architecture and loaded with its weights.
.torch_restore <- function(torch, model, device) {
  net <- .torch_build(model$module, model$shape, model$arch)
  net$load_state_dict(.state_tensors(torch, model$state))
  net$to(device = device)
  net$eval()
  net
}

# ---- the encoders --------------------------------------------------------------------------

# Length-safe pooling. A stack of halving pools assumes the sequence is at least as long as the
# number of stages doubled; handed a single step it would halve it to none. This pools while there
# is something to halve and is the identity once there is not, so the coarse end of a ladder runs
# through the same stack as the fine end rather than through a second architecture.
.pool_module <- function() {
  torch <- .torch()
  torch$nn_module(
    "timesift_lensafe_pool",
    initialize = function(kernel = 2L) {
      self$kernel <- kernel
      self$pool <- torch$nn_max_pool1d(kernel)
    },
    forward = function(x) {
      if (x$shape[3L] < self$kernel) x else self$pool(x)
    }
  )
}

.head_module <- function(torch, embed_dim, n_out, dropout) {
  torch$nn_sequential(torch$nn_dropout(dropout), torch$nn_linear(embed_dim, n_out))
}

.mlp_module <- function(in_ch, in_len, n_out, arch) {
  torch <- .torch()
  torch$nn_module(
    "timesift_mlp",
    initialize = function() {
      layers <- list(torch$nn_flatten())
      prev <- in_ch * in_len
      for (k in seq_along(arch$hidden)) {
        layers <- c(layers, list(torch$nn_linear(prev, arch$hidden[k]), torch$nn_relu()))
        if (k < length(arch$hidden)) {
          layers <- c(layers, list(torch$nn_dropout(arch$dropout)))
        }
        prev <- arch$hidden[k]
      }
      self$body <- do.call(torch$nn_sequential, layers)
      self$head <- .head_module(torch, prev, n_out, arch$dropout)
    },
    forward = function(x) self$head(self$body(x))
  )()
}

.cnn_module <- function(in_ch, in_len, n_out, arch) {
  torch <- .torch()
  pool <- .pool_module()
  torch$nn_module(
    "timesift_cnn",
    initialize = function() {
      layers <- list()
      prev <- in_ch
      for (c in arch$channels) {
        layers <- c(layers, list(
          torch$nn_conv1d(prev, c, arch$kernel, padding = arch$kernel %/% 2L),
          torch$nn_batch_norm1d(c), torch$nn_relu(), pool()))
        prev <- c
      }
      self$features <- do.call(torch$nn_sequential, layers)
      self$pool <- torch$nn_sequential(torch$nn_adaptive_avg_pool1d(1L), torch$nn_flatten())
      self$head <- .head_module(torch, prev, n_out, arch$dropout)
    },
    forward = function(x) self$head(self$pool(self$features(x)))
  )()
}

.rescnn_module <- function(in_ch, in_len, n_out, arch) {
  torch <- .torch()
  pool <- .pool_module()

  se <- torch$nn_module(
    "timesift_se",
    initialize = function(c, r = 8L) {
      h <- max(c %/% r, 4L)
      self$fc <- torch$nn_sequential(torch$nn_linear(c, h), torch$nn_gelu(),
                                     torch$nn_linear(h, c), torch$nn_sigmoid())
    },
    forward = function(x) x * self$fc(x$mean(dim = 3L))$unsqueeze(3L)
  )

  block <- torch$nn_module(
    "timesift_resblock",
    initialize = function(c, kernel, dilation, dropout) {
      pad <- (kernel %/% 2L) * dilation
      self$conv <- torch$nn_sequential(
        torch$nn_conv1d(c, c, kernel, padding = pad, dilation = dilation),
        torch$nn_batch_norm1d(c), torch$nn_gelu(), torch$nn_dropout(dropout),
        torch$nn_conv1d(c, c, kernel, padding = pad, dilation = dilation),
        torch$nn_batch_norm1d(c))
      self$se <- se(c)
    },
    forward = function(x) torch$nnf_gelu(x + self$se(self$conv(x)))
  )

  torch$nn_module(
    "timesift_rescnn",
    initialize = function() {
      self$stem <- torch$nn_sequential(
        torch$nn_conv1d(in_ch, arch$channels[1L], arch$kernel, padding = arch$kernel %/% 2L),
        torch$nn_batch_norm1d(arch$channels[1L]), torch$nn_gelu())
      layers <- list()
      prev <- arch$channels[1L]
      for (si in seq_along(arch$channels)) {
        c <- arch$channels[si]
        if (c != prev) {
          layers <- c(layers, list(torch$nn_conv1d(prev, c, 1L),
                                   torch$nn_batch_norm1d(c), torch$nn_gelu()))
        }
        d <- arch$dilations[((si - 1L) %% length(arch$dilations)) + 1L]
        for (b in seq_len(arch$blocks_per_stage)) {
          layers <- c(layers, list(block(c, arch$kernel, d, arch$dropout)))
        }
        layers <- c(layers, list(pool()))
        prev <- c
      }
      self$features <- do.call(torch$nn_sequential, layers)
      self$head <- .head_module(torch, prev * 2L, n_out, arch$dropout)
    },
    forward = function(x) {
      h <- self$features(self$stem(x))
      self$head(torch$torch_cat(list(h$mean(dim = 3L), h$amax(dim = 3L)), dim = 2L))
    }
  )()
}

# The encoders' module builders, by name. A fitted encoder stores the name and the builder is
# looked up when the network is rebuilt, for the reason `.learner_ref()` gives: a builder written
# into a fit carries a copy of its own body, and a fit read back after an upgrade would load this
# version's weights into the last version's architecture.
.torch_modules <- list(mlp = .mlp_module, cnn = .cnn_module, rescnn = .rescnn_module)

.torch_module <- function(name) {
  if (!is.character(name) || length(name) != 1L || !name %in% names(.torch_modules)) {
    stop("this fit was made by the ", .describe(name), " encoder, which this version of the ",
         "package does not carry. It has ", paste(names(.torch_modules), collapse = ", "), ".",
         call. = FALSE)
  }
  .torch_modules[[name]]
}
