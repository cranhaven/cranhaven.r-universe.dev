# unifying i_enumerate, isplitrows, isplitcols, chunk and chunkSize.????

.max_safe_int <- 2^.Machine$double.digits

# Template with shared logic used by icount, idiv, iteror.default
count_template <- function(input,
                           output,
                           output_chunk,
                           options=list(),
                           preamble=list(),
                           preamble_single=list(),
                           preamble_chunk=list()) {
  count <- 0
  i <- 0
  i.tmp <- 0
  chunkSize <- 0
  thisChunk <- 0

  args <- as.pairlist(eval(bquote(
    splice=TRUE,
    alist(..(input),
          ..(if (!("..." %in% names(input))) alist(...=) else list()),
          recycle=FALSE, chunkSize=, chunks=, ..(options)))))

  body <- bquote(splice=TRUE, {
    .( if (!("..." %in% names(input)))
      quote(stop_unused(...))) # any leftover arguments are an error
    ..(preamble)

    if (!is.numeric(count) || length(count) != 1 || is.na(count))
      stop('count must be a numeric value')
    if (is.finite(count) && (count < 0 || count > .max_safe_int)) {
      stop(paste0("I can't count to ", as.character(count)))
    }

    i <- count
    i[1] <- 0

    if (missing(chunks)) {
      if (missing(chunkSize)) { # single stepping
        ..(preamble_single)
        if (is.finite(count)) {
          if (recycle) {
            nextOr_ <- function(or) { # recycling, non-chunking
              if (i >= count) {
                i[1] <<- 1
                .(output(i))
              } else {
                i <<- i + 1
                .(output(i))
              }
            }
          } else {
            nextOr_ <- function(or) {  # non-recycling, non-chunking
              if (i >= count) {
                or
              } else {
                i <<- i + 1
                .(output(i))
              }
            }
          }
        } else {
          nextOr_ <- function(or) { # infinite, non-chunking
            i <<- i + 1
            .(output(i))
          }
        }
      } else { # chunking by chunkSize
        ..(preamble_chunk)
        chunkSize <- floor(chunkSize)
        if (is.finite(count)) {
          last <- count - chunkSize
          nextOr_ <- function(or) {  # finite, chunking by chunkSize
            i.tmp <- i
            if (i >= last) {
              chunkSize <- count - i
              if (recycle) {
                i[1] <<- 0
              } else {
                i[1] <<- Inf
              }
              if (chunkSize < 1) return(or)
            } else {
              i[1] <<- i[1] + chunkSize
            }
            .(output_chunk(i.tmp, chunkSize))
          }
        } else {
          nextOr_ <- function(or) {  # infinite, chunking by chunkSize
            i.tmp <- i
            i[1] <<- i[1] + chunkSize
            .(output_chunk(i.tmp, chunkSize))
          }
        }
      }
    } else { # chunking by no. chunks
      ..(preamble_chunk)
      chunks <- as.integer(chunks)
      chunksLeft <- chunks
      nextOr_ <- function(or) { # chunking by no. chunks
        repeat {
          if (chunksLeft <= 0L) {
            if (recycle) {
              chunksLeft <<- chunks
              i[1] <<- 0L
            } else {
              return(or)
            }
          }
          thisChunk <- as.integer(ceiling((count - i)/chunksLeft))
          chunksLeft <<- chunksLeft - 1L
          if (thisChunk==0) next
          i.tmp <- i
          i[1] <<- i[1] + thisChunk
          return(.(output_chunk(i.tmp, thisChunk)))
        }
      }
    }

    iteror_internal(nextOr_)
  })

  eval(call("function", args, body), parent.env(environment()))
}
