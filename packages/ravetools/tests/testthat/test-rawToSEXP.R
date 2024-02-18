library(testthat)

test_that("check conversion values are correct", {

  if(.Platform$endian == "little") {
    rev2 <- rev
  } else {
    rev2 <- function(x) { x }
  }


  bytes <- c("00", "02", "02", "ff", "80", "00", "7f", "80", "ff", "ff", "ff", "ff")
  bytes <- c(bytes, rev(bytes))

  num_ <- eval(parse(text = paste("c(", paste(sprintf("0x%s", bytes), collapse = ", "), ")")))
  x_ <- as.raw(num_)
  bits <- matrix(as.numeric(rawToBits(x_)), nrow = 8L)

  num_16_ <- eval(parse(text = paste("c(", paste(sprintf("0x%s", local({
    dim(bytes) <- c(2, length(bytes) / 2)
    apply(bytes, 2, function(x) { paste(rev2(x), sep = "", collapse = "") })
  })), collapse = ", "), ")")))

  num_32_ <- eval(parse(text = paste("c(", paste(sprintf("0x%s", local({
    dim(bytes) <- c(4, length(bytes) / 4)
    apply(bytes, 2, function(x) { paste(rev2(x), sep = "", collapse = "") })
  })), collapse = ", "), ")")))

  num_64_ <- local({
    dim(x_) <-  c(8, length(x_) / 8)
    bitstrings <- apply(x_, 2, function(x) {
      bits <- rev2(rawToBits(x))
      paste(as.integer(bits), collapse = "")
    })
    bit64::as.integer64(structure(bitstrings, class = "bitstring"))
  })

  float_ <- local({
    dim(bits) <- c(32, length(bits) / 32)
    b <- bits[,2]
    apply(bits, 2, function(b) {
      b <- rev2(b)
      sign <- ifelse(b[1] == 0, 1, -1)
      e <- sum(b[2:9] * 2^(7:0))
      mantissa <- b[-(1:9)]
      if(!length(mantissa)) {
        mantissa <- 0
      } else {
        mantissa <- sum(mantissa * 2^(-seq_along(mantissa)))
      }
      if(e == 0) {
        # denormalization
        e <- -126
      } else {
        e <- e - 127
        mantissa <- mantissa + 1L
      }
      if(e == 128) {
        if(mantissa == 0) {
          return(sign * Inf)
        } else {
          return(NaN)
        }
      }
      sign * mantissa * 2^e
    })
  })

  for(len in seq_along(num_)) {

    num <- num_[seq_len(len)]

    x <- x_[seq_len(len)]

    expect_equal(rawToUInt8(x), num)
    expect_equal(rawToInt8(x), local({
      num[num > 127] <- num[num > 127] - 256L
      num
    }))

    if(len %% 2 != 0) {
      expect_error(rawToUInt16(x))
      expect_error(rawToInt16(x))
    } else {
      num_16 <- num_16_[seq_len(floor(len/2))]
      expect_equal(rawToUInt16(x), num_16)
      expect_equal(rawToInt16(x), local({
        num_16[num_16 >= 2L^15L] <- num_16[num_16 >= 2L^15L] - 2L^16L
        num_16
      }))
    }

    if(len %% 4 != 0) {
      expect_error(rawToUInt32(x))
      expect_error(rawToInt32(x))
      expect_error(rawToFloat(x))
    } else {

      num_32 <- num_32_[seq_len(floor(len/4))]
      float <- float_[seq_len(floor(len/4))]

      expect_equal(rawToUInt32(x), num_32)
      expect_equal(rawToInt32(x), local({
        num_32[num_32 >= 2L^31L] <- num_32[num_32 >= 2L^31L] - 2L^32L
        num_32
      }))
      expect_equal(rawToFloat(x), float)
    }

    if(len %% 8 != 0) {
      expect_error(rawToInt64(x))
    } else {
      num_64 <- num_64_[seq_len(floor(len/8))]
      expect_equal(rawToInt64(x), num_64)
    }

  }




})
