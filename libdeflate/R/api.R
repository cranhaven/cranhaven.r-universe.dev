#' Allocate a libdeflate compressor
#'
#' Create a new libdeflate compressor at the specified compression level.
#'
#' @param level Default `6L`. Integer in [0, 12] giving the compression level (0 = no compression, 1 = fastest, 6 = default, 12 = slowest).
#' @return An external pointer (`externalptr`) to a libdeflate compressor.
#' @export
#' @examples
#' # allocate a compressor and compress a simple string
#' cmp = alloc_compressor()
#' raw_in = charToRaw("Example data")
#' raw_cmp = deflate_compress(cmp, raw_in)
#' stopifnot(is.raw(raw_cmp))
alloc_compressor = function(level = 6L) {
  stopifnot(level <= 12 && level >= 0L)
  .Call("C_alloc_compressor", as.integer(level), PACKAGE = "libdeflate")
}

#' Compress a raw vector with libdeflate
#'
#' Compress the given raw vector using the specified libdeflate compressor.
#'
#' @param compressor An external pointer created by `alloc_compressor()`.
#' @param input A raw vector (or object coercible to raw) containing the data to compress.
#' @return A raw vector containing the DEFLATE‐compressed output.
#' @export
#' @examples
#' # Low compression values might not compress at all
#' cmp = alloc_compressor(1L)
#' raw_in = charToRaw("Fast compression test: 1231231231231231")
#' raw_cmp_1 = deflate_compress(cmp, raw_in)
#' print(sprintf("Length in: %i Length out: %i", length(raw_in), length(raw_cmp_1) ))
#' # Max compression is 12
#' cmp = alloc_compressor(12L)
#' raw_cmp_12 = deflate_compress(cmp, raw_in)
#' print(sprintf("Length in: %i Length out: %i", length(raw_in), length(raw_cmp_12) ))
deflate_compress = function(compressor, input) {
  .Call(
    "C_deflate_compress",
    compressor,
    as.raw(input),
    PACKAGE = "libdeflate"
  )
}

#' Allocate a libdeflate decompressor
#'
#' Create a new libdeflate decompressor for raw DEFLATE streams.
#'
#' @return An external pointer (`externalptr`) to a libdeflate decompressor.
#' @export
#' @examples
#' dcmp = alloc_decompressor()
#' stopifnot(inherits(dcmp, "externalptr"))
alloc_decompressor = function() {
  .Call("C_alloc_decompressor", PACKAGE = "libdeflate")
}

#' Decompress a raw vector with libdeflate
#'
#' Decompress a raw DEFLATE stream to its original length.
#'
#' @param decompressor An external pointer created by `alloc_decompressor()`.
#' @param input A raw vector containing the compressed DEFLATE stream.
#' @param out_len Integer giving the expected uncompressed length (in bytes).
#' @return A raw vector of length `out_len` containing the decompressed data.
#' @export
#' @examples
#' # round-trip example
#' msg = "Round-trip test: 123123123123"
#' raw_in = charToRaw(msg)
#' cmp = alloc_compressor(12L)
#' raw_cmp = deflate_compress(cmp, raw_in)
#' dcmp = alloc_decompressor()
#' raw_out = deflate_decompress(dcmp, raw_cmp, length(raw_in))
#' stopifnot(identical(raw_out, raw_in))
deflate_decompress = function(decompressor, input, out_len) {
  .Call(
    "C_deflate_decompress",
    decompressor,
    as.raw(input),
    as.integer(out_len),
    PACKAGE = "libdeflate"
  )
}
