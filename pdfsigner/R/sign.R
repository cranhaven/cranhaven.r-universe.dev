#' pdfsigner: native PDF signing and verification
#'
#' Digitally sign PDF documents with a PKCS#12 keystore and verify their
#' signatures. The heavy lifting is done by a bundled, pure-Rust backend
#' (the `pdf_signer` crate) — no Java, OpenSSL, or external command-line tools
#' are required.
#'
#' @keywords internal
"_PACKAGE"

`%||%` <- function(a, b) if (is.null(a)) b else a

#' Digitally sign a PDF document
#'
#' Signs `pdf_file` using an RSA key + certificate stored in a PKCS#12
#' (`.p12`/`.pfx`) keystore, writing the signed document to `output_file`. The
#' signature is a detached `adbe.pkcs7.detached` CMS over the whole document and
#' is added as an incremental update, so any pre-existing signatures stay valid.
#'
#' @param pdf_file Path to the input PDF.
#' @param output_file Path where the signed PDF is written.
#' @param keystore_path Path to the `.p12`/`.pfx` keystore. Defaults to the
#'   `KEYSTORE_PATH` environment variable.
#' @param keystore_password Password for the keystore. Defaults to the
#'   `KEY_PASSWORD` environment variable.
#' @param signtext Optional text for a *visible* signature box. When `NULL` or
#'   empty the signature is invisible.
#' @param validate_link Optional validation URL appended to the visible box.
#' @param reason,signer_name Optional `/Reason` and `/Name` for the signature
#'   dictionary.
#' @param page 1-based page number for the visible box.
#' @param x,y,width,height Visible box geometry, in PDF points (origin at the
#'   page's bottom-left).
#' @param font_size Font size for the visible box, in points.
#' @param font Optional path to a TrueType/OpenType font file (`.ttf`/`.otf`) to
#'   embed in the visible box. When `NULL`, the standard Helvetica is used. Only
#'   the WinAnsi (Latin-1) glyph range is embedded. Ignored for invisible
#'   signatures.
#' @param image Optional path to a PNG or JPEG logo drawn in the visible box.
#'   Ignored for invisible signatures.
#' @param border Draw a border around the visible box.
#' @param translate If `TRUE`, the date label in the visible box is in
#'   Portuguese; otherwise English.
#' @param tsa_url Optional RFC 3161 Time-Stamping Authority `http://` URL.
#'   Required for `pades_level` `"bt"` and above. Requires network access.
#' @param pades_level PAdES conformance level: `"bb"` (baseline, default),
#'   `"bt"` (+ signature timestamp), `"blt"` (+ DSS with certificates and CRLs),
#'   or `"blta"` (+ a document timestamp over the whole file). Levels `"bt"` and
#'   above need `tsa_url`.
#'
#' @return Invisibly, the path to the signed PDF. Raises an error on failure.
#' @examples
#' \dontrun{
#' sign_pdf(
#'   pdf_file = "input.pdf",
#'   output_file = "signed.pdf",
#'   keystore_path = "keystore.p12",
#'   keystore_password = "password",
#'   signtext = "Document digitally signed by CastLab",
#'   validate_link = "https://castlab.org/validate",
#'   translate = TRUE
#' )
#' }
#' @export
sign_pdf <- function(pdf_file, output_file,
                     keystore_path = Sys.getenv("KEYSTORE_PATH"),
                     keystore_password = Sys.getenv("KEY_PASSWORD"),
                     signtext = NULL, validate_link = NULL,
                     reason = NULL, signer_name = NULL,
                     page = 1,
                     x = 36, y = 36, width = 320, height = 64, font_size = 8,
                     font = NULL, image = NULL,
                     border = TRUE, translate = FALSE, tsa_url = NULL,
                     pades_level = c("bb", "bt", "blt", "blta")) {
  pades_level <- match.arg(pades_level)
  if (pades_level != "bb" && (is.null(tsa_url) || !nzchar(tsa_url))) {
    stop("pades_level '", pades_level, "' requires a `tsa_url`.")
  }

  if (!file.exists(pdf_file)) {
    stop("The specified PDF file does not exist: ", pdf_file)
  }
  if (!dir.exists(dirname(output_file))) {
    stop("The output directory does not exist: ", dirname(output_file))
  }
  if (!file.exists(keystore_path)) {
    stop("The specified keystore does not exist: ", keystore_path)
  }
  if (!nzchar(keystore_password)) {
    stop("The keystore password cannot be empty.")
  }
  if (!is.numeric(page) || page < 1) {
    stop("`page` must be a positive number.")
  }
  if (!is.null(font) && !file.exists(font)) {
    stop("The specified font file does not exist: ", font)
  }
  if (!is.null(image) && !file.exists(image)) {
    stop("The specified image file does not exist: ", image)
  }

  visible <- !is.null(signtext) && nzchar(signtext)
  appearance_text <- ""
  if (visible) {
    fmt <- if (translate) "%A, %d de %B de %Y, %H:%M:%S" else "%A, %d %B %Y, %H:%M:%S"
    now <- format(Sys.time(), fmt)
    date_line <- if (translate) paste0("Data e hora: ", now) else paste0("Date and time: ", now)
    appearance_text <- paste(signtext, date_line, sep = "\n")
    if (!is.null(validate_link) && nzchar(validate_link)) {
      label <- if (translate) "Validar documento em: " else "Validate document at: "
      appearance_text <- paste(appearance_text, paste0(label, validate_link), sep = "\n")
    }
  }

  rust_sign_pdf(
    pdf_file = path.expand(pdf_file),
    output_file = path.expand(output_file),
    keystore_path = path.expand(keystore_path),
    keystore_password = keystore_password,
    reason = reason %||% "",
    name = signer_name %||% "",
    location = "",
    contact_info = "",
    signing_time = "",
    visible = visible,
    page = as.integer(page),
    x = as.numeric(x), y = as.numeric(y),
    width = as.numeric(width), height = as.numeric(height),
    font_size = as.numeric(font_size),
    appearance_text = appearance_text,
    border = isTRUE(border),
    tsa_url = tsa_url %||% "",
    pades_level = pades_level,
    font_path = if (is.null(font)) "" else path.expand(font),
    image_path = if (is.null(image)) "" else path.expand(image)
  )

  message("PDF successfully signed: ", output_file)
  invisible(output_file)
}

#' Verify the digital signatures of a PDF
#'
#' Cryptographically verifies every signature in `pdf_file` using the bundled
#' Rust backend. Each signature is checked by re-deriving its signed byte range,
#' confirming the `messageDigest` against `SHA-256` of the content and
#' validating the signer's RSA signature over the signed attributes.
#'
#' @param pdf_file Path to the PDF to verify.
#' @param roots Optional path to a PEM file of trusted root certificates (e.g.
#'   the ICP-Brasil AC Raiz set). When supplied, each signer certificate chain
#'   is validated against these roots and reported in `chain_trusted`.
#'
#' @return A list with one entry per signature. Each entry is a named list with
#'   `valid` (logical), `signer` (subject DN), `chain_trusted` (logical or `NA`
#'   when no `roots` given), `covers_whole_document` (logical), `signed_len`
#'   (bytes), `byte_range` (numeric length-4) and `detail`. A length-zero list
#'   means no signatures were found.
#' @examples
#' \dontrun{
#' result <- verify_pdf_signature("signed.pdf", roots = "icp-brasil-roots.pem")
#' vapply(result, function(s) s$valid, logical(1))
#' }
#' @export
verify_pdf_signature <- function(pdf_file, roots = NULL) {
  if (!file.exists(pdf_file)) {
    stop("The specified PDF file was not found: ", pdf_file)
  }
  if (!is.null(roots) && !file.exists(roots)) {
    stop("The roots PEM file was not found: ", roots)
  }
  rust_verify_pdf(path.expand(pdf_file), if (is.null(roots)) "" else path.expand(roots))
}
