#' Convert a Word File to Markdown with Optional Embedded Images
#'
#' This function converts a Word (`.docx`) file to Markdown using Pandoc. Optionally, it embeds images as Base64 for a self-contained Markdown file.
#'
#' @param docx_file Path to the input Word file.
#' @param output_file Path to the final Markdown output file. If null, the original file name.
#' @param embed_images Logical. If `TRUE`, all images will be embedded as Base64. Default is `FALSE`.
#' @param overwrite Logical. If `TRUE`, Output file is overwrote. Default is `FALSE`
#'
#' @return Saves a Markdown file (optionally with Base64-embedded images).
#' @export
#'
#' @examples
#' # Convert Word to Markdown without embedding images
#' \dontrun{
#' word_to_md("input.docx", "output.md", embed_images = FALSE)
#'
#' # Convert and embed images as Base64
#' word_to_md("input.docx", "output_embedded.md", embed_images = TRUE)
#' }
word_to_md <- function(docx_file, output_file = NULL, embed_images = FALSE, overwrite = FALSE) {

    # Check if Pandoc is available
    if (!rmarkdown::pandoc_available()) {
        stop("Pandoc is not available. Please install Pandoc to proceed.")
    }

    # Check if the input Word file exists
    if (!file.exists(docx_file)) {
        stop("Word file not found: ", docx_file)
    }

    if (is.null(output_file)) {
        output_file <- paste0(tools::file_path_sans_ext(docx_file), ".md")
    }

    # Check if the ouput Word file exists
    if (file.exists(output_file) && !overwrite) {
        stop("Output file is existed: ", output_file)
    }


    # Create a temporary directory
    temp_dir <- tempfile()
    dir.create(temp_dir)
    
    # Ensure cleanup on exit
    on.exit({
        if (dir.exists(temp_dir)) {
            unlink(temp_dir, recursive = TRUE)
        }
    }, add = TRUE)

    # Define paths
    temp_md_file <- file.path(temp_dir, "output.md")
    media_dir <- file.path(temp_dir, "media")

    # Run Pandoc to convert DOCX to Markdown and extract images
    pandoc_cmd <- paste(
        paste0('"', rmarkdown::pandoc_exec(), '"'),
        shQuote(docx_file), "-o", shQuote(temp_md_file),
        "--wrap=none --to=gfm --extract-media", shQuote(media_dir)
    )
    system(pandoc_cmd, wait = TRUE)

    # If image embedding is not required, just move the Markdown file
    if (!embed_images) {
        file.rename(temp_md_file, output_file)
        message("Markdown file saved as '", output_file, "'.")
        # Clean up temporary directory
        unlink(temp_dir, recursive = TRUE)
        return(invisible(NULL))
    }

    # Read the Markdown content
    md_content <- readLines(temp_md_file, warn = FALSE)

    pattern <- c("<img[^>]+src=[\"']([^\"']+)[\"']")
    j <- 1
    for (j in seq(along = pattern)) {
        # Find all image links
        img_pos <- grep(pattern[j], md_content)
        # Process each image
        i <- 1
        for (i in seq(along = img_pos)) {

            full_img_path <- stringr::str_match_all(md_content[img_pos[i]], pattern[j])[[1]]
            full_img_path <- unlist(full_img_path[,2])
            k <- 1
            for (k in seq(along = full_img_path)) {

                if (!file.exists(full_img_path[k])) {
                    stop("Cannot find image ", full_img_path[k])
                }
                # Encode image as Base64
                img_base64 <- base64enc::base64encode(full_img_path[k])

                # Get MIME type
                ext <- tolower(tools::file_ext(full_img_path[k]))
                mime_type <- switch(ext,
                                    "png" = "image/png",
                                    "jpg" = "image/jpeg",
                                    "jpeg" = "image/jpeg",
                                    "gif" = "image/gif",
                                    "bmp" = "image/bmp",
                                    "svg" = "image/svg+xml",
                                    "application/octet-stream")  # Default for unknown types

                # Create Base64 Markdown format
                base64_str <- paste0("data:", mime_type, ";base64,", img_base64)

                # Replace the image path in Markdown
                md_content[img_pos[i]] <- gsub(full_img_path[k], base64_str, md_content[img_pos[i]], fixed = TRUE)
            }
        }
    }
    # Save the final Markdown file with embedded images
    writeLines(md_content, output_file)
    message("Self-contained Markdown file saved as '", output_file, "'.")
    
    # Clean up temporary directory
    unlink(temp_dir, recursive = TRUE)
}
