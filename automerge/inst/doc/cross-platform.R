## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE # Set to FALSE since examples require Node.js and filesystem operations
)

## -----------------------------------------------------------------------------
# library(automerge)
# 
# # Load the document created in JavaScript
# doc_bytes <- readBin("shared_doc.automerge", "raw", 1e7)
# doc <- am_load(doc_bytes)
# 
# # Examine the document
# print(doc)
# 
# # Access data created in JavaScript
# cat("Title:", doc[["title"]], "\n")
# cat("Created by:", doc[["metadata"]][["created_by"]], "\n")
# 
# # Show datasets
# datasets <- doc[["datasets"]]
# cat("Number of datasets:", am_length(doc, datasets), "\n")
# 
# # Examine first dataset (R uses 1-based indexing)
# dataset1 <- am_get(doc, datasets, 1)
# cat(
#   "First dataset:",
#   am_get(doc, dataset1, "name"),
#   "with",
#   am_get(doc, dataset1, "rows"),
#   "rows\n"
# )

## -----------------------------------------------------------------------------
# # Continue from previous example
# # Add analysis results from R
# am_put(
#   doc,
#   AM_ROOT,
#   "r_analysis",
#   list(
#     performed_by = "R",
#     timestamp = Sys.time(),
#     R_version = paste(R.version$major, R.version$minor, sep = "."),
#     summary_stats = list(
#       mean_sales = 45231.5,
#       median_sales = 38900.0,
#       total_customers = 5000L
#     )
#   )
# )
# 
# # Commit changes
# am_commit(doc, "Added R analysis results")
# 
# # Save back to file
# writeBin(am_save(doc), "shared_doc.automerge")
# 
# cat("Document updated by R and saved\n")
# cat("R Actor ID:", am_get_actor_hex(doc), "\n")

## -----------------------------------------------------------------------------
# # Initial R document
# r_doc <- am_create() |>
#   am_put(AM_ROOT, "source", "R") |>
#   am_put(
#     AM_ROOT,
#     "data",
#     list(
#       r_value = 123,
#       timestamp = Sys.time()
#     )
#   ) |>
#   am_commit("Initial R doc")
# 
# # Create sync state
# r_sync <- am_sync_state()
# 
# # Generate sync message to send to JavaScript
# sync_msg_to_js <- am_sync_encode(r_doc, r_sync)
# 
# # Save sync message to file (in practice, send over network)
# writeBin(sync_msg_to_js, "r_to_js_sync.bin")
# 
# cat("R sync message ready:", length(sync_msg_to_js), "bytes\n")

## -----------------------------------------------------------------------------
# # Load sync message from JavaScript
# sync_msg_from_js <- readBin("js_to_r_sync.bin", "raw", 1e7)
# 
# # Apply sync message
# am_sync_decode(r_doc, r_sync, sync_msg_from_js)
# 
# # Documents are now synchronized
# cat("Sync complete!\n")
# cat("R document now contains:\n")
# print(names(r_doc))
# 
# # Verify we have data from JavaScript
# if (!is.null(r_doc[["data"]][["js_value"]])) {
#   cat("JavaScript value:", r_doc[["data"]][["js_value"]], "\n")
# }

## -----------------------------------------------------------------------------
# # Create a shared document
# shared <- am_create() |>
#   am_put(AM_ROOT, "document", "Shared Document") |>
#   am_put(AM_ROOT, "sections", am_list()) |>
#   am_commit("Initialize document")
# 
# # Save for both platforms
# shared_bytes <- am_save(shared)
# writeBin(shared_bytes, "concurrent_doc.automerge")

## -----------------------------------------------------------------------------
# # Load the same original document
# r_doc <- am_load(shared_bytes)
# 
# # R makes different changes to the same document
# sections <- r_doc[["sections"]]
# am_insert(
#   r_doc,
#   sections,
#   1,
#   list(
#     title = "R Statistical Analysis",
#     content = "Regression model results",
#     author = "R Team"
#   )
# )
# 
# am_put(r_doc, AM_ROOT, "r_edit_time", Sys.time())
# am_commit(r_doc, "Add R section")
# 
# # Save R changes
# writeBin(am_save(r_doc), "r_concurrent.automerge")

## -----------------------------------------------------------------------------
# # Load JavaScript version
# js_doc_bytes <- readBin("js_concurrent.automerge", "raw", 1e7)
# js_doc <- am_load(js_doc_bytes)
# 
# # Merge JavaScript changes into R document
# am_merge(r_doc, js_doc)
# 
# # Verify merge - should have both sections
# sections_merged <- r_doc[["sections"]]
# cat(
#   "After merge, document has",
#   am_length(r_doc, sections_merged),
#   "sections\n"
# )
# 
# # Section 1 (from R)
# section1 <- am_get(r_doc, sections_merged, 1)
# cat("Section 1:", am_get(r_doc, section1, "title"), "\n")
# 
# # Section 2 (from JavaScript)
# section2 <- am_get(r_doc, sections_merged, 2)
# cat("Section 2:", am_get(r_doc, section2, "title"), "\n")
# 
# # Both timestamps preserved
# cat("R edit time:", r_doc[["r_edit_time"]], "\n")
# cat("JS edit time:", r_doc[["js_edit_time"]], "\n")

## -----------------------------------------------------------------------------
# # Load text document
# text_doc <- am_load(readBin("text_doc.automerge", "raw", 1e7))
# 
# # Get text object
# notes <- am_get(text_doc, AM_ROOT, "notes")
# 
# # Append text in R (0-based position indexing)
# current_length <- am_length(text_doc, notes)
# am_text_splice(notes, current_length, 0, " and R!")
# am_commit(text_doc, "R appended text")
# 
# # Get full text
# full_text <- am_text_content(notes)
# cat("Text after R edit:", full_text, "\n")
# # Output: "Hello from JavaScript and R!"
# 
# # Save back
# writeBin(am_save(text_doc), "text_doc.automerge")

## -----------------------------------------------------------------------------
# # Ensure UTF-8 encoding when reading from files
# doc <- am_load(readBin("doc.automerge", "raw", 1e7))
# 
# # Check string encoding
# str_value <- doc[["string_field"]]
# Encoding(str_value) # Should be "UTF-8"

## -----------------------------------------------------------------------------
# # R - specify actor ID as raw bytes or hex string
# doc <- am_create(actor_id = "r-session-123")

## -----------------------------------------------------------------------------
# # Lists: R uses 1-based indexing
# list_obj <- doc[["items"]]
# first_item <- am_get(doc, list_obj, 1) # First element
# 
# # Text operations: 0-based positions (same as JavaScript)
# text_obj <- doc[["content"]]
# am_text_splice(text_obj, 0, 0, "Start") # Position 0 = before first char

