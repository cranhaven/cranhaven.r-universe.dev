#
# # Change tiddlers in a single file
#
# file <- "empty.html"
#
# single_file <- function(file) {
#     # tw_html <- rvest::read_html(file)
#     #
#     # tiddler_node <- rvest::html_element(tw_html, xpath = "//script[contains(@class, 'tiddlywiki-tiddler-store')]")
#     # tiddler_json <- rvest::html_text(tiddler_node)
#     # tiddler_json <- jsonlite::fromJSON(tiddler_json, simplifyVector = FALSE,
#     #                                    simplifyDataFrame = FALSE,
#     #                                    simplifyMatrix = FALSE)
#     # tiddler_json[[7]]
#     html <- readLines(file)
#
#     pattern <- '^\\{.*"title":"(?!\\$:/).*".*$'
#     pos <- grepl(pattern, html, perl = TRUE)
#     html[pos]
# }
#
#
# json_find_tiddler <- function(html, title) {
#     pattern <- sprintf('^\\{.*"title":"(?!\\$:/)%s".*$', title)
#     pos <- grepl(pattern, html, perl = TRUE)
#     html[pos]
# }
#
# all_tiddlers <- function(file) {
#     html <- readLines(file)
#     pattern <- '^\\{.*"title":"(?!\\$:/).*".*$'
#     pos <- grepl(pattern, html, perl = TRUE)
#     html[pos]
# }
#
# json_insert_tiddler <- function(html, tiddler) {
#
# }
