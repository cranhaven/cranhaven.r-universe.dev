## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)

## -----------------------------------------------------------------------------
# library(querychat)
# library(palmerpenguins)

## -----------------------------------------------------------------------------
# qc <- querychat(
#   penguins,
#   greeting = "greeting.md"
# )
# qc$app()  # Launch the app

## -----------------------------------------------------------------------------
# library(querychat)
# 
# # Create QueryChat object with your dataset
# qc <- querychat(penguins)
# 
# # Generate a greeting (this calls the LLM)
# greeting_text <- qc$generate_greeting(echo = "text")
# #> Hello! I'm here to help you explore and analyze the penguins dataset.
# #> Here are some example prompts you can try:
# #> ...
# 
# # Save it for reuse
# writeLines(greeting_text, "penguins_greeting.md")

## -----------------------------------------------------------------------------
# # Then use the saved greeting in your app
# querychat_app(
#   penguins,
#   greeting = "penguins_greeting.md"
# )

## -----------------------------------------------------------------------------
# qc <- QueryChat$new(orders, "orders")             # included automatically
# qc$add_table(customers, "customers")              # not included by default
# qc$add_table(products, "products", include_in_greeting = TRUE)  # opted in
# 
# qc$greeter$tables
# #> [1] "orders"   "products"

## -----------------------------------------------------------------------------
# qc$add_tables(con, include_in_greeting = c("orders", "customers"))

## -----------------------------------------------------------------------------
# qc$greeter$tables <- c("orders", "customers")
# qc$greeter$prompt <- "my-greeting-template.md"

