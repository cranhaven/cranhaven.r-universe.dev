## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)

## ----eval = FALSE-------------------------------------------------------------
# install.packages(c("ellmer", "ragnar", "shiny", "bslib", "shinychat", "jsonlite"))

## -----------------------------------------------------------------------------
# library(PRA)
# cat(PRA:::format_help_overview())

## -----------------------------------------------------------------------------
# cat(PRA:::format_command_help("mcs", PRA:::pra_command_registry()$mcs))

## ----eval = TRUE--------------------------------------------------------------
set.seed(42)
r <- PRA:::execute_command(
  '/mcs n=10000 tasks=[{"type":"normal","mean":10,"sd":2},{"type":"triangular","a":5,"b":10,"c":15},{"type":"uniform","min":8,"max":12}]'
)
cat(r$result)

## ----eval = TRUE, fig.width = 7, fig.height = 4.5, out.width = "100%"---------
# The /mcs command stores results for chaining — visualize them:
result <- PRA:::.pra_agent_env$last_mcs
hist(result$total_distribution,
  freq = FALSE, breaks = 50,
  main = "Monte Carlo Simulation Results",
  xlab = "Total Project Duration/Cost",
  col = "#18bc9c80", border = "white"
)
curve(dnorm(x, mean = result$total_mean, sd = result$total_sd),
  add = TRUE, col = "#2c3e50", lwd = 2
)
abline(
  v = quantile(result$total_distribution, c(0.50, 0.95)),
  col = c("#3498db", "#e74c3c"), lty = 2, lwd = 1.5
)
legend("topright",
  legend = c("Normal fit", "P50", "P95"),
  col = c("#2c3e50", "#3498db", "#e74c3c"),
  lty = c(1, 2, 2), lwd = c(2, 1.5, 1.5),
  cex = 0.8, bg = "white"
)

## ----eval = TRUE--------------------------------------------------------------
r <- PRA:::execute_command("/contingency phigh=0.95 pbase=0.50")
cat(r$result)

## ----eval = TRUE--------------------------------------------------------------
r <- PRA:::execute_command(
  '/sensitivity tasks=[{"type":"normal","mean":10,"sd":2},{"type":"triangular","a":5,"b":10,"c":15},{"type":"uniform","min":8,"max":12}]'
)
cat(r$result)

## ----eval = TRUE--------------------------------------------------------------
r <- PRA:::execute_command(
  "/evm bac=500000 schedule=[0.2,0.4,0.6,0.8,1.0] period=3 complete=0.35 costs=[90000,195000,310000]"
)
cat(r$result)

## ----eval = TRUE--------------------------------------------------------------
r <- PRA:::execute_command(
  "/risk causes=[0.3,0.2] given=[0.8,0.6] not_given=[0.2,0.4]"
)
cat(r$result)

## ----eval = TRUE--------------------------------------------------------------
r <- PRA:::execute_command(
  "/risk_post causes=[0.3,0.2] given=[0.8,0.6] not_given=[0.2,0.4] observed=[1,null]"
)
cat(r$result)

## ----eval = TRUE--------------------------------------------------------------
r <- PRA:::execute_command("/smm means=[10,12,8] vars=[4,9,2]")
cat(r$result)

## ----eval = TRUE--------------------------------------------------------------
# Missing required arguments
r <- PRA:::execute_command("/risk causes=[0.3]")
cat(r$result)

## ----eval = TRUE--------------------------------------------------------------
# Unknown command
r <- PRA:::execute_command("/simulate")
cat(r$result)

## -----------------------------------------------------------------------------
# library(PRA)
# chat <- pra_chat(model = "llama3.2")
# 
# # Tool call: user provides numerical data
# chat$chat("Run a Monte Carlo simulation for a 3-task project with
#   Task A ~ Normal(10, 2), Task B ~ Triangular(5, 10, 15),
#   Task C ~ Uniform(8, 12). Use 10,000 simulations.")
# 
# # RAG: conceptual question, no computation needed
# chat$chat("What is the difference between SPI and CPI?")

## -----------------------------------------------------------------------------
# # OpenAI
# chat <- pra_chat(chat = ellmer::chat_openai(model = "gpt-4o"))
# 
# # Anthropic
# chat <- pra_chat(chat = ellmer::chat_anthropic(model = "claude-sonnet-4-20250514"))

## -----------------------------------------------------------------------------
# pra_app()

## ----echo = FALSE, eval = TRUE, out.width = "100%"----------------------------
knitr::include_graphics("../man/figures/pra-app-welcome.png")

## ----echo = FALSE, eval = TRUE, out.width = "100%"----------------------------
knitr::include_graphics("../man/figures/pra-app-tool-call.png")

## -----------------------------------------------------------------------------
# # Custom model and port
# pra_app(model = "qwen2.5", port = 3838)
# 
# # Disable RAG for faster responses
# pra_app(rag = FALSE)

## -----------------------------------------------------------------------------
# store <- build_knowledge_base()
# 
# # Add a single file
# add_documents(store, "path/to/my_risk_register.md")
# 
# # Add all .md and .txt files in a directory
# add_documents(store, "path/to/project_docs/")

## -----------------------------------------------------------------------------
# # Chat without RAG
# chat <- pra_chat(model = "llama3.2", rag = FALSE)
# 
# # App without RAG
# pra_app(rag = FALSE)

## -----------------------------------------------------------------------------
# # Run evaluation
# source(system.file("eval/pra_eval.R", package = "PRA"))
# results <- run_pra_eval(model = "llama3.2")
# 
# # Compare models
# comparison <- run_pra_comparison(
#   models = c("llama3.2", "qwen2.5"),
#   rag_options = c(TRUE, FALSE)
# )

## -----------------------------------------------------------------------------
# # Instead of asking the LLM:
# chat$chat("Run a Monte Carlo simulation...")
# 
# # Use the /command directly in the app:
# # /mcs tasks=[{"type":"normal","mean":10,"sd":2}]

## -----------------------------------------------------------------------------
# install.packages("ragnar")

