## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5
)

# Check for required packages - vignette needs visualization packages
has_viz <- requireNamespace("ggraph", quietly = TRUE) &&
           requireNamespace("tidygraph", quietly = TRUE) &&
           requireNamespace("ggplot2", quietly = TRUE)

if (!has_viz) {
  knitr::opts_chunk$set(eval = FALSE)
  message("This vignette requires ggraph, tidygraph, and ggplot2 packages for visualizations.")
}

library(couplr)
library(tibble)

# Solve times from the package's solver benchmark: uniform integer costs on
# square matrices, the fastest of five interleaved repetitions per solver and
# size, in milliseconds.
solver_bench <- read.csv(
  system.file("extdata", "solver-benchmark.csv", package = "couplr"),
  stringsAsFactors = FALSE
)
bench_ms <- function(method, n) {
  solver_bench$min_ms[solver_bench$method == method & solver_bench$n == n]
}
bench_ratio <- function(slow, fast, n) bench_ms(slow, n) / bench_ms(fast, n)
race_n <- 500
race_methods <- c(munkres = "Munkres", network_simplex = "Network Simplex",
                  auction = "Auction", csa = "CSA", hungarian = "Hungarian",
                  jv = "Jonker-Volgenant")
race_ms <- vapply(names(race_methods), bench_ms, numeric(1), n = race_n)
if (has_viz) {
  library(ggplot2)
  library(ggraph)
  library(tidygraph)
}

# Color palette - professional, accessible
col_worker <- "#E07B39"    # Warm orange for workers/left nodes
col_job <- "#3D8EAF"       # Cool blue for jobs/right nodes
col_optimal <- "#5DD65D"   # Bright green for optimal/selected
col_nonopt <- "#B8B8B8"    # Gray for non-selected
col_highlight <- "#E74C3C" # Red for highlighting/current
col_text <- "#3E3F3A"      # Dark text (theme-aware)

# Theme for network diagrams
theme_graph_clean <- function() {
  theme_graph(base_family = "") +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 13,
                                margin = margin(b = 8), color = col_text),
      plot.subtitle = element_text(hjust = 0.5, size = 10,
                                   margin = margin(b = 20), color = col_text),
      plot.margin = margin(20, 10, 10, 10),
      legend.position = "bottom",
      legend.title = element_text(size = 9, color = col_text),
      legend.text = element_text(size = 8, color = col_text),
      legend.box = "horizontal",
      legend.spacing.x = unit(0.3, "cm")
    )
}

# Theme for non-network diagrams (bar charts, progressions)
theme_diagram <- function() {
  theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 13,
                                margin = margin(b = 4), color = col_text),
      plot.subtitle = element_text(hjust = 0.5, size = 10,
                                   margin = margin(b = 8), color = col_text),
      plot.margin = margin(10, 10, 10, 10),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank()
    )
}

## ----the-race, echo=FALSE, fig.width=9, fig.height=5, fig.alt="Six algorithms solving the same 500 by 500 assignment problem, from Munkres, the slowest, to Jonker-Volgenant, the fastest"----
race_data <- data.frame(
  algorithm = factor(unname(race_methods), levels = unname(race_methods)),
  time_ms = unname(race_ms)
)

ggplot(race_data, aes(x = algorithm, y = time_ms, fill = algorithm)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = sprintf("%.1f ms", time_ms)),
            hjust = -0.1, size = 5, fontface = "bold") +
  scale_fill_manual(values = c(
    "Munkres" = "#8c3b00",
    "Hungarian" = "#d9534f",
    "Jonker-Volgenant" = "#5bc0de",
    "Auction" = "#f0ad4e",
    "CSA" = "#5cb85c",
    "Network Simplex" = "#428bca"
  )) +
  scale_y_log10(expand = expansion(mult = c(0, 0.3))) +
  coord_flip(clip = "off") +
  labs(
    title = sprintf("Same Problem, Same Answer, %.0fx Speed Difference",
                    max(race_ms) / min(race_ms)),
    subtitle = sprintf("%d x %d uniform integer costs, fastest of 5 runs, log scale",
                       race_n, race_n),
    x = NULL,
    y = "Time (milliseconds)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11),
    axis.text.y = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 10),
    panel.grid = element_blank(),
    plot.margin = margin(10, 60, 10, 10)
  )

## ----bipartite-graph, fig.width=7, fig.height=4.5, echo=FALSE, fig.alt="Bipartite graph showing workers on left, jobs on right, with weighted edges and optimal assignment highlighted"----
# Build graph with tidygraph
nodes <- tibble(
  name = c("W1", "W2", "W3", "J1", "J2", "J3"),
  type = c(rep("Worker", 3), rep("Job", 3)),
  side = c(rep("left", 3), rep("right", 3))
)

edges <- tibble(
  from = c(1, 1, 2, 2, 3, 3),
  to = c(4, 5, 5, 4, 4, 6),
  cost = c(2, 4, 1, 3, 3, 2),
  optimal = c(TRUE, FALSE, TRUE, FALSE, FALSE, TRUE)
)

g <- tbl_graph(nodes = nodes, edges = edges, directed = FALSE)

# Manual bipartite layout
layout <- data.frame(
  x = c(0, 0, 0, 2, 2, 2),
  y = c(3, 2, 1, 3, 2, 1)
)

# Compute label positions along edges
edge_label_data <- data.frame(
  from_x = c(0, 0, 0, 0, 0, 0),
  from_y = c(3, 3, 2, 2, 1, 1),
  to_x   = c(2, 2, 2, 2, 2, 2),
  to_y   = c(3, 2, 2, 3, 3, 1),
  cost   = c(2, 4, 1, 3, 3, 2),
  optimal = c(TRUE, FALSE, TRUE, FALSE, FALSE, TRUE),
  pos = c(0.5, 0.25, 0.5, 0.75, 0.25, 0.5)
)

edge_label_data$x <- edge_label_data$from_x + edge_label_data$pos * (edge_label_data$to_x - edge_label_data$from_x)
edge_label_data$y <- edge_label_data$from_y + edge_label_data$pos * (edge_label_data$to_y - edge_label_data$from_y)
edge_label_data$angle <- atan2(edge_label_data$to_y - edge_label_data$from_y,
                                edge_label_data$to_x - edge_label_data$from_x) * 180 / pi
edge_label_data$edge_color <- ifelse(edge_label_data$optimal, col_optimal, col_nonopt)

# Create edge type factor for legend
edges <- edges |>
  mutate(edge_type = factor(ifelse(optimal, "Optimal", "Available"),
                            levels = c("Optimal", "Available")))
g <- tbl_graph(nodes = nodes, edges = edges, directed = FALSE)

ggraph(g, layout = "manual", x = layout$x, y = layout$y) +
  geom_edge_link(aes(edge_colour = edge_type, edge_width = edge_type)) +
  geom_label(data = edge_label_data,
             aes(x = x, y = y, label = cost, angle = angle),
             fill = edge_label_data$edge_color,
             color = "#3E3F3A", fontface = "bold", size = 5.5,
             label.padding = unit(0.2, "lines"),
             linewidth = 0) +
  geom_node_point(aes(fill = type),
                  shape = 21, size = 14, color = "white", stroke = 1.5) +
  geom_node_text(aes(label = name), color = "white", fontface = "bold", size = 4.5) +
  scale_fill_manual(values = c("Worker" = col_worker, "Job" = col_job),
                    name = NULL, guide = guide_legend(override.aes = list(size = 5))) +
  scale_edge_colour_manual(values = c("Optimal" = col_optimal, "Available" = col_nonopt),
                           name = NULL) +
  scale_edge_width_manual(values = c("Optimal" = 1.5, "Available" = 0.8), guide = "none") +
  labs(title = "Assignment as Bipartite Matching",
       subtitle = "Optimal: W1->J1 (2) + W2->J2 (1) + W3->J3 (2) = 5") +
  theme_graph_clean() +
  coord_fixed(clip = "off")

## ----hungarian-diagram, fig.width=7, fig.height=5, echo=FALSE, fig.alt="Hungarian algorithm showing alternating path augmentation through tight edges"----
# Nodes: 4 workers, 4 jobs
nodes <- tibble(
 name = c("W1", "W2", "W3", "W4", "J1", "J2", "J3", "J4"),
 type = c(rep("Worker", 4), rep("Job", 4)),
 state = c("matched", "matched", "seeking", "matched",
           "matched", "matched", "free", "matched")
)

# Edges: matched + tight + augmenting path
edges <- tibble(
 from = c(1, 2, 4,  3, 3, 3, 2),
 to = c(5, 6, 8,    7, 6, 5, 7),
 edge_type = factor(c("Matched", "Matched", "Matched",
                      "Tight", "Augmenting", "Tight", "Augmenting"),
                    levels = c("Matched", "Augmenting", "Tight"))
)

g <- tbl_graph(nodes = nodes, edges = edges, directed = FALSE)

layout <- data.frame(
 x = c(0, 0, 0, 0, 2.5, 2.5, 2.5, 2.5),
 y = c(4, 3, 2, 1, 4, 3, 2, 1)
)

ggraph(g, layout = "manual", x = layout$x, y = layout$y) +
 geom_edge_link(aes(edge_colour = edge_type, edge_width = edge_type,
                    edge_linetype = edge_type)) +
 geom_node_point(aes(fill = ifelse(state == "seeking", "Seeking",
                                   ifelse(state == "free", "Free", type))),
                 shape = 21, size = 12, color = "white", stroke = 1.5) +
 geom_node_text(aes(label = name), color = "white", fontface = "bold", size = 4) +
 scale_fill_manual(values = c("Worker" = col_worker, "Job" = col_job,
                              "Seeking" = col_highlight, "Free" = col_optimal),
                   name = NULL, guide = guide_legend(override.aes = list(size = 4))) +
 scale_edge_colour_manual(values = c("Matched" = col_job, "Augmenting" = col_highlight,
                                     "Tight" = col_nonopt), name = NULL) +
 scale_edge_width_manual(values = c("Matched" = 1.2, "Augmenting" = 1.2, "Tight" = 0.6),
                         guide = "none") +
 scale_edge_linetype_manual(values = c("Matched" = "solid", "Augmenting" = "solid",
                                       "Tight" = "dashed"), guide = "none") +
 labs(title = "Hungarian: Augmenting Path Search",
      subtitle = "W3 (red) finds path to free job J3 (green) via tight edges") +
 theme_graph_clean() +
 coord_fixed(clip = "off")

## ----hungarian-example--------------------------------------------------------
cost <- matrix(c(10, 19, 8, 15, 10, 11, 9, 12, 14), nrow = 3, byrow = TRUE)
result <- lap_solve(cost, method = "hungarian")
print(result)

## ----jv-diagram, fig.width=9, fig.height=4.5, echo=FALSE, fig.alt="JV algorithm showing column reduction initialization followed by shortest path augmentation"----
# JV visualization: cleaner flow diagram
steps <- data.frame(
  step = c("Column\nReduction", "Reduction\nTransfer", "Augmentation"),
  description = c(
    "Greedily assign rows\nto cheapest columns",
    "Handle collisions\nwith dual updates",
    "Shortest-path search\nfor remaining rows"
  ),
  progress = c("85%", "95%", "100%"),
  x = c(1, 2.5, 4),
  phase_color = c(col_job, col_job, col_optimal)
)

ggplot(steps) +
 # Connecting arrows
  annotate("segment", x = 1.55, xend = 1.95, y = 0.5, yend = 0.5,
           arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
           color = col_nonopt, linewidth = 1.5) +
 annotate("segment", x = 3.05, xend = 3.45, y = 0.5, yend = 0.5,
           arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
           color = col_nonopt, linewidth = 1.5) +
  # Phase boxes
  geom_tile(aes(x = x, y = 0.5, fill = phase_color),
            width = 1.1, height = 1.4, color = "white", linewidth = 1.5) +
  # Step titles (dark text for readability on green/blue backgrounds)
  geom_text(aes(x = x, y = 0.85, label = step),
            color = "#3E3F3A", fontface = "bold", size = 4.5, lineheight = 0.85) +
  # Progress indicators
  geom_label(aes(x = x, y = 0.1, label = progress),
             fill = "white", color = col_text, size = 5, fontface = "bold",
             label.padding = unit(0.3, "lines"), linewidth = 0) +
  scale_fill_identity() +
  # Description annotations below
  geom_text(aes(x = x, y = -0.55, label = description),
            size = 3.2, lineheight = 0.9, color = col_text) +
  labs(title = "Jonker-Volgenant: Start Fast, Fix Later",
       subtitle = "Column reduction handles most assignments; Dijkstra-style augmentation finishes the rest") +
  theme_diagram() +
  coord_fixed(ratio = 0.7, xlim = c(0.2, 4.8), ylim = c(-1, 1.4))

## ----jv-example---------------------------------------------------------------
set.seed(123)
n <- 100
cost <- matrix(runif(n * n, 0, 100), n, n)
result <- lap_solve(cost, method = "jv")
cat("Total cost:", round(get_total_cost(result), 2), "\n")

## ----auction-diagram, fig.width=7, fig.height=5, echo=FALSE, fig.alt="Auction algorithm bidding process showing workers bidding for jobs with prices"----
# Nodes - workers and jobs with prices in labels
nodes <- tibble(
  name = c("W1", "W2", "W3", "J1\n$5", "J2\n$3", "J3\n$0"),
  type = c(rep("Worker", 3), rep("Job", 3)),
  state = c("bidding", "matched", "waiting", "target", "matched", "available")
)

# Edges with costs
edges <- tibble(
  from = c(1, 1, 2, 2, 3, 3),
  to = c(4, 5, 5, 4, 4, 6),
  cost = c(7, 4, 5, 6, 8, 3),
  edge_type = factor(c("Bid", "Considering", "Matched", "Considering", "Considering", "Considering"),
                     levels = c("Bid", "Matched", "Considering"))
)

g <- tbl_graph(nodes = nodes, edges = edges, directed = FALSE)

layout <- data.frame(
  x = c(0, 0, 0, 2, 2, 2),
  y = c(3, 2, 1, 3, 2, 1)
)

# Edge label positions
edge_label_data <- data.frame(
  from_x = c(0, 0, 0, 0, 0, 0),
  from_y = c(3, 3, 2, 2, 1, 1),
  to_x   = c(2, 2, 2, 2, 2, 2),
  to_y   = c(3, 2, 2, 3, 3, 1),
  cost   = c(7, 4, 5, 6, 8, 3),
  edge_type = factor(c("Bid", "Considering", "Matched", "Considering", "Considering", "Considering"),
                     levels = c("Bid", "Matched", "Considering")),
  pos = c(0.5, 0.25, 0.5, 0.75, 0.25, 0.5)
)

edge_label_data$x <- edge_label_data$from_x + edge_label_data$pos * (edge_label_data$to_x - edge_label_data$from_x)
edge_label_data$y <- edge_label_data$from_y + edge_label_data$pos * (edge_label_data$to_y - edge_label_data$from_y)
edge_label_data$angle <- atan2(edge_label_data$to_y - edge_label_data$from_y,
                                edge_label_data$to_x - edge_label_data$from_x) * 180 / pi
edge_label_data$edge_color <- ifelse(edge_label_data$edge_type == "Bid", col_highlight,
                                     ifelse(edge_label_data$edge_type == "Matched", col_optimal, col_nonopt))

ggraph(g, layout = "manual", x = layout$x, y = layout$y) +
  geom_edge_link(aes(edge_colour = edge_type, edge_width = edge_type,
                     edge_linetype = edge_type)) +
  geom_label(data = edge_label_data,
             aes(x = x, y = y, label = paste0("$", cost), angle = angle),
             fill = edge_label_data$edge_color,
             color = "#3E3F3A", fontface = "bold", size = 5,
             label.padding = unit(0.2, "lines"), linewidth = 0) +
  geom_node_point(data = \(x) filter(x, type == "Worker"),
                  aes(fill = ifelse(state == "bidding", "Bidding",
                                    ifelse(state == "matched", "Matched", "Worker"))),
                  shape = 21, size = 14, color = "white", stroke = 1.5) +
  geom_node_point(data = \(x) filter(x, type == "Job"),
                  aes(fill = ifelse(state == "matched", "Matched", "Job")),
                  shape = 21, size = 14, color = "white", stroke = 1.5) +
  geom_node_text(aes(label = name), color = "white", fontface = "bold", size = 3.5,
                 lineheight = 0.9) +
  scale_fill_manual(values = c("Worker" = col_worker, "Job" = col_job,
                               "Bidding" = col_highlight, "Matched" = col_optimal),
                    name = NULL, guide = guide_legend(order = 1, override.aes = list(size = 4))) +
  scale_edge_colour_manual(values = c("Bid" = col_highlight, "Matched" = col_optimal,
                                      "Considering" = col_nonopt),
                           name = NULL, guide = guide_legend(order = 2)) +
  scale_edge_width_manual(values = c("Bid" = 1.5, "Matched" = 1.5, "Considering" = 0.8),
                          guide = "none") +
  scale_edge_linetype_manual(values = c("Bid" = "solid", "Matched" = "solid",
                                        "Considering" = "dashed"), guide = "none") +
  labs(title = "Auction: Bidding Phase",
       subtitle = "W1 (red) bids on J1 at price $5; W2 already matched to J2 at $3") +
  theme_graph_clean() +
  coord_fixed(clip = "off")

## ----auction-example----------------------------------------------------------
set.seed(123)
n <- 100
cost <- matrix(runif(n * n, 0, 100), n, n)
result <- lap_solve(cost, method = "auction")
cat("Total cost:", round(get_total_cost(result), 2), "\n")

## ----csa-diagram, fig.width=9, fig.height=4, echo=FALSE, fig.alt="CSA algorithm showing epsilon-scaling phases converging to optimal solution"----
# CSA visualization: epsilon scaling progression
phases <- data.frame(
  phase = 1:5,
  epsilon = c(100, 10, 1, 0.1, 0.01),
  x = 1:5
)

# Gradient of colors from light to dark green
phase_colors <- colorRampPalette(c("#A8D5BA", col_optimal))(5)

ggplot(phases) +
  # Connecting line
  annotate("segment", x = 1, xend = 5, y = 0.5, yend = 0.5,
           linewidth = 3, color = col_optimal) +
  # Phase points with increasing intensity
  geom_point(aes(x = x, y = 0.5), size = c(15, 16, 17, 18, 20),
             color = "white") +
  geom_point(aes(x = x, y = 0.5), size = c(13, 14, 15, 16, 18),
             color = phase_colors) +
  # Epsilon labels inside circles (dark text, same size)
  geom_text(aes(x = x, y = 0.5, label = paste0("e=", epsilon)),
            color = "#3E3F3A", fontface = "bold", size = 3.5) +
  # Phase labels below
  geom_text(aes(x = x, y = 0.15, label = paste0("Phase ", phase)),
            size = 3.5, color = col_text) +
  # Direction arrow
  annotate("segment", x = 1, xend = 5, y = 0.9, yend = 0.9,
           arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
           linewidth = 1, color = col_text) +
  annotate("text", x = 3, y = 0.98, label = "epsilon divided by 10 each phase -> precision improves",
           size = 3.8, color = col_text) +
  labs(title = "CSA: Systematic Epsilon-Scaling",
       subtitle = "Each phase divides epsilon by 10 and refines the assignment") +
  theme_diagram() +
  coord_fixed(ratio = 1.5, xlim = c(0.3, 5.7), ylim = c(0, 1.15))

## ----csa-example--------------------------------------------------------------
set.seed(456)
n <- 100
cost <- matrix(runif(n * n, 0, 100), n, n)
result <- lap_solve(cost, method = "csa")
cat("Total cost:", round(get_total_cost(result), 2), "\n")

## ----gabow-tarjan-diagram, fig.width=9, fig.height=4.5, echo=FALSE, fig.alt="Gabow-Tarjan bit-scaling showing costs processed from high bits to low bits"----
# Bit-scaling visualization
bits <- data.frame(
  bit = 7:0,
  x = 1:8,
  label = c("128", "64", "32", "16", "8", "4", "2", "1"),
  stage = c(rep("done", 4), "current", rep("pending", 3))
)

# Colors for stages
stage_colors <- c("done" = col_optimal, "current" = "#F39C12", "pending" = col_nonopt)

ggplot(bits) +
  # Bit boxes with status coloring
  geom_tile(aes(x = x, y = 0.5, fill = stage),
            width = 0.85, height = 0.7, color = "white", linewidth = 2) +
  # Bit value labels
  geom_text(aes(x = x, y = 0.5, label = label),
            fontface = "bold", size = 5, color = "white") +
  # Bit position labels
  geom_text(aes(x = x, y = 0.02, label = paste0("bit ", 8 - bit)),
            size = 3, color = col_text) +
  scale_fill_manual(values = stage_colors,
                    labels = c("done" = "Processed", "current" = "Current", "pending" = "Remaining"),
                    name = "") +
  # Direction indicator
  annotate("segment", x = 1, xend = 8, y = 1.05, yend = 1.05,
           arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
           linewidth = 1.2, color = col_text) +
  annotate("text", x = 4.5, y = 1.18, label = "Process most significant -> least significant",
           size = 3.8, color = col_text) +
  labs(title = "Gabow-Tarjan: Bit-Scaling",
       subtitle = "Process integer costs bit-by-bit from coarse to fine. Complexity: O(sqrt(n) m log(nC))") +
  theme_diagram() +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 10, color = col_text)) +
  coord_fixed(ratio = 1.2, xlim = c(0.3, 8.7), ylim = c(-0.15, 1.35))

## ----gabow-tarjan-example-----------------------------------------------------
set.seed(42)
n <- 50
# Use integer costs with large range - Gabow-Tarjan's strength
cost <- matrix(sample(1:100000, n * n, replace = TRUE), n, n)
result <- lap_solve(cost, method = "gabow_tarjan")
cat("Total cost:", get_total_cost(result), "\n")

## ----network-simplex-diagram, fig.width=7, fig.height=4.5, echo=FALSE, fig.alt="Network simplex spanning tree structure for assignment problem"----
# Flow network: Source -> Workers -> Jobs -> Sink
col_source <- "#9B59B6"  # Purple for source/sink

nodes <- tibble(
  name = c("S", "W1", "W2", "J1", "J2", "T"),
  type = c("Source/Sink", "Worker", "Worker", "Job", "Job", "Source/Sink")
)

# Edges: source->workers, workers->jobs (with costs), jobs->sink
# Tree edges shown solid, non-tree dashed
edges <- tibble(
  from = c(1, 1, 2, 2, 3, 3, 4, 5),
  to =   c(2, 3, 4, 5, 4, 5, 6, 6),
  cost = c(0, 0, 3, 5, 4, 2, 0, 0),
  in_tree = c(FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE)
)

g <- tbl_graph(nodes = nodes, edges = edges, directed = FALSE)

# Layout: Source | Workers | Jobs | Sink
layout <- data.frame(
  x = c(0, 1.2, 1.2, 2.4, 2.4, 3.6),
  y = c(1.5, 2, 1, 2, 1, 1.5)
)

# Edge labels for worker->job edges only (costs > 0)
edge_label_data <- data.frame(
  from_x = c(1.2, 1.2, 1.2, 1.2),
  from_y = c(2, 2, 1, 1),
  to_x   = c(2.4, 2.4, 2.4, 2.4),
  to_y   = c(2, 1, 2, 1),
  cost   = c(3, 5, 4, 2),
  in_tree = c(TRUE, FALSE, FALSE, TRUE),
  pos = c(0.5, 0.3, 0.7, 0.5)
)

edge_label_data$x <- edge_label_data$from_x + edge_label_data$pos * (edge_label_data$to_x - edge_label_data$from_x)
edge_label_data$y <- edge_label_data$from_y + edge_label_data$pos * (edge_label_data$to_y - edge_label_data$from_y)
edge_label_data$angle <- atan2(edge_label_data$to_y - edge_label_data$from_y,
                                edge_label_data$to_x - edge_label_data$from_x) * 180 / pi
edge_label_data$edge_color <- ifelse(edge_label_data$in_tree, col_optimal, col_nonopt)

ggraph(g, layout = "manual", x = layout$x, y = layout$y) +
  # Edges
  geom_edge_link(aes(edge_colour = in_tree, edge_width = in_tree,
                     edge_linetype = in_tree)) +
  # Cost labels on worker->job edges
  geom_label(data = edge_label_data,
             aes(x = x, y = y, label = cost, angle = angle),
             fill = edge_label_data$edge_color,
             color = "#3E3F3A", fontface = "bold", size = 4.5,
             label.padding = unit(0.15, "lines"), linewidth = 0) +
  # Nodes with fill aesthetic for legend
  geom_node_point(aes(fill = type), shape = 21, size = 12, color = "white", stroke = 1.5) +
  # Node labels
  geom_node_text(aes(label = name), color = "white", fontface = "bold", size = 4) +
  scale_fill_manual(values = c("Source/Sink" = col_source, "Worker" = col_worker,
                               "Job" = col_job),
                    name = NULL, guide = guide_legend(order = 1, override.aes = list(size = 4))) +
  scale_edge_colour_manual(values = c("TRUE" = col_optimal, "FALSE" = col_nonopt),
                           labels = c("Non-tree", "Tree"), name = NULL,
                           guide = guide_legend(order = 2)) +
  scale_edge_width_manual(values = c("TRUE" = 1.5, "FALSE" = 0.8), guide = "none") +
  scale_edge_linetype_manual(values = c("TRUE" = "solid", "FALSE" = "dashed"), guide = "none") +
  labs(title = "Network Simplex: Flow Network",
       subtitle = "Assignment as min-cost flow: S sends 1 unit through each worker to sink T") +
  theme_graph_clean() +
  coord_fixed(clip = "off")

## ----network-simplex-example--------------------------------------------------
set.seed(789)
n <- 100
cost <- matrix(runif(n * n, 0, 100), n, n)
result <- lap_solve(cost, method = "network_simplex")
cat("Total cost:", round(get_total_cost(result), 2), "\n")

## ----push-relabel-example-----------------------------------------------------
set.seed(222)
n <- 100
cost <- matrix(runif(n * n, 0, 100), n, n)
result <- lap_solve(cost, method = "push_relabel")
cat("Total cost:", round(get_total_cost(result), 2), "\n")

## ----hk01-example-------------------------------------------------------------
set.seed(101)
n <- 100
cost <- matrix(sample(0:1, n^2, replace = TRUE, prob = c(0.3, 0.7)), n, n)
result <- lap_solve(cost, method = "hk01")
cat("Total cost:", get_total_cost(result), "\n")

## ----sap-example--------------------------------------------------------------
set.seed(789)
n <- 100
cost <- matrix(Inf, n, n)
edges <- sample(1:(n^2), floor(0.2 * n^2))  # Only 20% allowed
cost[edges] <- runif(length(edges), 0, 100)
result <- lap_solve(cost, method = "sap")
cat("Total cost:", round(get_total_cost(result), 2), "\n")

## ----sap-dense-example--------------------------------------------------------
set.seed(789)
n <- 60
cost <- matrix(runif(n * n, 0, 100), n, n)
result <- lap_solve(cost, method = "sap_dense")
cat("Total cost:", round(get_total_cost(result), 2), "
")

## ----ramshaw-tarjan-example---------------------------------------------------
set.seed(333)
n_rows <- 30
n_cols <- 100  # Highly rectangular: 30 x 100
cost <- matrix(runif(n_rows * n_cols, 0, 100), n_rows, n_cols)
result <- lap_solve(cost, method = "ramshaw_tarjan")
cat("Matched", sum(result$assignment > 0), "of", n_rows, "rows\n")

## ----murty-example------------------------------------------------------------
cost <- matrix(c(10, 19, 8, 15, 10, 18, 7, 17, 13, 16, 9, 14, 12, 19, 8, 18),
               nrow = 4, byrow = TRUE)
kbest <- lap_solve_kbest(cost, k = 5)
summary(kbest)

## ----bottleneck-example-------------------------------------------------------
cost <- matrix(c(5, 9, 2, 10, 3, 7, 8, 4, 6), nrow = 3, byrow = TRUE)
result <- bottleneck_assignment(cost)
cat("Bottleneck (max edge):", result$bottleneck, "\n")

## ----sinkhorn-example---------------------------------------------------------
cost <- matrix(c(1, 2, 3, 4), nrow = 2)
result <- sinkhorn(cost, lambda = 10)
print(round(result$transport_plan, 3))

## ----duals-example------------------------------------------------------------
cost <- matrix(c(10, 19, 8, 15, 10, 18, 7, 17, 13), nrow = 3, byrow = TRUE)
result <- assignment_duals(cost)
cat("Row duals (u):", result$u, "\n")
cat("Col duals (v):", result$v, "\n")

## ----benchmark-plot, fig.width=9, fig.height=6, echo=FALSE, fig.alt="Solve time against matrix size for six algorithms on dense uniform integer costs, with Jonker-Volgenant fastest at every size"----
bench_sizes <- c(100, 200, 500, 1000)
bench_results <- solver_bench[solver_bench$method %in% names(race_methods) &
                                solver_bench$n %in% bench_sizes, ]
bench_results$method <- factor(race_methods[bench_results$method],
                               levels = unname(race_methods))

ggplot(bench_results, aes(x = n, y = min_ms, color = method, group = method)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3, aes(shape = method)) +
  scale_y_log10(labels = function(x) format(x, scientific = FALSE, drop0trailing = TRUE, trim = TRUE)) +
  scale_x_log10(breaks = bench_sizes) +
  scale_color_manual(values = c(
    "Munkres" = "#8c3b00",
    "Hungarian" = "#d9534f",
    "Jonker-Volgenant" = "#5bc0de",
    "Auction" = "#f0ad4e",
    "CSA" = "#5cb85c",
    "Network Simplex" = "#428bca"
  )) +
  labs(
    title = "Algorithm Scaling: Dense Matrices",
    subtitle = "Uniform integer costs, fastest of 5 runs, log-log scale",
    x = "Matrix Size (n x n)",
    y = "Time (milliseconds, log scale)",
    color = "Algorithm",
    shape = "Algorithm"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14, colour = "#3E3F3A"),
    plot.subtitle = element_text(size = 11, colour = "#3E3F3A"),
    panel.grid.minor = element_blank()
  )

