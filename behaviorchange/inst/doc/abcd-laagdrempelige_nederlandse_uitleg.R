## ----setup, echo = FALSE------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
);
#require('webshot', quiet=TRUE);

## ----raa, echo=FALSE, fig.width=3, fig.height=2, fig.cap="Figuur 1: De reasoned action approach."----

raaGraph <- DiagrammeR::create_graph();
raaGraph <- DiagrammeR::add_node(raaGraph, label="Gedrag");
raaGraph <- DiagrammeR::add_node(raaGraph, label="Intentie", to=1);
raaGraph <- DiagrammeR::add_node(raaGraph, label="Attitude", to=2);
raaGraph <- DiagrammeR::add_node(raaGraph, label="Waargenomen norm", to=2);
raaGraph <- DiagrammeR::add_node(raaGraph, label="Waargenomen gedragscontrole", to=2);
raaGraph <-
  behaviorchange::apply_graph_theme(raaGraph,
                                    c("layout", "dot", "graph"),
                                    c("rankdir", "LR", "graph"),
                                    c("outputorder", "nodesfirst", "graph"),
                                    c("fixedsize", "false", "node"),
                                    c("shape", "box", "node"),
                                    c("style", "rounded,filled", "node"),
                                    c("color", "#000000", "node"),
                                    c("color", "#000000", "edge"),
                                    c("dir", "forward", "edge"),
                                    c("fillcolor", "#FFFFFF", "node"));

if (knitr::is_latex_output()) {
  ### From DiagrammeR::export_graph
  dot_code <- DiagrammeR::generate_dot(raaGraph);
  graphSvg <-
    DiagrammeRsvg::export_svg(DiagrammeR::grViz(dot_code));
  graphSvg <-
    sub(".*\n<svg ", "<svg ", graphSvg);
  graphSvg <- gsub('<svg width=\"[0-9]+pt\" height=\"[0-9]+pt\"\n viewBox=',
                   '<svg width="2000px" height="1000px" viewBox=',
                   graphSvg);
  grid::grid.newpage();
  grid::grid.raster(png::readPNG(rsvg::rsvg_png(charToRaw(graphSvg))));
} else if (knitr::is_html_output()) {
  DiagrammeR::render_graph(raaGraph);
}


## ----abcd-specificatie, echo=FALSE, warning=FALSE, results="asis"-------------
abcd_specs_dutch_xtc <- behaviorchange::abcd_specs_dutch_xtc;

names(abcd_specs_dutch_xtc) <-
  c("Gedrags-veranderings-principes",
    "Voorwaarden voor effectiviteit",
    "Toepassingen",
    "Sub-determinanten",
    "Determinanten",
    "Sub-gedragingen",
    "Doelgedrag");

if (knitr::is_latex_output()) {
  cat("\n
\\newpage\n
\\blandscape\n
\n
");

  print(
    kableExtra::kable_styling(kableExtra::column_spec(
      knitr::kable(abcd_specs_dutch_xtc,
                   caption="Een voorbeeld van een ABCD matrix.",
                   booktabs = TRUE,
                   row.names = FALSE,
                   longtable = TRUE),
    column = 1:7,
    width = c("2.5cm", "5cm", "4cm",
              "3.5cm", "2cm", "2.5cm", "1.8cm")
    )));

  cat("\n
\\elandscape\n
\\newpage\n
\n
");
} else {
  knitr::kable(abcd_specs_dutch_xtc);
}

## ----abcd-example, echo=FALSE, fig.cap="De ABCD voor het XTC voorbeeld", out.width="100%"----
knitr::include_graphics("abcd_specs_dutch_xtc.png");

## ----abcd-diagram, echo=FALSE, fig.width=14, fig.height=7, eval=FALSE---------
# abcd_specs_dutch_xtc_graph <-
#   behaviorchange::abcd(behaviorchange::abcd_specs_dutch_xtc);
# print(abcd_specs_dutch_xtc_graph);
# DiagrammeR::export_graph(abcd_specs_dutch_xtc_graph$output$graph,
#                          here::here('vignettes', 'abcd_specs_dutch_xtc.png'),
#                          title="Acyclic Behavior Change Diagram",
#                          width=3000,
#                          height=1000);

