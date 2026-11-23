devtools::install()


rmarkdown::render(
    input = "./vignettes/user_guide.Rmd",
    output_dir = "./vignettes/",
    output_file = "user_guide.html"
)

rmarkdown::render(
    input = "./vignettes/analysis.Rmd",
    output_dir = "./vignettes/",
    output_file = "analysis.html"
)

