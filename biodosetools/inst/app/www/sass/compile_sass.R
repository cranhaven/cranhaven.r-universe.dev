options(sass.cache = FALSE)

sass::sass(
  input = sass::sass_file(here::here("inst/app/www/sass", "main.scss")),
  output = here::here("inst/app/www", "biodosetools_style.css")
)
