# Core translations are written with escapes so every Windows locale can load them.
fafa_translations <- function(language = "en") {
  if (!identical(language, "tr")) return(character(0))
  c(
    "Project & Reports" = "Proje ve Raporlar",
    "Select Data" = "Veri Se\u00e7imi"
  )
}

fafa_language <- function(language = "en") {
  value <- if (is.function(language)) language() else language
  if (identical(value, "tr")) "tr" else "en"
}

fafa_text <- function(language, english, turkish) {
  if (identical(fafa_language(language), "tr")) turkish else english
}
