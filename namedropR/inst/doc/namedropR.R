## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, eval=FALSE--------------------------------------------------------
#  library(namedropR)

## ---- eval=FALSE--------------------------------------------------------------
#  bib_path <- system.file("testdata", "sample.bib", package = "namedropR")
#  namedropR::drop_name(bib_path)

## ---- eval=FALSE--------------------------------------------------------------
#  library(bib2df)
#  bib_path <- system.file("testdata", "sample.bib", package = "namedropR")
#  bib <- bib2df::bib2df(bib_path)
#  
#  namedropR::drop_name(bib)

## ---- eval=FALSE--------------------------------------------------------------
#  # create a visual citation as HTML (default, equivalent to export_as = "html")
#  drop_name(bib, cite_key = "Hawking1974")
#  #> [1] "visual_citations/Hawking1974.html"
#  
#  # create a visual citation as PNG
#  drop_name(bib, cite_key = "Hawking1974", export_as = "png")
#  #> [1] "visual_citations/Hawking1974.html.png"
#  

## ---- eval=FALSE--------------------------------------------------------------
#  
#  # using the temporary directory tempdir()
#  drop_name(bib,
#    output_dir = tempdir(),
#    export_as = "png",
#    cite_key = "Hawking1974"
#  )
#  
#  # by passing a relative path
#  drop_name(bib,
#    output_dir = "my_custom_folder",
#    export_as = "png",
#    cite_key = "Hawking1974"
#  )
#  
#  # by passing an absolute path
#  drop_name(bib,
#    output_dir = "/home/user_name/some/project/path",
#    export_as = "png",
#    cite_key = "Hawking1974"
#  )
#  
#  # (note: if passing an absolute path, the path must already exist!)

## ---- eval=FALSE--------------------------------------------------------------
#  drop_name(bib,
#    export_as = "png",
#    path_absolute = TRUE,
#    cite_key = "Hawking1974"
#  )
#  #> [1] ""/home/user_name/some/project/path/Hawking1974.html.png"

## ---- eval=FALSE--------------------------------------------------------------
#  drop_name(bib)
#  
#  #> No cite_key specified. Working through all possible 5 entries in the bibliography.
#  #> [1] "visual_citations/Eschrich1983.html"
#  #> [2] "visual_citations/collaboration_2019_ApJL.html"
#  #> [3] "visual_citations/Hawking1974.html"
#  #> [4] "visual_citations/Hawking1975.html"
#  #> [5] "visual_citations/Hawking1976.html"

## ---- eval=FALSE--------------------------------------------------------------
#  drop_name(bib,
#    cite_key = c(
#      "Hawking1974",
#      "Eschrich1983",
#      "collaboration_2019_ApJL"
#    )
#  )
#  
#  #> 3 cite_key(s) specified. Working through all of them.
#  #> [1] "visual_citations/Hawking1974.html"
#  #> [2] "visual_citations/Eschrich1983.html"
#  #> [3] "visual_citations/collaboration_2019_ApJL.html"

## ---- eval=FALSE--------------------------------------------------------------
#  # PNG
#  knitr::include_graphics(
#    drop_name(bib,
#      cite_key = "Hawking1974",
#      export_as = "png",
#      style = "clean"
#    )
#  )
#  
#  # HTML
#  htmltools::includeHTML(
#    drop_name(bib,
#      cite_key = "Hawking1974",
#      export_as = "hmtl",
#      style = "clean",
#      use_xaringan = TRUE
#    )
#  )

## ----eval=FALSE---------------------------------------------------------------
#  drop_name(bib,
#    cite_key = "Hawking1974",
#    export_as = "png",
#    style = "modern"
#  )
#  
#  drop_name(bib,
#    cite_key = "Hawking1974",
#    export_as = "png",
#    style = "classic"
#  )
#  
#  drop_name(bib,
#    cite_key = "Hawking1974",
#    export_as = "png",
#    style = "clean"
#  )
#  
#  drop_name(bib,
#    cite_key = "Hawking1974",
#    export_as = "png",
#    style = "fancy"
#  )
#  
#  drop_name(bib,
#    cite_key = "Hawking1974",
#    export_as = "png",
#    style = "newspaper"
#  )

## ---- eval=FALSE--------------------------------------------------------------
#  
#  drop_name(bib,
#    cite_key = "Hawking1974",
#    export_as = "png",
#    style = "newspaper",
#    # default size is 250, minimum is 150:
#    qr_size = 150,
#    # default width of the text area is 600
#    vc_width = 1100
#  )

## ----eval=FALSE---------------------------------------------------------------
#  drop_name(bib,
#    cite_key = "Hawking1974",
#    export_as = "png",
#    style = "compact"
#  )

