uitpOripReporting <-
  tabPanel(
    "ORIP Reporting",

    # Side Panel
    div(
      style = paste(
        "float: left; width: 400px; height: 100vh; padding: 10px;",
        "border: 1px solid lightgray; background-color: #EDEDED;",
        "margin-left: 3px; margin-top: 3px;",
        "border-radius: 25px; box-shadow: 0 0 5px 2px #888"
      ),
      helpText(
        "This tab will eventually contain a report formatted
        for submission to ORIP."
      )
    ),

    # Main Panel
    div(
      style = "margin-left:425px;padding:10px;",
      img(
        src = file.path("extdata", "www", "under_construction.jpg"),
        height = 300L,
        width = 600L
      )
    )

  )
