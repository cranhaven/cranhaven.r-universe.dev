uitpPedigreeBrowser <-
  tabPanel("Pedigree Browser", div(
    div(
      # Side Panel
      div(
        style = paste(
          "float: left; width: 350px; height: 295px; padding: 10px;",
          "border: 1px solid lightgray; background-color: #EDEDED;",
          "margin-left:3px; margin-top: 3px; margin-bottom: 3px;",
          "border-radius: 25px; box-shadow: 0 0 5px 2px #888"
        ),
        includeHTML(file.path("..", "extdata", "ui_guidance",
                              "pedigree_browser.html"))
      ),
      # Main Panel
      div(
        style = "margin-left: 425px; padding: 10px;",
        helpText(
          paste0(
            "IDs of selected focal animals may be manually ",
            "entered here if analysis of all individuals is ",
            "not needed. IDs may be pasted from Excel or you ",
            "can browse for and select a file containing the list ",
            "of focal animals."
          )
        ),
        tags$textarea(id = "focalAnimalIds", rows = 5L, cols = 60L, ""),
        fileInput(
          "focalAnimalUpdate",
          "Choose CSV file with focal animals",
          multiple = FALSE,
          # nolint start: nonportable_path_linter
          accept = c("text/csv", "text/comma-separated-values,text/plain",
                     ".csv")
          # nolint end
        ),
        shinyBS::popify(
          actionButton("specifyFocalAnimal", label = "Update Focal Animals"),
          paste0(
            "Read selected file of focal animals or list of animals ",
            "entered and update the table below"
          )
        ),
        shinyBS::popify(
          checkboxInput("clearFocalAnimals", label = "Clear Focal Animals",
                        value = FALSE),
          paste0(
            "Focal animal list is to be cleared as well using a file ",
            "with no animal IDs. (Files are not affected)."
          )
        ),
        helpText(
          paste0(
            "The search field below will search all columns for ",
            "matches to any text or number entered."
          ),
          style = "color:blue"
        )
      )
    ),
    div(
      # Right Side Panel
      style = paste(
        "margin-left: 550px;",
        "width: 500px; height: 180px; padding: 10px"
      ),
      shinyBS::popify(
        checkboxInput("uid", label = "Display Unknown IDs", value = TRUE),
        paste0(
          "Unknown IDs, beginning with a capital U, are created ",
          "by the application for all animals with only one  ",
          "parent."
        )
      ),
      shinyBS::popify(
        checkboxInput("trim", label = "Trim pedigree based on focal animals",
                      value = FALSE),
        paste0(
          "Trim the pedigree to include only relatives of the focal ",
          "animals provided."
        )
      ),
      shinyBS::popify(
        downloadButton("downloadPedigree", "Export"),
        paste0("Export the pedigree into a CSV (comma separted value)  ",
               "file.")
      ),
      helpText(
        "A population must be defined before proceeding
               to the Genetic Value Analysis.",
        style = "color:blue"
      )
    ),
    DT::dataTableOutput("pedigree")
  ))
