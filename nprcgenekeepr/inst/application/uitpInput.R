uitpInput <-
  tabPanel(
    "Input",

    tags$style(
      type = "text/css", # nolint: nonportable_path_linter
      "table {border: 1px solid black; width: 100%; padding: 15px;}",
      "tr, td, th {border: 1px solid black; padding: 5px;}",
      "th {font-weight: bold; background-color: #7CFC00;}",
      "hr {border-width:2px;border-color:#A9A9A9;}"
    ),
    titlePanel(div(
      style = "height:200px;width:100%",
      div(
        style = "float:left;width:45%",
        img(
          src = getLogo()$file,
          height = getLogo()$height,
          width = getLogo()$width
        )
      ),
      div(style = "float:right;text-align:right;width:45%",
          "Genetic Management Tools")
    )),
    # Side Panel
    sidebarLayout(
      sidebarPanel(
        style = paste(
          "float: left; width: 360px; height: 100%; padding: 10px;",
          "border: 1px solid lightgray; background-color: #EDEDED;",
          "margin-left: 3px;",
          "border-radius: 25px; box-shadow: 0 0 5px 2px #888"
        ),
        helpText("Select how you are submitting data."),
        prettyRadioButtons(
          "fileType",
          label = "File Type",
          choices = list(Excel = "fileTypeExcel", Text = "fileTypeText"),
          selected = NULL,
          outline = TRUE
        ),
        prettyRadioButtons(
          "fileContent",
          label = "File Content",
          choices = list(
            "Pedigree(s) file only; genotypes not provided" = "pedFile",
            "Pedigree(s) and genotypes in one file" = "commonPedGenoFile",
            "Pedigree(s) and genotypes in separate files" =
              "separatePedGenoFile",
            "Focal animals only; pedigree built from database" = "focalAnimals"
          ),
          selected = NULL
        ),
        conditionalPanel(
          condition = "input.fileType == 'fileTypeText'",
          prettyRadioButtons(
            "separator",
            label = "Separator",
            choices = list(
              Comma = ",",
              Semicolon = ";",
              Tab = "\t"
            ),
            selected = ",",
            outline = TRUE
          )
        ),
        conditionalPanel(
          condition = "input.fileContent == 'pedFile'",
          fileInput("pedigreeFileOne", label = "Select Pedigree File")
        ),
        conditionalPanel(
          condition = "input.fileContent == 'commonPedGenoFile'",
          fileInput("pedigreeFileTwo", label = "Select Pedigree-Genotype File")
        ),
        conditionalPanel(
          condition = "input.fileContent == 'separatePedGenoFile'",
          fileInput("pedigreeFileThree", label = "Select Pedigree File"),
          fileInput("genotypeFile", label = "Select Genotype File")
        ),
        conditionalPanel(
          condition = "input.fileContent == 'focalAnimals'",
          fileInput("breederFile", label = "Select Focal Animals File")
        ),
        shinyBS::popify(
          textInput("minParentAge", label = "Minimum Parent Age (years)",
                    value = "2.0"),
          NULL,
          paste(
            "Parents must be at least as old as the minimum parent",
            "age at the birthdate of an offspring. If not, the",
            "file will not be accepted and a file named",
            "<b>lowParentAge.csv</b> containing a list of parents",
            "below the minimum age will",
            "be written to the user&#39s home directory. Animals",
            "without birthdates are not affected by this rule."
          )
        ),

        actionButton("getData", "Read and Check Pedigree"),
        checkboxInput("debugger", label = "Debug on", value = FALSE)#,
      ),
      # Main Panel
      mainPanel(#style = "margin-left:425px;padding:10px;", # nolint
                includeHTML(file.path("..", "extdata", "ui_guidance",
                                      "input_format.html")))
    )
  )
