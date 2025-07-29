uitpBreedingGroupFormation <-
  tabPanel(
    "Breeding Group Formation",
    div(
      fluidRow(
        column(
          4L,
          offset = 1L,
          style = paste0(
            "border: 1px solid lightgray; background-color: #EDEDED; ",
            "border-radius: 1px; box-shadow: 0 0 5px 2px #888"
          ),
          prettyRadioButtons(
            "group_formation_rb",
            "Choose one group formation workflow:",
            choiceNames = list(
              paste0(
                "Randomly select from only high-value ",
                "animals in genetic value analysis"
              ),
              paste0("Randomly select from all animals in  ",
                     "genetic value analysis"),
              paste0("Use candidate animals entered below  ", "to form groups")
            ),
            choiceValues = list("high-value", "all", "candidates"),
            width = "650px",
            selected = character(0L),
            outline = TRUE
          ),
          conditionalPanel(
            condition = paste0("input.group_formation_rb == 'candidates' "),
            div(
              style = "display:inline-block;width:350px;padding:5px",
              helpText("Enter IDs of candidates to be added to new group(s):"),
              tags$textarea(id = "grpIds", rows = 10L, cols = 40L, "")
            )
          )
        )
      ),
      fluidRow(
        column(
          4L,
          offset = 1L,
          style = paste(
            "border: 1px solid lightgray; background-color: #EDEDED;",
            "border-radius: 1px; box-shadow: 0 0 5px 2px #888"
          ),
          prettyRadioButtons(
            "group_sex_rb",
            "Sex of animals in groups:",
            choiceNames = list(
              "Ignore sex when forming groups ",
              "Form harems (one male)",
              "User specified sex ratio of breeders"
            ),
            choiceValues = list("ignore-sex", "harems", "sex-ratio"),
            width = "350px",
            selected = "ignore-sex",
            outline = TRUE
          ),
          conditionalPanel(condition = "input.group_sex_rb == 'sex-ratio'", div(
            shinyBS::popify(
              numericInput(
                "sexRatio",
                label = "Sex Ratio (F/M):",
                value = 0.0,
                min = 0.5,
                max = 20L,
                step = 0.5
              ),
              NULL,
              paste0(
                "Final sex ratios are approximate but are guaranteed to be ",
                "the nearest possible value given creation of the largest ",
                "group possible."
              )
            )
          ))
        )
      ),
      fluidRow(column(
        4L,
        offset = 1L,
        style = paste0(
          "padding-top:1px;display:inline-block;",
          "padding-bottom:1px"
        ),
        checkboxInput(
          "seedAnimals",
          label = paste0("Optional: Seed Groups with ", "Specific Animals"),
          width = "150%",
          value = FALSE
        )
      )),
      fluidRow(
        column(
          4L,
          offset = 1L,
          style = paste0(
            "padding-top:1px;display:inline-block;",
            "width:400px;padding-bottom:1px"
          ),
          numericInput(
            "numGp",
            label = "Number of Groups Desired:",
            value = 1L,
            min = 1L,
            max = MAXGROUPS,
            step = 1.0
          ),
          uiOutput("minParentAge", inline = FALSE),
          conditionalPanel(condition = "!input.useMinParentAge", div(
            shinyBS::popify(
              numericInput(
                "minAge",
                label = "Animals will be grouped with the mother below age:",
                value = 1L,
                min = 0L,
                max = 40L,
                step = 0.1
              ),
              NULL,
              paste0("Animals will be groups with the mother below the age ",
                     "you select.")
            )
          )),
          div(
            selectInput(
              "kinThresh",
              label = "Animals with kinship above this value will be excluded:",
              choices = list(
                "0.015625 (second cousins)" = 0.015625,
                # nolint start: nonportable_path_linter
                "0.0625 (great-grandparent/great-grandchild; first cousins)" =
                  0.0625,
                "0.125 (grandparent/grandchild; half-siblings; avuncular)" =
                  0.125,
                # nolint end
                "0.25 (parent/child)" = 0.25
              ),
              selected = 1L
            )
          ),
          div(
            selectInput(
              "ffRel",
              label = paste0("Ignore females at or above the ",
                             "minimum parent age:"),
              choices = list(Yes = TRUE, No = FALSE),
              selected = 0L
            ),
            checkboxInput("withKin",
                          label = "Include kinship in display of groups",
                          value = FALSE),
            numericInput(
              "gpIter",
              label = "Number of simulations:",
              value = 10L,
              min = 1L,
              max = 1000000L
            )
          ),
          conditionalPanel(
            condition = "input.group_formation_rb == 'high-value' |
                       input.group_formation_rb == 'all' |
                       input.group_formation_rb == 'candidates'",
            div(
              style = paste0(
                "padding-top:5px;padding-bottom:5px;",
                "border:1px solid #4BCEEF;background-color:#4BCEEF;",
                "border-radius:1px; box-shadow: 0 0 5px 2px #888"
              ),
              actionButton(
                "grpSim",
                label = "Make Groups",
                width = "100%",
                height = "120%",
                style = "color: #fff; background-color: #4BCEEF;
                                 border-color: #4BCEEF;font-size:150%;
                                 font:bold"
              )
            )
          ),
          div(
            style = "display:inline-block;width:250px;padding:5px",
            numericInput(
              "viewGrp",
              label = "Enter the group to view:",
              value = 1L,
              min = 1L,
              max = MAXGROUPS,
              step = 1L
            ),
            downloadButton("downloadGroup", "Export Current Group"),
            downloadButton("downloadGroupKin",
                           "Export Current Group Kinship Matrix")
          )

        ),
        column(1L),
        column(
          5L,
          offset = 0L,
          style = "padding-top:5px",
          conditionalPanel(condition = "input.seedAnimals == true",
                           div(uiOutput("currentGroups")))
        )
      )
    ),
    fluidRow(
      column(
        width = 10L,
        offset = 1L,
        DT::dataTableOutput("breedingGroups"),
        DT::dataTableOutput("breedingGroupKin")
      )
    ),
    fluidRow(column(
      width = 10L,
      offset = 1L,
      style = paste0(
        "border: 1px solid lightgray; background-color: #EDEDED; ",
        "border-radius: 15px; box-shadow: 0 0 5px 2px #888"
      ),
      # nolint start: nonportable_path_linter
      includeHTML("../extdata/ui_guidance/group_formation.html")
      # nolint end
    ))
  )
