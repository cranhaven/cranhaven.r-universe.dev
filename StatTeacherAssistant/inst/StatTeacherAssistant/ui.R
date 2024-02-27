
library(shiny)
library(shinyBS)
library(shinyjs)
library(shinyalert)
library(DT)
library(plotly)
library(rhandsontable)
library(stringi)


# distribution shapes and families if user randomly generates data
shapes <- list("Severely Left Skewed", "Very Left Skewed", "Left Skewed",
  "Normal", "Right Skewed", "Very Right Skewed", "Severely Right Skewed"
)

families_quant <- list("Normal", "t", "F", "Chi-square", "Uniform (Continuous)",
  "Uniform (Discrete)", "Poisson", "Exponential", "Gamma", "Beta"
)

families_cat <- list("Binomial", "Multinomial")


# function for adding line breaks
linebreaks <- function(n) HTML(strrep(br(), n))



navbarPage("StatTeacherAssistant",


  ##############################################################################
  ################################ Home tabpanel ###############################
  ##############################################################################

  tabPanel("Home", icon = icon("home"),

    # include this code to hide/show download buttons in app
    shinyjs::useShinyjs(),


    tags$head(tags$style(HTML("ul {padding-left: 6px;}"))),

    # tweak style of popup modals
    tags$head(tags$style(HTML(".modal-lg {width: 95%; margin-bottom: 5%}"))),
    #tags$head(tags$style(HTML(".modal-footer {display: none}"))),


    # suppress all warnings in console
    # tags$style(type = "text/css",
    #   ".shiny-output-error {visibility: hidden;}",
    #   ".shiny-output-error:before {visibility: hidden;}"
    # ),


    # remove arrows from all numeric inputs
    tags$style(HTML("
      input[type=number]::-webkit-inner-spin-button,
      input[type=number]::-webkit-outer-spin-button {
          -webkit-appearance: none;
          -moz-appearance: none;
          appearance: none;
          margin: 0;
      }"
    )),


    column(3, p("")),

    column(6,
      h1(style = "color: green; font-weight: bold; text-align: center;", "To
        begin, follow 1 of the 3 sets of instructions found below:"
      ),
      br(),

      tags$ul(
        tags$li(HTML("<h3><b><span style = 'color: green; text-decoration:
          underline green'> Upload a data set* <br></b></span> Click the 'Input
          Data' tab above, followed by 'Upload Data Set.'"
        )),

        tags$li(HTML("<h3><b><span style = 'color: green; text-decoration:
          underline green'> Input a data set manually* <br></b></span> Click the
          'Input Data' tab above, followed by 'Manually Input.'"
        )),

        tags$li(HTML("<h3><b><span style = 'color: green; text-decoration:
          underline green'> Create a data set containing randomly generated data
          only <br></b></span> Click the 'Generate Data' tab above."
        )),

        br(),

        h4("* If you upload or manually input a data set, you can still randomly
          generate data and combine it with that data set."
        )
      )
    ),

    column(3, p(""))
  ),




  ##############################################################################
  ############################# Input Data tabpanel ############################
  ##############################################################################

  navbarMenu("Input Data", icon = icon("upload"),

    ### data set uploaded by user
    tabPanel("Upload Data Set",

      column(3,
        div(style = "margin-top: -2em;"),
        h3(style = "color: red; font-weight: bold;", "Step 1"),
        p(style = "color: red; font-weight: bold;", "Click the 'Browse...'
          button below to upload a data set. Then click the 'Store Data Set'
          button."
        ),

        wellPanel(
          fileInput("data_file", NULL, multiple = FALSE),

          conditionalPanel(
            condition = "output.na_input_status",    # from server

            checkboxInput("missing_input", strong(style = "color: green;", "Check
              Box if Your Data Set Contains Missing Data"),
              value = FALSE
            ),

            conditionalPanel(
              condition = "input.missing_input",

              p("If the missing values in your data set are represented by empty
                cells, you may uncheck the box. If they are represented by
                something else (e.g., a period), specify the symbol, character,
                etc. that represents the missing values. If there are multiple,
                separate each by a comma. Any entries with missing data will
                appears as empty cells."
              ),

              textInput("denote_missing_input", NULL, value = ".", width = "25%")
            )
          ),

          actionButton("upload_dataset", "Store Data Set",
            class = "btn btn-primary"
          )
        )
      ),

      column(6,
        h4("Original Data Set"),
        DTOutput("df_original_upload"),
      ),

      column(3,
        div(style = "margin-top: -2em;"),
        h3(style = "color: red; font-weight: bold;", "Step 2"),
        p(style = "color: red; font-weight: bold;", "Continue to one of the
          following:"
        ),

        p(style = "color: red; font-weight: bold;", "(1) the 'Generate Data' tab,
          if you want to add randomly generated data to your uploaded data set,"
        ),

        p(style = "color: red; font-weight: bold;", "(2) the 'Adjust Data' tab,
          if you want to update your existing data, or"
        ),

        p(style = "color: red; font-weight: bold;", "(3) the 'Check Data' tab, if
          you want to check the suitability of your data using statistical
          method(s)."
        )
      )
    ),

    ### data set manually created by user
    tabPanel("Manually Input",
      column(3,

        div(style = "margin-top: -2em;"),
        h3(style = "color: red; font-weight: bold;", "Step 1"),
        p(style = "color: red; font-weight: bold;", "Set up a new spreadsheet by
          inputting the number of rows and column names."
        ),

        wellPanel(
          numericInput("manual_num_rows", "Number of Observations/Rows",
            value = 20, min = 1
          ),
          tags$head(tags$style(
            type = "text/css", "#manual_num_rows{width: 125px;}"
          )),

          textAreaInput("manual_var_names_num", "Type the names of all variables
            that contain numbers (quantitative or categorical with numeric
            values). Don't use spaces, and separate the names using commas.",
            placeholder = "numeric.variable1, numeric.variable2"
          ),

          textAreaInput("manual_var_names_chr", "Type the names of all variables
            that contain letters (categorical with categories such as freshman,
            sophomore, etc.). Don't use spaces, and separate the names using
            commas.",
            placeholder = "cat_var1, cat_var2"
          ),

          actionButton("set_manual_dims", "Generate Empty Table",
            class = "btn btn-primary"
          )
        )
      ),

      column(5,
        div(style = "margin-top: -2em;"),
        h3(style = "color: red; font-weight: bold;", "Step 2"),
        p(style = "color: red; font-weight: bold;", "Input data manually. This
          includes the option of copying and pasting data from a spreadsheet.
          When pasting, you must use the Ctrl+V (Windows) or Cmd+V (Mac)
          shortcut."
        ),

        linebreaks(2),

        rHandsontableOutput("df_original_manual", height = "500px"),

        tags$head(tags$style(HTML(".handsontable {overflow: hidden;}")))
      ),

      column(2,
        div(style = "margin-top: -2em;"),
        h3(style = "color: red; font-weight: bold;", "Step 3"),
        p(style = "color: red; font-weight: bold;", "If your data set contains
          missing data, check the box below. If it doesn't, continue to Step 4."
        ),

        checkboxInput("missing_manual", strong("Data Set Contains Missing Data"),
          value = FALSE
        ),

        conditionalPanel(
          condition = "input.missing_manual",

          p("If the missing values in your data set are represented by empty
            cells, you may uncheck the box. If they are represented by something
            else (e.g., a period), specify the symbol, character, etc. that
            represents the missing values. If there are multiple, separate each
            by a comma. Any entries with missing data will appears as empty
            cells."
          ),

          textInput("denote_missing_manual", NULL, value = ".", width = "25%")
        )
      ),

      column(2,
        div(style = "margin-top: -2em;"),
        h3(style = "color: red; font-weight: bold;", "Step 4"),
        p(style = "color: red; font-weight: bold;", "Click the button below to
          store your data."
        ),

        actionButton("store_manual_df", "Store Data", class = "btn btn-primary"),

        conditionalPanel(
          condition = "input.store_manual_df",

          br(),

          uiOutput("store_manual_success")
        ),

        h3(style = "color: red; font-weight: bold;", "Step 5"),
        p(style = "color: red; font-weight: bold;", "Continue to one of the
          following:"
        ),

        p(style = "color: red; font-weight: bold;", "(1) the 'Generate Data' tab,
          if you want to add randomly generated data to your data set,"
        ),

        p(style = "color: red; font-weight: bold;", "(2) the 'Adjust Data' tab,
          if you want to update your existing data, or"
        ),

        p(style = "color: red; font-weight: bold;", "(3) the 'Check Data' tab, if
          you want to check the suitability of your data using statistical
          method(s)."
        )
      )
    )
  ),




  ##############################################################################
  ############################ Generate Data tabpanel ##########################
  ##############################################################################

  tabPanel("Generate Data", icon = icon("bolt"),

    column(3,
      div(style = "margin-top: -2em;"),
      h3(style = "color: red; font-weight: bold;", "Step 1"),
      p(style = "color: red; font-weight: bold;", "Generate data."),

      wellPanel(
        radioButtons("sim_var_type", "Variable Type",
          choices = c("Quantitative" = "quant", "Categorical" = "cat"),
          selected = "quant"
        ),

        ### randomly generate quantitative data
        conditionalPanel(
          condition = "input.sim_var_type == 'quant'",

          radioButtons("pop_option_quant", "Population Distribution",
            choices = c(
              "Select Shape" = "basic",
              "Select Distribution" = "advanced"
            )
          ),

          conditionalPanel(
            condition = "input.pop_option_quant == 'basic'",

            selectInput("pop_shape", "Shape", choices = shapes,
              selected = "Normal"
            ),

            conditionalPanel(
              condition = "input.pop_shape == 'Normal'",

              splitLayout(
                numericInput("norm_mean_basic", "Mean", value = 0),
                numericInput("norm_sd_basic", "SD", value = 1)
              )
            ),

            conditionalPanel(
              condition = "input.pop_shape != 'Normal'",

              splitLayout(
                numericInput("beta4_min", "Min", value = 0),
                numericInput("beta4_max", "Max", value = 100)
              )
            )
          ),

          conditionalPanel(
            condition = "input.pop_option_quant == 'advanced'",

            selectInput("pop_fam_quant", "Family",
              choices = families_quant,
              selected = "Normal"
            ),

            conditionalPanel(
              condition = "input.pop_fam_quant == 'Normal'",

              splitLayout(
                numericInput("norm_mean", "Mean", value = 0),
                numericInput("norm_sd", "SD", value = 1)
              )
            ),

            conditionalPanel(
              condition = "input.pop_fam_quant == 't'",

              numericInput("t_df", "DF", value = 10, width = "50%")
            ),

            conditionalPanel(
              condition = "input.pop_fam_quant == 'Chi-square'",

              numericInput("chisq_df", "DF", value = 5, width = "50%")
            ),

            conditionalPanel(
              condition = "input.pop_fam_quant == 'F'",

              splitLayout(
                numericInput("f_df1", "DF 1", value = 5),
                numericInput("f_df2", "DF 2", value = 5)
              )
            ),

            conditionalPanel(
              condition = "input.pop_fam_quant == 'Uniform (Continuous)'",

              splitLayout(
                numericInput("uni_min", "Min", value = 0),
                numericInput("uni_max", "Max", value = 1)
              )
            ),

            conditionalPanel(
              condition = "input.pop_fam_quant == 'Uniform (Discrete)'",

              textAreaInput("uni_discr_values", "Values (separated by commas)",
                placeholder = "1, 2, 3, 4"
              )
            ),

            conditionalPanel(
              condition = "input.pop_fam_quant == 'Poisson'",

              numericInput("pois_rate", "Rate", value = 5,
                width = "50%"
              )
            ),

            conditionalPanel(
              condition = "input.pop_fam_quant == 'Exponential'",

              numericInput("exp_rate", "Rate", value = 5, width = "50%")
            ),

            conditionalPanel(
              condition = "input.pop_fam_quant == 'Gamma'",

              splitLayout(
                numericInput("gamma_shape", "Shape", value = 5),
                numericInput("gamma_rate", "Rate", value = 5)
              )
            ),

            conditionalPanel(
              condition = "input.pop_fam_quant == 'Beta'",

              splitLayout(
                numericInput("beta_shape1", "Shape 1", value = 5),
                numericInput("beta_shape2", "Shape 2", value = 5)
              )
            )
          ),

          numericInput("samp_size_quant", "Sample Size", value = 20,
            width = "125px"
          ),

          numericInput("n_vars_quant", "Number of Samples", value = 1),
          tags$head(tags$style(
            type = "text/css", "#n_vars_quant{width: 125px;}"
          )),

          actionButton("gen_data_quant", "Generate Data",
            class = "btn btn-primary"
          )
        ),

        ### randomly generate categorical data
        conditionalPanel(
          condition = "input.sim_var_type == 'cat'",

          selectInput("pop_fam_cat", "Family", choices = families_cat,
            selected = "bin"
          ),

          conditionalPanel(
            condition = "input.pop_fam_cat == 'Binomial'",
            numericInput("binom_p", "Probability of Success", value = 0.5,
              step = 0.05
            ),
            tags$head(tags$style(type = "text/css", "#binom_p{width: 125px;}")),
          ),

          conditionalPanel(
            condition = "input.pop_fam_cat == 'Multinomial'",

            numericInput("multi_outcomes", "Number of Groups", value = 3),
            tags$head(tags$style(
              type = "text/css", "#multi_outcomes{width: 125px;}"
            )),

            # dynamic number of numeric inputs for probabilities
            uiOutput("multi_probs")
          ),

          numericInput("samp_size_cat", "Sample Size", value = 20,
            width = "125px"
          ),

          numericInput("n_vars_cat", "Number of Samples", value = 1),
          tags$head(tags$style(type = "text/css", "#n_vars_cat{width: 125px;}")),

          actionButton("gen_data_cat", "Generate Data",
            class = "btn btn-primary"
          )
        )
      )
    ),

    column(4,
      h4("Current Variable(s)"),
      DTOutput("sim_var")
    ),

    ### allow user to specify main name of new variable(s) and/or category labels
    column(2,
      div(style = "margin-top: -2em;"),
      h3(style = "color: red; font-weight: bold;", "Step 2"),
      p(style = "color: red; font-weight: bold;", "Specify the variable name. If
        there are multiple samples, the name will be followed by a different
        number for each. If you have categorical data, you can specify those
        labels as well."
      ),

      wellPanel(
        textInput("sim_var_name", "Variable Name", placeholder = "new.var"),

        conditionalPanel(
          condition = "input.sim_var_type == 'cat'",

          checkboxInput("cat_labels", "Add Group Labels", value = FALSE),

          conditionalPanel(
            condition = "input.cat_labels",

            conditionalPanel(
              condition = "input.pop_fam_cat == 'Binomial'",
              uiOutput("binom_labels")
            ),

            conditionalPanel(
              condition = "input.pop_fam_cat == 'Multinomial'",
              uiOutput("multinom_labels")
            )
          )
        ),

        actionButton("set_sim_names", "Set Name(s)", class = "btn btn-primary")
      )
    ),


    column(2,
      div(style = "margin-top: -2em;"),
      h3(style = "color: red; font-weight: bold;", "Step 3"),
      p(style = "color: red; font-weight: bold;", "Add the generated data to your
        existing data set. (If there is no existing data set, a new one will be
        created, and you can ignore the position selection.) Then continue to
        either the 'Adjust Data' tab or 'Check Data' tab."
      ),

      ### make button to add generated data to existing data set
      wellPanel(
        radioButtons("sim_vs_existing", "Generated data position",
          choices = c(
            "Before existing data" = "beginning",
            "After existing data" = "end"
          ),
          selected = "beginning"
        ),

        actionButton("add_sim_to_existing", "Add Generated Data",
          class = "btn btn-primary"
        ),

        conditionalPanel(
          condition = "input.add_sim_to_existing",

          br(),

          uiOutput("add_success")
        )
      )
    )
  ),




  ##############################################################################
  ############################# Adjust Data tabpanel ###########################
  ##############################################################################

  tabPanel("Adjust Data", icon = icon("exchange-alt"),

    column(12,
      fluidRow(
        column(3,
          div(style = "margin-top: -2em;"),
          h3(style = "color: red; font-weight: bold;", "Step 1"),
          p(style = "color: red; font-weight: bold;", "Add to or modify your
            data.  First select the variable(s) you want to modify,* and then how
            using the options below."
          )
        ),

        column(9,
          fluidRow(
            column(6,
              div(style = "margin-top: 2em;"),

              p(style = "color: red; font-weight: bold;", "* All selected
                variable(s) will be modified.  If you want to keep the original
                version of a variable, make a copy and then modify that."
              )
            ),

            column(6,
              div(style = "margin-top: -2em;"),

              h3(style = "color: red; font-weight: bold;", "Step 2"),
              p(style = "color: red; font-weight: bold;", "Continue to the 'Check
                Data' tab after making all desired adjustments. You can check any
                variables contained in the data set below."
              )
            )
          )
        )
      )
    ),

    column(3,
      wellPanel(

        # select variable
        strong("Variable(s) to Update"),

        p("Click the box below to view all of the variables. You can select
          multiple. To deselect one, click the variable name and then the
          backspace button."
        ),

        selectInput("select_var", NULL, choices = list(""), selected = NULL,
          multiple = TRUE
        ),

        # set min and/or max
        # checkboxInput("min_max_check", "Set Min and/or Max", value = FALSE),
        #
        # conditionalPanel(
        #   condition = "input.min_max_check",
        #
        #   p("Specify the minimum and/or maximum value the variable(s) can take
        #     on. If you only want to set one, leave the other input blank."
        #   ),
        #
        #   splitLayout(
        #     numericInput("set_min", "Min", value = -Inf),
        #     numericInput("set_max", "Max", value = Inf)
        #   ),
        #
        #   actionButton("set_min_max", "Set Min and/or Max",
        #     class = "btn btn-primary"
        #   )
        # ),

        # transform linearly
        checkboxInput("transform_lin_check", "Transform Variable: Linear Function",
          value = FALSE
        ),

        conditionalPanel(
          condition = "input.transform_lin_check",

          HTML("Transform each value of the selected variable using the
            formula:<br><b>new = a + b*(current)</b>."
          ),

          splitLayout(
            numericInput("linear_yint", "a", value = 0),
            numericInput("linear_slope", "b", value = 1)
          ),

          actionButton("transform_lin", "Transform Data",
            class = "btn btn-primary"
          )
        ),

        # apply user-specified transformation (mainly for non-linear)
        checkboxInput("transform_any_check", "Transform Variable: Custom Function",
          value = FALSE
        ),

        conditionalPanel(
          condition = "input.transform_any_check",

          p("Specify the function you want used to transform the data. Use 'x'
            (without quotes) instead of the variable name when typing the
            variable in the function. For natural log, log with base 10, and
            exponential functions, use log(), log10(), and exp(), respectively.
            See the 'Help' tab for more."
          ),

          textInput("transform_any_fxn", NULL,
            placeholder = "1.5*x^2 + exp(x) - 2*log(x) + log10(x)"
          ),

          actionButton("transform_any", "Transform Data",
            class = "btn btn-primary"
          )
        ),

        # combine variables
        checkboxInput("combine_vars_check", "Combine Variables", value = FALSE),

        conditionalPanel(
          condition = "input.combine_vars_check",

          # allow drop-down select inputs to be fully visible; otherwise most
          #  options cut off
          tags$head(tags$style(
            HTML(".shiny-split-layout > div {overflow: visible;}")
          )),


          actionButton("view_combo_options", "View Combination Options",
            class = "btn btn-primary"
          ),

          bsModal("combine_popup", NULL, "view_combo_options", size = "large",
            uiOutput("combine_popup")
          )
        ),

        # add noise
        checkboxInput("noise_check", "Add Noise", value = FALSE),

        conditionalPanel(
          condition = "input.noise_check",

          p("Add random noise from the normal distribution with the mean and SD
            below to each value of the selected variable."
          ),

          splitLayout(
            numericInput("noise_mean", "Mean", value = 0),
            numericInput("noise_sd", "SD", value = 1, min = 1e-10)
          ),

          actionButton("add_noise", "Add Noise", class = "btn btn-primary")
        ),

        # add outlier
        checkboxInput("outlier_check", "Add Outlier", value = FALSE),

        conditionalPanel(
          condition = "input.outlier_check",

          radioButtons("outlier_type", "How to Obtain Outlier",
            choices = c(
              "Convert Existing Value(s) to Outlier(s)" = "convert",
              "Add New Observation(s) as Outlier(s)" = "add"
            ),
            selected = "convert"
          ),

          conditionalPanel(
            condition = "input.outlier_type == 'convert'",

            p("Convert the largest or smallest value(s) of the selected
              variable to outlier(s)."
            ),

            radioButtons("outlier_convert_high_low", "",
              choices = c(
                "Largest to Outlier(s)" = "high",
                "Smallest to Outlier(s)" = "low"
              ),
              selected = "high"
            ),

            strong("Number of Values to Convert"),

            numericInput("n_outlier_convert", NULL, value = 1, width = "150px"),

            actionButton("convert_outlier", "Convert to Outlier",
              class = "btn btn-primary"
            )
          ),

          conditionalPanel(
            condition = "input.outlier_type == 'add'",

            p("Add new observation(s), which are outlier(s), to the data set."),

            radioButtons("outlier_add_high_low", "",
              choices = c(
                "Outlier(s) on High End" = "high",
                "Outlier(s) on Low End" = "low"
              ),
              selected = "high"
            ),

            strong("Number of Outliers to Add"),

            numericInput("n_outlier_add", NULL, value = 1, width = "150px"),

            actionButton("add_outlier", "Add Outlier(s)",
              class = "btn btn-primary"
            )
          )
        ),

        # force non-constant variance (regression)
        checkboxInput("force_NCV_check", "Force Non-constant Variance
          (Regression)",
          value = FALSE
        ),

        conditionalPanel(
          condition = "input.force_NCV_check",

          selectInput("force_NCV_expl", "Explanatory Variable", choices = "",
            selected = ""
          ),

          p("Specify the name you want used for the generated response variable."),

          textInput("response_name", "Response Variable Name", "response.var"),

          p("Specify the min and max values the response variable can take on."),

          splitLayout(
            numericInput("force_NCV_min_y", "Min", value = 0),
            numericInput("force_NCV_max_y", "Max", value = 100)
          ),

          selectInput("telescope_slope", "Slope of Regression Line",
            choices = c("Positive" = "pos", "Negative" = "neg"),
            selected = "pos",
            width = "75%"
          ),

          selectInput("telescope_direction", "Direction to Telescope Residuals",
            choices = c(
              "Increasing" = "inc",
              "Decreasing" = "dec",
              "Combo (Inc. then Dec.)" = "inc_dec",
              "Combo (Dec. then Inc.)" = "dec_inc"
            ),
            selected = "inc",
            width = "75%"
          ),

          p("The new response variable will be added in the first column."),

          actionButton("force_NCV", "Add Response Variable",
            class = "btn btn-primary"
          )
        ),

        # round to nearest digit
        checkboxInput("round_check", "Round Values to Nearest Digit",
          value = FALSE
        ),

        conditionalPanel(
          condition = "input.round_check",

          selectInput("round_digits", "Round to Nearest",
            choices = c(
              "Billion", "Hundred Million", "Ten Million", "Million",
              "Hundred Thousand", "Ten Thousand", "Thousand", "Hundred", "Ten",
              "Integer", "Tenth", "Hundredth", "Thousandth", "Ten Thousandth"
            ),
            selected = "Integer"
          ),

          actionButton("do_rounding", "Round Values",
            class = "btn btn-primary"
          )
        ),

        # round to specified min and/or max
        checkboxInput("round_min_max_check",
          "Round Values Outside Desired Range",
          value = FALSE
        ),

        conditionalPanel(
          condition = "input.round_min_max_check",

          p("Round all values below the specified minimum to that minimum and/or
            all values above the specified maximum to that maximum. Leave the min
            or max input blank if you don't want to round any values below/above
            it."
          ),

          splitLayout(
            numericInput("round_min", "Min", value = NULL),
            numericInput("round_max", "Max", value = NULL)
          ),

          actionButton("round_min_max", "Round Values",
            class = "btn btn-primary"
          )
        ),

        # apply floor or ceiling function
        checkboxInput("floor_ceil_check", "Round Values to Floor or Ceiling",
          value = FALSE
        ),

        conditionalPanel(
          condition = "input.floor_ceil_check",

          radioButtons("floor_or_ceil", "Choose floor or ceiling function",
            choices = c("Floor" = "floor", "Ceiling" = "ceiling"),
            selected = "floor"
          ),

          actionButton("round_floor_ceil", "Round Values",
            class = "btn btn-primary"
          )
        ),

        # adjust categorical variable counts
        checkboxInput("adjust_cat_check", "Adjust Categorical Variable Counts",
          value = FALSE
        ),

        conditionalPanel(
          condition = "input.adjust_cat_check",

          verbatimTextOutput("cat_var_cats"),

          p("Specify the proportions used when randomly sampling from a
            bi/multinomial distribution."
          ),

          uiOutput("cat_var_proportions"),

          actionButton("adjust_cat_counts", "Generate New Counts",
            class = "btn btn-primary"
          )
        ),

        # specify missing data
        checkboxInput("spec_missing_check", "Specify Missing Data",
          value = FALSE
        ),

        conditionalPanel(
          condition = "input.spec_missing_check",

          p("Specify the symbol, character, etc. you want treated as a missing
            value. After converting, it will appear as an empty cell."
          ),

          textInput("denote_missing", NULL, value = "."),

          actionButton("convert_to_missing", "Convert to Missing",
            class = "btn btn-primary"
          )
        ),

        # copy variable
        checkboxInput("copy_var_check", "Copy Variable", value = FALSE),

        conditionalPanel(
          condition = "input.copy_var_check",

          numericInput("num_copies", "Number of Copies", value = 1,
            width = "125px"
          ),

          actionButton("copy_var", "Copy Variable", class = "btn btn-primary")
        ),

        # remove variable(s)
        checkboxInput("remove_var_check", "Remove Variable", value = FALSE),

        conditionalPanel(
          condition = "input.remove_var_check",

          actionButton("remove_var", "Remove Variable(s)",
            class = "btn btn-primary"
          )
        ),

        # rename variable
        checkboxInput("rename_var_check", "Rename Variable", value = FALSE),

        conditionalPanel(
          condition = "input.rename_var_check",

          p("Only one variable at a time can be renamed."),

          textInput("new_name", "New Name (no spaces)",
            placeholder = "new.name"
          ),

          actionButton("rename_var", "Rename Variable",
            class = "btn btn-primary"
          )
        )
      )
    ),

    column(9,
      fluidRow(
        column(3,
          wellPanel(style = list("padding-bottom: 5px;"),
            selectInput("data_view", "Data Set View",
              choices = c("Table" = "table", "Spreadsheet" = "spreadsheet"),
              selected = "table"
            )
          ),

          wellPanel(style = list("padding-top: 10px; padding-bottom: 5px;"),

            # add row(s)
            checkboxInput("add_row_check", "Add Row(s)", value = FALSE),

            conditionalPanel(
              condition = "input.add_row_check",

              p("Note 1: All new rows will appear at the end of the data set."),
              p("Note 2: Data can be input manually by selecting 'Spreadsheet' in
                'Data Set View' above."
              ),

              numericInput("rows_to_add", "Number to Add", value = 1,
                width = "125px"
              ),

              actionButton("add_row", "Add Row(s)", class = "btn btn-primary")
            ),

            # remove row
            checkboxInput("remove_row_check", "Remove Row", value = FALSE),

            conditionalPanel(
              condition = "input.remove_row_check",

              numericInput("row_to_remove", "Row Number", value = 1,
                width = "125px"
              ),

              actionButton("remove_row", "Remove Row", class = "btn btn-primary")
            ),

            # reset data set to original
            checkboxInput("reset_check", "Reset Data Set", value = FALSE),

            conditionalPanel(
              condition = "input.reset_check",

              actionButton("reset_dataset", "Reset to Original",
                class = "btn btn-primary"
              )
            )
          )
        ),

        column(9,
          div(style = "margin-top: -0.7em;"),

          h4("Current Data Set"),

          conditionalPanel(
            condition = "input.data_view == 'table'",
            DTOutput("df_new_dt")
          ),

          conditionalPanel(
            condition = "input.data_view == 'spreadsheet'",
            rHandsontableOutput("df_new_rhot", height = "500px")
          )
        )
      )
    )
  ),




  ##############################################################################
  ############################## Check Data tabpanel ###########################
  ##############################################################################

  navbarMenu("Check Data", icon = icon("signal"),

    ### Check Data -- Descriptive Methods tabpanel
    #######################################################

    tabPanel("Descriptive Methods",

      ### need this to take screenshots
      ######################################
      # tags$head(
      #   # include html2canvas library
      #   tags$script(src = "http://html2canvas.hertzen.com/dist/html2canvas.min.js"),
      #   # script for creating the prompt for download
      #   tags$script(
      #     "
      #     function saveAs(uri, filename) {
      #
      #         var link = document.createElement('a');
      #
      #         if (typeof link.download === 'string') {
      #
      #             link.href = uri;
      #             link.download = filename;
      #
      #             //Firefox requires the link to be in the body
      #             document.body.appendChild(link);
      #
      #             //simulate click
      #             link.click();
      #
      #             //remove the link when done
      #             document.body.removeChild(link);
      #
      #         } else {
      #
      #             window.open(uri);
      #
      #         }
      #     }
      #     "
      #   )
      # ),
      #
      # useShinyjs(),


      ### descriptive statistics and plots -- inputs
      #################################################
      column(3,
        div(style = "margin-top: -2em;"),
        h3(style = "color: red; font-weight: bold;", "Step 1"),
        p(style = "color: red; font-weight: bold;", "Run the desired statistical
          method for the variable(s) of interest."
        ),

        wellPanel(
          radioButtons("descript_number_vars", "Number of Variables",
            choices = c(
              "1  (Univariate Analysis)" = "1",
              "2  (Bivariate Analysis)" = "2"
            ),
            selected = "1"
          ),

          conditionalPanel(
            condition = "input.descript_number_vars == '1'",

            selectInput("descript_select_1var_var", "Variable of Interest",
              choices = "", selected = ""
            ),

            radioButtons("descript_1_var_type", "Variable Type",
              choices = c("Quantitative" = "quant", "Categorical" = "cat"),
              selected = "quant"
            ),

            conditionalPanel(
              condition = "input.descript_1_var_type == 'quant'",

              actionButton("calc_descript_1_var_quant", "Calculate & Plot",
                class = "btn btn-primary"
              )
            ),

            conditionalPanel(
              condition = "input.descript_1_var_type == 'cat'",

              actionButton("calc_descript_1_var_cat", "Calculate & Plot",
                class = "btn btn-primary"
              )
            )
          ),

          conditionalPanel(
            condition = "input.descript_number_vars == '2'",

            selectInput("descript_select_2vars_var_resp", "Response Variable",
              choices = "", selected = ""
            ),

            selectInput("descript_select_2vars_var_expl",
              "Explanatory (or Group) Variable",
              choices = "", selected = ""
            ),

            radioButtons("descript_2_var_types", "Variable Types",
              choices = c(
                "Response = Quantitative; Explanatory = Quantitative" =
                  "quant_quant",
                "Response = Quantitative; Explanatory = Categorical" =
                  "quant_cat",
                "Response = Categorical; Explanatory = Categorical" =
                  "cat_cat"
              ),
              selected = "quant_quant"
            ),

            conditionalPanel(
              condition = "input.descript_2_var_types == 'quant_quant'",

              actionButton("calc_descript_2_var_quant_quant",
                "Calculate & Plot",
                class = "btn btn-primary"
              )
            ),

            conditionalPanel(
              condition = "input.descript_2_var_types == 'quant_cat'",

              actionButton("calc_descript_2_var_quant_cat", "Calculate & Plot",
                class = "btn btn-primary"
              )
            ),

            conditionalPanel(
              condition = "input.descript_2_var_types == 'cat_cat'",

              actionButton("calc_descript_2_var_cat_cat", "Calculate & Plot",
                class = "btn btn-primary"
              )
            )
          ) #,

          # linebreaks(1),
          #
          # actionButton("descrip_screenshot", "Screenshot Page",
          #   class = "btn btn-primary"
          # )
        )
      ),  # end column(3 for wellPanel


      ### descriptive statistics and plots -- outputs
      #################################################
      column(5,
        div(style = "margin-top: -2em;"),
        h3(style = "color: red; font-weight: bold;", "Step 2"),
        p(style = "color: red; font-weight: bold;", "Assess the suitability of
          the data, then repeat for all variables of interest. If necessary, make
          changes in the 'Adjust Data' tab and rerun Steps 1 and 2."
        )
      ),

      column(4,
        div(style = "margin-top: -2em;"),
        h3(style = "color: red; font-weight: bold;", "Step 3"),
        p(style = "color: red; font-weight: bold;", "When satisfied with all of
          the data, continue to the 'Download Data' tab."
        )
      ),

      column(9,
        conditionalPanel(
          condition = "input.descript_number_vars == '1'",

          ### one var -- quantitative
          conditionalPanel(
            condition = "input.descript_1_var_type == 'quant'",

            column(4,

              strong("Summaries"),
              verbatimTextOutput("descript_1_stats_quant"),
              tags$head(tags$style("#descript_1_stats_quant{width: 225px;}")),

              linebreaks(1),

              numericInput("percentile",
                "Calculate Percentile (input as proportion)",
                value = 0.9
              ),
              tags$head(tags$style("#percentile{width: 125px;}")),

              verbatimTextOutput("descript_1_percentile"),
              tags$head(tags$style("#descript_1_percentile{width: 125px;}"))
            ),

            column(6,
              div(
                style = "display: inline-block; vertical-align: top;",

                # stri_dup() here pads string w/ whitespace
                strong(paste0("Histogram of Data Values",
                  stri_dup(intToUtf8(160), 8)
                ))
              ),
              div(
                style = "display: inline-block; vertical-align:center;",
                p(paste0("# Bins:", stri_dup(intToUtf8(160), 1)))
              ),
              div(
                style = "display: inline-block",
                numericInput("descript_1_hist_bins", NULL, value = 15,
                  width = "65px"
                )
              ),
              plotOutput("descript_1_hist", height = "250px"),

              strong("Boxplot of Data Values"),
              plotOutput("descript_1_box", height = "250px")
            )
          ),

          ### one var -- categorical
          conditionalPanel(
            condition = "input.descript_1_var_type == 'cat'",

            column(4,
              strong("Summaries"),
              verbatimTextOutput("descript_1_stats_cat"),
              tags$head(tags$style("#descript_1_stats_cat{width: 225px;}")),

              linebreaks(2),

              strong("Counts and Percentages"),
              verbatimTextOutput("descript_1_counts_percents_cat"),
              tags$head(tags$style(
                "#descript_1_counts_percents_cat{width: 225px;}"
              ))
            ),

            column(6,
              strong("Bar Chart of Data"),
              plotOutput("descript_1_bar_cat", height = "250px"),

              strong("Pie Chart of Data"),
              plotlyOutput("descript_1_pie_cat", height = "250px")
            )
          )
        ),

        conditionalPanel(
          condition = "input.descript_number_vars == '2'",

          ### two vars -- quantitative vs. quantitative
          conditionalPanel(
            condition = "input.descript_2_var_types == 'quant_quant'",

            column(5,
              strong("Bivariate Summaries: Correlation and Regression"),
              verbatimTextOutput("descript_2_stats_quant_quant_bi"),
              tags$head(tags$style(
                "#descript_2_stats_quant_quant_bi{width: 250px;}"
              ))
            ),

            column(6,
              strong("Univariate Summaries"),
              verbatimTextOutput("descript_2_stats_quant_quant_uni"),
              tags$head(tags$style(
                "#descript_2_stats_quant_quant_uni{width: 400px;}"
              ))
            ),

            column(6,
              strong("Scatterplot"),
              plotOutput("descript_2_scatter", height = "300px")
            )
          ),

          ### two vars -- quantitative vs. categorical
          conditionalPanel(
            condition = "input.descript_2_var_types == 'quant_cat'",

            column(12,
              strong("Summaries: Response Variable by Explanatory Variable
                Group"
              ),

              verbatimTextOutput("descript_2_stats_quant_cat_bygroup"),

              tags$head(tags$style(
                "#descript_2_stats_quant_cat_bygroup{width: 750px;}"
              )),

              linebreaks(1)
            ),

            column(4,
              strong("# Missing Obs. in Grouping Variable"),

              verbatimTextOutput("descript_2_stats_quant_cat_NAs_x"),

              tags$head(tags$style(
                "#descript_2_stats_quant_cat_NAs_x{width: 100px;}"
              ))
            ),

            column(6,
              strong("Boxplots: Response Variable by Explanatory Variable
                Group"
              ),

              plotOutput("descript_2_boxplots", height = "350px")
            )
          ),

          ### two vars -- categorical vs. categorical
          conditionalPanel(
            condition = "input.descript_2_var_types == 'cat_cat'",

            column(5,
              strong("Counts"),
              verbatimTextOutput("descript_2_counts"),
            ),

            column(5,
              strong("Joint and Marginal Percentages"),
              verbatimTextOutput("descript_2_joint_marg")
            ),

            column(5,
              strong("Conditional Percentages: By Row"),
              verbatimTextOutput("descript_2_cond_byrow")
            ),

            column(5,
              strong("Conditional Percentages: By Column"),
              verbatimTextOutput("descript_2_cond_bycol")
            ),

            column(6,
              strong("Clustered Bar Chart: Colored by Response Variable Group"),
              plotOutput("descript_2_clustbar", height = "350px")
            ),

            column(1),

            column(3,
              strong("# Missing Observations"),

              verbatimTextOutput("descript_2_NAs"),

              tags$head(tags$style("#descript_2_NAs{width: 100px;}"))
            )
          )
        )
      )
    ),  # end descriptive methods tabPanel



    ### Check Data -- Inferential Methods tabpanel
    #######################################################
    tabPanel("Inferential Methods",

      ### inferential methods -- inputs
      #################################################

      column(3,
        div(style = "margin-top: -2em;"),
        h3(style = "color: red; font-weight: bold;", "Step 1"),
        p(style = "color: red; font-weight: bold;", "Run the desired statistical
          method for the variable(s) of interest."
        ),

        wellPanel(
          selectInput("inference_method", "Select Procedure",
            choices = c(
              "One-Sample T" = "t_1",
              "One-Proportion Z" = "z_1",
              "Paired Samples T" = "t_paired",
              "Independent Samples T" = "t_indep",
              "Two-Proportion Z (Independent)" = "z_2",
              "One-Way ANOVA" = "anova",
              "Simple Linear Regression" = "regression",
              "Chi-square Goodness-of-fit Test" = "cs_gof",
              "Chi-square Test of Independence" = "cs_indep",
              "Chi-square Test of Homogeneity" = "cs_homog"
            )
          ),

          ### one-sample t procedures
          conditionalPanel(
            condition = "input.inference_method == 't_1'",

            selectInput("t_1_select_var", "Quantitative Variable", choices = "",
              selected = ""
            ),

            radioButtons("t_1_inf_type", "Type of Inferential Method",
              choices = c(
                "Hypothesis Test" = "ht",
                "Confidence Interval" = "ci"
              ),
              selected = "ht"
            ),

            conditionalPanel(
              condition = "input.t_1_inf_type == 'ht'",

              numericInput("t_1_null_value", "Test Value (Mean)", value = 0)
            ),

            conditionalPanel(
              condition = "input.t_1_inf_type == 'ci'",

              numericInput("t_1_conf_level", "Confidence Level (%)",
                value = 95
              )
            ),

            actionButton("run_t_1", "Run Procedure", class = "btn btn-primary")
          ),

          ### one-proportion z procedures
          conditionalPanel(
            condition = "input.inference_method == 'z_1'",

            selectInput("z_1_select_var", "Variable of Interest", choices = "",
              selected = ""
            ),

            actionButton("set_z_1_var", "Set Variable",
              class = "btn btn-primary"
            ),

            conditionalPanel(
              condition = "input.set_z_1_var",

              linebreaks(1),

              selectInput("z_1_success", "Success Category", choices = "",
                selected = ""
              ),

              radioButtons("z_1_inf_type", "Type of Inferential Method",
                choices = c(
                  "Hypothesis Test" = "ht",
                  "Confidence Interval" = "ci"
                ),
                selected = "ht"
              ),

              conditionalPanel(
                condition = "input.z_1_inf_type == 'ht'",

                numericInput("z_1_null_value", "Test Value (Proportion)",
                  value = 0.5
                )
              ),

              conditionalPanel(
                condition = "input.z_1_inf_type == 'ci'",

                numericInput("z_1_conf_level", "Confidence Level (%)",
                  value = 95
                )
              ),

              actionButton("run_z_1", "Run Procedure",
                class = "btn btn-primary"
              )
            )
          ),

          ### paired-samples t procedures
          conditionalPanel(
            condition = "input.inference_method == 't_paired'",

            selectInput("t_paired_select_var_1", "Quantitative Variable (Group 1)",
              choices = "", selected = ""
            ),
            selectInput("t_paired_select_var_2", "Quantitative Variable (Group 2)",
              choices = "", selected = ""
            ),

            radioButtons("t_paired_inf_type", "Type of Inferential Method",
              choices = c(
                "Hypothesis Test" = "ht",
                "Confidence Interval" = "ci"
              ),
              selected = "ht"
            ),

            conditionalPanel(
              condition = "input.t_paired_inf_type == 'ht'",

              numericInput("t_paired_null_value",
                "Test Value (Mean Difference)",
                value = 0
              )
            ),

            conditionalPanel(
              condition = "input.t_paired_inf_type == 'ci'",

              numericInput("t_paired_conf_level", "Confidence Level (%)",
                value = 95
              )
            ),

            actionButton("run_t_paired", "Run Procedure",
              class = "btn btn-primary"
            )
          ),

          ### independent samples t procedures
          conditionalPanel(
            condition = "input.inference_method == 't_indep'",

            radioButtons("t_indep_data_format", "Select Data Format",
              choices = c(
                # mm = measurement measurement, mg = measurement group
                "Each column = quantitative data for individual group" = "mm",
                "One column contains all quantitative data & another column
                 contains both group identifiers" = "mg"
              ),
              selected = "mm"
            ),

            conditionalPanel(
              condition = "input.t_indep_data_format == 'mm'",

              # mm = measurement measurement
              selectInput("t_indep_select_var_1mm", "Group 1 Quantitative
                Variable", choices = "", selected = ""
              ),

              selectInput("t_indep_select_var_2mm", "Group 2 Quantitative
                Variable", choices = "", selected = ""
              ),

              radioButtons("t_indep_mm_inf_type", "Type of Inferential Method",
                choices = c(
                  "Hypothesis Test" = "ht",
                  "Confidence Interval" = "ci"
                ),
                selected = "ht"
              ),

              conditionalPanel(
                condition = "input.t_indep_mm_inf_type == 'ht'",

                numericInput("t_indep_mm_null_value",
                  "Test Value (Difference of Means)",
                  value = 0
                )
              ),

              conditionalPanel(
                condition = "input.t_indep_mm_inf_type == 'ci'",

                numericInput("t_indep_mm_conf_level", "Confidence Level (%)",
                  value = 95
                )
              ),

              actionButton("run_t_indep_mm", "Run Procedure",
                class = "btn btn-primary"
              )
            ),

            conditionalPanel(
              condition = "input.t_indep_data_format == 'mg'",

              # mg = measurement group
              selectInput("t_indep_select_var_1mg",
                "Variable with All Quantitative Data",
                choices = "", selected = ""
              ),

              selectInput("t_indep_select_var_2mg",
                "Variable with Group Identifiers",
                choices = "", selected = ""
              ),

              actionButton("set_t_indep_mg", "Set Variables",
                class = "btn btn-primary"
              ),

              conditionalPanel(
                condition = "input.set_t_indep_mg",

                linebreaks(1),

                selectInput("t_indep_mg_grp_1", "Group 1 Identifier",
                  choices = "", selected = ""
                ),

                selectInput("t_indep_mg_grp_2", "Group 2 Identifier",
                  choices = "", selected = ""
                ),

                radioButtons("t_indep_mg_inf_type",
                  "Type of Inferential Method",
                  choices = c(
                    "Hypothesis Test" = "ht",
                    "Confidence Interval" = "ci"
                  ),
                  selected = "ht"
                ),

                conditionalPanel(
                  condition = "input.t_indep_mg_inf_type == 'ht'",

                  numericInput("t_indep_mg_null_value",
                    "Test Value (Difference of Means)",
                    value = 0
                  )
                ),

                conditionalPanel(
                  condition = "input.t_indep_mg_inf_type == 'ci'",

                  numericInput("t_indep_mg_conf_level", "Confidence Level (%)",
                    value = 95
                  )
                ),

                actionButton("run_t_indep_mg", "Run Procedure",
                  class = "btn btn-primary"
                )
              )
            )
          ),

          ### two-proportion z procedures
          conditionalPanel(
            condition = "input.inference_method == 'z_2'",

            radioButtons("z_2_data_format", "Select Data Format",
              choices = c(
                # mm = measurement measurement, mg = measurement group
                "Column 1 = group 1 data, Column 2 = group 2 data" = "mm",
                "Column 1 = group 1 and 2 data, Column 2 = group identifiers" =
                  "mg"
              ),
              selected = "mm"
            ),

            conditionalPanel(
              condition = "input.z_2_data_format == 'mm'",

              selectInput("z_2_select_var_1mm", "Group 1 Data Variable",
                choices = "", selected = ""
              ),

              selectInput("z_2_select_var_2mm", "Group 2 Data Variable",
                choices = "", selected = ""
              ),

              actionButton("set_z_2_mm_vars", "Set Variables",
                class = "btn btn-primary"
              ),

              linebreaks(1),

              conditionalPanel(
                condition = "input.set_z_2_mm_vars",

                selectInput("z_2_mm_success", "Success Category", choices = "",
                  selected = ""
                ),

                radioButtons("z_2_mm_inf_type", "Type of Inferential Method",
                  choices = c(
                    "Hypothesis Test" = "ht",
                    "Confidence Interval" = "ci"
                  ),
                  selected = "ht"
                ),

                conditionalPanel(
                  condition = "input.z_2_mm_inf_type == 'ht'",

                  numericInput("z_2_mm_null_value",
                    "Test Value (Difference of Proportions)",
                    value = 0
                  )
                ),

                conditionalPanel(
                  condition = "input.z_2_mm_inf_type == 'ci'",

                  numericInput("z_2_mm_conf_level", "Confidence Level (%)",
                    value = 95
                  )
                ),

                actionButton("run_z_2_mm", "Run Procedure",
                  class = "btn btn-primary"
                )
              )
            ),

            conditionalPanel(
              condition = "input.z_2_data_format == 'mg'",

              selectInput("z_2_select_var_1mg",
                "Variable with Measurement Data",
                choices = "", selected = ""
              ),

              selectInput("z_2_select_var_2mg",
                "Variable with Group Identifiers",
                choices = "", selected = ""
              ),

              actionButton("set_z_2_mg_vars", "Set Variables",
                class = "btn btn-primary"
              ),

              linebreaks(1),

              conditionalPanel(
                condition = "input.set_z_2_mg_vars",

                selectInput("z_2_mg_success", "Success Category", choices = "",
                  selected = ""
                ),

                selectInput("z_2_mg_group_1", "Group 1 Identifier",
                  choices = "", selected = ""
                ),

                selectInput("z_2_mg_group_2", "Group 2 Identifier",
                  choices = "", selected = ""
                ),

                radioButtons("z_2_mg_inf_type", "Type of Inferential Method",
                  choices = c(
                    "Hypothesis Test" = "ht",
                    "Confidence Interval" = "ci"
                  ),
                  selected = "ht"
                ),

                conditionalPanel(
                  condition = "input.z_2_mg_inf_type == 'ht'",

                  numericInput("z_2_mg_null_value",
                    "Test Value (Difference of Proportions)",
                    value = 0
                  )
                ),

                conditionalPanel(
                  condition = "input.z_2_mg_inf_type == 'ci'",

                  numericInput("z_2_mg_conf_level", "Confidence Level (%)",
                    value = 95
                  )
                ),

                actionButton("run_z_2_mg", "Run Procedure",
                  class = "btn btn-primary"
                )
              )
            )
          ),

          ### one-way anova
          conditionalPanel(
            condition = "input.inference_method == 'anova'",

            radioButtons("anova_data_format", "Select Data Format",
              choices = c(
                # mm = measurement measurement, mg = measurement group
                "Each column = quantitative data for individual group" = "mm",
                "One column contains all quantitative data & another column
                 contains all group identifiers" = "mg"
              ),
              selected = "mm"
            ),

            conditionalPanel(
              condition = "input.anova_data_format == 'mm'",

              # mm = measurement measurement
              numericInput("anova_num_grps", "Number of Groups", value = 3),

              # dynamic number of select inputs
              uiOutput("anova_select_groups"),

              numericInput("anova_mm_mult_compars_level",
                "Pairwise Comparisons: Family-wise Confidence Level (%)",
                value = 95
              ),

              actionButton("run_anova_mm", "Run Procedure",
                class = "btn btn-primary"
              )
            ),

            conditionalPanel(
              condition = "input.anova_data_format == 'mg'",

              # mg = measurement group
              selectInput("anova_select_var_1mg",
                "Variable with Quantitative Data",
                choices = "", selected = ""
              ),

              selectInput("anova_select_var_2mg",
                "Variable with Group Identifiers",
                choices = "", selected = ""
              ),

              numericInput("anova_mg_mult_compars_level",
                "Pairwise Comparisons: Family-wise Confidence Level (%)",
                value = 95
              ),

              actionButton("run_anova_mg", "Run Procedure",
                class = "btn btn-primary"
              )
            )
          ),

          ### simple linear regression (inference)
          conditionalPanel(
            condition = "input.inference_method == 'regression'",

            selectInput("reg_inference_select_resp_var", "Response Variable",
              choices = "", selected = ""
            ),

            selectInput("reg_inference_select_expl_var",
              "Explanatory Variable",
              choices = "", selected = ""
            ),

            actionButton("run_reg_inference", "Calculate",
              class = "btn btn-primary"
            )
          ),

          ### chi-square goodness-of-fit test
          conditionalPanel(
            condition = "input.inference_method == 'cs_gof'",

            radioButtons("cs_gof_data_format", "Select Data Format",
              choices = c(
                "Use column with individual observations" = "obs",
                "Input group counts" = "counts"
              ),
              selected = "obs"
            ),

            conditionalPanel(
              condition = "input.cs_gof_data_format == 'obs'",

              selectInput("cs_gof_select_var", "Categorical Variable",
                choices = "", selected = ""
              ),

              actionButton("set_cs_gof_obs_var", "Select Variable",
                class = "btn btn-primary"
              ),

              conditionalPanel(
                condition = "input.set_cs_gof_obs_var",

                linebreaks(1),

                strong(style = "color: green;", "*The groups below and in the
                  output are ordered alphabetically if letters/words are used.*"
                ),

                linebreaks(1),

                verbatimTextOutput("cs_gof_obs_cats"),

                uiOutput("cs_gof_obs_null_values"),

                actionButton("run_cs_gof_obs", "Run Procedure",
                  class = "btn btn-primary"
                )
              )
            ),

            conditionalPanel(
              condition = "input.cs_gof_data_format == 'counts'",

              textAreaInput("cs_gof_counts", "Type the counts below,
                separating each by a comma.",
                placeholder = "2, 3, 4"
              ),

              uiOutput("cs_gof_counts_null_values"),

              actionButton("run_cs_gof_counts", "Run Procedure",
                class = "btn btn-primary"
              )
            )
          ),

          ### chi-square test of independence
          conditionalPanel(
            condition = "input.inference_method == 'cs_indep'",

            radioButtons("cs_indep_data_format", "Select Data Format",
              choices = c(
                "Use columns with individual observations" = "dataset",
                "Input data manually" = "manual"
              ),
              selected = "dataset"
            ),

            conditionalPanel(
              condition = "input.cs_indep_data_format == 'dataset'",

              selectInput("cs_indep_select_var_1", "Categorical Variable 1",
                choices = "", selected = ""
              ),

              selectInput("cs_indep_select_var_2", "Categorical Variable 2",
                choices = "", selected = ""
              ),

              actionButton("run_cs_indep_dataset", "Run Test",
                class = "btn btn-primary"
              )
            ),

            conditionalPanel(
              condition = "input.cs_indep_data_format == 'manual'",

              p("Type the categories below, separated by commas."),

              textAreaInput("cs_indep_manual_1", "Variable 1 Categories",
                placeholder = "fresh, soph, junior, senior"
              ),

              textAreaInput("cs_indep_manual_2", "Variable 2 Categories",
                placeholder = "short, medium, tall"
              ),

              actionButton("set_cs_indep_manual_cats", "Set Categories",
                class = "btn btn-primary"
              )
            )
          ),

          ### chi-square test of homogeneity
          conditionalPanel(
            condition = "input.inference_method == 'cs_homog'",

            radioButtons("cs_homog_data_format", "Select Data Format",
              choices = c(
                "Use columns with individual observations" = "dataset",
                "Input data manually" = "manual"
              ),
              selected = "dataset"
            ),

            conditionalPanel(
              condition = "input.cs_homog_data_format == 'dataset'",

              selectInput("cs_homog_select_var_1", "Categorical Variable 1",
                choices = "", selected = ""
              ),

              selectInput("cs_homog_select_var_2", "Categorical Variable 2",
                choices = "", selected = ""
              ),

              actionButton("run_cs_homog_dataset", "Run Test",
                class = "btn btn-primary"
              )
            ),

            conditionalPanel(
              condition = "input.cs_homog_data_format == 'manual'",

              p("Type the category and group names below, separated by
                commas."
              ),

              textAreaInput("cs_homog_manual_1", "Variable Categories",
                placeholder = "short, medium, tall"
              ),

              textAreaInput("cs_homog_manual_2", "Group Names",
                placeholder = "fresh, soph, junior, senior"
              ),

              actionButton("set_cs_homog_manual_cats", "Set Categories",
                class = "btn btn-primary"
              )
            )
          ) #,

          # linebreaks(1),
          #
          # actionButton("inf_screenshot", "Screenshot Page",
          #   class = "btn btn-primary"
          # )
        )
      ),  #


      ### inferential methods -- output
      #################################################
      column(5,
        div(style = "margin-top: -2em;"),
        h3(style = "color: red; font-weight: bold;", "Step 2"),
        p(style = "color: red; font-weight: bold;", "Assess the suitability of
          the data, then repeat for all variables of interest. If necessary, make
          changes in the 'Adjust Data' tab and rerun Steps 1 and 2."
        )
      ),

      column(4,
        div(style = "margin-top: -2em;"),
        h3(style = "color: red; font-weight: bold;", "Step 3"),
        p(style = "color: red; font-weight: bold;", "When satisfied with all of
          the data, continue to the 'Download Data' tab."
        )
      ),

      column(9,

        ### one-sample t procedures
        conditionalPanel(
          condition = "input.inference_method == 't_1'",

          column(5,
            strong("Summaries"),
            verbatimTextOutput("t_1_summaries"),
            tags$head(tags$style("#t_1_summaries{width: 225px;}")),
            linebreaks(1),

            strong("Hypothesis Test Results"),
            verbatimTextOutput("t_1_test"),
            tags$head(tags$style("#t_1_test{width: 275px;}")),
            linebreaks(1),

            strong("Confidence Interval"),
            verbatimTextOutput("t_1_ci"),
            tags$head(tags$style("#t_1_ci{width: 225px;}"))
          ),

          column(6,
            strong("Histogram of Data Values"),
            linebreaks(1),

            # stri_dup() here pads string w/ whitespace
            div(
              style = "display: inline-block;",
              p(paste0("# Bins:", stri_dup(intToUtf8(160), 1)))
            ),
            div(
              style = "display: inline-block",
              numericInput("hist_t_1_bins", NULL, value = 15,
                width = "65px"
              )
            ),

            plotOutput("hist_t_1", height = "250px"),

            strong("Normal QQ Plot of Data Values"),
            plotOutput("norm_qq_t_1", height = "250px")
          )
        ),

        ### one-proportion z procedures
        conditionalPanel(
          condition = "input.inference_method == 'z_1'",

          column(4,
            strong("Summaries"),
            verbatimTextOutput("z_1_summaries"),
            tags$head(tags$style("#z_1_summaries{width: 225px;}")),

            linebreaks(2),

            strong("Assumption Checking Information"),
            verbatimTextOutput("z_1_assumptions"),
            tags$head(tags$style("#z_1_assumptions{width: 250px;}"))
          ),

          column(4,
            strong("Hypothesis Test Results"),
            verbatimTextOutput("z_1_test"),
            tags$head(tags$style("#z_1_test{width: 275px;}")),

            linebreaks(1),

            strong("Confidence Interval"),
            verbatimTextOutput("z_1_ci"),
            tags$head(tags$style("#z_1_ci{width: 225px;}"))
          )
        ),

        ### paired-samples t procedures
        conditionalPanel(
          condition = "input.inference_method == 't_paired'",

          p("If you already have a variable with the differenced data, use the
            One-Sample T procedure with that variable."
          ),
          p("The output below assumes the differenced data were calculated
            using: Group 1 - Group 2."
          ),

          column(5,
            strong("Groups"),
            verbatimTextOutput("t_paired_names"),
            tags$head(tags$style("#t_paired_names{width: 275px;}")),
            linebreaks(1),

            strong("Summaries of Differenced Data"),
            verbatimTextOutput("t_paired_summaries"),
            tags$head(tags$style("#t_paired_summaries{width: 275px;}")),
            linebreaks(1),

            strong("Hypothesis Test Results"),
            verbatimTextOutput("t_paired_test"),
            tags$head(tags$style("#t_paired_test{width: 275px;}")),
            linebreaks(1),

            strong("Confidence Interval"),
            verbatimTextOutput("t_paired_ci"),
            tags$head(tags$style("#t_paired_ci{width: 225px;}"))
          ),

          column(5,
            strong("Histogram of Differenced Data Values"),
            linebreaks(1),

            # stri_dup() here pads string w/ whitespace
            div(
              style = "display: inline-block;",
              p(paste0("# Bins:", stri_dup(intToUtf8(160), 1)))
            ),
            div(
              style = "display: inline-block",
              numericInput("hist_t_paired_bins", NULL, value = 15,
                width = "65px"
              )
            ),

            plotOutput("hist_t_paired", height = "225px"),

            strong("Normal QQ Plot of Differenced Data Values"),
            plotOutput("norm_qq_t_paired", height = "225px")
          )
        ),

        ### independent samples t procedures
        conditionalPanel(
          condition = "input.inference_method == 't_indep'",

          p("The output below assumes the parameter of interest is (Population
            mean 1 - Population mean 2) based on groups 1 and 2, respectively."
          ),

          conditionalPanel(
            condition = "input.t_indep_data_format == 'mm'",

            column(4,
              strong("Groups"),
              verbatimTextOutput("t_indep_mm_names"),

              strong("Summaries by Group"),
              verbatimTextOutput("t_indep_mm_summaries"),

              strong("Levene's Test Results"),
              verbatimTextOutput("t_indep_mm_out_levene"),

              strong("T Procedure Results Assuming Equal Variances"),
              verbatimTextOutput("t_indep_mm_test_out_equal"),
              verbatimTextOutput("t_indep_mm_ci_out_equal"),

              strong("T Procedure Results Assuming Unequal Variances"),
              verbatimTextOutput("t_indep_mm_test_out_unequal"),
              verbatimTextOutput("t_indep_mm_ci_out_unequal")
            ),

            column(4,
              strong("Histogram of Group 1 Data"),
              linebreaks(1),

              # stri_dup() here pads string w/ whitespace
              div(
                style = "display: inline-block;", # vertical-align: center;",
                p(paste0("# Bins:", stri_dup(intToUtf8(160), 1)))
              ),
              div(
                style = "display: inline-block",
                numericInput("hist_t_indep_mm_1_bins", NULL, value = 15,
                  width = "65px"
                )
              ),

              plotOutput("hist_t_indep_mm_1", height = "225px"),

              strong("Normal QQ Plot of Group 1 Data"),
              plotOutput("norm_qq_t_indep_mm_1", height = "225px")
            ),

            column(4,
              strong("Histogram of Group 2 Data"),
              linebreaks(1),

              # stri_dup() here pads string w/ whitespace
              div(
                style = "display: inline-block;", # vertical-align: center;",
                p(paste0("# Bins:", stri_dup(intToUtf8(160), 1)))
              ),
              div(
                style = "display: inline-block",
                numericInput("hist_t_indep_mm_2_bins", NULL, value = 15,
                  width = "65px"
                )
              ),

              plotOutput("hist_t_indep_mm_2", height = "225px"),

              strong("Normal QQ Plot of Group 2 Data"),
              plotOutput("norm_qq_t_indep_mm_2", height = "225px")
            )
          ),

          conditionalPanel(
            condition = "input.t_indep_data_format == 'mg'",

            column(4,
              strong("Groups"),
              verbatimTextOutput("t_indep_mg_names"),

              strong("Summaries by Group"),
              verbatimTextOutput("t_indep_mg_summaries"),

              strong("Levene's Test Results"),
              verbatimTextOutput("t_indep_mg_out_levene"),

              strong("T Procedure Results Assuming Equal Variances"),
              verbatimTextOutput("t_indep_mg_test_out_equal"),
              verbatimTextOutput("t_indep_mg_ci_out_equal"),

              strong("T Procedure Results Assuming Unequal Variances"),
              verbatimTextOutput("t_indep_mg_test_out_unequal"),
              verbatimTextOutput("t_indep_mg_ci_out_unequal")
            ),

            column(4,
              strong("Histogram of Group 1 Data"),
              linebreaks(1),

              # stri_dup() here pads string w/ whitespace
              div(
                style = "display: inline-block;",
                p(paste0("# Bins:", stri_dup(intToUtf8(160), 1)))
              ),
              div(
                style = "display: inline-block",
                numericInput("hist_t_indep_mg_1_bins", NULL, value = 15,
                  width = "65px"
                )
              ),

              plotOutput("hist_t_indep_mg_1", height = "225px"),

              strong("Normal QQ Plot of Group 1 Data"),
              plotOutput("norm_qq_t_indep_mg_1", height = "225px")
            ),

            column(4,
              strong("Histogram of Group 2 Data"),
              linebreaks(1),

              # stri_dup() here pads string w/ whitespace
              div(
                style = "display: inline-block;",
                p(paste0("# Bins:", stri_dup(intToUtf8(160), 1)))
              ),
              div(
                style = "display: inline-block",
                numericInput("hist_t_indep_mg_2_bins", NULL, value = 15,
                  width = "65px"
                )
              ),

              plotOutput("hist_t_indep_mg_2", height = "225px"),

              strong("Normal QQ Plot of Group 2 Data"),
              plotOutput("norm_qq_t_indep_mg_2", height = "225px")
            )
          )
        ),

        ### two-proportion z procedures
        conditionalPanel(
          condition = "input.inference_method == 'z_2'",

          p("The output below assumes the parameter of interest is (Population
            proportion 1 - Population proportion 2) based on groups 1 and 2,
            respectively."
          ),

          conditionalPanel(
            condition = "input.z_2_data_format == 'mm'",

            column(5,
              strong("Groups"),
              verbatimTextOutput("z_2_mm_names"),
              tags$head(tags$style("#z_2_mm_names{width: 325px;}")),
              linebreaks(1),

              strong("Summaries"),
              verbatimTextOutput("z_2_mm_summaries"),
              tags$head(tags$style("#z_2_mm_summaries{width: 325px;}"))
            ),

            column(4,
              strong("Hypothesis Test Results"),
              verbatimTextOutput("z_2_mm_test"),
              tags$head(tags$style("#z_2_mm_test{width: 275px;}")),
              linebreaks(1),

              strong("Confidence Interval"),
              verbatimTextOutput("z_2_mm_ci"),
              tags$head(tags$style("#z_2_mm_ci{width: 225px;}"))
            )
          ),

          conditionalPanel(
            condition = "input.z_2_data_format == 'mg'",

            column(5,
              strong("Groups"),
              verbatimTextOutput("z_2_mg_names"),
              tags$head(tags$style("#z_2_mg_names{width: 325px;}")),
              linebreaks(1),

              strong("Summaries"),
              verbatimTextOutput("z_2_mg_summaries"),
              tags$head(tags$style("#z_2_mg_summaries{width: 325px;}"))
            ),

            column(4,
              strong("Hypothesis Test Results"),
              verbatimTextOutput("z_2_mg_test"),
              tags$head(tags$style("#z_2_mg_test{width: 275px;}")),
              linebreaks(1),

              strong("Confidence Interval"),
              verbatimTextOutput("z_2_mg_ci"),
              tags$head(tags$style("#z_2_mg_ci{width: 225px;}"))
            )
          )
        ),

        ### one-way anova
        conditionalPanel(
          condition = "input.inference_method == 'anova'",

          conditionalPanel(
            condition = "input.anova_data_format == 'mm'",

            column(7,
              column(7,
                strong("Groups"),
                verbatimTextOutput("anova_mm_names")
              ),

              column(5,
                strong("Levene's Test"),
                verbatimTextOutput("anova_mm_levene")
              ),

              column(12,
                strong("Summaries"),
                verbatimTextOutput("anova_mm_stats")
              ),

              column(12,
                strong("ANOVA Table"),
                verbatimTextOutput("anova_mm_output")
              )
            ),

            column(5,
              strong("Boxplots of Response Values by Group"),
              plotOutput("boxplots_anova_mm", height = "250px")
            ),

            column(12,
              column(4,
                strong("Histogram of Residuals"),
                linebreaks(1),

                # stri_dup() here pads string w/ whitespace
                div(
                  style = "display: inline-block;",
                  p(paste0("# Bins:", stri_dup(intToUtf8(160), 1)))
                ),
                div(
                  style = "display: inline-block",
                  numericInput("hist_anova_mm_resids_bins", NULL, value = 15,
                    width = "65px"
                  )
                ),

                plotOutput("hist_anova_mm", height = "250px")
              ),

              column(4,
                strong("Normal QQ Plot of Residuals"),
                plotOutput("norm_qq_anova_mm", height = "250px")
              ),

              column(4,
                strong("Residual Plot"),
                plotOutput("resid_plot_anova_mm", height = "250px")
              )
            ),

            column(10,
              strong("Pairwise Comparisons using Tukey's HSD"),
              verbatimTextOutput("anova_mm_tukey"),
              tags$head(tags$style("#anova_mm_tukey{width: 550px;}")),
            ),

            column(10,
              strong("Pairwise Comparisons using a Bonferroni Adjustment"),
              verbatimTextOutput("anova_mm_bonf"),
              tags$head(tags$style("#anova_mm_bonf{width: 550px;}")),
            )
          ),

          conditionalPanel(
            condition = "input.anova_data_format == 'mg'",

            column(7,
              column(12,
                strong("Summaries"),
                verbatimTextOutput("anova_mg_stats")
              ),

              column(12,
                strong("ANOVA Table"),
                verbatimTextOutput("anova_mg_output")
              ),

              column(6,
                strong("Levene's Test"),
                verbatimTextOutput("anova_mg_levene")
              )
            ),

            column(5,
              strong("Boxplots of Response Values by Group"),
              plotOutput("boxplots_anova_mg", height = "250px")
            ),

            column(12,
              column(4,
                strong("Histogram of Residuals"),
                linebreaks(1),

                # stri_dup() here pads string w/ whitespace
                div(
                  style = "display: inline-block;",
                  p(paste0("# Bins:", stri_dup(intToUtf8(160), 1)))
                ),
                div(
                  style = "display: inline-block",
                  numericInput("hist_anova_mg_resids_bins", NULL, value = 15,
                    width = "65px"
                  )
                ),

                plotOutput("hist_anova_mg", height = "250px")
              ),

              column(4,
                strong("Normal QQ Plot of Residuals"),
                plotOutput("norm_qq_anova_mg", height = "250px")
              ),

              column(4,
                strong("Residual Plot"),
                plotOutput("resid_plot_anova_mg", height = "250px")
              )
            ),

            column(10,
              strong("Pairwise Comparisons using Tukey's HSD"),
              verbatimTextOutput("anova_mg_tukey"),
              tags$head(tags$style("#anova_mg_tukey{width: 550px;}")),
            ),

            column(10,
              strong("Pairwise Comparisons using a Bonferroni Adjustment"),
              verbatimTextOutput("anova_mg_bonf"),
              tags$head(tags$style("#anova_mg_bonf{width: 550px;}")),
            )
          )
        ),

        ### simple linear regression (inference)
        conditionalPanel(
          condition = "input.inference_method == 'regression'",

          column(7,
            strong("Regression Inference Output"),
            verbatimTextOutput("reg_inference_out"),

            strong("Scatterplot"),
            plotOutput("reg_inference_scatter", height = "300px"),

            strong("Residual Plot"),
            plotOutput("resid_plot_reg", height = "200px")
          ),

          column(5,
            strong("Regression Summaries"),
            verbatimTextOutput("reg_inference_summaries"),
            tags$head(tags$style("#reg_inference_summaries{width: 250px;}")),

            strong("Histogram of Residuals"),
            linebreaks(1),

            # stri_dup() here pads string w/ whitespace
            div(
              style = "display: inline-block;",
              p(paste0("# Bins:", stri_dup(intToUtf8(160), 1)))
            ),
            div(
              style = "display: inline-block",
              numericInput("hist_reg_resids_bins", NULL, value = 15,
                width = "65px"
              )
            ),
            plotOutput("hist_reg", height = "200px"),

            strong("Normal QQ Plot of Residuals"),
            plotOutput("norm_qq_reg", height = "200px")
          )
        ),

        ### chi-square goodness-of-fit test
        conditionalPanel(
          condition = "input.inference_method == 'cs_gof'",

          conditionalPanel(
            condition = "input.cs_gof_data_format == 'obs'",

            fluidRow(
              column(3,
                strong("Test Results"),
                verbatimTextOutput("cs_gof_obs_out"),
                tags$head(tags$style("#cs_gof_obs_out{width: 225px;}"))
              )
            ),

            linebreaks(1),

            fluidRow(
              column(4,
                strong("Observed and Expected Counts"),
                verbatimTextOutput("cs_gof_obs_all_counts")
              )
            ),

            linebreaks(1),

            fluidRow(
              column(3,
                strong("# Missing Observations"),
                verbatimTextOutput("cs_gof_obs_NAs"),
                tags$head(tags$style("#cs_gof_obs_NAs{width: 100px;}"))
              )
            ),

            column(9, p(""))
          ),

          conditionalPanel(
            condition = "input.cs_gof_data_format == 'counts'",

            fluidRow(
              column(3,
                strong("Test Results"),
                verbatimTextOutput("cs_gof_counts_out"),
                tags$head(tags$style("#cs_gof_counts_out{width: 225px;}"))
              )
            ),

            linebreaks(1),

            fluidRow(
              column(4,
                strong("Observed and Expected Counts"),
                verbatimTextOutput("cs_gof_counts_all_counts"),
              )
            )
          )
        ),

        ### chi-square test of independence
        conditionalPanel(
          condition = "input.inference_method == 'cs_indep'",

          conditionalPanel(
            condition = "input.cs_indep_data_format == 'dataset'",

            fluidRow(
              column(4,
                strong("Test Results"),
                verbatimTextOutput("cs_indep_dataset_test_out"),
                tags$head(tags$style("#cs_indep_dataset_test_out{width: 225px;}"))
              ),

              column(4,
                strong("Observed Counts"),
                verbatimTextOutput("cs_indep_dataset_obs_counts")
              )
            ),

            linebreaks(1),

            fluidRow(
              column(3,
                strong("# Missing Observations"),
                verbatimTextOutput("cs_indep_obs_NAs"),
                tags$head(tags$style("#cs_indep_obs_NAs{width: 100px;}"))
              ),

              column(1,
                p("")
              ),

              column(4,
                strong("Expected Counts"),
                verbatimTextOutput("cs_indep_dataset_exp_counts")
              )
            )
          ),

          conditionalPanel(
            condition = "input.cs_indep_data_format == 'manual'",

            column(4,
              strong("First set the categories to the left, then enter the
                observed counts below, and finally click the 'Run Test' button."
              ),

              linebreaks(2),

              rHandsontableOutput("cs_indep_manual_table"),

              linebreaks(1),

              actionButton("run_cs_indep_manual", "Run Test",
                class = "btn btn-primary"
              )
            ),

            column(4,
              strong("Test Results"),
              verbatimTextOutput("cs_indep_manual_test_out"),
              tags$head(tags$style("#cs_indep_manual_test_out{width: 225px;}"))
            ),

            column(4,
              strong("Expected Counts"),
              verbatimTextOutput("cs_indep_manual_exp_counts"),
              tags$head(tags$style(
                "#cs_indep_manual_exp_counts{width: 225px;}"
              ))
            )
          )
        ),

        ### chi-square test of homogeneity
        conditionalPanel(
          condition = "input.inference_method == 'cs_homog'",

          conditionalPanel(
            condition = "input.cs_homog_data_format == 'dataset'",

            fluidRow(
              column(4,
                strong("Test Results"),
                  verbatimTextOutput("cs_homog_dataset_test_out"),
                  tags$head(tags$style("#cs_homog_dataset_test_out{width: 225px;}"))
              ),

              column(4,
                strong("Observed Counts"),
                verbatimTextOutput("cs_homog_dataset_obs_counts")
              )
            ),

            linebreaks(1),

            fluidRow(
              column(3,
                strong("# Missing Observations"),
                verbatimTextOutput("cs_homog_obs_NAs"),
                tags$head(tags$style("#cs_homog_obs_NAs{width: 100px;}"))
              ),

              column(1,
                p("")
              ),

              column(4,
                strong("Expected Counts"),
                verbatimTextOutput("cs_homog_dataset_exp_counts")
              )
            )
          ),

          conditionalPanel(
            condition = "input.cs_homog_data_format == 'manual'",

            column(4,
              strong("First set the categories and groups to the left, then
                enter the observed counts below, and finally click the 'Run
                Test' button."
              ),

              linebreaks(2),

              rHandsontableOutput("cs_homog_manual_table"),

              linebreaks(1),

              actionButton("run_cs_homog_manual", "Run Test",
                class = "btn btn-primary"
              )
            ),

            column(4,
              strong("Test Results"),
              verbatimTextOutput("cs_homog_manual_test_out"),
              tags$head(tags$style("#cs_homog_manual_test_out{width: 225px;}"))
            ),

            column(4,
              strong("Expected Counts"),
              verbatimTextOutput("cs_homog_manual_exp_counts"),
              tags$head(tags$style(
                "#cs_homog_manual_exp_counts{width: 225px;}"
              ))
            )
          )
        )
      )
    )
  ),  # end narbarMenu for check data




  ##############################################################################
  ############################ Download Data tabpanel ##########################
  ##############################################################################

  tabPanel("Download Data", icon = icon("download"),

    column(12,
      fluidRow(
        column(3,
          div(style = "margin-top: -2em;"),
          h3(style = "color: red; font-weight: bold;", "Optional Step"),
          p(style = "color: red; font-weight: bold;", "If you want missing values
            in your data set to be represented by a specific symbol (e.g., a
            period) or number, as opposed to an empty cell (the default), check
            the box below."
          )
        ),

        column(4,
          div(style = "margin-top: -2em;"),
          h3(style = "color: red; font-weight: bold;", "Optional Step"),
          p(style = "color: red; font-weight: bold;", "If you want to download
            the entire data set, leave the box below unchecked and continue to
            the Final Step. However, if you only want to select specific
            variables from the data set (for ex., if you want separate data sets
            for individual students as opposed to one that all students use),
            check the box below and select the variables. This selection can be
            repeated for each new data set desired."
          )
        ),

        column(3,
          div(style = "margin-top: -2em;"),
          h3(style = "color: red; font-weight: bold;", "Final Step"),
          p(style = "color: red; font-weight: bold;", "Specify the desired file
            name and file type for your data set, and then click the 'Download'
            button."
          )
        )
      )
    ),

    column(3,
      wellPanel(
        checkboxInput("missing_output", strong("Update Missing Values"),
          value = FALSE
        ),

        conditionalPanel(
          condition = "input.missing_output",

          p("Specify the symbol or number that you want to represent all
            missing values in your data set."
          ),

          textInput("denote_missing_output", NULL, value = ".", width = "25%"),

          actionButton("convert_missing_out", "Convert Missing Values",
            class = "btn btn-primary"
          ),

          conditionalPanel(
            condition = "input.convert_missing_out",

            br(),

            uiOutput("convert_success")
          )
        )
      )
    ),


    column(4,
  		wellPanel(
  		  checkboxInput("select_vars_output", strong("Select Variable(s)"),
          value = FALSE
        ),

        conditionalPanel(
          condition = "input.select_vars_output",

          p("Select all variables of interest."),

  			  selectInput("final_vars", NULL, choices = list(""), selected = NULL,
            multiple = TRUE
          )
        )
  		),

      wellPanel(
        checkboxInput("sort_vars_output", strong("Reorder Variable(s)")),

        conditionalPanel(
          condition = "input.sort_vars_output",

          p("Reorder the variables from first to last."),

          uiOutput("sort_vars_ui")
        )
      )
    ),


    column(3,
  		wellPanel(
  		  textInput("file_name", "Specify File Name",
          value = "new_dataset"
        ),

        selectInput("file_type_out", "Select File Type (Extension)",
          choices = c(
            "CSV (.csv)" = "csv",
            "Excel Spreadsheet (.xlsx)" = "xlsx",
            "MATLAB (.mat)" = "mat",
            "Plain Text (.txt)" = "txt",
            "R (.rda)" = "rda",
            "R (.RData)" = "RData",
            "R (.rds)" = "rds",
            "SPSS (.sav)" = "sav"
          ),

          selected = "csv"
        ),


  		  # downloadButtonCustom <- function(outputId, label = "Download",
  		  #   class = NULL, ...
  		  # ) {
  		  #
  		  #   aTag <-
  		  #     tags$a(
  		  #       id = outputId,
  		  #       class = paste("btn btn-default shiny-download-link")
  		  #     )
  		  # }


  		  # actionButton("save_final", "Save"),


  			downloadButton("download_dataset", "Download",
  			  class = "btn btn-primary"
  		  )
  		)
    )
  ),




  ##############################################################################
  ################################ Help tabpanel ###############################
  ##############################################################################

  tabPanel("Help", icon = icon("question-circle"),

    ### input data and generate data instructions
    column(4,
      h3(style = "color: blue;",  "Input Data"),

      strong("Uploading a data set"),

      p("The app accepts files with the following (common) extensions when
        uploading data: .csv, .xlsx, .xls, .rda, .RData, .rds, .mtp, .mat, .txt,
        .sas7bdat, .sav, and .dta."
      ),

      strong("Manually inputting data"),

      p("If you want to manually input data, follow the steps below:"),
      p("(1) Specify the total number of rows you want in the data set, and type
        the names of all of the variables in the appropriate text boxes. Then
        click the 'Generate Empty Table' button to create an empty spreadsheet."
      ),
      p("(2) Input your data in the table that appears, and specify if any
        missing data is present. Then click the 'Store Data' button."
      ),
      hr(),

      h3(style = "color: blue;", "Generate Data"),

      p("You have the option of randomly generating quantitative and/or
        categorical data. (This is completely optional if you already input data
        previously.)"
      ),
      p("To generate data, move left to right across your screen."),
      p("(1) Select the desired options in the left-hand menu and click the
        'Generate Data' button. The data will then appear under 'Current
        Variable(s).'"
      ),
      p("(2) When satisfied with the randomly generated data, input the variable
        name (and the category names for any categorical variables), and then
        click the 'Set Name(s)' button. If there are multiple variables, then
        for each variable, a different number will be added to the end of the
        variable name you specified."
      ),
      p("(3) Click the 'Add Generated Data' button to store the randomly
        generated data, which can be placed either before or after the data in
        the existing data set (if there is one already)."
      ),

      br()
    ),

    ### adjust data instructions
    column(4,
      h3(style = "color: blue;", "Adjust Data"),

      strong("Viewing the data set"),
      p("After doing at least one of the following -- uploading data, manually
        inputting data, or randomly generating data -- the data set will appear
        under 'Current Data Set.'"
      ),
      p("You can view the data set as either a table or a spreadsheet. Data in
        individual cells can be updated manually when in spreadsheet view."
      ),

      strong("Adjusting quantitative data"),
      p("First select all variables you want to adjust under 'Variable(s) to
        Update.' If you select multiple, they will be updated simultaneously and
        using the exact same adjustment options you select."
      ),
      p("You can choose from the following options when adjusting values: (1)
        transform data linearly, (2) transform data using a custom function
        (which must always be a function of x), (3) combine variables, (4) add
        random normal noise, (5) add outlier(s) or convert existing value(s) to
        outlier(s), (6) force non-constant variance for regression scenarios, (7)
        round all values to the nearest user-specified digit, (8) round all
        values below a specified minimum to that minimum and/or all values above
        a specified maximum to that maximum, and (9) round all values using
        either the floor or ceiling function."
      ),
      p("When using a custom transformation that involves a logarithmic or
        exponential function, note that log(x) has a default base of e; i.e., use
        log(x) for ln(x). For log with base 10, use log10(x). Additionally, use
        exp() for exponential functions. For example, use exp(x) for e^x or
        exp(4) for e^4."
      ),

      strong("Adjusting categorical data"),
      p("First select all variables you want to adjust under 'Variable(s) to
        Update.' If you select multiple, they will be updated simultaneously and
        using the exact same adjustment options you select."
      ),
      p("You can adjust the number of occurrences of each category.")
    ),

    ### adjust data (cont.), check data, and download data instructions
    column(4,
      h3(style = "color: blue;", "Adjust Data (cont.)"),

      strong("Making multiple (different) versions of a variable"),
      p("(1) Make the desired number of copies of the variable. (For example, if
        you want 8 new versions of a variable, make 8 copies of it.) All copies
        will appear at the beginning of the data set."
      ),
      p("(2) Select all of the variables you want to adjust (e.g., the 8 copies),
        which is done by the app immediately after making the copies. Then make
        the desired updates using the appropriate checkbox options. All of the
        selected variables will be updated simultaneously."
      ),

      strong("Resetting the data set"),
      p("To undo all changes made to the data set in 'Adjust Data,' click the
        'Reset Data Set' checkbox, followed by the 'Reset to Original' button."
      ),
      hr(),


      h3(style = "color: blue;", "Check Data"),

      p("You have the option of selecting from various descriptive and
        inferential methods when checking the suitability of the data. If you
        want to save the output, it is recommended you take a screenshot."
      ),
      hr(),


      h3(style = "color: blue;", "Download Data"),

      p("When downloading the data set, you must type a name for the file. Then
        select from the following list of file extensions: .csv, .xlsx, .mat,
        .txt, .rda, .RData, .rds, and .sav. Three additional functionalities are
        available as well: for specifying missing values, selecting specific
        variables to download, and reordering variables. Note that if you
        download a file with extension .mat, and there are periods in any of the
        variable names, those periods will be converted automatically to
        underscores."
      )
    )
  ),




  ##############################################################################
  ################################ About tabpanel ##############################
  ##############################################################################

  tabPanel("About", icon = icon("info-circle"),

    column(4,
      h4("The information below will be updated after the completion of the
        blind review process for the associated article."
      ),

      linebreaks(1),

      # information about the developers
      h4("Developers"),

      p("This tool was developed by Anonymous."),

      linebreaks(1),

      # information about the app
      h4("About the App"),
      p("This tool enables users to randomly generate data, make new versions of
        existing data through common adjustments (e.g., adding random normal
        noise and performing transformations), and check the suitability of the
        resulting data for statistical analyses. The app was designed to support
        educators in wide-ranging disciplines, with a particular focus on those
        teaching introductory statistical methods (descriptive and/or
        inferential) for data analysis."
      ),

      linebreaks(1),


      # contact info
      h4("Contact"),

      p("Email: Anonymous"),

      linebreaks(1),


      # copyright statement
      p("Copyright \uA9 2020-2022 by Anonymous."),

      p("The license statement can be found",
        a("here.", href = "https://choosealicense.com/licenses/mit/",
          target = "_blank"
        )
      )
    )
  )
)



