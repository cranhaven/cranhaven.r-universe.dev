
library(shiny)
library(shinyalert)
library(shinyjs)
library(ggplot2); theme_set(theme_grey(16))
library(tidyr)
library(stringr)
library(DT)
library(DescTools)
library(teachingApps)  # for the rbeta4 fxn when randomly generating data
library(plotly)
library(rhandsontable)
library(rio)     # make sure rio loaded after plotly due to masking
library(rmatio)  # use rmatio instead of rio when importing .mat files
library(sortable)
library(dplyr)


shinyServer(function(session, input, output) {

  ##############################################################################
  ################################ Initial Stuff ###############################
  ##############################################################################

  ### create and initialize reactive stuff
  ##################################################
  bag <- reactiveValues(

    # descriptive
    do_descript_1_quant = FALSE, do_descript_1_cat = FALSE,
    do_descript_2_quant_quant = FALSE, do_descript_2_quant_cat = FALSE,
    do_descript_2_cat_cat = FALSE,

    # inferential
    do_t_1 = FALSE, do_z_1 = FALSE, do_t_paired = FALSE, do_t_indep_mm = FALSE,
    do_t_indep_mg = FALSE, do_z_2_mm = FALSE, do_z_2_mg = FALSE,
    use_z_2_mg_vars = FALSE, do_anova_mm = FALSE, do_anova_mg = FALSE,
    do_reg_inf = FALSE, use_cs_gof_obs_var = FALSE, do_cs_gof_obs = FALSE,
    do_cs_gof_counts = FALSE, do_cs_indep_dataset = FALSE,
    do_cs_indep_manual = FALSE, do_cs_homog_dataset = FALSE,
    do_cs_homog_manual = FALSE
  )

  bag$sim_df <- data.frame(init_var = 0)
  bag$reg_counter <- 0
  bag$copy_num <- 0
  bag$combine_num_vars <- 2



  ### make fxns
  ##################################################

  # print warning popup alert and keep app from crashing if no var selected
  no_var_warning <- function(x) {

    if (is.null(x)) {
      shinyalert("Warning!", "You must select at least one variable before
        continuing.",
        type = "error"
      )
    }

    validate(need(is.null(x) == FALSE, ""))
  }


  # update var name select inputs in Check Data tab
  update_var_names <- function() {

    # descriptive methods -- 1 var
    updateSelectInput(session, "descript_select_1var_var",
      "Variable of Interest",
      choices = names(bag$data_new_df)
    )

    # descriptive methods -- 2 vars
    updateSelectInput(session, "descript_select_2vars_var_resp",
      "Response Variable",
      choices = names(bag$data_new_df)
    )
    updateSelectInput(session, "descript_select_2vars_var_expl",
      "Explanatory (or Group) Variable",
      choices = names(bag$data_new_df)
    )

    # one-sample t procedures
    updateSelectInput(session, "t_1_select_var", "Quantitative Variable",
      choices = names(bag$data_new_df)
    )

    # one-proportion z procedures
    updateSelectInput(session, "z_1_select_var", "Categorical Variable",
      choices = names(bag$data_new_df)
    )

    # paired-samples t procedures
    updateSelectInput(session, "t_paired_select_var_1", "Quantitative Variable
      (Group 1)",
      choices = names(bag$data_new_df)
    )
    updateSelectInput(session, "t_paired_select_var_2", "Quantitative Variable
      (Group 2)",
      choices = names(bag$data_new_df)
    )

    # independent samples t procedures
    updateSelectInput(session, "t_indep_select_var_1mm", "Quantitative Variable
      (Group 1)",
      choices = names(bag$data_new_df)
    )
    updateSelectInput(session, "t_indep_select_var_2mm", "Quantitative Variable
      (Group 2)",
      choices = names(bag$data_new_df)
    )

    updateSelectInput(session, "t_indep_select_var_1mg",
      "Variable with All Quantitative Data",
      choices = names(bag$data_new_df)
    )
    updateSelectInput(session, "t_indep_select_var_2mg",
      "Variable with Group Identifiers",
      choices = names(bag$data_new_df)
    )

    # two-proportion z procedures
    updateSelectInput(session, "z_2_select_var_1mm", "Group 1 Data Variable",
      choices = names(bag$data_new_df)
    )
    updateSelectInput(session, "z_2_select_var_2mm", "Group 2 Data Variable",
      choices = names(bag$data_new_df)
    )

    updateSelectInput(session, "z_2_select_var_1mg",
      "Variable with Measurement Data",
      choices = names(bag$data_new_df)
    )
    updateSelectInput(session, "z_2_select_var_2mg",
      "Variable with Group Identifiers",
      choices = names(bag$data_new_df)
    )

    # one-way anova: if user selects measurement-measurement option
    output$anova_select_groups <- renderUI({

      bag$anova_num_groups <- as.integer(input$anova_num_grps)

      lapply(1:bag$anova_num_groups, function(i) {
        selectInput(
          paste0("anova_sel_input_", i),
          paste0("Quantitative Variable (Group ", i, ")"),
          choices = names(bag$data_new_df)
        )
      })
    })

    # one-way anova: if user selects measurement-group option
    updateSelectInput(session, "anova_select_var_1mg", "Variable with All
      Quantitative Data",
      choices = names(bag$data_new_df)
    )
    updateSelectInput(session, "anova_select_var_2mg",
      "Variable with Group Identifiers",
      choices = names(bag$data_new_df)
    )

    # regression inference
    updateSelectInput(session, "reg_inference_select_resp_var",
      "Response Variable",
      choices = names(bag$data_new_df)
    )
    updateSelectInput(session, "reg_inference_select_expl_var",
      "Explanatory Variable",
      choices = names(bag$data_new_df)
    )

    # chi-square gof test: if user selects a variable
    updateSelectInput(session, "cs_gof_select_var", "Categorical Variable",
      choices = names(bag$data_new_df)
    )

    # chi-square test of independence
    updateSelectInput(session, "cs_indep_select_var_1", "Categorical Variable 1",
      choices = names(bag$data_new_df)
    )
    updateSelectInput(session, "cs_indep_select_var_2", "Categorical Variable 2",
      choices = names(bag$data_new_df)
    )

    # chi-square test of homogeneity
    updateSelectInput(session, "cs_homog_select_var_1", "Categorical Variable 1",
      choices = names(bag$data_new_df)
    )
    updateSelectInput(session, "cs_homog_select_var_2", "Categorical Variable 2",
      choices = names(bag$data_new_df)
    )
  }



  ### show idle timeout popup on startup
  ######################################################
  showModal(
    modalDialog(
      h3(style = "color: red; font-weight: bold; text-align: center;",
        "Important message:"
      ),

      h3(style = "text-align: center;", "Any uploaded file must be smaller than
        5 MB in size."
      ),

      easyClose = TRUE
    )
  )



  ##############################################################################
  ############################### Input Data tab ###############################
  ##############################################################################

  ### upload data
  ##################################################
  observeEvent(input$data_file, {

    # determine file extension of uploaded file
    bag$file_extension <- tools::file_ext(input$data_file$datapath)
  })


  output$na_input_status <- reactive({
    req(bag$file_extension)

    bag$file_extension %in% c("csv", "xls", "xlsx", "txt")
  })

  # don't suspend value of na_input_status output
  outputOptions(output, "na_input_status", suspendWhenHidden = FALSE)


  # output$na_output_status <- reactive({
  #   req(bag$file_extension)
  #
  #   bag$file_extension %in% c("csv", "xls", "xlsx", "txt")
  # })
  #
  # # don't suspend value of na_input_status output
  # outputOptions(output, "na_output_status", suspendWhenHidden = FALSE)


  observeEvent(input$upload_dataset, {

    req(input$data_file)


    # read file
    if (bag$file_extension == "csv") {

      if (input$missing_input) {
        nas <- input$denote_missing_input %>%
          strsplit(split = ",") %>%  # split if see comma
          unlist() %>%
          str_trim()    # remove leading or trailing whitespace

        nas <- c("", " ", nas)
      } else {
        nas <- c("", " ")
      }


      # use read.csv() instead of fread() from data.table package that rio
      #  package uses
      bag$data_original_df <- read.csv(input$data_file$datapath, na.strings = nas)

    } else if (bag$file_extension %in% c("xls", "xlsx")) {

      if (input$missing_input) {
        nas <- input$denote_missing_input %>%
          strsplit(split = ",") %>%  # split if see comma
          unlist() %>%
          str_trim()    # remove leading or trailing whitespace

        nas <- c("", " ", nas)
      } else {
        nas <- c("", " ")
      }

      bag$data_original_df <- rio::import(input$data_file$datapath, na = nas)

    } else if (bag$file_extension == "txt") {

      if (input$missing_input) {
        nas <- input$denote_missing_input %>%
          strsplit(split = ",") %>%  # split if see comma
          unlist() %>%
          str_trim()    # remove leading or trailing whitespace

        nas <- c("", " ", nas)
      } else {
        nas <- c("", " ")
      }

      bag$data_original_df <- rio::import(input$data_file$datapath, na.strings = nas)

    } else if (bag$file_extension == "mat") {

      bag$data_original_df <- as.data.frame(read.mat(input$data_file$datapath))

    } else {

      bag$data_original_df <- rio::import(input$data_file$datapath)
    }


    # update variable names list for main drop-down menu in Adjust Data tab
    updateSelectInput(session, "select_var", NULL,
      choices =  names(bag$data_original_df)
    )

    # update variable names list for drop-down menu in Download Data tab
    updateSelectInput(session, "final_vars", NULL,
      choices =  names(bag$data_original_df)
    )


    # copy data set for making changes
    bag$data_new_df <- bag$data_original_df


    # print original data set in Adjust Data tab
    if (input$data_view == "table") {
      output$df_new_dt <- renderDT(bag$data_original_df,
        options = list(scrollX = TRUE)
      )
    } else if (input$data_view == "spreadsheet") {
      output$df_new_rhot <- renderRHandsontable(
        rhandsontable(bag$data_original_df)
      )
    }


    # update variable name select inputs in Check Data tab
    update_var_names()
  })


  # print original data set in Input Data - Upload Data Set tab
  output$df_original_upload <- renderDT({
    datatable(bag$data_original_df, options = list(pageLength = 10, scrollX = TRUE))
  })



  ### make data set using manual inputs
  ##################################################
  observeEvent(input$set_manual_dims, {

    # warning popup alert if no value input for number of rows
    if (is.na(input$manual_num_rows)) {
      shinyalert("Warning!", "You must specify the number of rows you want in the
        data set.",
        type = "error"
      )
    }

    # warning popup alert if no variable names (and thus no columns) specified
    if (input$manual_var_names_num == "" & input$manual_var_names_chr == "") {
      shinyalert("Warning!", "The data set must contain at least one column.
        Please specify at least one variable name.",
        type = "error"
      )
    }


    validate(need(
      is.numeric(input$manual_num_rows) &
      (input$manual_var_names_num != "" | input$manual_var_names_chr != ""),
      ""
    ))


    manual_col_names_num <- input$manual_var_names_num %>%
      strsplit(split = "\n|,") %>%    # split if see comma or new line
      unlist() %>%
      str_trim()    # remove leading or trailing whitespace

    bag$manual_col_names_num <- manual_col_names_num


    manual_col_names_chr <- input$manual_var_names_chr %>%
      strsplit(split = "\n|,") %>%    # split if see comma or new line
      unlist() %>%
      str_trim()    # remove leading or trailing whitespace


    # make empty table of user-specified dimensions
    manual_table_num_mat <- matrix(as.character(NA), nrow = input$manual_num_rows,
      ncol = length(manual_col_names_num)
    )
    manual_table_chr_mat <- matrix(as.character(NA), nrow = input$manual_num_rows,
      ncol = length(manual_col_names_chr)
    )
    manual_df <- data.frame(manual_table_num_mat, manual_table_chr_mat,
      stringsAsFactors = FALSE
    )
    names(manual_df) <- c(manual_col_names_num, manual_col_names_chr)


    # make sure not to delete
    bag$data_original_df <- manual_df


    # update variable names list for drop-down menu in Adjust Data tab
    updateSelectInput(session, "select_var", NULL,
      choices = names(bag$data_original_df)
    )

    # update variable names list for drop-down menu in Download Data tab
    updateSelectInput(session, "final_vars", NULL,
      choices = names(bag$data_original_df)
    )
  })



  # print data set in Input Data tab; need to use rhandsontable()
  output$df_original_manual <- renderRHandsontable({

    req(bag$data_original_df)

    rhandsontable(bag$data_original_df)
  })



  ### store data set that user input manually
  ##################################################
  observeEvent(input$store_manual_df, {

    # warning popup alert if no data set input manually
    if (is.null(input$df_original_manual)) {
      shinyalert("Warning!", "You did not manually input any data.",
        type = "error"
      )
    }

    validate(need(!is.null(bag$data_original_df), ""))


    # warning popup alert if no data input after creating blank spreadsheet
    if (nrow(na.omit(hot_to_r(input$df_original_manual))) == 0) {
      shinyalert("Warning!", "You did not manually input any data.",
        type = "error"
      )
    }


    validate(need(nrow(na.omit(hot_to_r(input$df_original_manual))) > 0, ""))


    # need to use hot_to_r here
    bag$data_original_df <- hot_to_r(input$df_original_manual)


    bag$data_new_df <- bag$data_original_df


    # replace missing data characters w/ NAs
    if (input$missing_manual) {
      nas <- input$denote_missing_manual %>%
        strsplit(split = ",") %>%    # split if see comma
        unlist() %>%
        str_trim()    # remove leading or trailing whitespace

      nas <- c("", " ", nas)

      for (i in 1:length(nas)) {
        bag$data_new_df[bag$data_new_df == nas[i]] <- NA
      }
    }


    # convert columns to numeric when applicable
    if (length(bag$manual_col_names_num) > 0) {
      for (i in 1:length(bag$manual_col_names_num)) {
        bag$data_new_df[, i] <- as.numeric(bag$data_new_df[, i])
      }
    }


    # print message indicating successful storage
    output$store_manual_success <- renderUI({
      HTML("<b><span style = 'color: forestgreen';>Data stored successfully</b>")
    })


    # update variable name select inputs in Check Data tab
    update_var_names()
  })



  # print data set in Adjust Data tab
  output$df_new_dt <- renderDT({

    req(bag$data_new_df)

    bag$data_new_df

  }, options = list(scrollX = TRUE)
  )



  # update data set if users changes it in manually-input spreadsheet
  observeEvent(input$df_original_manual, {

    # need to use hot_to_r() here
    bag$data_original_df <- hot_to_r(input$df_original_manual)


    bag$data_new_df <- bag$data_original_df


    # replace missing data characters w/ NAs
    if (input$missing_manual) {
      nas <- input$denote_missing_manual %>%
        strsplit(split = ",") %>%    # split if see comma
        unlist() %>%
        str_trim()    # remove leading or trailing whitespace

      nas <- c("", " ", nas)

      for (i in 1:length(nas)) {
        bag$data_new_df[bag$data_new_df == nas[i]] <- NA
      }
    }


    # convert columns to numeric when applicable
    if (length(bag$manual_col_names_num) > 0) {
      for (i in 1:length(bag$manual_col_names_num)) {
        bag$data_new_df[, i] <- as.numeric(bag$data_new_df[, i])
      }
    }


    # print message indicating successful storage
    output$store_manual_success <- renderUI({
      HTML("<b><span style = 'color: forestgreen';>Data stored successfully</b>")
    })


    # update variable name select inputs in Check Data tab
    update_var_names()


    # need to render again, otherwise buggy
    output$df_new_dt <- renderDT(bag$data_new_df)
  })




  ##############################################################################
  ############################# Generate Data tab ##############################
  ##############################################################################

  ### randomly generate quantitative data
  #################################################
  observeEvent(input$gen_data_quant, {

    # warning popup alert if no sample size input
    if (is.na(input$samp_size_quant)) {
      shinyalert("Warning!", "You must specify the sample size to be used.",
        type = "error"
      )
    }

    # warning popup alert if no number of samples input
    if (is.na(input$n_vars_quant)) {
      shinyalert("Warning!", "You must specify the number of samples.",
        type = "error"
      )
    }

    validate(need(
      is.numeric(input$samp_size_quant) & is.numeric(input$n_vars_quant),
      ""
    ))


    # warning popup alert if sample size < 1 input
    if (input$samp_size_quant < 1) {
      shinyalert("Warning!", "The sample size must be at least 1.",
        type = "error"
      )
    }

    # warning popup alert if # samples input < 1
    if (input$n_vars_quant < 1) {
      shinyalert("Warning!", "The number of samples must be at least 1.",
        type = "error"
      )
    }

    validate(need(
      is.numeric(input$samp_size_quant) &
      input$samp_size_quant >= 1 &
      is.numeric(input$n_vars_quant) &
      input$n_vars_quant >= 1,
      ""
    ))


    if (input$pop_option_quant == "basic") {

      if (input$pop_shape == "Normal") {

        # warning popup alert if no mean and/or SD input
        if (is.na(input$norm_mean_basic) | is.na(input$norm_sd_basic)) {
          shinyalert("Warning!", "You must specify both a mean and SD.",
            type = "error"
          )
        }

        validate(need(
          is.numeric(input$norm_mean_basic) & is.numeric(input$norm_sd_basic),
          ""
        ))

        # warning popup alert if SD < 0
        if (input$norm_sd_basic < 0) {
          shinyalert("Warning!", "The SD cannot be negative.",
            type = "error"
          )
        }

        validate(need(input$norm_sd_basic >= 0, ""))


        current_vars <- replicate(input$n_vars_quant,
          rnorm(input$samp_size_quant, input$norm_mean_basic, input$norm_sd_basic)
        )
      } else {

        # parameters of beta distribution
        params <- switch(input$pop_shape,
          "Severely Left Skewed" = c(4, 1),
          "Very Left Skewed" = c(3, 1),
          "Left Skewed" = c(4, 2),
          "Right Skewed" = c(2, 4),
          "Very Right Skewed" = c(1, 3),
          "Severely Right Skewed" = c(1, 4)
        )


        # warning popup alert if no min and/or max input
        if (is.na(input$beta4_min) | is.na(input$beta4_max)) {
          shinyalert("Warning!", "You must specify both a min and max.",
            type = "error"
          )
        }

        validate(need(
          is.numeric(input$beta4_min) & is.numeric(input$beta4_max),
          ""
        ))

        if (input$beta4_min >= input$beta4_max) {
          shinyalert("Warning!", "The min must be less than the max.",
            type = "error"
          )
        }

        validate(need(input$beta4_min < input$beta4_max, ""))


        # randomly sample data; rbeta4 = 4-parameter beta
        current_vars <- replicate(input$n_vars_quant,
          rbeta4(input$samp_size_quant, min = input$beta4_min,
            max = input$beta4_max, shape1 = params[1], shape2 = params[2],
            seed = sample(1:1e5, 1)
          )
        )
      }
    } else {

      # pop. distribution to randomly sample from
      dist_random <- switch(input$pop_fam_quant,
        "Normal" = rnorm, "t" = rt, "F" = rf, "Uniform (Continuous)" = runif,
        "Chi-square" = rchisq, "Poisson" = rpois, "Exponential" = rexp,
        "Gamma" = rgamma, "Beta" = rbeta
      )

      # parameters of pop. distribution
      params <- switch(input$pop_fam_quant,
        "Normal" = c(input$norm_mean, input$norm_sd),
        "t" = input$t_df,
        "F" = c(input$f_df1, input$f_df2),
        "Uniform (Continuous)" = c(input$uni_min, input$uni_max),
        "Chi-square" = input$chisq_df,
        "Poisson" = input$pois_rate,
        "Exponential" = input$exp_rate,
        "Gamma" = c(input$gamma_shape, input$gamma_rate),
        "Beta" = c(input$beta_shape1, input$beta_shape2)
      )


      # warning popup alert if any required parameters not input
      if (anyNA(params)) {
        shinyalert("Warning!", "You didn't specify all necessary parameters.",
          type = "error"
        )
      }

      validate(need(anyNA(params) == FALSE, ""))


      # warning popup alert if parameter value not valid
      if (input$pop_fam_quant == "Normal") {

        if (params[2] < 0) {
          shinyalert("Warning!", "The SD cannot be negative.",
            type = "error"
          )
        }

        validate(need(params[2] >= 0, ""))

      } else if (input$pop_fam_quant == "t") {

        if (params <= 0) {
          shinyalert("Warning!", "The DF must be positive.",
            type = "error"
          )
        }

        validate(need(params > 0, ""))

      } else if (input$pop_fam_quant == "F") {

        if (params[1] <= 0 | params[2] <= 0) {
          shinyalert("Warning!", "Both DF values must be positive.",
            type = "error"
          )
        }

        validate(need(params[1] > 0 & params[2] > 0, ""))

      } else if (input$pop_fam_quant == "Chi-square") {

        if (params <= 0) {
          shinyalert("Warning!", "The DF must be positive.", type = "error")
        }

        validate(need(params > 0, ""))

      } else if (input$pop_fam_quant == "Uniform (Continuous)") {

        if (params[1] >= params[2]) {
          shinyalert("Warning!", "The min must be less than the max.",
            type = "error"
          )
        }

        validate(need(params[1] < params[2], ""))

      } else if (input$pop_fam_quant == "Uniform (Discrete)") {

        values <- input$uni_discr_values %>%
          strsplit(split = "\n|,") %>%
          unlist() %>%
          as.numeric()

        if (anyNA(values)) {
          shinyalert("Warning!", "All values input must be numbers.",
            type = "error"
          )
        }

        if (length(values) < 2) {
          shinyalert("Warning!", "You must input at least two values.",
            type = "error"
          )
        }

        validate(need(!anyNA(values) & (length(values) >= 2), ""))

      } else if (input$pop_fam_quant == "Poisson") {

        if (params <= 0) {
          shinyalert("Warning!", "The rate must be positive.", type = "error")
        }

        validate(need(params > 0, ""))

      } else if (input$pop_fam_quant == "Exponential") {

        if (params <= 0) {
          shinyalert("Warning!", "The rate must be positive.", type = "error")
        }

        validate(need(params > 0, ""))

      } else if (input$pop_fam_quant == "Gamma") {

        if (params[1] <= 0 | params[2] <= 0) {
          shinyalert("Warning!", "The shape and rate parameters must both be
            positive.",
            type = "error"
          )
        }

        validate(need(params[1] > 0 & params[2] > 0, ""))

      } else if (input$pop_fam_quant == "Beta") {

        if (params[1] <= 0 | params[2] <= 0) {
          shinyalert("Warning!", "Both shape parameters must be positive.",
            type = "error"
          )
        }

        validate(need(params[1] > 0 & params[2] > 0, ""))

      }


      # randomly sample data
      if (input$pop_fam_quant %in% c("t", "Chi-square", "Poisson", "Exponential")) {

        current_vars <- replicate(
          input$n_vars_quant,
          dist_random(input$samp_size_quant, params)
        )

      } else if (input$pop_fam_quant == "Uniform (Discrete)") {

        values <- input$uni_discr_values %>%
          strsplit(split = "\n|,") %>%
          unlist() %>%
          as.numeric()

        current_vars <- replicate(
          input$n_vars_quant,
          sample(values, input$samp_size_quant, replace = TRUE)
        )

      } else {

        current_vars <- replicate(
          input$n_vars_quant,
          dist_random(input$samp_size_quant, params[1], params[2])
        )
      }
    }


    # round values to 2 decimal places
    bag$current_vars <- current_vars %>%
      round(2) %>%
      as.data.frame()
  })



  # print current var
  output$sim_var <- renderDT({
    datatable(bag$current_vars, options = list(scrollX = TRUE, bFilter = FALSE))
  })



  ### update numeric inputs for multinomial model
  #################################################
  observeEvent(input$multi_outcomes, {

    # numericInputs if user selects multinomial dist
    output$multi_probs <- renderUI({

      bag$multi_n_probs <- as.integer(input$multi_outcomes)


      # require at least 3 probabilities to be input
      validate(need(bag$multi_n_probs >= 3, ""))


      probs <- round(rep(1/bag$multi_n_probs, bag$multi_n_probs), 4)

      # create numericInputs
      all_inputs <- list()
      all_inputs <- lapply(1:(bag$multi_n_probs - 1), function(i) {
        numericInput(paste0("multinom_p_", i), paste("Group", i, "Probability"),
          value = probs[i], step = 0.05
        )
      })

      all_inputs[[bag$multi_n_probs]] <- numericInput(
        paste0("multinom_p_", bag$multi_n_probs),
        paste("Group", bag$multi_n_probs, "Probability"),
        value = 1 - sum(probs[-bag$multi_n_probs]),
        step = 0.05
      )

      all_inputs
    })
  })



  ### randomly generate categorical data
  #################################################
  observeEvent(input$gen_data_cat, {

    # warning popup alert if no sample size input
    if (is.na(input$samp_size_cat)) {
      shinyalert("Warning!", "You must specify the sample size to be used.",
        type = "error"
      )
    }

    # warning popup alert if no number of samples input
    if (is.na(input$n_vars_cat)) {
      shinyalert("Warning!", "You must specify the number of samples.",
        type = "error"
      )
    }

    validate(need(
      is.numeric(input$samp_size_cat) & is.numeric(input$n_vars_cat),
      ""
    ))


    if (input$pop_fam_cat == "Binomial") {

      # warning popup alert if no prob of success input
      if (is.na(input$binom_p)) {
        shinyalert("Warning!", "You must specify the probability of success.",
          type = "error"
        )
      }

      validate(need(!is.na(input$binom_p), ""))


      # warning popup alert if prob of success input not between 0 and 1
      if (input$binom_p < 0 | input$binom_p > 1) {
        shinyalert("Warning!", "The probability of success must be between
          0 and 1.",
          type = "error"
        )
      }

      validate(need(input$binom_p >= 0 & input$binom_p <= 1, ""))


      # randomly sample data
      current_vars <- replicate(input$n_vars_cat,
        rbinom(input$samp_size_cat, 1, input$binom_p)
      )

    } else if (input$pop_fam_cat == "Multinomial") {

      # warning popup alert if no number of groups input
      if (!is.numeric(input$multi_outcomes)) {
        shinyalert("Warning!", "You must specify the number of
          groups/categories.",
          type = "error"
        )
      }

      validate(need(is.numeric(input$multi_outcomes), ""))


      # warning popup alert if only 1 or 2 groups input
      if (bag$multi_n_probs %in% 1:2) {
        # if only one group supplied, give popup warning message and keep app
        #  from crashing
        shinyalert("Warning!", "You need more than two groups.",
          type = "error"
        )

        validate(need(bag$multi_n_probs >= 3, ""))
      } else if (bag$multi_n_probs >= 3) {

        probs <- vector()
        for (i in 1:bag$multi_n_probs) {
          probs[i] <- input[[paste0("multinom_p_", i)]]
        }
      }


      # warning popup alert if at least one probability of success missing
      if (anyNA(probs)) {
        shinyalert("Warning!", "You must input a probability of success for
          each group/category.",
          type = "error"
        )
      }

      validate(need(!anyNA(probs), ""))


      # warning popup alert if probabilities don't add to 1
      if (sum(probs) != 1) {
        shinyalert("Warning!", "The probabilities must add to exactly 1.",
          type = "error"
        )
      }

      validate(need(sum(probs) == 1, ""))


      # initialize matrix
      current_vars <- matrix(NA,
        nrow = input$samp_size_cat,
        ncol = input$n_vars_cat
      )

      for (j in 1:input$n_vars_cat) {

        # randomly sample number of values in each group
        multi_successes <- as.vector(
          rmultinom(1, input$samp_size_cat, prob = probs)
        )

        # convert counts of values to actual data values
        multi_rep_list <- lapply(1:length(multi_successes), function(i) {
          rep(i, each = multi_successes[i])
        })

        # convert list to vector
        ordered_values <- do.call(c, multi_rep_list)

        # randomly permute order of values
        current_vars[, j] <- sample(ordered_values, replace = FALSE)
      }
    }


    # convert matrix to df
    bag$current_vars <- as.data.frame(current_vars)


    # print current simulated var(s)
    output$sim_var <- renderDT({
      datatable(bag$current_vars, options = list(scrollX = TRUE, bFilter = FALSE))
    })
  })







  ### update text inputs if user checks box to add cat.
  ###  var. labels
  #####################################################
  observeEvent(input$cat_labels, {

    if (input$pop_fam_cat == "Binomial") {
      output$binom_labels <- renderUI({
        list(
          textInput("binom_success_label", "Success Label",
            placeholder = "success"
          ),
          textInput("binom_failure_label", "Failure Label",
            placeholder = "failure"
          )
        )
      })
    } else if (input$pop_fam_cat == "Multinomial") {
      output$multinom_labels <- renderUI({
        lapply(1:bag$multi_n_probs, function(i) {
          textInput(paste0("multinom_label_", i), paste("Group", i, "Label"),
            placeholder = paste0("group", i)
          )
        })
      })
    }
  })



  ### set randomly generated variable names
  #######################################################
  observeEvent(input$set_sim_names, {

    req(bag$current_vars)


    # apply group labels if user chooses option
    if (input$cat_labels) {

      if (input$pop_fam_cat == "Binomial") {

        if (input$binom_success_label == "" | input$binom_failure_label == "") {
          shinyalert("Warning!", "At least one group label is missing.",
            type = "error"
          )

          validate(need(
            input$binom_success_label != "" & input$binom_failure_label != "",
            ""
          ))
        }

        bag$current_vars[bag$current_vars == 1] <- input$binom_success_label
        bag$current_vars[bag$current_vars == 0] <- input$binom_failure_label

      } else if (input$pop_fam_cat == "Multinomial") {

        multi_label_inputs <- vector()
        for (i in 1:bag$multi_n_probs) {
          multi_label_inputs[i] <- input[[paste0("multinom_label_", i)]]
        }

        if (any(multi_label_inputs == "")) {
          shinyalert("Warning!", "At least one group label is missing.",
            type = "error"
          )

          validate(need(all(multi_label_inputs != ""), ""))
        }


        # convert multinomial values to vector w/ user labels
        for (i in 1:bag$multi_n_probs) {
          bag$current_vars[bag$current_vars == i] <- input[[
            paste0("multinom_label_", i)
          ]]
        }
      }
    }


    # warning popup alert if no variable name entered
    if (input$sim_var_name == "") {
      shinyalert("Warning!", "You did not input a main variable name.",
        type = "error"
      )
    }

    validate(need(input$sim_var_name != "", ""))


    new_name <- input$sim_var_name


    # update name(s) of randomly generated var(s)
    if (ncol(bag$current_vars) == 1) {

      dup_name_counter <- 0

      # prevent duplicate name issues
      while (new_name %in% names(bag$sim_df)) {
        dup_name_counter <- dup_name_counter + 1
        new_name <- paste0(new_name, ".", dup_name_counter)
      }

      names(bag$current_vars) <- new_name

    } else {
      dup_name_counter <- 0

      for (i in 1:ncol(bag$current_vars)) {

        # prevent duplicate name issues
        curr_iter_name <- paste0(new_name, ".", i)
        while (curr_iter_name %in% names(bag$sim_df)) {
          dup_name_counter <- dup_name_counter + 1
          curr_iter_name <- paste0(curr_iter_name, ".", dup_name_counter)
        }

        names(bag$current_vars)[i] <- curr_iter_name
      }
    }
  })




  ### add randomly generated data to existing data set
  #########################################################
  observeEvent(input$add_sim_to_existing, {

    # warning popup alert if no data generated
    if (is.null(bag$current_vars)) {
      shinyalert("Warning!", "You did not randomly generate any data yet.",
        type = "error"
      )
    }

    # warning popup alert if no variable name entered
    if (input$sim_var_name == "") {
      shinyalert("Warning!", "You did not input a main variable name.",
        type = "error"
      )
    }

    # warning popup alert if variable name already used
    if (any(names(bag$current_vars) %in% names(bag$data_new_df))) {
      shinyalert("Warning!", "At least one variable name is already in use in the
        data set. Please use a different name.",
        type = "error"
      )
    }


    validate(need(
      !is.null(bag$current_vars) &
      input$sim_var_name != "" &
      !any(names(bag$current_vars) %in% names(bag$data_new_df)),
      ""
    ))


    if (!is.null(bag$data_new_df)) {

      # add NAs, if necessary, to ensure equal number of rows when combining df's
      if (nrow(bag$current_vars) > nrow(bag$data_new_df)) {
        bag$data_new_df[(nrow(bag$data_new_df) + 1):nrow(bag$current_vars), ] <- NA
      } else if (nrow(bag$current_vars) < nrow(bag$data_new_df)) {
        bag$current_vars[(nrow(bag$current_vars) + 1):nrow(bag$data_new_df), ] <- NA
      }


      if (input$sim_vs_existing == "beginning") {
        # store combined data sets as original in case user resets data set in
        #  Adjust Data tab
        bag$data_original_df <- dplyr::bind_cols(bag$current_vars, bag$data_new_df)

        # add current variable to existing df
        bag$data_new_df <- dplyr::bind_cols(bag$current_vars, bag$data_new_df)
      } else {
        # store combined data sets as original in case user resets data set in
        #  Adjust Data tab
        bag$data_original_df <- dplyr::bind_cols(bag$data_new_df, bag$current_vars)

        # add current variable to existing df
        bag$data_new_df <- dplyr::bind_cols(bag$data_new_df, bag$current_vars)
      }
    } else {

      # store generated data set as original in case user resets data set in
      #  Adjust Data tab
      bag$data_original_df <- bag$current_vars

      # store generated data set as main data set
      bag$data_new_df <- bag$current_vars
    }


    # update variable names list for drop-down menu in Adjust Data tab
    updateSelectInput(session, "select_var", NULL,
      choices = names(bag$data_new_df)
    )

    # update variable names list for drop-down menu in Download Data tab
    updateSelectInput(session, "final_vars", NULL,
      choices =  names(bag$data_new_df)
    )


    # print data set in Adjust Data tab
    if (input$data_view == "table") {
      output$df_new_dt <- renderDT(bag$data_new_df, options = list(scrollX = TRUE))
    } else if (input$data_view == "spreadsheet") {
      output$df_new_rhot <- renderRHandsontable(rhandsontable(bag$data_new_df))
    }


    # print message indicating successful storage
    output$add_success <- renderUI({
      HTML("<b><span style = 'color: forestgreen';>Data stored successfully</b>")
    })

    shinyjs::show("add_success")


    # print data set in Adjust Data tab
    if (input$data_view == "table") {
      output$df_new_dt <- renderDT(bag$data_new_df, options = list(scrollX = TRUE))
    } else if (input$data_view == "spreadsheet") {
      output$df_new_rhot <- renderRHandsontable(rhandsontable(bag$data_new_df))
    }


    # update variable name select inputs in Check Data tab
    update_var_names()


    # reset current variable(s)
    bag$current_vars <- NULL


    # print current simulated var(s)
    output$sim_var <- renderDT({
      datatable(bag$current_vars, options = list(scrollX = TRUE, bFilter = FALSE))
    })


    # reset simulated variable name
    updateTextInput(session, "sim_var_name", "Variable Name", value = "",
      placeholder = "new.var"
    )


    # reset categorical variable labels, when applicable
    if (input$pop_fam_cat == "Binomial") {

      updateTextInput(session, "binom_success_label", "Success Label",
        value = "", placeholder = "success"
      )

      updateTextInput(session, "binom_failure_label", "Failure Label",
        value = "", placeholder = "failure"
      )

      # uncheck "Add Group Labels" checkbox, when applicable
      updateCheckboxInput(session, "cat_labels", "Add Group Labels",
        value = FALSE
      )

    } else if (input$pop_fam_cat == "Multinomial") {

      lapply(1:bag$multi_n_probs, function(i) {
        updateTextInput(session,
          paste0("multinom_label_", i),
          paste("Group", i, "Label"),
          value = "",
          placeholder = paste0("group", i)
        )
      })

      # uncheck "Add Group Labels" checkbox, when applicable
      updateCheckboxInput(session, "cat_labels", "Add Group Labels",
        value = FALSE
      )
    }
  })



  ### hide success message when generated data added to data set
  #################################################################
  observeEvent(input$gen_data_quant | input$gen_data_cat, {

    shinyjs::hide("add_success")
  })




  ##############################################################################
  ############################## Adjust Data tab ###############################
  ##############################################################################

  ### transform data linearly and print new data
  #################################################
  observeEvent(input$transform_lin, {

    # print warning if no var selected
    no_var_warning(input$select_var)


    selected_data <- bag$data_new_df[, input$select_var]


    # determine which variables are numeric; tf = true/false
    numeric_vars_tf <- sapply(selected_data, is.numeric)


    # warning popup alert if any of the selected vars isn't numeric
    if (any(numeric_vars_tf == FALSE)) {
      shinyalert("Warning!", "At least one of the selected variables is not
        quantitative. Please make sure only quantitative variables are selected.",
        type = "error"
      )
    }

    # keeps the app from crashing if any of the selected vars aren't numeric
    validate(need(all(numeric_vars_tf == TRUE), ""))


    # warning popup alert if y-int or slope missing
    if (is.na(input$linear_yint) | is.na(input$linear_slope)) {
      shinyalert("Warning!", "You must specify both a y-intercept and slope.",
        type = "error"
      )
    }

    validate(need(
      is.numeric(input$linear_yint) & is.numeric(input$linear_slope),
      ""
    ))


    # perform linear transformation
    new_values <- input$linear_yint + input$linear_slope*selected_data


    bag$data_new_df[, input$select_var] <- new_values


    # print new data set
    if (input$data_view == "table") {
      output$df_new_dt <- renderDT(bag$data_new_df, options = list(scrollX = TRUE))
    } else if (input$data_view == "spreadsheet") {
      output$df_new_rhot <- renderRHandsontable(rhandsontable(bag$data_new_df))
    }

  })



  ### use custom transformation and print new data
  #################################################
  observeEvent(input$transform_any, {

    # print warning if no var selected
    no_var_warning(input$select_var)


    selected_data <- dplyr::select(bag$data_new_df, input$select_var)


    # determine which variables are numeric; tf = true/false
    numeric_vars_tf <- sapply(selected_data, is.numeric)


    # warning popup alert if any of the selected vars isn't numeric
    if (any(numeric_vars_tf == FALSE)) {
      shinyalert("Warning!", "At least one of the selected variables is not
        quantitative. Please make sure only quantitative variables are selected.",
        type = "error"
      )
    }

    # keeps the app from crashing if any of the selected vars aren't numeric
    validate(need(all(numeric_vars_tf == TRUE), ""))


    # warning popup alert if no fxn input by user
    if (input$transform_any_fxn == "") {
      shinyalert("Warning!", "You must specify a function to use.",
        type = "error"
      )
    }

    validate(need(input$transform_any_fxn != "", ""))


    fxn_body <- input$transform_any_fxn


    x_counter <- str_count(fxn_body, "x")

    # warning popup alert if no 'x' found in fxn input by user
    if (x_counter == 0) {
      shinyalert("Warning!", "Your function needs to be a function of x.",
        type = "error"
      )
    }

    validate(need(x_counter > 0, ""))


    eval(parse(
      text = paste("transf_any <- function(", "x", ") { return(", fxn_body, ") }",
        sep = ""
      )
    ))


    # store temporary new values
    temp_values <- transf_any(selected_data)


    # warning popup alert and code invalidation if attempting to apply a
    #  transformation when not appropriate (e.g., log of a non-positive number)
    bad_identifier <- vector()

    for (i in 1:ncol(temp_values)) {
      bad_identifier[i] <- any(c(NaN, -Inf, Inf) %in% temp_values[, i])

      if (bad_identifier[i]) {
        shinyalert("Warning!", "The desired transformation isn't possible for
          certain values of the selected variable(s). Use a different
          transformation or different variable(s).",
          type = "error"
        )
        break
      }
    }

    validate(need(all(bad_identifier == FALSE), ""))


    new_values <- temp_values


    bag$data_new_df[, input$select_var] <- new_values


    # print new data set
    if (input$data_view == "table") {
      output$df_new_dt <- renderDT(bag$data_new_df, options = list(scrollX = TRUE))
    } else if (input$data_view == "spreadsheet") {
      output$df_new_rhot <- renderRHandsontable(rhandsontable(bag$data_new_df))
    }

  })




  ### make popup window for combining variables
  #################################################
  output$combine_popup <- renderUI({

    tagList(
      fluidRow(
        column(3,
          div(style = "margin-top: -2em;"),
          h3(style = "color: red; font-weight: bold;", "Step 1"),
          p(style = "color: red; font-weight: bold;", "Input the name of the new
            variable and the number of existing variables used to create it."
          )
        ),

        column(5,
          div(style = "margin-top: -2em;"),
          h3(style = "color: red; font-weight: bold;", "Step 2"),
          p(style = "color: red; font-weight: bold;", "Specify the inputs below.")
        ),

        column(2,
          div(style = "margin-top: -2em;"),
          h3(style = "color: red; font-weight: bold;", "Step 3"),
          p(style = "color: red; font-weight: bold;", "Click the button below.
            The new variable will be added to the beginning of the data set."
          )
        ),

        column(2,
          div(style = "margin-top: -2em;"),
          h3(style = "color: red; font-weight: bold;", "Step 4"),
          p(style = "color: red; font-weight: bold;", "Close out of this window
            to return to the app."
          )
        )
      ),

      fluidRow(
        column(3,
          strong("New Variable Name"),
          textInput("combined_var_name", NULL, "new.variable", width = "250px"),

          strong("Number of Variables to Combine"),
          textInput("combine_num_vars", NULL, value = 2, width = "75px")
        ),

        column(5,
          splitLayout(
            cellWidths = c("30%", "15%", "20%", "35%"),

            h3(style = "color: blue;", "New Variable ="),

            p(""),

            list(
              strong("Coefficient 1"),

              textInput(
                "combine_coeff_1",
                NULL,
                value = 1
              )
            ),

            list(
              strong("Quantitative Variable 1"),

              selectInput(
                "combine_var_1",
                NULL,
                choices = names(bag$data_new_df)
              )
            )
          ),

          lapply(2:bag$combine_num_vars, function(i) {

            list(
              splitLayout(cellWidths = c("30%", "15%", "20%", "35%"),

              p(""),

              selectInput(
                paste0("combine_operation_", i),
                strong(""),
                choices = c(
                  "+" = "+",
                  "\u2013" = "-",
                  "\u00D7" = "*",
                  "\u00F7" = "/"
                ),
                selected = ""
              ),

              list(
                strong(paste("Coefficient", i)),

                textInput(
                  paste0("combine_coeff_", i),
                  NULL,
                  value = 1
                )
              ),

              list(
                strong(paste("Quantitative Variable", i)),

                selectInput(
                  paste0("combine_var_", i),
                  NULL,
                  choices = names(bag$data_new_df)
                )
              )
            ))
          })
        ),

        column(2,
          actionButton("combine_vars", "Create New Variable",
            class = "btn btn-primary"
          )
        )
      )
    )
  })




  ### update input for number of variables to combine
  #####################################################
  observeEvent(input$combine_num_vars, {

    # warning popup alert if number of vars input not at least 2
    if (as.numeric(input$combine_num_vars) < 2 |
        is.na(as.numeric(input$combine_num_vars))
    ) {
      shinyalert("Warning!", "You must combine at least two variables.",
        type = "error"
      )
    }


    validate(need(
      !is.na(as.numeric(input$combine_num_vars)) &
        as.numeric(input$combine_num_vars) >= 2,
      ""
    ))


    updateTextInput(session, "combine_num_vars", NULL,
      value = input$combine_num_vars
    )

    bag$combine_num_vars <- as.numeric(input$combine_num_vars)
  })




  ### combine variables and print new data
  #################################################
  observeEvent(input$combine_vars, {

    # make sure all variables are quantitative
    selected_vars <- data.frame(var1 = bag$data_new_df[, input$combine_var_1])

    for (i in 2:bag$combine_num_vars) {
      selected_vars <- dplyr::bind_cols(
        selected_vars,
        data.frame(
          bag$data_new_df[, input[[paste0("combine_var_", i)]]]
        )
      )
      names(selected_vars)[ncol(selected_vars)] <- paste0("var", i)
    }

    any_non_numeric_vars <- any(unlist(lapply(selected_vars, is.numeric)) == FALSE)

    if (any_non_numeric_vars) {
      shinyalert("Warning!", "At least one of the variables you selected contains
        data that isn't quantitative. Please use a different set
        of variables.",
        type = "error"
      )
    }


    # warning popup alert if no variable name entered
    if (input$combined_var_name == "") {
      shinyalert("Warning!", "You did not input a main variable name.",
        type = "error"
      )
    }


    # warning popup alert if variable name already used
    if (input$combined_var_name %in% names(bag$data_new_df)) {
      shinyalert("Warning!", "The variable name you input is already in use in
        the data set. Please use a different name.",
        type = "error"
      )
    }


    validate(need(
      any_non_numeric_vars == FALSE &
      input$combined_var_name != "" &
      (input$combined_var_name %in% names(bag$data_new_df)) == FALSE,
      ""
    ))



    # 1st variable
    combo_expression_1 <- paste(
      input$combine_coeff_1,
      "*",
      paste0("bag$data_new_df[,'", input$combine_var_1, "']")
    )


    # all variables after 1st
    combo_expression_after_1 <- ""

    for (i in 2:bag$combine_num_vars) {
      combo_expression_after_1 <- paste(
        combo_expression_after_1,
        input[[paste0("combine_operation_", i)]],
        "(",
        input[[paste0("combine_coeff_", i)]],
        "*",
        paste0("bag$data_new_df[,'", input[[paste0("combine_var_", i)]], "']"),
        ")"
      )
    }

    combo_expression_all <- paste(combo_expression_1, combo_expression_after_1)


    # make sure calculations don't lead to Inf, -Inf, or NaN
    temp_calculation <- eval(parse(text = combo_expression_all))

    if (any(is.infinite(temp_calculation) | is.nan(temp_calculation))) {
      shinyalert("Warning!", "Your calculation involves an operation (e.g., dividing
        by 0) that results in something that isn't a number
        (e.g., infinity). Please use a different formula.",
        type = "error"
      )
    }

    validate(need(
      all(!is.infinite(temp_calculation)) & all(!is.nan(temp_calculation)),
      ""
    ))


    combo_var_df <- data.frame(combo_var = temp_calculation)
    names(combo_var_df)[1] <- input$combined_var_name


    bag$data_new_df <- dplyr::bind_cols(combo_var_df, bag$data_new_df)


    # print new data set
    if (input$data_view == "table") {
      output$df_new_dt <- renderDT(bag$data_new_df, options = list(scrollX = TRUE))
    } else if (input$data_view == "spreadsheet") {
      output$df_new_rhot <- renderRHandsontable(rhandsontable(bag$data_new_df))
    }


    # update variable names list for drop-down menu in Adjust Data tab
    updateSelectInput(session, "select_var", NULL,
      choices = names(bag$data_new_df)
    )


    # update variable name select inputs in Check Data tab
    update_var_names()


    # update variable names list for drop-down menu in Download Data tab
    updateSelectInput(session, "final_vars", NULL,
      choices =  names(bag$data_new_df)
    )
  })




  ### add random normal noise and print new data
  #################################################
  observeEvent(input$add_noise, {

    # print warning if no var selected
    no_var_warning(input$select_var)


    selected_data <- bag$data_new_df[, input$select_var]


    # determine which variables are numeric; tf = true/false
    numeric_vars_tf <- sapply(selected_data, is.numeric)


    # warning popup alert if any of the selected vars isn't numeric
    if (any(numeric_vars_tf == FALSE)) {
      shinyalert("Warning!", "At least one of the selected variables is not
        quantitative. Please make sure only quantitative variables are selected.",
        type = "error"
      )
    }

    # keeps the app from crashing if any of the selected vars aren't numeric
    validate(need(all(numeric_vars_tf == TRUE), ""))


    # warning popup alert if mean or SD missing
    if (is.na(input$noise_mean) | is.na(input$noise_sd)) {
      shinyalert("Warning!", "You must specify both a mean and SD.",
        type = "error"
      )
    }

    validate(need(is.numeric(input$noise_mean) & is.numeric(input$noise_sd), ""))


    # warning popup alert if SD < 0
    if (input$noise_sd < 0) {
      shinyalert("Warning!", "The SD cannot be negative.", type = "error")
    }

    validate(need(input$noise_sd >= 0, ""))


    # add random normal noise
    if (length(input$select_var) == 1) {
      noise <- rnorm(length(selected_data), input$noise_mean, input$noise_sd)
    } else {
      noise <- rnorm(nrow(selected_data)*ncol(selected_data), input$noise_mean,
        input$noise_sd
      )
    }
    new_values <- selected_data + noise


    bag$data_new_df[, input$select_var] <- new_values


    # print new data set
    if (input$data_view == "table") {
      output$df_new_dt <- renderDT(bag$data_new_df, options = list(scrollX = TRUE))
    } else if (input$data_view == "spreadsheet") {
      output$df_new_rhot <- renderRHandsontable(rhandsontable(bag$data_new_df))
    }

  })



  ### convert existing value(s) to outlier(s)
  #################################################
  observeEvent(input$convert_outlier, {

    # print warning and keep app from crashing if no var selected
    no_var_warning(input$select_var)


    # need as.data.frame() in case only one var selected
    selected_data <- as.data.frame(bag$data_new_df[, input$select_var])


    # determine which variables are numeric; tf = true/false
    numeric_vars_tf <- sapply(selected_data, is.numeric)


    # warning popup alert if any of the selected vars isn't numeric
    if (any(numeric_vars_tf == FALSE)) {
      shinyalert("Warning!", "At least one of the selected variables is not
        quantitative. Please make sure only quantitative variables are selected.",
        type = "error"
      )
    }

    # keeps the app from crashing if any of the selected vars aren't numeric
    validate(need(all(numeric_vars_tf == TRUE), ""))


    # warning popup alert if no number of values to convert input
    if (is.na(input$n_outlier_convert)) {
      shinyalert("Warning!", "You must specify the number of values to convert.",
        type = "error"
      )
    }

    validate(need(is.numeric(input$n_outlier_convert), ""))


    # warning popup alert if number of values to convert input less than 1
    if (input$n_outlier_convert < 1) {
      shinyalert("Warning!", "The number of values to convert must be at least 1.",
        type = "error"
      )
    }

    validate(need(input$n_outlier_convert >= 1, ""))


    updated_values <- selected_data
    new_values <- vector()

    # new value = 3*IQR from 1st or 3rd quartile
    if (input$outlier_convert_high_low == "high") {

      for (j in 1:ncol(selected_data)) {
        largest <- sort(selected_data[, j], dec = TRUE)[1:input$n_outlier_convert]
        convert_obs <- which(
          selected_data[, j] >= largest[length(largest)]
        )[1:input$n_outlier_convert]

        for (i in 1:input$n_outlier_convert) {
          new_values[i] <- quantile(selected_data[, j], 0.75, na.rm = TRUE) +
            (3 + 0.1*i)*IQR(selected_data[, j], na.rm = TRUE)
        }

        updated_values[convert_obs, j] <- new_values

        # reset new_values
        new_values <- vector()
      }
    } else {

      for (j in 1:ncol(selected_data)) {
        smallest <- sort(selected_data[, j])[1:input$n_outlier_convert]
        convert_obs <- which(
          selected_data[, j] <= smallest[length(smallest)]
        )[1:input$n_outlier_convert]

        for (i in 1:input$n_outlier_convert) {
          new_values[i] <- quantile(selected_data[, j], 0.25, na.rm = TRUE) -
            (3 + 0.1*i)*IQR(selected_data[, j], na.rm = TRUE)
        }

        updated_values[convert_obs, j] <- new_values

        # reset new_values
        new_values <- vector()
      }
    }


    # updated data set
    bag$data_new_df[, input$select_var] <- updated_values


    # print new data set
    if (input$data_view == "table") {
      output$df_new_dt <- renderDT(bag$data_new_df, options = list(scrollX = TRUE))
    } else if (input$data_view == "spreadsheet") {
      output$df_new_rhot <- renderRHandsontable(rhandsontable(bag$data_new_df))
    }

  })



  ### add outlier(s)
  #################################################
  observeEvent(input$add_outlier, {

    # print warning and keep app from crashing if no var selected
    no_var_warning(input$select_var)


    # need as.data.frame() in case only one var selected
    selected_data <- as.data.frame(bag$data_new_df[, input$select_var])


    # determine which variables are numeric; tf = true/false
    numeric_vars_tf <- sapply(selected_data, is.numeric)


    # warning popup alert if any of the selected vars isn't numeric
    if (any(numeric_vars_tf == FALSE)) {
      shinyalert("Warning!", "At least one of the selected variables is not
        quantitative. Please make sure only quantitative variables are selected.",
        type = "error"
      )
    }

    # keeps the app from crashing if any of the selected vars aren't numeric
    validate(need(all(numeric_vars_tf == TRUE), ""))


    # warning popup alert if no number of outliers to add input
    if (is.na(input$n_outlier_add)) {
      shinyalert("Warning!", "You must specify the number of outliers to add.",
        type = "error"
      )
    }

    validate(need(is.numeric(input$n_outlier_add), ""))


    # warning popup alert if number of outliers to add input less than 1
    if (input$n_outlier_add < 1) {
      shinyalert("Warning!", "The number of outliers to add must be at least 1.",
        type = "error"
      )
    }

    validate(need(input$n_outlier_add >= 1, ""))


    # initialize matrix for new values
    new_values <- matrix(NA,
      nrow = input$n_outlier_add,
      ncol = ncol(selected_data)
    )

    # new value = at least 3*IQR from 1st or 3rd quartile
    if (input$outlier_add_high_low == "high") {

      for (j in 1:ncol(selected_data)) {
        for (i in 1:input$n_outlier_add) {
          new_values[i, j] <- quantile(selected_data[, j], 0.75, na.rm = TRUE) +
            (3 + 0.1*i)*IQR(selected_data[, j], na.rm = TRUE)
        }
      }
    } else {

      for (j in 1:ncol(selected_data)) {
        for (i in 1:input$n_outlier_add) {
          new_values[i, j] <- quantile(selected_data[, j], 0.25, na.rm = TRUE) -
            (3 + 0.1*i)*IQR(selected_data[, j], na.rm = TRUE)
        }
      }
    }


    # create new observation w/ all NAs, then update desired ones to new values
    n_obs <- nrow(bag$data_new_df)
    bag$data_new_df[(n_obs + 1):(n_obs + input$n_outlier_add), ] <- NA
    bag$data_new_df[(n_obs + 1):(n_obs + input$n_outlier_add), input$select_var] <-
      new_values


    # print new data set
    if (input$data_view == "table") {
      output$df_new_dt <- renderDT(bag$data_new_df, options = list(scrollX = TRUE))
    } else if (input$data_view == "spreadsheet") {
      output$df_new_rhot <- renderRHandsontable(rhandsontable(bag$data_new_df))
    }
  })



  ### force non-constant variance (regression)
  #################################################
  observeEvent(input$force_NCV_check, {

    # update variable names list for force non-constant variance in Adjust Data
    updateSelectInput(session, "force_NCV_expl", "Explanatory Variable",
      choices = names(bag$data_new_df)
    )
  })


  observeEvent(input$force_NCV, {

    # subset data for selected variable
    x <- bag$data_new_df[, input$force_NCV_expl]


    # warning popup alert if x variable isn't numeric
    if (!is.numeric(x)) {
      shinyalert("Warning!", "The explanatory variable you selected is not
        quantitative. Please select a quantitative variable.",
        type = "error"
      )
    }

    # keep the app from crashing if at least one selected var isn't numeric
    validate(need(is.numeric(x), ""))


    # warning popup alert if response min or max not input
    if (is.na(input$force_NCV_min_y) | is.na(input$force_NCV_max_y)) {
      shinyalert("Warning!", "You must specify both a min and max for the
        response.",
        type = "error"
      )
    }

    validate(need(
      is.numeric(input$force_NCV_min_y) & is.numeric(input$force_NCV_max_y),
      ""
    ))


    # warning popup alert if no new name input
    if (input$response_name == "") {
      shinyalert("Warning!", "You must specify a name for the response variable.",
        type = "error"
      )
    }

    # warning popup alert if new name contains any spaces
    if (str_detect(input$response_name, " ")) {
      shinyalert("Warning!", "The response variable name you specified contains a
        space. Please choose a name without any spaces.",
        type = "error"
      )
    }

    # warning popup alert if df already contains a variable w/ that new name
    if (input$response_name %in% names(bag$data_new_df)) {
      shinyalert("Warning!", "The data set already contains a variable with that
        name. Please choose a different name.",
        type = "error"
      )
    }


    # keep the app from assigning a variable name w/ spaces and from assigning a
    #  duplicate variable name
    validate(need(
      (input$response_name != "") &
      (str_detect(input$response_name, " ") == FALSE) &
      ((input$response_name %in% names(bag$data_new_df)) == FALSE),
      ""
    ))


    # arrange values in increasing order
    df <- data.frame(obs = 1:length(x), x = x)
    df_inc_x <- dplyr::arrange(df, x)


    # update so stay w/in bounds w/ noise
    new_min_y <- input$force_NCV_min_y +
      0.1*(input$force_NCV_max_y - input$force_NCV_min_y)
    new_max_y <- input$force_NCV_max_y -
      0.1*(input$force_NCV_max_y - input$force_NCV_min_y)


    # midpoint between min and max y values input by user
    mid_y <- mean(c(input$force_NCV_min_y, input$force_NCV_max_y))


    # count number of non-NA observations and initialize y vector
    n_values <- length(na.omit(x))
    y <- rep(NA, length(x))

    if (input$telescope_slope == "pos") {
      if (input$telescope_direction == "inc") {

        end1 <- new_max_y
        end2 <- mean(c(mid_y, end1))
        end4 <- mean(c(mid_y, new_min_y))
        end3 <- mean(c(mid_y, end4))

        start <- new_min_y + (end2 - mid_y)

        slope1 <- (end1 - start) /
          (max(df$x, na.rm = TRUE) - min(df$x, na.rm = TRUE))
        slope2 <- (end2 - start) /
          (max(df$x, na.rm = TRUE) - min(df$x, na.rm = TRUE))
        slope3 <- (end3 - start) /
          (max(df$x, na.rm = TRUE) - min(df$x, na.rm = TRUE))
        slope4 <- (end4 - start) /
          (max(df$x, na.rm = TRUE) - min(df$x, na.rm = TRUE))


        noise_max_outer <- input$force_NCV_max_y - end1
        noise_max_inner <- end2 - mid_y


        if ((n_values %% 4) %in% c(0, 2)) {
          noise_outer <- runif(n_values/2, -noise_max_outer, noise_max_outer)
          noise_inner <- runif(n_values/2, -noise_max_inner, noise_max_inner)
        } else if ((n_values %% 4) == 1) {
          noise_outer <- runif(ceiling(n_values/2), -noise_max_outer, noise_max_outer)
          noise_inner <- runif(floor(n_values/2), -noise_max_inner, noise_max_inner)
        } else if ((n_values %% 4) == 3) {
          noise_outer <- runif(floor(n_values/2), -noise_max_outer, noise_max_outer)
          noise_inner <- runif(ceiling(n_values/2), -noise_max_inner, noise_max_inner)
        }


        outer_counter <- 1
        inner_counter <- 1

        for (i in 1:n_values) {
          if (i %in% seq(1, n_values, by = 4)) {
            y[i] <- start +
              slope1*(df_inc_x$x[i] - min(df_inc_x$x, na.rm = TRUE)) +
              noise_outer[outer_counter]

            outer_counter <- outer_counter + 1
          } else if (i %in% seq(2, n_values, by = 4)) {
            y[i] <- start +
              slope2*(df_inc_x$x[i] - min(df_inc_x$x, na.rm = TRUE)) +
              noise_inner[inner_counter]

            inner_counter <- inner_counter + 1
          } else if (i %in% seq(3, n_values, by = 4)) {
            y[i] <- start +
              slope3*(df_inc_x$x[i] - min(df_inc_x$x, na.rm = TRUE)) +
              noise_inner[inner_counter]

            inner_counter <- inner_counter + 1
          } else {
            y[i] <- start +
              slope4*(df_inc_x$x[i] - min(df_inc_x$x, na.rm = TRUE)) +
              noise_outer[outer_counter]

            outer_counter <- outer_counter + 1
          }
        }
      } else if (input$telescope_direction == "dec") {

        start1 <- mean(c(mid_y, new_max_y))
        start2 <- mean(c(mid_y, start1))
        start4 <- mean(c(mid_y, new_min_y))
        start3 <- mean(c(mid_y, start4))

        end <- new_max_y - (start2 - mid_y)

        slope1 <- (end - start1) /
          (max(df$x, na.rm = TRUE) - min(df$x, na.rm = TRUE))
        slope2 <- (end - start2) /
          (max(df$x, na.rm = TRUE) - min(df$x, na.rm = TRUE))
        slope3 <- (end - start3) /
          (max(df$x, na.rm = TRUE) - min(df$x, na.rm = TRUE))
        slope4 <- (end - start4) /
          (max(df$x, na.rm = TRUE) - min(df$x, na.rm = TRUE))


        noise_max_outer <- input$force_NCV_max_y - new_max_y
        noise_max_inner <- start2 - mid_y


        if ((n_values %% 4) %in% c(0, 2)) {
          noise_outer <- runif(n_values/2, -noise_max_outer, noise_max_outer)
          noise_inner <- runif(n_values/2, -noise_max_inner, noise_max_inner)
        } else if ((n_values %% 4) == 1) {
          noise_outer <- runif(ceiling(n_values/2), -noise_max_outer, noise_max_outer)
          noise_inner <- runif(floor(n_values/2), -noise_max_inner, noise_max_inner)
        } else if ((n_values %% 4) == 3) {
          noise_outer <- runif(floor(n_values/2), -noise_max_outer, noise_max_outer)
          noise_inner <- runif(ceiling(n_values/2), -noise_max_inner, noise_max_inner)
        }


        outer_counter <- 1
        inner_counter <- 1

        for (i in 1:n_values) {
          if (i %in% seq(1, n_values, by = 4)) {
            y[i] <- start1 +
              slope1*(df_inc_x$x[i] - min(df_inc_x$x, na.rm = TRUE)) +
              noise_outer[outer_counter]

            outer_counter <- outer_counter + 1
          } else if (i %in% seq(2, n_values, by = 4)) {
            y[i] <- start2 +
              slope2*(df_inc_x$x[i] - min(df_inc_x$x, na.rm = TRUE)) +
              noise_inner[inner_counter]

            inner_counter <- inner_counter + 1
          } else if (i %in% seq(3, n_values, by = 4)) {
            y[i] <- start3 +
              slope3*(df_inc_x$x[i] - min(df_inc_x$x, na.rm = TRUE)) +
              noise_inner[inner_counter]

            inner_counter <- inner_counter + 1
          } else {
            y[i] <- start4 +
              slope4*(df_inc_x$x[i] - min(df_inc_x$x, na.rm = TRUE)) +
              noise_outer[outer_counter]

            outer_counter <- outer_counter + 1
          }
        }
      } else if (input$telescope_direction == "inc_dec") {

        start_inc <- new_min_y
        end_inc1 <- mean(c(mid_y, new_max_y))
        end_inc2 <- mean(c(mid_y, end_inc1))
        end_inc4 <- mean(c(mid_y, new_min_y))
        end_inc3 <- mean(c(mid_y, end_inc4))

        #mid_x <- median(c(min(df$x, na.rm = TRUE), max(df$x, na.rm = TRUE)))
        mid_x <- mean(df$x, na.rm = TRUE)
        slope_inc1 <- (end_inc1 - start_inc) / (mid_x - min(df$x, na.rm = TRUE))
        slope_inc2 <- (end_inc2 - start_inc) / (mid_x - min(df$x, na.rm = TRUE))
        slope_inc3 <- (end_inc3 - start_inc) / (mid_x - min(df$x, na.rm = TRUE))
        slope_inc4 <- (end_inc4 - start_inc) / (mid_x - min(df$x, na.rm = TRUE))

        slope_dec1 <- (new_max_y - end_inc1) / (max(df$x, na.rm = TRUE) - mid_x)
        slope_dec2 <- (new_max_y - end_inc2) / (max(df$x, na.rm = TRUE) - mid_x)
        slope_dec3 <- (new_max_y - end_inc3) / (max(df$x, na.rm = TRUE) - mid_x)
        slope_dec4 <- (new_max_y - end_inc4) / (max(df$x, na.rm = TRUE) - mid_x)


        noise_max_outer <- input$force_NCV_max_y - new_max_y
        noise_max_inner <- end_inc2 - mid_y


        if ((n_values %% 4) %in% c(0, 2)) {
          noise_outer <- runif(n_values/2, -noise_max_outer, noise_max_outer)
          noise_inner <- runif(n_values/2, -noise_max_inner, noise_max_inner)
        } else if ((n_values %% 4) == 1) {
          noise_outer <- runif(ceiling(n_values/2), -noise_max_outer, noise_max_outer)
          noise_inner <- runif(floor(n_values/2), -noise_max_inner, noise_max_inner)
        } else if ((n_values %% 4) == 3) {
          noise_outer <- runif(floor(n_values/2), -noise_max_outer, noise_max_outer)
          noise_inner <- runif(ceiling(n_values/2), -noise_max_inner, noise_max_inner)
        }


        outer_counter <- 1
        inner_counter <- 1


        obs_inc_end <- RoundTo(n_values/2, 4)

        for (i in 1:obs_inc_end) {
          if (i %in% seq(1, obs_inc_end, by = 4)) {
            y[i] <- start_inc +
              slope_inc1*(df_inc_x$x[i] - min(df_inc_x$x, na.rm = TRUE)) +
              noise_outer[outer_counter]

            outer_counter <- outer_counter + 1
          } else if (i %in% seq(2, obs_inc_end, by = 4)) {
            y[i] <- start_inc +
              slope_inc2*(df_inc_x$x[i] - min(df_inc_x$x, na.rm = TRUE)) +
              noise_inner[inner_counter]

            inner_counter <- inner_counter + 1
          } else if (i %in% seq(3, obs_inc_end, by = 4)) {
            y[i] <- start_inc +
              slope_inc3*(df_inc_x$x[i] - min(df_inc_x$x, na.rm = TRUE)) +
              noise_inner[inner_counter]

            inner_counter <- inner_counter + 1
          } else {
            y[i] <- start_inc +
              slope_inc4*(df_inc_x$x[i] - min(df_inc_x$x, na.rm = TRUE)) +
              noise_outer[outer_counter]

            outer_counter <- outer_counter + 1
          }
        }


        obs_dec_start <- obs_inc_end + 1

        for (i in obs_dec_start:n_values) {
          if (i %in% seq(obs_dec_start, n_values, by = 4)) {
            y[i] <- end_inc4 +
              slope_dec4*(df_inc_x$x[i] - df_inc_x$x[obs_inc_end-3]) +
              noise_outer[outer_counter]

            outer_counter <- outer_counter + 1
          } else if (i %in% seq(obs_dec_start + 1, n_values, by = 4)) {
            y[i] <- end_inc3 +
              slope_dec3*(df_inc_x$x[i] - df_inc_x$x[obs_inc_end-2]) +
              noise_inner[inner_counter]

            inner_counter <- inner_counter + 1
          } else if (i %in% seq(obs_dec_start + 2, n_values, by = 4)) {
            y[i] <- end_inc2 +
              slope_dec2*(df_inc_x$x[i] - df_inc_x$x[obs_inc_end-1]) +
              noise_inner[inner_counter]

            inner_counter <- inner_counter + 1
          } else {
            y[i] <- end_inc1 +
              slope_dec1*(df_inc_x$x[i] - df_inc_x$x[obs_inc_end]) +
              noise_outer[outer_counter]

            outer_counter <- outer_counter + 1
          }
        }
      } else if (input$telescope_direction == "dec_inc") {

        start1 <- mid_y
        start2 <- mid_y - 1/3*(mid_y - new_min_y)
        start3 <- new_min_y + 1/3*(mid_y - new_min_y)
        start4 <- new_min_y

        end1 <- new_max_y
        end2 <- new_max_y - 1/3*(new_max_y - mid_y)
        end3 <- mid_y + 1/3*(new_max_y - mid_y)
        end4 <- mid_y

        mid_x <- mean(df$x, na.rm = TRUE)
        slope_dec1 <- (mid_y - start1) / (mid_x - min(df$x, na.rm = TRUE))
        slope_dec2 <- (mid_y - start2) / (mid_x - min(df$x, na.rm = TRUE))
        slope_dec3 <- (mid_y - start3) / (mid_x - min(df$x, na.rm = TRUE))
        slope_dec4 <- (mid_y - start4) / (mid_x - min(df$x, na.rm = TRUE))

        slope_inc1 <- (end1 - mid_y) / (max(df$x, na.rm = TRUE) - mid_x)
        slope_inc2 <- (end2 - mid_y) / (max(df$x, na.rm = TRUE) - mid_x)
        slope_inc3 <- (end3 - mid_y) / (max(df$x, na.rm = TRUE) - mid_x)
        slope_inc4 <- (end4 - mid_y) / (max(df$x, na.rm = TRUE) - mid_x)


        noise_max_outer <- input$force_NCV_max_y - new_max_y
        noise_max_inner <- mid_y - start2


        if ((n_values %% 4) %in% c(0, 2)) {
          noise_outer <- runif(n_values/2, -noise_max_outer, noise_max_outer)
          noise_inner <- runif(n_values/2, -noise_max_inner, noise_max_inner)
        } else if ((n_values %% 4) == 1) {
          noise_outer <- runif(ceiling(n_values/2), -noise_max_outer, noise_max_outer)
          noise_inner <- runif(floor(n_values/2), -noise_max_inner, noise_max_inner)
        } else if ((n_values %% 4) == 3) {
          noise_outer <- runif(floor(n_values/2), -noise_max_outer, noise_max_outer)
          noise_inner <- runif(ceiling(n_values/2), -noise_max_inner, noise_max_inner)
        }


        outer_counter <- 1
        inner_counter <- 1


        obs_dec_end <- RoundTo(n_values/2, 4)

        for (i in 1:obs_dec_end) {
          if (i %in% seq(1, obs_dec_end, by = 4)) {
            y[i] <- start1 +
              slope_dec1*(df_inc_x$x[i] - min(df_inc_x$x, na.rm = TRUE)) +
              noise_outer[outer_counter]

            outer_counter <- outer_counter + 1
          } else if (i %in% seq(2, obs_dec_end, by = 4)) {
            y[i] <- start2 +
              slope_dec2*(df_inc_x$x[i] - min(df_inc_x$x, na.rm = TRUE)) +
              noise_inner[inner_counter]

            inner_counter <- inner_counter + 1
          } else if (i %in% seq(3, obs_dec_end, by = 4)) {
            y[i] <- start3 +
              slope_dec3*(df_inc_x$x[i] - min(df_inc_x$x, na.rm = TRUE)) +
              noise_inner[inner_counter]

            inner_counter <- inner_counter + 1
          } else {
            y[i] <- start4 +
              slope_dec4*(df_inc_x$x[i] - min(df_inc_x$x, na.rm = TRUE)) +
              noise_outer[outer_counter]

            outer_counter <- outer_counter + 1
          }
        }


        obs_inc_start <- obs_dec_end + 1

        for (i in obs_inc_start:n_values) {
          if (i %in% seq(obs_inc_start, n_values, by = 4)) {
            y[i] <- mid_y +
              slope_inc4*(df_inc_x$x[i] - df_inc_x$x[obs_dec_end-3]) +
              noise_outer[outer_counter]

            outer_counter <- outer_counter + 1
          } else if (i %in% seq(obs_inc_start + 1, n_values, by = 4)) {
            y[i] <- mid_y +
              slope_inc3*(df_inc_x$x[i] - df_inc_x$x[obs_dec_end-2]) +
              noise_inner[inner_counter]

            inner_counter <- inner_counter + 1
          } else if (i %in% seq(obs_inc_start + 2, n_values, by = 4)) {
            y[i] <- mid_y +
              slope_inc2*(df_inc_x$x[i] - df_inc_x$x[obs_dec_end-1]) +
              noise_inner[inner_counter]

            inner_counter <- inner_counter + 1
          } else {
            y[i] <- mid_y +
              slope_inc1*(df_inc_x$x[i] - df_inc_x$x[obs_dec_end]) +
              noise_outer[outer_counter]

            outer_counter <- outer_counter + 1
          }
        }
      }
    } else if (input$telescope_slope == "neg") {
      if (input$telescope_direction == "inc") {

        start1 <- new_max_y
        start2 <- mean(c(mid_y, start1))
        start4 <- mean(c(mid_y, new_min_y))
        start3 <- mean(c(mid_y, start4))

        end <- new_min_y + (start2 - mid_y)

        slope1 <- (end - start1) /
          (max(df$x, na.rm = TRUE) - min(df$x, na.rm = TRUE))
        slope2 <- (end - start2) /
          (max(df$x, na.rm = TRUE) - min(df$x, na.rm = TRUE))
        slope3 <- (end - start3) /
          (max(df$x, na.rm = TRUE) - min(df$x, na.rm = TRUE))
        slope4 <- (end - start4) /
          (max(df$x, na.rm = TRUE) - min(df$x, na.rm = TRUE))


        noise_max_outer <- input$force_NCV_max_y - start1
        noise_max_inner <- start2 - mid_y


        if ((n_values %% 4) %in% c(0, 2)) {
          noise_outer <- runif(n_values/2, -noise_max_outer, noise_max_outer)
          noise_inner <- runif(n_values/2, -noise_max_inner, noise_max_inner)
        } else if ((n_values %% 4) == 1) {
          noise_outer <- runif(ceiling(n_values/2), -noise_max_outer, noise_max_outer)
          noise_inner <- runif(floor(n_values/2), -noise_max_inner, noise_max_inner)
        } else if ((n_values %% 4) == 3) {
          noise_outer <- runif(floor(n_values/2), -noise_max_outer, noise_max_outer)
          noise_inner <- runif(ceiling(n_values/2), -noise_max_inner, noise_max_inner)
        }


        outer_counter <- 1
        inner_counter <- 1

        for (i in 1:n_values) {
          if (i %in% seq(1, n_values, by = 4)) {
            y[i] <- start1 +
              slope1*(df_inc_x$x[i] - min(df_inc_x$x, na.rm = TRUE)) +
              noise_outer[outer_counter]

            outer_counter <- outer_counter + 1
          } else if (i %in% seq(2, n_values, by = 4)) {
            y[i] <- start2 +
              slope2*(df_inc_x$x[i] - min(df_inc_x$x, na.rm = TRUE)) +
              noise_inner[inner_counter]

            inner_counter <- inner_counter + 1
          } else if (i %in% seq(3, n_values, by = 4)) {
            y[i] <- start3 +
              slope3*(df_inc_x$x[i] - min(df_inc_x$x, na.rm = TRUE)) +
              noise_inner[inner_counter]

            inner_counter <- inner_counter + 1
          } else {
            y[i] <- start4 +
              slope4*(df_inc_x$x[i] - min(df_inc_x$x, na.rm = TRUE)) +
              noise_outer[outer_counter]

            outer_counter <- outer_counter + 1
          }
        }
      } else if (input$telescope_direction == "dec") {

        end1 <- mean(c(mid_y, new_max_y))
        end2 <- mean(c(mid_y, end1))
        end3 <- mean(c(mid_y, new_min_y))
        end4 <- new_min_y

        start <- new_max_y - (end2 - mid_y)

        slope1 <- (end1 - start) /
          (max(df$x, na.rm = TRUE) - min(df$x, na.rm = TRUE))
        slope2 <- (end2 - start) /
          (max(df$x, na.rm = TRUE) - min(df$x, na.rm = TRUE))
        slope3 <- (end3 - start) /
          (max(df$x, na.rm = TRUE) - min(df$x, na.rm = TRUE))
        slope4 <- (end4 - start) /
          (max(df$x, na.rm = TRUE) - min(df$x, na.rm = TRUE))


        noise_max_outer <- input$force_NCV_max_y - new_max_y
        noise_max_inner <- end2 - mid_y


        if ((n_values %% 4) %in% c(0, 2)) {
          noise_outer <- runif(n_values/2, -noise_max_outer, noise_max_outer)
          noise_inner <- runif(n_values/2, -noise_max_inner, noise_max_inner)
        } else if ((n_values %% 4) == 1) {
          noise_outer <- runif(ceiling(n_values/2), -noise_max_outer, noise_max_outer)
          noise_inner <- runif(floor(n_values/2), -noise_max_inner, noise_max_inner)
        } else if ((n_values %% 4) == 3) {
          noise_outer <- runif(floor(n_values/2), -noise_max_outer, noise_max_outer)
          noise_inner <- runif(ceiling(n_values/2), -noise_max_inner, noise_max_inner)
        }


        outer_counter <- 1
        inner_counter <- 1

        for (i in 1:n_values) {
          if (i %in% seq(1, n_values, by = 4)) {
            y[i] <- start +
              slope1*(df_inc_x$x[i] - min(df_inc_x$x, na.rm = TRUE)) +
              noise_outer[outer_counter]

            outer_counter <- outer_counter + 1
          } else if (i %in% seq(2, n_values, by = 4)) {
            y[i] <- start +
              slope2*(df_inc_x$x[i] - min(df_inc_x$x, na.rm = TRUE)) +
              noise_inner[inner_counter]

            inner_counter <- inner_counter + 1
          } else if (i %in% seq(3, n_values, by = 4)) {
            y[i] <- start +
              slope3*(df_inc_x$x[i] - min(df_inc_x$x, na.rm = TRUE)) +
              noise_inner[inner_counter]

            inner_counter <- inner_counter + 1
          } else {
            y[i] <- start +
              slope4*(df_inc_x$x[i] - min(df_inc_x$x, na.rm = TRUE)) +
              noise_outer[outer_counter]

            outer_counter <- outer_counter + 1
          }
        }
      } else if (input$telescope_direction == "inc_dec") {

        end_inc1 <- mean(c(mid_y, new_max_y))
        end_inc2 <- mean(c(mid_y, end_inc1))
        end_inc4 <- mean(c(mid_y, new_min_y))
        end_inc3 <- mean(c(mid_y, end_inc4))

        start_inc <- new_max_y - (end_inc2 - mid_y)

        mid_x <- mean(df$x, na.rm = TRUE)
        slope_inc1 <- (end_inc1 - start_inc) / (mid_x - min(df$x, na.rm = TRUE))
        slope_inc2 <- (end_inc2 - start_inc) / (mid_x - min(df$x, na.rm = TRUE))
        slope_inc3 <- (end_inc3 - start_inc) / (mid_x - min(df$x, na.rm = TRUE))
        slope_inc4 <- (end_inc4 - start_inc) / (mid_x - min(df$x, na.rm = TRUE))

        slope_dec1 <- (new_min_y - end_inc1) / (max(df$x, na.rm = TRUE) - mid_x)
        slope_dec2 <- (new_min_y - end_inc2) / (max(df$x, na.rm = TRUE) - mid_x)
        slope_dec3 <- (new_min_y - end_inc3) / (max(df$x, na.rm = TRUE) - mid_x)
        slope_dec4 <- (new_min_y - end_inc4) / (max(df$x, na.rm = TRUE) - mid_x)


        noise_max_outer <- input$force_NCV_max_y - new_max_y
        noise_max_inner <- end_inc2 - mid_y


        if ((n_values %% 4) %in% c(0, 2)) {
          noise_outer <- runif(n_values/2, -noise_max_outer, noise_max_outer)
          noise_inner <- runif(n_values/2, -noise_max_inner, noise_max_inner)
        } else if ((n_values %% 4) == 1) {
          noise_outer <- runif(ceiling(n_values/2), -noise_max_outer, noise_max_outer)
          noise_inner <- runif(floor(n_values/2), -noise_max_inner, noise_max_inner)
        } else if ((n_values %% 4) == 3) {
          noise_outer <- runif(floor(n_values/2), -noise_max_outer, noise_max_outer)
          noise_inner <- runif(ceiling(n_values/2), -noise_max_inner, noise_max_inner)
        }


        outer_counter <- 1
        inner_counter <- 1


        obs_inc_end <- RoundTo(n_values/2, 4)

        for (i in 1:obs_inc_end) {
          if (i %in% seq(1, obs_inc_end, by = 4)) {
            y[i] <- start_inc +
              slope_inc1*(df_inc_x$x[i] - min(df_inc_x$x, na.rm = TRUE)) +
              noise_outer[outer_counter]

            outer_counter <- outer_counter + 1
          } else if (i %in% seq(2, obs_inc_end, by = 4)) {
            y[i] <- start_inc +
              slope_inc2*(df_inc_x$x[i] - min(df_inc_x$x, na.rm = TRUE)) +
              noise_inner[inner_counter]

            inner_counter <- inner_counter + 1
          } else if (i %in% seq(3, obs_inc_end, by = 4)) {
            y[i] <- start_inc +
              slope_inc3*(df_inc_x$x[i] - min(df_inc_x$x, na.rm = TRUE)) +
              noise_inner[inner_counter]

            inner_counter <- inner_counter + 1
          } else {
            y[i] <- start_inc +
              slope_inc4*(df_inc_x$x[i] - min(df_inc_x$x, na.rm = TRUE)) +
              noise_outer[outer_counter]

            outer_counter <- outer_counter + 1
          }
        }


        obs_dec_start <- obs_inc_end + 1

        for (i in obs_dec_start:n_values) {
          if (i %in% seq(obs_dec_start, n_values, by = 4)) {
            y[i] <- end_inc4 +
              slope_dec4*(df_inc_x$x[i] - df_inc_x$x[obs_inc_end-3]) +
              noise_outer[outer_counter]

            outer_counter <- outer_counter + 1
          } else if (i %in% seq(obs_dec_start + 1, n_values, by = 4)) {
            y[i] <- end_inc3 +
              slope_dec3*(df_inc_x$x[i] - df_inc_x$x[obs_inc_end-2]) +
              noise_inner[inner_counter]

            inner_counter <- inner_counter + 1
          } else if (i %in% seq(obs_dec_start + 2, n_values, by = 4)) {
            y[i] <- end_inc2 +
              slope_dec2*(df_inc_x$x[i] - df_inc_x$x[obs_inc_end-1]) +
              noise_inner[inner_counter]

            inner_counter <- inner_counter + 1
          } else {
            y[i] <- end_inc1 +
              slope_dec1*(df_inc_x$x[i] - df_inc_x$x[obs_inc_end]) +
              noise_outer[outer_counter]

            outer_counter <- outer_counter + 1
          }
        }
      } else if (input$telescope_direction == "dec_inc") {

        start1 <- new_max_y
        start2 <- new_max_y - 1/3*(new_max_y - mid_y)
        start3 <- mid_y + 1/3*(new_max_y - mid_y)
        start4 <- mid_y

        end1 <- mid_y
        end2 <- mid_y - 1/3*(new_max_y - mid_y)
        end3 <- new_min_y + 1/3*(new_max_y - mid_y)
        end4 <- new_min_y

        mid_x <- mean(df$x, na.rm = TRUE)
        slope_dec1 <- (mid_y - start1) / (mid_x - min(df$x, na.rm = TRUE))
        slope_dec2 <- (mid_y - start2) / (mid_x - min(df$x, na.rm = TRUE))
        slope_dec3 <- (mid_y - start3) / (mid_x - min(df$x, na.rm = TRUE))
        slope_dec4 <- (mid_y - start4) / (mid_x - min(df$x, na.rm = TRUE))

        slope_inc1 <- (end1 - mid_y) / (max(df$x, na.rm = TRUE) - mid_x)
        slope_inc2 <- (end2 - mid_y) / (max(df$x, na.rm = TRUE) - mid_x)
        slope_inc3 <- (end3 - mid_y) / (max(df$x, na.rm = TRUE) - mid_x)
        slope_inc4 <- (end4 - mid_y) / (max(df$x, na.rm = TRUE) - mid_x)


        noise_max_outer <- input$force_NCV_max_y - new_max_y
        noise_max_inner <- start3 - mid_y


        if ((n_values %% 4) %in% c(0, 2)) {
          noise_outer <- runif(n_values/2, -noise_max_outer, noise_max_outer)
          noise_inner <- runif(n_values/2, -noise_max_inner, noise_max_inner)
        } else if ((n_values %% 4) == 1) {
          noise_outer <- runif(ceiling(n_values/2), -noise_max_outer, noise_max_outer)
          noise_inner <- runif(floor(n_values/2), -noise_max_inner, noise_max_inner)
        } else if ((n_values %% 4) == 3) {
          noise_outer <- runif(floor(n_values/2), -noise_max_outer, noise_max_outer)
          noise_inner <- runif(ceiling(n_values/2), -noise_max_inner, noise_max_inner)
        }


        outer_counter <- 1
        inner_counter <- 1


        obs_dec_end <- RoundTo(n_values/2, 4)

        for (i in 1:obs_dec_end) {
          if (i %in% seq(1, obs_dec_end, by = 4)) {
            y[i] <- start1 +
              slope_dec1*(df_inc_x$x[i] - min(df_inc_x$x, na.rm = TRUE)) +
              noise_outer[outer_counter]

            outer_counter <- outer_counter + 1
          } else if (i %in% seq(2, obs_dec_end, by = 4)) {
            y[i] <- start2 +
              slope_dec2*(df_inc_x$x[i] - min(df_inc_x$x, na.rm = TRUE)) +
              noise_inner[inner_counter]

            inner_counter <- inner_counter + 1
          } else if (i %in% seq(3, obs_dec_end, by = 4)) {
            y[i] <- start3 +
              slope_dec3*(df_inc_x$x[i] - min(df_inc_x$x, na.rm = TRUE)) +
              noise_inner[inner_counter]

            inner_counter <- inner_counter + 1
          } else {
            y[i] <- start4 +
              slope_dec4*(df_inc_x$x[i] - min(df_inc_x$x, na.rm = TRUE)) +
              noise_outer[outer_counter]

            outer_counter <- outer_counter + 1
          }
        }


        obs_inc_start <- obs_dec_end + 1

        for (i in obs_inc_start:n_values) {
          if (i %in% seq(obs_inc_start, n_values, by = 4)) {
            y[i] <- mid_y +
              slope_inc4*(df_inc_x$x[i] - df_inc_x$x[obs_dec_end-3]) +
              noise_outer[outer_counter]

            outer_counter <- outer_counter + 1
          } else if (i %in% seq(obs_inc_start + 1, n_values, by = 4)) {
            y[i] <- mid_y +
              slope_inc3*(df_inc_x$x[i] - df_inc_x$x[obs_dec_end-2]) +
              noise_inner[inner_counter]

            inner_counter <- inner_counter + 1
          } else if (i %in% seq(obs_inc_start + 2, n_values, by = 4)) {
            y[i] <- mid_y +
              slope_inc2*(df_inc_x$x[i] - df_inc_x$x[obs_dec_end-1]) +
              noise_inner[inner_counter]

            inner_counter <- inner_counter + 1
          } else {
            y[i] <- mid_y +
              slope_inc1*(df_inc_x$x[i] - df_inc_x$x[obs_dec_end]) +
              noise_outer[outer_counter]

            outer_counter <- outer_counter + 1
          }
        }
      }
    }


    # convert any values below min to min and any above max to max
    y[y < input$force_NCV_min_y] <- input$force_NCV_min_y
    y[y > input$force_NCV_max_y] <- input$force_NCV_max_y


    # make df with x and y data
    new_xy <- cbind(df_inc_x, y)
    new_xy <- dplyr::arrange(new_xy, obs)


    # add new variable as last column in bag$data_new_df
    bag$data_new_df[, ncol(bag$data_new_df) + 1] <- new_xy$y


    # move new variable to beginning of df
    new_col_index <- ncol(bag$data_new_df)
    bag$data_new_df <- dplyr::relocate(bag$data_new_df, all_of(new_col_index),
      .before = names(bag$data_new_df)[1]
    )


    # set response variable name
    names(bag$data_new_df)[1] <- input$response_name


    # update drop-down menu options for selected variable in Adjust Data tab
    updateSelectInput(session, "select_var", NULL,
      choices = names(bag$data_new_df)
    )

    # update variable names list for drop-down menu in Download Data tab
    updateSelectInput(session, "final_vars", NULL,
      choices =  names(bag$data_new_df)
    )


    # print new data set
    if (input$data_view == "table") {
      output$df_new_dt <- renderDT(bag$data_new_df, options = list(scrollX = TRUE))
    } else if (input$data_view == "spreadsheet") {
      output$df_new_rhot <- renderRHandsontable(rhandsontable(bag$data_new_df))
    }


    # update variable name select inputs in Check Data tab
    update_var_names()
  })



  ### round values to nearest digit and print new data
  #################################################
  observeEvent(input$do_rounding, {

    # print warning if no var selected
    no_var_warning(input$select_var)


    rounding <- switch(input$round_digits,
      "Billion" = -9, "Hundred Million" = -8, "Ten Million" = -7, "Million" = -6,
      "Hundred Thousand" = -5, "Ten Thousand" = -4, "Thousand" = -3,
      "Hundred" = -2, "Ten" = -1, "Integer" = 0, "Tenth" = 1, "Hundredth" = 2,
      "Thousandth" = 3, "Ten Thousandth" = 4
    )


    selected_data <- bag$data_new_df[, input$select_var]


    # determine which variables are numeric; tf = true/false
    numeric_vars_tf <- sapply(selected_data, is.numeric)


    # warning popup alert if any of the selected vars isn't numeric
    if (any(numeric_vars_tf == FALSE)) {
      shinyalert("Warning!", "At least one of the selected variables is not
        quantitative. Please make sure only quantitative variables are selected.",
        type = "error"
      )
    }

    # keeps the app from crashing if any of the selected vars aren't numeric
    validate(need(all(numeric_vars_tf == TRUE), ""))


    bag$data_new_df[, input$select_var] <- round(
      bag$data_new_df[, input$select_var],
      rounding
    )


    # print new data set
    if (input$data_view == "table") {
      output$df_new_dt <- renderDT(bag$data_new_df, options = list(scrollX = TRUE))
    } else if (input$data_view == "spreadsheet") {
      output$df_new_rhot <- renderRHandsontable(rhandsontable(bag$data_new_df))
    }

  })



  ### round all values below specified min and/or
  ###  above specified max to those values, then
  ###  print new data
  #################################################
  observeEvent(input$round_min_max, {

    # print warning if no var selected
    no_var_warning(input$select_var)


    selected_data <- bag$data_new_df[, input$select_var]


    # determine which variables are numeric; tf = true/false
    numeric_vars_tf <- sapply(selected_data, is.numeric)


    # warning popup alert if any of the selected vars isn't numeric
    if (any(numeric_vars_tf == FALSE)) {
      shinyalert("Warning!", "At least one of the selected variables is not
        quantitative. Please make sure only quantitative variables are selected.",
        type = "error"
      )
    }

    # keeps the app from crashing if any of the selected vars aren't numeric
    validate(need(all(numeric_vars_tf == TRUE), ""))


    # convert/round values
    new_data <- selected_data

    if (is.numeric(input$round_min)) {
      new_data[new_data < input$round_min] <- input$round_min
    }

    if (is.numeric(input$round_max)) {
      new_data[new_data > input$round_max] <- input$round_max
    }


    bag$data_new_df[, input$select_var] <- new_data


    # print new data set
    if (input$data_view == "table") {
      output$df_new_dt <- renderDT(bag$data_new_df, options = list(scrollX = TRUE))
    } else if (input$data_view == "spreadsheet") {
      output$df_new_rhot <- renderRHandsontable(rhandsontable(bag$data_new_df))
    }

  })



  ### round values using floor or ceiling fxn
  ###  and print new data
  #################################################
  observeEvent(input$round_floor_ceil, {

    # print warning if no var selected
    no_var_warning(input$select_var)


    selected_data <- bag$data_new_df[, input$select_var]


    # determine which variables are numeric; tf = true/false
    numeric_vars_tf <- sapply(selected_data, is.numeric)


    # warning popup alert if any of the selected vars isn't numeric
    if (any(numeric_vars_tf == FALSE)) {
      shinyalert("Warning!", "At least one of the selected variables is not
        quantitative. Please make sure only quantitative variables are selected.",
        type = "error"
      )
    }

    # keeps the app from crashing if any of the selected vars aren't numeric
    validate(need(all(numeric_vars_tf == TRUE), ""))


    if (input$floor_or_ceil == "floor") {
      new_values <- floor(selected_data)
    } else {
      new_values <- ceiling(selected_data)
    }


    bag$data_new_df[, input$select_var] <- new_values


    # print new data set
    if (input$data_view == "table") {
      output$df_new_dt <- renderDT(bag$data_new_df, options = list(scrollX = TRUE))
    } else if (input$data_view == "spreadsheet") {
      output$df_new_rhot <- renderRHandsontable(rhandsontable(bag$data_new_df))
    }

  })




  ### generate new categorical variable counts
  #################################################

  # make reactive expression to subset bag$data_new_df
  subset_df <- reactive({

    if (!is.null(input$select_var)) bag$data_new_df[, input$select_var]
  })


  # make reactive expression to specify all categories of selected variables
  all_cats <- reactive({

    req(subset_df())

    subset_df() %>%
      as.matrix() %>%
      as.character() %>%
      na.omit() %>%
      unique() %>%
      sort()
  })


  # print categories w/ assigned numbers
  output$cat_var_cats <- renderPrint({

    # make df w/ category numbers and labels
    cat_var_identify_cats_df <- data.frame(
      col1 = 1:length(all_cats()),
      col2 = all_cats(),
      stringsAsFactors = FALSE
    )
    names(cat_var_identify_cats_df) <- c("Group #", " Group Label")

    print.data.frame(cat_var_identify_cats_df, row.names = FALSE)
  })



  ### update proportion options based on variable selected
  #############################################################
  observeEvent(input$adjust_cat_check, {

    output$cat_var_proportions <- renderUI({

      n_cats <- length(all_cats())

      probs <- round(rep(1/n_cats, n_cats), 4)

      all_inputs <- list()
      all_inputs <- lapply(1:(n_cats - 1),
        function(i) {
          numericInput(
            paste0("cat_var_prop_", i),
            paste("Group", i, "Proportion"),
            value = probs[i],
            step = 0.05
          )
        }
      )

      all_inputs[[n_cats]] <- numericInput(
        paste0("cat_var_prop_", n_cats),
        paste("Group", n_cats, "Proportion"),
        value = 1 - sum(probs[-n_cats]),
        step = 0.05
      )

      all_inputs
    })
  })



  ### adjust categorical var counts
  #####################################################
  observeEvent(input$adjust_cat_counts, {

    # print warning and keep app from crashing if no var selected
    no_var_warning(input$select_var)


    selected_data <- as.data.frame(subset_df(), stringsAsFactors = FALSE)


    # determine all non-NA categories and sort
    cat_names <- selected_data[, 1] %>%
      na.omit() %>%
      unique() %>%
      sort() %>%
      as.vector()


    probs <- vector()
    for (i in 1:length(all_cats())) {
      probs[i] <- input[[paste0("cat_var_prop_", i)]]
    }


    # warning popup alert if any proportions missing
    if (anyNA(probs)) {
      shinyalert("Warning!", "At least one proportion is missing.",
        type = "error"
      )
    }

    # keeps the app from crashing if any proportions missing
    validate(need(!anyNA(probs), ""))


    # warning popup alert if proportions don't add to 1
    if (sum(probs) != 1) {
      shinyalert("Warning!", "The proportions must add to exactly 1.",
        type = "error"
      )
    }

    # keeps the app from crashing if proportions don't add to 1
    validate(need(sum(probs) == 1, ""))


    # initialize matrix
    new_vars <- matrix(NA, nrow = nrow(selected_data),
      ncol = ncol(selected_data)
    )

    for (j in 1:ncol(selected_data)) {

      # randomly sample number of values in each group
      multi_successes <- as.vector(
        rmultinom(1, length(na.omit(selected_data[, j])), prob = probs)
      )

      # convert counts of values to actual data values
      multi_rep_list <- lapply(1:length(multi_successes), function(i) {
        rep(cat_names[i], each = multi_successes[i])
      })

      # convert list to vector
      ordered_values <- do.call(c, multi_rep_list)

      # permute groups so not all in a row
      new_vars[, j] <- sample(ordered_values, replace = FALSE)
    }


    # convert matrix to df
    new_vars <- as.data.frame(new_vars)


    bag$data_new_df[, input$select_var] <- new_vars


    # print new data set
    if (input$data_view == "table") {
      output$df_new_dt <- renderDT(bag$data_new_df, options = list(scrollX = TRUE))
    } else if (input$data_view == "spreadsheet") {
      output$df_new_rhot <- renderRHandsontable(rhandsontable(bag$data_new_df))
    }


    ### update proportion inputs
    #################################
    n_cats <- length(all_cats())

    lapply(1:(n_cats - 1),
      function(i) {
        updateNumericInput(
          session,
          paste0("cat_var_prop_", i),
          paste("Group", i, "Proportion"),
          value = input[[paste0("cat_var_prop_", i)]],
          step = 0.05
        )
      }
    )

    current_probs_no_last <- 0

    for (i in 1:(n_cats - 1)) {
      current_probs_no_last <- current_probs_no_last +
        input[[paste0("cat_var_prop_", i)]]
    }

    updateNumericInput(
      session,
      paste0("cat_var_prop_", n_cats),
      paste("Group", n_cats, "Proportion"),
      value = 1 - current_probs_no_last,
      step = 0.05
    )
  })



  ### convert entries denoting missing values to NAs
  #####################################################
  observeEvent(input$convert_to_missing, {

    # print warning and keep app from crashing if no var selected
    no_var_warning(input$select_var)


    bag$data_new_df[, input$select_var] <- dplyr::na_if(
      bag$data_new_df[, input$select_var], input$denote_missing
    )


    # convert to numeric vector if possible
    if (any(!is.na(as.numeric(bag$data_new_df[, input$select_var])))) {
      bag$data_new_df[, input$select_var] <- as.numeric(
        bag$data_new_df[, input$select_var]
      )
    }


    # print new data set
    if (input$data_view == "table") {
      output$df_new_dt <- renderDT(bag$data_new_df, options = list(scrollX = TRUE))
    } else if (input$data_view == "spreadsheet") {
      output$df_new_rhot <- renderRHandsontable(rhandsontable(bag$data_new_df))
    }

  })



  ### copy variable(s)
  #################################################
  observeEvent(input$copy_var, {

    # print warning and keep app from crashing if no var selected
    no_var_warning(input$select_var)


    # select data; reverse order of columns so maintain order in which user
    #  selects variables when making copies
    selected_data <- dplyr::select(bag$data_new_df, rev(input$select_var))


    # warning popup alert if no number of copies input
    if (is.na(input$num_copies)) {
      shinyalert("Warning!", "You must specify the number of copies to make.",
        type = "error"
      )
    }

    validate(need(is.numeric(input$num_copies), ""))


    # count number of columns of df
    old_n_cols <- ncol(bag$data_new_df)


    for (j in 1:ncol(selected_data)) {
      for (i in 1:input$num_copies) {

        # update number of columns of df
        old_n_cols <- ncol(bag$data_new_df)

        # copy selected variable(s) and place into (new) last column(s)
        bag$data_new_df[, old_n_cols + 1] <- selected_data[, j]


        # update copy name(s)
        new_name <- paste0(names(selected_data)[j], ".", i)

        if (new_name %in% names(bag$data_new_df)) {
          names(bag$data_new_df)[ncol(bag$data_new_df)] <- paste0(
            new_name, ".", i
          )
        } else {
          names(bag$data_new_df)[ncol(bag$data_new_df)] <- new_name
        }
      }


      # move copies to beginning of df
      new_col_indices <- (ncol(bag$data_new_df) - input$num_copies + 1) :
        ncol(bag$data_new_df)
      bag$data_new_df <- dplyr::relocate(bag$data_new_df, all_of(new_col_indices),
        .before = names(bag$data_new_df)[1]
      )


      # move original version which made copies of right before copies
      bag$data_new_df <- dplyr::relocate(bag$data_new_df, names(selected_data)[j],
        .before = names(bag$data_new_df)[1]
      )
    }



    # update drop-down menu options for selected variable in Adjust Data tab
    # make sure to keep selected, otherwise no variable will be selected as the
    #  default, and Shiny might crash depending on what user does next
    updateSelectInput(session, "select_var", NULL,
      choices = names(bag$data_new_df),
      selected = names(bag$data_new_df)[2:(input$num_copies + 1)]
    )

    # update variable names list for drop-down menu in Download Data tab
    updateSelectInput(session, "final_vars", NULL,
      choices =  names(bag$data_new_df)
    )


    # print new data set
    if (input$data_view == "table") {
      output$df_new_dt <- renderDT(bag$data_new_df, options = list(scrollX = TRUE))
    } else if (input$data_view == "spreadsheet") {
      output$df_new_rhot <- renderRHandsontable(rhandsontable(bag$data_new_df))
    }


    # update variable name select inputs in Check Data tab
    update_var_names()
  })



  ### remove variable(s)
  #################################################
  observeEvent(input$remove_var, {

    # print warning and keep app from crashing if no var selected
    no_var_warning(input$select_var)


    # remove variable(s) from df
    bag$data_new_df <- dplyr::select(bag$data_new_df, -input$select_var)


    # update drop-down menu options for selected variable in Adjust Data tab
    updateSelectInput(session, "select_var", NULL,
      choices = names(bag$data_new_df)
    )

    # update variable names list for drop-down menu in Download Data tab
    updateSelectInput(session, "final_vars", NULL,
      choices =  names(bag$data_new_df)
    )


    # print new data set
    if (input$data_view == "table") {
      output$df_new_dt <- renderDT(bag$data_new_df, options = list(scrollX = TRUE))
    } else if (input$data_view == "spreadsheet") {
      output$df_new_rhot <- renderRHandsontable(rhandsontable(bag$data_new_df))
    }


    # update variable name select inputs in Check Data tab
    update_var_names()
  })



  ### rename variable
  #################################################
  observeEvent(input$rename_var, {

    req(input$new_name)

    # print warning if no var selected
    if (is.null(input$select_var)) {
      shinyalert("Warning!", "You must select a variable before continuing.",
        type = "error"
      )
    }

    validate(need(is.null(input$select_var) == FALSE, ""))


    # warning popup alert if new name contains any spaces
    if (str_detect(input$new_name, " ")) {
      shinyalert("Warning!", "The name you specified contains a space. Please
        choose a name without any spaces.",
        type = "error"
      )
    }


    # warning popup alert if df already contains a variable w/ that new name
    if (input$new_name %in% names(bag$data_new_df)) {
      shinyalert("Warning!", "The data set already contains a variable with that
        name. Please choose a different name.",
        type = "error"
      )
    }


    # keep the app from assigning a variable name w/ spaces and from assigning a
    #  duplicate variable name
    validate(need(
      (str_detect(input$new_name, " ") == FALSE) &
      ((input$new_name %in% names(bag$data_new_df)) == FALSE),
      ""
    ))


    # rename selected variable
    names(bag$data_new_df)[which(names(bag$data_new_df) == input$select_var)] <-
      input$new_name


    # update drop-down menu options for selected variable in Adjust Data tab
    updateSelectInput(session, "select_var", NULL,
      choices = names(bag$data_new_df)
    )

    # update variable names list for drop-down menu in Download Data tab
    updateSelectInput(session, "final_vars", NULL,
      choices =  names(bag$data_new_df)
    )


    # print new data set
    if (input$data_view == "table") {
      output$df_new_dt <- renderDT(bag$data_new_df, options = list(scrollX = TRUE))
    } else if (input$data_view == "spreadsheet") {
      output$df_new_rhot <- renderRHandsontable(rhandsontable(bag$data_new_df))
    }


    # update variable name select inputs in Check Data tab
    update_var_names()
  })



  ### add row(s)
  #################################################
  observeEvent(input$add_row, {

    # warning popup alert if no number of rows to add input
    if (is.na(input$rows_to_add)) {
      shinyalert("Warning!", "You must specify the number of rows to add.",
        type = "error"
      )
    }

    validate(need(is.numeric(input$rows_to_add), ""))

    # warning popup alert if number of rows to add input less than 1
    if (input$rows_to_add < 1) {
      shinyalert("Warning!", "The number of rows to add must be at least 1.",
        type = "error"
      )
    }

    validate(need(input$rows_to_add >= 1, ""))


    bag$data_new_df[nrow(bag$data_new_df) + input$rows_to_add, ] <- NA


    # print new data set
    if (input$data_view == "table") {
      output$df_new_dt <- renderDT(bag$data_new_df, options = list(scrollX = TRUE))
    } else if (input$data_view == "spreadsheet") {
      output$df_new_rhot <- renderRHandsontable(rhandsontable(bag$data_new_df))
    }

  })



  ### remove observation
  #################################################
  observeEvent(input$remove_row, {

    # warning popup alert if no row number input
    if (is.na(input$row_to_remove)) {
      shinyalert("Warning!", "You must specify the observation to remove.",
        type = "error"
      )
    }

    validate(need(is.numeric(input$row_to_remove), ""))


    # warning popup alert if row number input not one of existing row numbers
    if (!(input$row_to_remove %in% 1:nrow(bag$data_new_df))) {
      shinyalert("Warning!", "The row number you entered is not valid.",
        type = "error"
      )
    }

    validate(need(input$row_to_remove %in% 1:nrow(bag$data_new_df), ""))


    # store variable names in case only one variable since need to readd soon
    stored_var_names <- names(bag$data_new_df)


    # remove specified observation number
    bag$data_new_df <- bag$data_new_df[-input$row_to_remove, ]


    if (is.vector(bag$data_new_df)) {
      bag$data_new_df <- as.data.frame(bag$data_new_df)
      names(bag$data_new_df) <- stored_var_names
    }


    # need to update row names, otherwise can get confusing for user
    row.names(bag$data_new_df) <- 1:nrow(bag$data_new_df)


    # print new data set
    if (input$data_view == "table") {
      output$df_new_dt <- renderDT(bag$data_new_df, options = list(scrollX = TRUE))
    } else if (input$data_view == "spreadsheet") {
      output$df_new_rhot <- renderRHandsontable(rhandsontable(bag$data_new_df))
    }

  })



  ### reset and print original data set
  #################################################
  observeEvent(input$reset_dataset, {

    # reset entire data set to original values
    bag$data_new_df <- bag$data_original_df


    # update drop-down menu options for selected variable in Adjust Data tab
    updateSelectInput(session, "select_var", NULL,
      choices = names(bag$data_new_df)
    )

    # update variable names list for drop-down menu in Download Data tab
    updateSelectInput(session, "final_vars", NULL,
      choices =  names(bag$data_new_df)
    )


    # print original data set
    if (input$data_view == "table") {
      output$df_new_dt <- renderDT(bag$data_new_df, options = list(scrollX = TRUE))
    } else if (input$data_view == "spreadsheet") {
      output$df_new_rhot <- renderRHandsontable(rhandsontable(bag$data_new_df))
    }


    # update variable name select inputs in Check Data tab
    update_var_names()
  })



  ### change data view -- table vs. spreadsheet
  #################################################
  observeEvent(input$data_view, {

    if (input$data_view == "table") {
      output$df_new_dt <- renderDT(bag$data_new_df, options = list(scrollX = TRUE))
    } else if (input$data_view == "spreadsheet") {
      output$df_new_rhot <- renderRHandsontable(rhandsontable(bag$data_new_df))
    }
  })


  # update data set if users changes it in spreadsheet view
  observeEvent(input$df_new_rhot, {

    # need to use hot_to_r here
    new <- hot_to_r(input$df_new_rhot)
    bag$data_new_df <- as.data.frame(new)


    # need to render spreadsheet again, otherwise buggy
    if (input$data_view == "table") {
      output$df_new_dt <- renderDT(bag$data_new_df, options = list(scrollX = TRUE))
    } else if (input$data_view == "spreadsheet") {
      output$df_new_rhot <- renderRHandsontable(rhandsontable(bag$data_new_df))
    }

  })




  ##############################################################################
  ############################### Check Data tab ###############################
  ##############################################################################

  ### check data set
  #################################################


  ### take screenshot of current descriptive methods page
  # observeEvent(input$descrip_screenshot, {
  #
  #   shinyjs::runjs(
  #     "html2canvas(document.querySelector('body')).then(canvas => {
  #       saveAs(canvas.toDataURL(), 'Output.png');
  #     });"
  #   )
  # })



  ### Descriptive - 1 Quantitative Var
  ###################################################
  observeEvent(input$calc_descript_1_var_quant, {

    req(input$descript_select_1var_var)


    bag$selected_var <- bag$data_new_df[, input$descript_select_1var_var]


    # warning popup alert if selected var isn't numeric
    if (!is.numeric(bag$selected_var)) {
      shinyalert("Warning!", "The variable you selected is not quantitative.
        Please select the correct variable type.",
        type = "error"
      )
    }

    # keeps the app from crashing if the selected var isn't numeric
    validate(need(is.numeric(bag$selected_var), ""))


    # make df w/ summaries
    bag$summaries <- data.frame(
      "." = c(
        round(mean(bag$selected_var, na.rm = TRUE), 4),
        round(median(bag$selected_var, na.rm = TRUE), 4),
        round(sd(bag$selected_var, na.rm = TRUE), 4),
        round(IQR(bag$selected_var, na.rm = TRUE), 4),
        round(min(bag$selected_var, na.rm = TRUE), 4),
        round(max(bag$selected_var, na.rm = TRUE), 4),
        formatC(length(na.omit(bag$selected_var))),
        sum(is.na(bag$selected_var))
      ),
      row.names = c("Mean", "Median", "Std. Dev.", "IQR", "Min", "Max",
        "# Non-missing Obs.  ", "# Missing Obs."
      )
    )


    # need this to reset output if new variable selected
    bag$do_descript_1_quant <- input$calc_descript_1_var_quant
  })


  # reset output if new variable selected
  observeEvent(input$descript_select_1var_var, {
    bag$do_descript_1_quant <- FALSE
  })


  # print summaries
  output$descript_1_stats_quant <- renderPrint({

    # don't show output or placeholder if no vars selected or if user changes var
    #  (until button pressed)
    if (bag$do_descript_1_quant != FALSE) {
      cat(capture.output(bag$summaries)[-1], sep = "\n")
    }
  })

  # print user-specified percentile
  output$descript_1_percentile <- renderPrint({

    # don't show output or placeholder if no vars selected or if user changes var
    #  (until button pressed)
    if (bag$do_descript_1_quant != FALSE) {
      round(quantile(bag$selected_var, input$percentile, na.rm = TRUE), 4)
    }
  })

  # plot histogram
  output$descript_1_hist <- renderPlot({

    # don't show output or placeholder if no vars selected or if user changes var
    #  (until button pressed)
    if (bag$do_descript_1_quant != FALSE) {

      suppressWarnings(print(
        ggplot(bag$data_new_df,
          aes_string(x = paste0("`", input$descript_select_1var_var, "`"))
        ) +
          geom_histogram(bins = input$descript_1_hist_bins) +
          ylab("Count")
      ))
    }
  })

  # plot boxplot
  output$descript_1_box <- renderPlot({

    # don't show output or placeholder if no vars selected or if user changes var
    #  (until button pressed)
    if (bag$do_descript_1_quant != FALSE) {

      suppressWarnings(print(
        ggplot(bag$data_new_df,
          aes_string(x = paste0("`", input$descript_select_1var_var, "`"))
        ) +
          geom_boxplot() +
          theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
      ))
    }
  })




  ### Descriptive - 1 Categorical Var
  ###################################################
  observeEvent(input$calc_descript_1_var_cat, {

    req(input$descript_select_1var_var)

    # subset selected variable
    bag$selected_var <- bag$data_new_df[, input$descript_select_1var_var]


    # make df w/ summaries
    #if (is.numeric(bag$selected_var)) {
      bag$summaries <- data.frame(
        "." = c(
          formatC(length(na.omit(bag$selected_var))),
          sum(is.na(bag$selected_var))
        ),
        row.names = c("# Non-missing Obs.  ", "# Missing Obs.")
      )
    # } else if (is.character(bag$selected_var)) {
    #   bag$summaries <- data.frame(
    #     "." = c(
    #       formatC(length(bag$selected_var) - sum(bag$selected_var == "")),
    #       sum(bag$selected_var == "")
    #     ),
    #     row.names = c("# Non-missing Obs.  ", "# Missing Obs.")
    #   )
    # }


    # calculate counts and percentages of each category
    bag$freqs <- table(bag$selected_var)
    bag$percents <- prop.table(bag$freqs)*100


    # need this to reset output if new variable selected
    bag$do_descript_1_cat <- input$calc_descript_1_var_cat
  })


  # reset output if new variable selected
  observeEvent(input$descript_select_1var_var, {
    bag$do_descript_1_cat <- FALSE
  })


  # print summaries
  output$descript_1_stats_cat <- renderPrint({

    # don't show output or placeholder if no vars selected or if user changes var
    #  (until button pressed)
    if (bag$do_descript_1_cat != FALSE) {
      cat(capture.output(bag$summaries)[-1], sep = "\n")
    }
  })

  # print count and percentage for each category
  output$descript_1_counts_percents_cat <- renderPrint({

    # don't show output or placeholder if no vars selected or if user changes var
    #  (until button pressed)
    if (bag$do_descript_1_cat != FALSE) {
      freqs_percents <- as.data.frame(bag$freqs)
      freqs_percents$percents <- bag$percents %>%
        unname() %>%
        round(2) %>%
        paste0("%")

      names(freqs_percents) <- c("Group", "Count", "Percentage")
      print.data.frame(freqs_percents, row.names = FALSE)
    }
  })

  # plot bar chart
  output$descript_1_bar_cat <- renderPlot({

    # don't show output or placeholder if no vars selected or if user changes var
    #  (until button pressed)
    if (bag$do_descript_1_cat != FALSE) {
      suppressWarnings(print(
        ggplot(
          data = drop_na(
            bag$data_new_df, .data[[input$descript_select_1var_var]]
          ),
          aes_string(x = paste0("`", input$descript_select_1var_var, "`"))
        ) +
          geom_bar() +
          labs(y = "Count")
      ))
    }
  })

  # plot pie chart
  output$descript_1_pie_cat <- renderPlotly({

    # don't show output or placeholder if no vars selected or if user changes var
    #  (until button pressed)
    if (bag$do_descript_1_cat != FALSE) {

      # make df of counts for each category
      counts_df <- bag$data_new_df %>%
        drop_na(.data[[input$descript_select_1var_var]]) %>%
        dplyr::group_by(.data[[input$descript_select_1var_var]]) %>%
        dplyr::tally()

      bag$pie_cat <- plot_ly(counts_df,
        labels = ~.data[[input$descript_select_1var_var]],
        values = ~n,
        type = "pie",
        textposition = "outside",
        textinfo = "label+percent"
      ) %>%
      layout(
        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
      )

      hide_legend(bag$pie_cat)
    }
  })



  ### Descriptive - Quant vs. Quant
  ###################################################
  observeEvent(input$calc_descript_2_var_quant_quant, {

    req(
      input$descript_select_2vars_var_resp,
      input$descript_select_2vars_var_expl
    )


    # subset data for selected variables
    bag$y <- bag$data_new_df[, input$descript_select_2vars_var_resp]
    bag$x <- bag$data_new_df[, input$descript_select_2vars_var_expl]


    # warning popup alert if at least one selected var isn't numeric
    if (!is.numeric(bag$y) | !is.numeric(bag$x)) {
      shinyalert("Warning!", "At least one of the variables you selected is not
        quantitative. Please select the correct variable types.",
        type = "error"
      )
    }

    # keeps the app from crashing if at least one selected var isn't numeric
    validate(need(is.numeric(bag$y) & is.numeric(bag$x), ""))


    # make df w/ univariate summaries -- both variables
    bag$summaries_uni <- data.frame(
      Response.Var = c(
        round(mean(bag$y, na.rm = TRUE), 4),
        round(median(bag$y, na.rm = TRUE), 4),
        round(sd(bag$y, na.rm = TRUE), 4),
        round(IQR(bag$y, na.rm = TRUE), 4),
        round(min(bag$y, na.rm = TRUE), 4),
        round(max(bag$y, na.rm = TRUE), 4),
        formatC(length(na.omit(bag$y))),
        sum(is.na(bag$y))
      ),
      Explanatory.Var = c(
        round(mean(bag$x, na.rm = TRUE), 4),
        round(median(bag$x, na.rm = TRUE), 4),
        round(sd(bag$x, na.rm = TRUE), 4),
        round(IQR(bag$x, na.rm = TRUE), 4),
        round(min(bag$x, na.rm = TRUE), 4),
        round(max(bag$x, na.rm = TRUE), 4),
        formatC(length(na.omit(bag$x))),
        sum(is.na(bag$x))
      ),
      row.names = c("Mean", "Median", "Std. Dev.", "IQR", "Min", "Max",
        "# Non-missing Obs.  ", "# Missing Obs."
      )
    )


    # fit regression line and calculate summaries
    bag$reg_summary <- suppressWarnings(summary(lm(bag$y ~ bag$x)))


    # make df w/ bivariate summaries
    bag$summaries_bi <- data.frame(
      "." = c(
        round(cor(x = bag$x, y = bag$y, use = "pairwise.complete.obs"), 4),
        round(bag$reg_summary$r.squared, 4),
        round(bag$reg_summary$sigma, 4),
        round(bag$reg_summary$coefficients[1, 1], 4),
        round(bag$reg_summary$coefficients[2, 1], 4),
        formatC(length(bag$reg_summary$residuals)),
        length(bag$y) - length(bag$reg_summary$residuals)
      ),
      row.names = c("r", "r-squared", "Resid. Std. Error  ", "y-intercept",
        "slope", "# Non-missing Obs.  ", "# Missing Obs."
      )
    )


    # need this to reset output if new variable selected
    bag$do_descript_2_quant_quant <- input$calc_descript_2_var_quant_quant
  })


  # reset output if new variable selected
  observeEvent(input$descript_select_2vars_var_resp, {
    bag$do_descript_2_quant_quant <- FALSE
  })

  observeEvent(input$descript_select_2vars_var_expl, {
    bag$do_descript_2_quant_quant <- FALSE
  })


  # univariate summaries output
  output$descript_2_stats_quant_quant_uni <- renderPrint({

    # don't show output or placeholder if no vars selected or if user changes var
    #  (until button pressed)
    if (bag$do_descript_2_quant_quant != FALSE) bag$summaries_uni
  })

  # correlation and regression output
  output$descript_2_stats_quant_quant_bi <- renderPrint({

    # don't show output or placeholder if no vars selected or if user changes var
    #  (until button pressed)
    if (bag$do_descript_2_quant_quant != FALSE) {
      cat(capture.output(bag$summaries_bi)[-1], sep = "\n")
    }
  })

  # scatterplot
  output$descript_2_scatter <- renderPlot({

    # don't show output or placeholder if no vars selected or if user changes var
    #  (until button pressed)
    if (bag$do_descript_2_quant_quant != FALSE) {
      suppressWarnings(print(
        ggplot(bag$data_new_df,
          aes_string(
            x = paste0("`", input$descript_select_2vars_var_expl, "`"),
            y = paste0("`", input$descript_select_2vars_var_resp, "`")
          )
        ) +
          geom_abline(
            intercept = bag$reg_summary$coefficients[1, 1],
            slope = bag$reg_summary$coefficients[2, 1],
            color = "blue",
            alpha = 0.75,
            size = 1.5
          ) +
          geom_point(size = 2)
      ))
    }
  })



  ### Descriptive - Quant vs. Categorical
  ###################################################
  observeEvent(input$calc_descript_2_var_quant_cat, {

    req(
      input$descript_select_2vars_var_resp,
      input$descript_select_2vars_var_expl
    )


    # subset selected variables
    bag$selected_vars <- data.frame(
      x = bag$data_new_df[, input$descript_select_2vars_var_expl],
      y = bag$data_new_df[, input$descript_select_2vars_var_resp]
    )


    # warning popup alert if response var isn't numeric
    if (!is.numeric(bag$selected_vars$y)) {
      shinyalert("Warning!", "The response variable is not quantitative. Please
        select the correct variable type.",
        type = "error"
      )
    }

    # keeps the app from crashing if response var isn't numeric
    validate(need(is.numeric(bag$selected_vars$y), ""))


    # count number of NAs for grouping (x) variable
    bag$x_n_NA <- sum(is.na(bag$selected_vars$x))


    # remove rows with missing data for grouping (x) variable, otherwise
    # considered separate group
    bag$selected_vars <- bag$selected_vars %>% dplyr::filter(x != "")


    # calculate response var summaries by explanatory var group
    bag$summaries_bygroup <- bag$selected_vars %>%
      dplyr::group_by(x) %>%
      dplyr::summarize(
        Mean = mean(y, na.rm = TRUE),
        Median = median(y, na.rm = TRUE),
        SD = sd(y, na.rm = TRUE),
        IQR = IQR(y, na.rm = TRUE),
        Min = min(y, na.rm = TRUE),
        Max = max(y, na.rm = TRUE),
        " # Non-missing Obs." = sum(!is.na(y)),
        " # Missing Obs." = sum(is.na(y))
      ) %>%
      as.data.frame() %>%
      dplyr::rename(Group = x)


    # round summaries
    bag$summaries_bygroup[, 2:ncol(bag$summaries_bygroup)] <- round(
      bag$summaries_bygroup[, 2:ncol(bag$summaries_bygroup)], 4
    )


    if (!is.factor(bag$selected_vars$x)) {
      bag$selected_vars$x <- factor(bag$selected_vars$x)
    }


    # need this to reset output if new variable selected
    bag$do_descript_2_quant_cat <- input$calc_descript_2_var_quant_cat
  })


  # reset output if new variable selected
  observeEvent(input$descript_select_2vars_var_resp, {
    bag$do_descript_2_quant_cat <- FALSE
  })

  observeEvent(input$descript_select_2vars_var_expl, {
    bag$do_descript_2_quant_cat <- FALSE
  })


  # response var summaries output by explanatory var group
  output$descript_2_stats_quant_cat_bygroup <- renderPrint({

    # don't show output or placeholder if no vars selected or if user changes var
    #  (until button pressed)
    if (bag$do_descript_2_quant_cat != FALSE) {
      print.data.frame(bag$summaries_bygroup, row.names = FALSE)
    }
  })


  # number of NAs in grouping (x) variable
  output$descript_2_stats_quant_cat_NAs_x <- renderPrint({

    # don't show output or placeholder if no vars selected or if user changes var
    #  (until button pressed)
    if (bag$do_descript_2_quant_cat != FALSE) {
      cat(stri_dup(intToUtf8(160), 4), bag$x_n_NA)
    }
  })


  # make side-by-side boxplots
  output$descript_2_boxplots <- renderPlot({

    # don't show output or placeholder if no vars selected or if user changes var
    #  (until button pressed)
    if (bag$do_descript_2_quant_cat != FALSE) {
      suppressWarnings(print(
        ggplot(data = na.omit(bag$selected_vars), aes(x = x, y = y)) +
          geom_boxplot() +
          labs(x = "Explanatory Variable", y = "Response Variable")
      ))
    }
  })



  ### Descriptive - Categorical vs. Categorical
  ###################################################
  observeEvent(input$calc_descript_2_var_cat_cat, {

    req(
      input$descript_select_2vars_var_resp,
      input$descript_select_2vars_var_expl
    )


    # subset selected variables
    selected_vars_all <- bag$data_new_df[,
      c(input$descript_select_2vars_var_expl,
        input$descript_select_2vars_var_resp
      )
    ]


    # remove observations w/ NAs
    selected_vars <- na.omit(selected_vars_all)


    # make contingency table of counts
    table_counts <- table(selected_vars)


    # add row and column totals
    table_counts_totals <- addmargins(table_counts)


    # update "total" names
    row.names(table_counts_totals)[nrow(table_counts_totals)] <- "Total"
    colnames(table_counts_totals)[ncol(table_counts_totals)] <- "Total"

    bag$table_counts_totals <- table_counts_totals


    # calculate joint and marginal percentages
    table_joint_marg <- round(prop.table(table_counts)*100, 2) # fxn of table w/o totals
    table_joint_marg <- addmargins(table_joint_marg)
    row.names(table_joint_marg)[nrow(table_joint_marg)] <- "Total"
    colnames(table_joint_marg)[ncol(table_joint_marg)] <- "Total"

    table_joint_marg[1:nrow(table_joint_marg), 1:ncol(table_joint_marg)] <- paste0(
      table_joint_marg[1:nrow(table_joint_marg), 1:ncol(table_joint_marg)], "%"
    )

    bag$table_joint_marg <- table_joint_marg


    # calculate conditional percentages -- by row
    table_cond_byrow <- round(prop.table(table_counts, 1)*100, 2)
    table_cond_byrow[1:nrow(table_cond_byrow), 1:ncol(table_cond_byrow)] <- paste0(
      table_cond_byrow[1:nrow(table_cond_byrow), 1:ncol(table_cond_byrow)], "%"
    )

    bag$table_cond_byrow <- table_cond_byrow


    # calculate conditional percentages -- by column
    table_cond_bycol <- round(prop.table(table_counts, 2)*100, 2)
    table_cond_bycol[1:nrow(table_cond_bycol), 1:ncol(table_cond_bycol)] <- paste0(
      table_cond_bycol[1:nrow(table_cond_bycol), 1:ncol(table_cond_bycol)], "%"
    )

    bag$table_cond_bycol <- table_cond_bycol


    # make df w/ Freq variable -- used in clustered bar chart
    bag$df_counts <- as.data.frame(table_counts)


    # count number of missing values
    bag$cat_cat_NA <- nrow(selected_vars_all) - nrow(selected_vars)


    # need this to reset output if new variable selected
    bag$do_descript_2_cat_cat <- input$calc_descript_2_var_cat_cat
  })


  # reset output if new variable selected
  observeEvent(input$descript_select_2vars_var_resp, {
    bag$do_descript_2_cat_cat <- FALSE
  })

  observeEvent(input$descript_select_2vars_var_expl, {
    bag$do_descript_2_cat_cat <- FALSE
  })


  output$descript_2_counts <- renderPrint({

    # don't show output or placeholder if no vars selected or if user changes var
    #  (until button pressed)
    if (bag$do_descript_2_cat_cat != FALSE) bag$table_counts_totals
  })


  output$descript_2_joint_marg <- renderPrint({

    # don't show output or placeholder if no vars selected or if user changes var
    #  (until button pressed)
    if (bag$do_descript_2_cat_cat != FALSE) bag$table_joint_marg
  })


  output$descript_2_cond_byrow <- renderPrint({

    # don't show output or placeholder if no vars selected or if user changes var
    #  (until button pressed)
    if (bag$do_descript_2_cat_cat != FALSE) bag$table_cond_byrow
  })


  output$descript_2_cond_bycol <- renderPrint({

    # don't show output or placeholder if no vars selected or if user changes var
    #  (until button pressed)
    if (bag$do_descript_2_cat_cat != FALSE) bag$table_cond_bycol
  })


  output$descript_2_NAs <- renderPrint({

    # don't show output or placeholder if no vars selected or if user changes var
    #  (until button pressed)
    if (bag$do_descript_2_cat_cat != FALSE) {
      cat(stri_dup(intToUtf8(160), 4), bag$cat_cat_NA)
    }
  })


  # make clustered bar chart
  output$descript_2_clustbar <- renderPlot({

    # don't show output or placeholder if no vars selected or if user changes var
    #  (until button pressed)
    if (bag$do_descript_2_cat_cat != FALSE) {
      suppressWarnings(print(
        ggplot(bag$df_counts,
          aes_string(
            x = gsub(" ", ".", input$descript_select_2vars_var_expl),
            fill = gsub(" ", ".", input$descript_select_2vars_var_resp)
          )
        ) +
          geom_bar(aes(y = Freq), stat = "identity", position = "dodge") +
          labs(y = "Count")
      ))
    }
  })



  ### take screenshot of current inferential methods page
  # observeEvent(input$inf_screenshot, {
  #
  #   shinyjs::runjs(
  #     "html2canvas(document.querySelector('body')).then(canvas => {
  #         saveAs(canvas.toDataURL(), 'Output.png');
  #     });"
  #   )
  # })



  ### Inferential - One-Sample t Procedures
  ###################################################
  observeEvent(input$run_t_1, {

    req(input$t_1_select_var)


    # select variable of interest
    bag$t_1_data <- data.frame(data = bag$data_new_df[, input$t_1_select_var])


    # warning popup alert if selected var isn't numeric
    if (!is.numeric(bag$t_1_data$data)) {
      shinyalert("Warning!", "The selected variable is not quantitative. Please
        select a quantitative variable.",
        type = "error"
      )
    }

    # keeps the app from crashing if selected var isn't numeric
    validate(need(is.numeric(bag$t_1_data$data), ""))


    # lt = left-tailed, rt = right-tailed, 2t = two-tailed
    t_1_test_lt <- t.test(
      x = bag$t_1_data,
      mu = input$t_1_null_value,
      alternative = "less"
    )

    t_1_test_rt <- t.test(
      x = bag$t_1_data,
      mu = input$t_1_null_value,
      alternative = "greater"
    )

    t_1_test_2t <- t.test(
      x = bag$t_1_data,
      mu = input$t_1_null_value,
      alternative = "two.sided"
    )

    bag$t_1_ci <- t.test(
      x = bag$t_1_data,
      conf.level = input$t_1_conf_level/100,
      alternative = "two.sided"
    )


    # remove missing values
    t_1_noNA <- na.omit(bag$t_1_data$data)

    bag$t_1_summaries <- data.frame(
      "." = formatC(c(
        mean(t_1_noNA), sd(t_1_noNA), length(t_1_noNA),
        sum(is.na(bag$t_1_data$data))
      )),
      row.names = c("Sample Mean", "Sample SD", "# Non-missing Obs.  ",
        "# Missing Obs."
      )
    )


    # store test output
    bag$t_1_test <- data.frame(
      "." = formatC(c(t_1_test_2t$statistic, t_1_test_2t$parameter,
        t_1_test_2t$p.value, t_1_test_lt$p.value, t_1_test_rt$p.value)
      ),
      row.names = c("Test Statistic", "df", "p-value (2-sided)  ", "p-value (<)",
        "p-value (>)"
      )
    )


    # need this to reset output if new variable selected
    bag$do_t_1 <- input$run_t_1
  })


  # reset output if new variable selected
  observeEvent(input$t_1_select_var, {
    bag$do_t_1 <- FALSE
  })


  # print output for procedures
  output$t_1_summaries <- renderPrint({

    # don't show output or placeholder if no vars selected or if user changes var
    #  (until button pressed)
    if (bag$do_t_1 != FALSE) cat(capture.output(bag$t_1_summaries)[-1], sep = "\n")
  })

  output$t_1_test <- renderPrint({

    # don't show output or placeholder if no vars selected or if user changes var
    #  (until button pressed)
    if (bag$do_t_1 != FALSE) cat(capture.output(bag$t_1_test)[-1], sep = "\n")
  })

  output$t_1_ci <- renderText({

    # don't show output or placeholder if no vars selected or if user changes var
    #  (until button pressed)
    if (bag$do_t_1 != FALSE) {
      paste0("(", round(bag$t_1_ci$conf.int[1], 4), ", ",
        round(bag$t_1_ci$conf.int[2], 4), ")"
      )
    }
  })


  # histogram of values for one-sample t procedures
  output$hist_t_1 <- renderPlot({

    # don't show output or placeholder if no vars selected or if user changes var
    #  (until button pressed)
    if (bag$do_t_1 != FALSE) {
      suppressWarnings(print(
        ggplot(bag$t_1_data, aes(x = data)) +
          geom_histogram(bins = input$hist_t_1_bins) +
          labs(y = "Count")
      ))
    }
  })


  # normal QQ plot of values for one-sample t procedures
  output$norm_qq_t_1 <- renderPlot({

    # don't show output or placeholder if no vars selected or if user changes var
    #  (until button pressed)
    if (bag$do_t_1 != FALSE) {
      suppressWarnings(print(
        ggplot(bag$t_1_data, aes(sample = data)) +
          stat_qq() +
          stat_qq_line() +
          labs(x = "Theoretical Quantile", y = "Sample Quantile")
      ))
    }
  })



  ### Inferential - One-Proportion z Procedures
  ###################################################
  observeEvent(input$set_z_1_var, {

    updateSelectInput(session, "z_1_success", "Success Category",
      choices = unique(na.omit(bag$data_new_df[, input$z_1_select_var]))
    )
  })


  observeEvent(input$run_z_1, {

    req(input$z_1_success)

    z_1_data <- bag$data_new_df[, input$z_1_select_var]

    # number of successes, observations, and missing values
    z_1_x <- sum(z_1_data == input$z_1_success, na.rm = TRUE)
    z_1_n <- length(na.omit(z_1_data))

    # calculate sample proportion and store test proportion
    z_1_p_sample <- z_1_x/z_1_n
    z_1_p_test <- input$z_1_null_value

    # calculate Z test statistic
    z_1_test_stat <- (z_1_p_sample - z_1_p_test) /
      sqrt(z_1_p_test*(1-z_1_p_test)/z_1_n)

    # calculate p-values
    # lt = left-tailed, rt = right-tailed, 2t = two-tailed
    z_1_pval_lt <- pnorm(z_1_test_stat)
    z_1_pval_rt <- 1 - z_1_pval_lt
    z_1_pval_2t <- ifelse(z_1_pval_lt < 0.5, 2*z_1_pval_lt, 2*z_1_pval_rt)

    # calculate wald CI
    bag$z_1_ci <- BinomCI(
      x = z_1_x,
      n = z_1_n,
      conf.level = input$z_1_conf_level/100,
      sides = "two.sided",
      method = "wald"
    )

    bag$z_1_summaries <- data.frame(
      "." = formatC(c(
        z_1_p_sample,
        z_1_n,
        sum(is.na(z_1_data))
      )),
      row.names = c("Sample Proportion  ", "# Non-missing Obs.", "# Missing Obs.")
    )

    bag$z_1_test <- data.frame(
      "." = formatC(c(z_1_test_stat, z_1_pval_2t, z_1_pval_lt, z_1_pval_rt)),
      row.names = c("Test Statistic", "p-value (2-sided)  ", "p-value (<)",
        "p-value (>)"
      )
    )

    bag$z_1_assumps <- data.frame(
      "." = formatC(c(z_1_x, z_1_n - z_1_x, z_1_n*input$z_1_null_value,
        z_1_n - z_1_n*input$z_1_null_value)
      ),
      row.names = c("Successes", "Failures", "Expected Successes  ",
        "Expected Failures"
      )
    )


    # need this to reset output if new variable selected
    bag$do_z_1 <- input$run_z_1
  })


  # reset output and input if new variable selected
  observeEvent(input$z_1_select_var, {
    bag$do_z_1 <- FALSE

    updateSelectInput(session, "z_1_success", "Success Category",
      choices = ""
    )
  })


  # print output for procedures
  output$z_1_summaries <- renderPrint({

    # don't show output or placeholder if no vars selected or if user changes var
    #  (until button pressed)
    if (bag$do_z_1 != FALSE) {
      cat(capture.output(bag$z_1_summaries)[-1], sep = "\n")
    }
  })

  output$z_1_test <- renderPrint({

    # don't show output or placeholder if no vars selected or if user changes var
    #  (until button pressed)
    if (bag$do_z_1 != FALSE) cat(capture.output(bag$z_1_test)[-1], sep = "\n")
  })

  output$z_1_ci <- renderText({

    # don't show output or placeholder if no vars selected or if user changes var
    #  (until button pressed)
    if (bag$do_z_1 != FALSE) {
      paste0("(", round(bag$z_1_ci[2], 4), ", ", round(bag$z_1_ci[3], 4), ")")
    }
  })


  # print output for checking assumptions
  output$z_1_assumptions <- renderPrint({

    # don't show output or placeholder if no vars selected or if user changes var
    #  (until button pressed)
    if (bag$do_z_1 != FALSE) {
      cat(capture.output(bag$z_1_assumps)[-1], sep = "\n")
    }
  })



  ### Inferential - Paired Samples t Procedures
  ###################################################
  observeEvent(input$run_t_paired, {

    req(input$t_paired_select_var_1, input$t_paired_select_var_2)


    # select variables of interest
    t_paired_data <- data.frame(
      var1 = bag$data_new_df[, input$t_paired_select_var_1],
      var2 = bag$data_new_df[, input$t_paired_select_var_2]
    )


    # warning popup alert if at least one selected var isn't numeric
    if (!is.numeric(t_paired_data$var1) | !is.numeric(t_paired_data$var2)) {
      shinyalert("Warning!", "At least one of the selected variables is not
        quantitative. Please select two quantitative variables.",
        type = "error"
      )
    }

    # keeps the app from crashing if at least one selected var isn't numeric
    validate(
      need(
        is.numeric(t_paired_data$var1) & is.numeric(t_paired_data$var2),
        ""
      )
    )


    # remove observations with a missing value
    t_paired_noNA <- na.omit(t_paired_data)

    # make variable w/ differenced data
    bag$t_paired_diff <- data.frame(diff = t_paired_noNA[, 1] - t_paired_noNA[, 2])
    t_paired_diff_vec <- as.vector(bag$t_paired_diff[, 1])


    # calculate summaries
    bag$t_paired_summaries <- data.frame(
      "." = formatC(c(mean(t_paired_diff_vec), sd(t_paired_diff_vec),
        length(t_paired_diff_vec), nrow(t_paired_data) - nrow(t_paired_noNA)
      )),
      row.names = c("Mean", "SD", "# Non-missing Obs.  ", "# Missing Obs.")
    )


    # lt = left-tailed, rt = right-tailed, 2t = two-tailed
    t_paired_test_lt <- t.test(
      x = t_paired_diff_vec,
      mu = input$t_paired_null_value,
      alternative = "less"
    )

    t_paired_test_rt <- t.test(
      x = t_paired_diff_vec,
      mu = input$t_paired_null_value,
      alternative = "greater"
    )

    t_paired_test_2t <- t.test(
      x = t_paired_diff_vec,
      mu = input$t_paired_null_value,
      alternative = "two.sided"
    )

    bag$t_paired_ci <- t.test(
      x = t_paired_diff_vec,
      conf.level = input$t_paired_conf_level/100,
      alternative = "two.sided"
    )


    # store hyp. test and CI results
    bag$t_paired_test <- data.frame(
      "." = formatC(c(t_paired_test_2t$statistic, t_paired_test_2t$parameter,
        t_paired_test_2t$p.value, t_paired_test_lt$p.value,
        t_paired_test_rt$p.value
      ), 4),
      row.names = c("Test Statistic", "df", "p-value (2-sided)  ", "p-value (<)",
        "p-value (>)"
      )
    )


    # store df w/ variable names
    bag$t_paired_names <- data.frame(
      "." = c(input$t_paired_select_var_1, input$t_paired_select_var_2),
      row.names = c("Group 1: ", "Group 2: ")
    )


    # need this to reset output if new variable selected
    bag$do_t_paired <- input$run_t_paired
  })


  # reset output if new variable selected
  observeEvent(input$t_paired_select_var_1, {
    bag$do_t_paired <- FALSE
  })

  observeEvent(input$t_paired_select_var_2, {
    bag$do_t_paired <- FALSE
  })


  # print variable names
  output$t_paired_names <- renderPrint({

    # don't show output or placeholder if no vars selected or if user changes var
    #  (until button pressed)
    if (bag$do_t_paired != FALSE) {
      cat(
        capture.output(
          format(bag$t_paired_names, justify = "left")
        )[-1],
        sep = "\n"
      )
    }
  })

  # print output for procedures
  output$t_paired_summaries <- renderPrint({

    # don't show output or placeholder if no vars selected or if user changes var
    #  (until button pressed)
    if (bag$do_t_paired != FALSE) {
      cat(capture.output(bag$t_paired_summaries)[-1], sep = "\n")
    }
  })

  output$t_paired_test <- renderPrint({

    # don't show output or placeholder if no vars selected or if user changes var
    #  (until button pressed)
    if (bag$do_t_paired != FALSE) {
      cat(capture.output(bag$t_paired_test)[-1], sep = "\n")
    }
  })

  output$t_paired_ci <- renderText({

    # don't show output or placeholder if no vars selected or if user changes var
    #  (until button pressed)
    if (bag$do_t_paired != FALSE) {
      paste0("(", round(bag$t_paired_ci$conf.int[1], 4), ", ",
        round(bag$t_paired_ci$conf.int[2], 4), ")"
      )
    }
  })


  # histogram of values for paired samples procedures
  output$hist_t_paired <- renderPlot({

    # don't show output or placeholder if no vars selected or if user changes var
    #  (until button pressed)
    if (bag$do_t_paired != FALSE) {
      suppressWarnings(print(
        ggplot(bag$t_paired_diff, aes(x = diff)) +
          geom_histogram(bins = input$hist_t_paired_bins) +
          labs(y = "Count")
      ))
    }
  })


  # normal QQ plot of values for paired samples procedures
  output$norm_qq_t_paired <- renderPlot({

    # don't show output or placeholder if no vars selected or if user changes var
    #  (until button pressed)
    if (bag$do_t_paired != FALSE) {
      suppressWarnings(print(
        ggplot(bag$t_paired_diff, aes(sample = diff)) +
          stat_qq() +
          stat_qq_line() +
          labs(x = "Theoretical Quantile", y = "Sample Quantile")
      ))
    }
  })



  ### Inferential - Independent Samples t Procedures -
  ###  assuming measurement-measurement format
  ####################################################
  observeEvent(input$run_t_indep_mm, {

    req(input$t_indep_select_var_1mm, input$t_indep_select_var_2mm)


    # format data - wide format for histograms and QQ plots
    bag$t_indep_wide <- data.frame(
      var1 = bag$data_new_df[, input$t_indep_select_var_1mm],
      var2 = bag$data_new_df[, input$t_indep_select_var_2mm]
    )


    # warning popup alert if at least one selected var isn't numeric
    if ((is.numeric(bag$t_indep_wide$var1) == FALSE) |
        (is.numeric(bag$t_indep_wide$var2) == FALSE)
    ) {
      shinyalert("Warning!", "At least one of the selected variables is not
        quantitative. Please select two quantitative variables.",
        type = "error"
      )
    }

    # keeps the app from crashing if at least one selected var isn't numeric
    validate(
      need(
        is.numeric(bag$t_indep_wide$var1) & is.numeric(bag$t_indep_wide$var2),
        ""
      )
    )


    # format data - tidy format
    t_indep_values <- c(
      bag$data_new_df[, input$t_indep_select_var_1mm],
      bag$data_new_df[, input$t_indep_select_var_2mm]
    )

    t_indep_group <- c(
      rep(1, length(bag$data_new_df[, input$t_indep_select_var_1mm])),
      rep(2, length(bag$data_new_df[, input$t_indep_select_var_2mm]))
    )

    t_indep_data <- data.frame(
      response = t_indep_values,
      group = factor(t_indep_group)
    )


    # calculate summaries by group
    bag$t_indep_summaries <- t_indep_data %>%
      dplyr::group_by(group) %>%
      dplyr::summarize(
        Mean = mean(response, na.rm = TRUE),
        "Std. Dev." = sd(response, na.rm = TRUE),
        " # Non-missing Obs." = sum(!is.na(response)),
        " # Missing Obs." = sum(is.na(response))
      ) %>%
      dplyr::rename(Group = group) %>%
      as.data.frame()

    bag$t_indep_summaries[, 2:ncol(bag$t_indep_summaries)] <- round(
      bag$t_indep_summaries[, 2:ncol(bag$t_indep_summaries)], 4
    )


    ### run independent samples t procedures -- assuming equal variances
    # lt = left-tailed, rt = right-tailed, 2t = two-tailed
    t_indep_test_equal_lt <- t.test(response ~ group, data = t_indep_data,
      mu = input$t_indep_mm_null_value,
      alternative = "less",
      var.equal = TRUE
    )

    t_indep_test_equal_rt <- t.test(response ~ group, data = t_indep_data,
      mu = input$t_indep_mm_null_value,
      alternative = "greater",
      var.equal = TRUE
    )

    t_indep_test_equal_2t <- t.test(response ~ group, data = t_indep_data,
      mu = input$t_indep_mm_null_value,
      alternative = "two.sided",
      var.equal = TRUE
    )

    bag$t_indep_ci_equal <- t.test(response ~ group, data = t_indep_data,
      conf.level = input$t_indep_mm_conf_level/100,
      alternative = "two.sided",
      var.equal = TRUE
    )


    # store hyp. test and CI results -- assuming equal variances
    bag$t_indep_test_equal <- data.frame(
      "." = formatC(c(t_indep_test_equal_2t$statistic,
        t_indep_test_equal_2t$parameter, t_indep_test_equal_2t$p.value,
        t_indep_test_equal_lt$p.value, t_indep_test_equal_rt$p.value)
      ),
      row.names = c("Test Statistic", "df", "p-value (2-sided)  ", "p-value (<)",
        "p-value (>)"
      )
    )


    ### run independent samples t procedures -- assuming unequal variances
    t_indep_test_unequal_lt <- t.test(response ~ group, data = t_indep_data,
      mu = input$t_indep_mm_null_value,
      alternative = "less",
      var.equal = FALSE
    )

    t_indep_test_unequal_rt <- t.test(response ~ group, data = t_indep_data,
      mu = input$t_indep_mm_null_value,
      alternative = "greater",
      var.equal = FALSE
    )

    t_indep_test_unequal_2t <- t.test(response ~ group, data = t_indep_data,
      mu = input$t_indep_mm_null_value,
      alternative = "two.sided",
      var.equal = FALSE
    )

    bag$t_indep_ci_unequal <- t.test(response ~ group, data = t_indep_data,
      conf.level = input$t_indep_mm_conf_level/100,
      alternative = "two.sided",
      var.equal = FALSE
    )


    # store hyp. test and CI results -- assuming unequal variances
    bag$t_indep_test_unequal <- data.frame(
      "." = formatC(c(t_indep_test_unequal_2t$statistic,
        t_indep_test_unequal_2t$parameter, t_indep_test_unequal_2t$p.value,
        t_indep_test_unequal_lt$p.value, t_indep_test_unequal_rt$p.value)
      ),
      row.names = c("Test Statistic", "df", "p-value (2-sided)  ", "p-value (<)",
        "p-value (>)"
      )
    )


    # run Levene's test
    bag$t_indep_levene <- LeveneTest(response ~ group, data = t_indep_data)


    # store df w/ group names
    bag$t_indep_mm_names <- data.frame(
      "." = c(input$t_indep_select_var_1mm, input$t_indep_select_var_2mm),
      row.names = c("Group 1: ", "Group 2: ")
    )


    # need this to reset output if new variable selected
    bag$do_t_indep_mm <- input$run_t_indep_mm
  })


  # reset output if new variable selected
  observeEvent(input$t_indep_select_var_1mm, {
    bag$do_t_indep_mm <- FALSE
  })

  observeEvent(input$t_indep_select_var_2mm, {
    bag$do_t_indep_mm <- FALSE
  })


  # print group names
  output$t_indep_mm_names <- renderPrint({

    # don't show output or placeholder if no vars selected or if user changes var
    #  (until button pressed)
    if (bag$do_t_indep_mm != FALSE) {
      cat(
        capture.output(
          format(bag$t_indep_mm_names, justify = "left")
        )[-1],
        sep = "\n"
      )
    }
  })

  # print summaries
  output$t_indep_mm_summaries <- renderPrint({

    # don't show output or placeholder if no vars selected or if user changes var
    #  (until button pressed)
    if (bag$do_t_indep_mm != FALSE) {
      print.data.frame(bag$t_indep_summaries, row.names = FALSE)
    }
  })


  # print output of Levene's test
  output$t_indep_mm_out_levene <- renderPrint({

    # don't show output or placeholder if no vars selected or if user changes var
    #  (until button pressed)
    if (bag$do_t_indep_mm != FALSE) {
      # format output
      levene_df <- as.data.frame(bag$t_indep_levene[1, 1:3])
      levene_df[1, 2] <- round(levene_df[1, 2], 3)
      levene_df[1, 3] <- signif(levene_df[1, 3], digits = 4)
      names(levene_df) <- c("df", " F Stat", " p-value")

      print.data.frame(levene_df, row.names = FALSE)
    }
  })


  # print output for t test -- assumes equal variances
  output$t_indep_mm_test_out_equal <- renderPrint({

    # don't show output or placeholder if no vars selected or if user changes var
    #  (until button pressed)
    if (bag$do_t_indep_mm != FALSE) {
      cat(capture.output(bag$t_indep_test_equal)[-1], sep = "\n")
    }
  })


  # print output for ci -- assumes equal variances
  output$t_indep_mm_ci_out_equal <- renderText({

    # don't show output or placeholder if no vars selected or if user changes var
    #  (until button pressed)
    if (bag$do_t_indep_mm != FALSE) {
      paste0("CI: ", "(", round(bag$t_indep_ci_equal$conf.int[1], 4), ", ",
        round(bag$t_indep_ci_equal$conf.int[2], 4), ")"
      )
    }
  })


  # print output for t test -- assumes unequal variances
  output$t_indep_mm_test_out_unequal <- renderPrint({

    # don't show output or placeholder if no vars selected or if user changes var
    #  (until button pressed)
    if (bag$do_t_indep_mm != FALSE) {
      cat(capture.output(bag$t_indep_test_unequal)[-1], sep = "\n")
    }
  })

  # print output for ci -- assumes unequal variances
  output$t_indep_mm_ci_out_unequal <- renderText({

    # don't show output or placeholder if no vars selected or if user changes var
    #  (until button pressed)
    if (bag$do_t_indep_mm != FALSE) {
      paste0("CI: ", "(", round(bag$t_indep_ci_unequal$conf.int[1], 4), ", ",
        round(bag$t_indep_ci_unequal$conf.int[2], 4), ")"
      )
    }
  })


  # histograms of values for independent samples t procedures
  output$hist_t_indep_mm_1 <- renderPlot({

    # don't show output or placeholder if no vars selected or if user changes var
    #  (until button pressed)
    if (bag$do_t_indep_mm != FALSE) {
      suppressWarnings(print(
        ggplot(bag$t_indep_wide, aes(x = var1)) +
          geom_histogram(bins = input$hist_t_indep_mm_1_bins) +
          labs(y = "Count")
      ))
    }
  })

  output$hist_t_indep_mm_2 <- renderPlot({

    # don't show output or placeholder if no vars selected or if user changes var
    #  (until button pressed)
    if (bag$do_t_indep_mm != FALSE) {
      suppressWarnings(print(
        ggplot(bag$t_indep_wide, aes(x = var2)) +
          geom_histogram(bins = input$hist_t_indep_mm_2_bins) +
          labs(y = "Count")
      ))
    }
  })


  # normal QQ plots of values for independent samples t procedures
  output$norm_qq_t_indep_mm_1 <- renderPlot({

    # don't show output or placeholder if no vars selected or if user changes var
    #  (until button pressed)
    if (bag$do_t_indep_mm != FALSE) {
      suppressWarnings(print(
        ggplot(bag$t_indep_wide, aes(sample = var1)) +
          stat_qq() +
          stat_qq_line() +
          labs(x = "Theoretical Quantile", y = "Sample Quantile")
      ))
    }
  })

  output$norm_qq_t_indep_mm_2 <- renderPlot({

    # don't show output or placeholder if no vars selected or if user changes var
    #  (until button pressed)
    if (bag$do_t_indep_mm != FALSE) {
      suppressWarnings(print(
        ggplot(bag$t_indep_wide, aes(sample = var2)) +
          stat_qq() +
          stat_qq_line() +
          labs(x = "Theoretical Quantile", y = "Sample Quantile")
      ))
    }
  })



  ### Inferential - Independent Samples t Procedures -
  ###  assuming measurement-group format
  ####################################################

  # update variable inputs
  observeEvent(input$set_t_indep_mg, {

    updateSelectInput(session, "t_indep_mg_grp_1", "Group 1 Identifier",
      choices = unique(na.omit(bag$data_new_df[, input$t_indep_select_var_2mg]))
    )

    updateSelectInput(session, "t_indep_mg_grp_2", "Group 2 Identifier",
      choices = unique(na.omit(bag$data_new_df[, input$t_indep_select_var_2mg]))
    )
  })


  # run independent samples t procedures
  observeEvent(input$run_t_indep_mg, {

    req(input$t_indep_mg_grp_1, input$t_indep_mg_grp_2)


    # need df format for var1 and var2 for plots
    bag$var1 <- bag$data_new_df %>%
      dplyr::filter(.data[[input$t_indep_select_var_2mg]] == input$t_indep_mg_grp_1) %>%
      dplyr::select(input$t_indep_select_var_1mg)

    bag$var2 <- bag$data_new_df %>%
      dplyr::filter(.data[[input$t_indep_select_var_2mg]] == input$t_indep_mg_grp_2) %>%
      dplyr::select(input$t_indep_select_var_1mg)


    # format data - tidy format
    t_indep_values <- c(as.vector(bag$var1[, 1]), as.vector(bag$var2)[, 1])
    t_indep_group <- c(rep(1, nrow(bag$var1)), rep(2, nrow(bag$var2)))

    t_indep_data <- data.frame(
      response = t_indep_values,
      group = factor(t_indep_group)
    )


    # warning popup alert if the response var isn't numeric
    if (!is.numeric(t_indep_data$response)) {
      shinyalert("Warning!", "The selected measurement variable is not
        quantitative. Please select a quantitative variable.",
        type = "error"
      )
    }

    # keeps the app from crashing if the response var isn't numeric
    validate(need(is.numeric(t_indep_data$response), ""))


    # calculate summaries by group
    bag$t_indep_summaries <- t_indep_data %>%
      dplyr::group_by(group) %>%
      dplyr::summarize(
        Mean = mean(response, na.rm = TRUE),
        "Std. Dev." = sd(response, na.rm = TRUE),
        " # Non-missing Obs." = sum(!is.na(response)),
        " # Missing Obs." = sum(is.na(response))
      ) %>%
      dplyr::rename(Group = group) %>%
      as.data.frame()

    bag$t_indep_summaries[, 2:ncol(bag$t_indep_summaries)] <- round(
      bag$t_indep_summaries[, 2:ncol(bag$t_indep_summaries)], 4
    )


    ### run independent samples t procedures -- assuming equal variances
    # lt = left-tailed, rt = right-tailed, 2t = two-tailed
    t_indep_test_equal_lt <- t.test(response ~ group, data = t_indep_data,
      mu = input$t_indep_mg_null_value,
      alternative = "less",
      var.equal = TRUE
    )

    t_indep_test_equal_rt <- t.test(response ~ group, data = t_indep_data,
      mu = input$t_indep_mg_null_value,
      alternative = "greater",
      var.equal = TRUE
    )

    t_indep_test_equal_2t <- t.test(response ~ group, data = t_indep_data,
      mu = input$t_indep_mg_null_value,
      alternative = "two.sided",
      var.equal = TRUE
    )

    bag$t_indep_ci_equal <- t.test(response ~ group, data = t_indep_data,
      conf.level = input$t_indep_mg_conf_level/100,
      alternative = "two.sided",
      var.equal = TRUE
    )


    # store hyp. test and CI results -- assuming equal variances
    bag$t_indep_test_equal <- data.frame(
      "." = formatC(c(t_indep_test_equal_2t$statistic,
        t_indep_test_equal_2t$parameter, t_indep_test_equal_2t$p.value,
        t_indep_test_equal_lt$p.value, t_indep_test_equal_rt$p.value)
      ),
      row.names = c("Test Statistic", "df", "p-value (2-sided)  ", "p-value (<)",
        "p-value (>)"
      )
    )


    ### run independent samples t procedures -- assuming unequal variances
    t_indep_test_unequal_lt <- t.test(response ~ group, data = t_indep_data,
      mu = input$t_indep_mg_null_value,
      alternative = "less",
      var.equal = FALSE
    )

    t_indep_test_unequal_rt <- t.test(response ~ group, data = t_indep_data,
      mu = input$t_indep_mg_null_value,
      alternative = "greater",
      var.equal = FALSE
    )

    t_indep_test_unequal_2t <- t.test(response ~ group, data = t_indep_data,
      mu = input$t_indep_mg_null_value,
      alternative = "two.sided",
      var.equal = FALSE
    )

    bag$t_indep_ci_unequal <- t.test(response ~ group, data = t_indep_data,
      conf.level = input$t_indep_mg_conf_level/100,
      alternative = "two.sided",
      var.equal = FALSE
    )


    # store hyp. test and CI results -- assuming unequal variances
    bag$t_indep_test_unequal <- data.frame(
      "." = formatC(c(t_indep_test_unequal_2t$statistic,
        t_indep_test_unequal_2t$parameter, t_indep_test_unequal_2t$p.value,
        t_indep_test_unequal_lt$p.value, t_indep_test_unequal_rt$p.value)
      ),
      row.names = c("Test Statistic", "df", "p-value (2-sided)  ", "p-value (<)",
        "p-value (>)"
      )
    )


    # run Levene's test
    bag$t_indep_levene <- LeveneTest(response ~ group, data = t_indep_data)


    # store df w/ group names
    bag$t_indep_mg_names <- data.frame(
      "." = c(input$t_indep_mg_grp_1, input$t_indep_mg_grp_2),
      row.names = c("Group 1: ", "Group 2: ")
    )


    # need this to reset output if new variable selected
    bag$do_t_indep_mg <- input$run_t_indep_mg
  })


  # reset output if new variable selected
  observeEvent(input$t_indep_select_var_1mg, {
    bag$do_t_indep_mg <- FALSE
  })

  observeEvent(input$t_indep_select_var_2mg, {
    bag$do_t_indep_mg <- FALSE
  })


  # print group names
  output$t_indep_mg_names <- renderPrint({

    # don't show output or placeholder if no vars selected or if user changes var
    #  (until button pressed)
    if (bag$do_t_indep_mg != FALSE) {
      cat(
        capture.output(
          format(bag$t_indep_mg_names, justify = "left")
        )[-1],
        sep = "\n"
      )
    }
  })

  # print summaries
  output$t_indep_mg_summaries <- renderPrint({

    # don't show output or placeholder if no vars selected or if user changes var
    #  (until button pressed)
    if (bag$do_t_indep_mg != FALSE) {
      print.data.frame(bag$t_indep_summaries, row.names = FALSE)
    }
  })


  # print output of Levene's test
  output$t_indep_mg_out_levene <- renderPrint({

    # don't show output or placeholder if no vars selected or if user changes var
    #  (until button pressed)
    if (bag$do_t_indep_mg != FALSE) {
      # format output
      levene_df <- as.data.frame(bag$t_indep_levene[1, 1:3])
      levene_df[1, 2] <- round(levene_df[1, 2], 3)
      levene_df[1, 3] <- signif(levene_df[1, 3], digits = 4)
      names(levene_df) <- c("df", " F Stat", " p-value")

      print.data.frame(levene_df, row.names = FALSE)
    }
  })


  # print output for t test -- assumes equal variances
  output$t_indep_mg_test_out_equal <- renderPrint({

    # don't show output or placeholder if no vars selected or if user changes var
    #  (until button pressed)
    if (bag$do_t_indep_mg != FALSE) {
      cat(capture.output(bag$t_indep_test_equal)[-1], sep = "\n")
    }
  })


  # print output for ci -- assumes equal variances
  output$t_indep_mg_ci_out_equal <- renderText({

    # don't show output or placeholder if no vars selected or if user changes var
    #  (until button pressed)
    if (bag$do_t_indep_mg != FALSE) {
      paste0("CI: ", "(", round(bag$t_indep_ci_equal$conf.int[1], 4), ", ",
        round(bag$t_indep_ci_equal$conf.int[2], 4), ")"
      )
    }
  })


  # print output for t test -- assumes unequal variances
  output$t_indep_mg_test_out_unequal <- renderPrint({

    # don't show output or placeholder if no vars selected or if user changes var
    #  (until button pressed)
    if (bag$do_t_indep_mg != FALSE) {
      cat(capture.output(bag$t_indep_test_unequal)[-1], sep = "\n")
    }
  })

  # print output for ci -- assumes unequal variances
  output$t_indep_mg_ci_out_unequal <- renderText({

    # don't show output or placeholder if no vars selected or if user changes var
    #  (until button pressed)
    if (bag$do_t_indep_mg != FALSE) {
      paste0("CI: ", "(", round(bag$t_indep_ci_unequal$conf.int[1], 4), ", ",
        round(bag$t_indep_ci_unequal$conf.int[2], 4), ")"
      )
    }
  })


  # histograms of values for independent samples t procedures
  output$hist_t_indep_mg_1 <- renderPlot({

    # don't show output or placeholder if no vars selected or if user changes var
    #  (until button pressed)
    if (bag$do_t_indep_mg != FALSE) {
      suppressWarnings(print(
        ggplot(bag$var1, aes_string(x = input$t_indep_select_var_1mg)) +
          geom_histogram(bins = input$hist_t_indep_mg_1_bins) +
          labs(y = "Count")
      ))
    }
  })

  output$hist_t_indep_mg_2 <- renderPlot({

    # don't show output or placeholder if no vars selected or if user changes var
    #  (until button pressed)
    if (bag$do_t_indep_mg != FALSE) {
      suppressWarnings(print(
        ggplot(bag$var2, aes_string(x = input$t_indep_select_var_1mg)) +
          geom_histogram(bins = input$hist_t_indep_mg_2_bins) +
          labs(y = "Count")
      ))
    }
  })


  # normal QQ plots of values for independent samples t procedures
  output$norm_qq_t_indep_mg_1 <- renderPlot({

    # don't show output or placeholder if no vars selected or if user changes var
    #  (until button pressed)
    if (bag$do_t_indep_mg != FALSE) {
      suppressWarnings(print(
        ggplot(bag$var1, aes_string(sample = input$t_indep_select_var_1mg)) +
          stat_qq() +
          stat_qq_line() +
          labs(x = "Theoretical Quantile", y = "Sample Quantile")
      ))
    }
  })

  output$norm_qq_t_indep_mg_2 <- renderPlot({

    # don't show output or placeholder if no vars selected or if user changes var
    #  (until button pressed)
    if (bag$do_t_indep_mg != FALSE) {
      suppressWarnings(print(
        ggplot(bag$var2, aes_string(sample = input$t_indep_select_var_1mg)) +
          stat_qq() +
          stat_qq_line() +
          labs(x = "Theoretical Quantile", y = "Sample Quantile")
      ))
    }
  })



  ### Inferential - Two-Proportion z Procedures -
  ###  assuming measurement-measurement format
  ####################################################

  # set success category input
  observeEvent(input$set_z_2_mm_vars, {

    # assuming the category labels are consistent across the 2 groups
    updateSelectInput(session, "z_2_mm_success", "Success Category",
      choices = unique(na.omit(bag$data_new_df[, input$z_2_select_var_1mm]))
    )
  })


  # run two-proportion z procedures
  observeEvent(input$run_z_2_mm, {

    req(input$z_2_mm_success)

    z_2_data <- data.frame(
      data_grp1 = bag$data_new_df[, input$z_2_select_var_1mm],
      data_grp2 = bag$data_new_df[, input$z_2_select_var_2mm]
    )


    # number of successes and sample size per sample
    z_2_x1 <- sum(z_2_data$data_grp1 == input$z_2_mm_success, na.rm = TRUE)
    z_2_x2 <- sum(z_2_data$data_grp2 == input$z_2_mm_success, na.rm = TRUE)
    z_2_n1 <- length(na.omit(z_2_data$data_grp1))
    z_2_n2 <- length(na.omit(z_2_data$data_grp2))
    z_2_p_sample_1 <- z_2_x1/z_2_n1
    z_2_p_sample_2 <- z_2_x2/z_2_n2
    z_2_p_combo <- (z_2_x1 + z_2_x2)/(z_2_n1 + z_2_n2)

    z_2_p_test <- input$z_2_mm_null_value


    # calculate Z test statistic
    z_2_test_stat <- ((z_2_p_sample_1 - z_2_p_sample_2) - z_2_p_test) /
      sqrt(z_2_p_combo*(1-z_2_p_combo)*(1/z_2_n1 + 1/z_2_n2))

    # calculate p-values
    # lt = left-tailed, rt = right-tailed, 2t = two-tailed
    z_2_pval_lt <- pnorm(z_2_test_stat)
    z_2_pval_rt <- 1 - z_2_pval_lt
    z_2_pval_2t <- ifelse(z_2_pval_lt < 0.5, 2*z_2_pval_lt, 2*z_2_pval_rt)


    # calculate wald CI
    bag$z_2_mm_ci <- BinomDiffCI(
      x1 = z_2_x1, n1 = z_2_n1,
      x2 = z_2_x2, n2 = z_2_n2,
      conf.level = input$z_2_mm_conf_level/100,
      method = "wald"
    )


    bag$z_2_mm_summaries <- data.frame(
      samp1 = formatC(c(z_2_p_sample_1, z_2_x1, z_2_n1 - z_2_x1, z_2_n1,
        sum(is.na(z_2_data$data_grp1))
      )),
      samp2 = formatC(c(z_2_p_sample_2, z_2_x2, z_2_n2 - z_2_x2, z_2_n2,
        sum(is.na(z_2_data$data_grp2))
      )),
      row.names = c("Sample Proportion", "Successes", "Failures",
        "# Non-missing Obs.", "# Missing Obs."
      )
    )

    bag$z_2_mm_test <- data.frame(
      "." = formatC(c(z_2_test_stat, z_2_pval_2t, z_2_pval_lt, z_2_pval_rt)),
      row.names = c("Test Statistic", "p-value (2-sided)  ", "p-value (<)",
        "p-value (>)"
      )
    )


    # store df w/ group names
    bag$z_2_mm_names <- data.frame(
      "." = c(input$z_2_select_var_1mm, input$z_2_select_var_2mm),
      row.names = c("Group 1: ", "Group 2: ")
    )


    # need this to reset output if new variable selected
    bag$do_z_2_mm <- input$run_z_2_mm
  })


  # reset output and input if new variable selected
  observeEvent(input$z_2_select_var_1mm, {
    bag$do_z_2_mm <- FALSE

    updateSelectInput(session, "z_2_mm_success", "Success Category",
      choices = ""
    )
  })

  observeEvent(input$z_2_select_var_2mm, {
    bag$do_z_2_mm <- FALSE

    updateSelectInput(session, "z_2_mm_success", "Success Category",
      choices = ""
    )
  })

  # reset output if different radio button clicked
  observeEvent(input$z_2_data_format, {
    bag$do_z_2_mm <- FALSE
  })


  # print group names
  output$z_2_mm_names <- renderPrint({

    # don't show output or placeholder if no vars selected or if user changes var
    #  (until button pressed)
    if (bag$do_z_2_mm != FALSE) {
      cat(
        capture.output(
          format(bag$z_2_mm_names, justify = "left")
        )[-1],
        sep = "\n"
      )
    }
  })

  # print output for procedures
  output$z_2_mm_summaries <- renderPrint({

    # don't show output or placeholder if no vars selected or if user changes var
    #  (until button pressed)
    if (bag$do_z_2_mm != FALSE) {
      names(bag$z_2_mm_summaries) <- c("Group 1", " Group 2")
      bag$z_2_mm_summaries
    }
  })

  output$z_2_mm_test <- renderPrint({

    # don't show output or placeholder if no vars selected or if user changes var
    #  (until button pressed)
    if (bag$do_z_2_mm != FALSE) {
      cat(capture.output(bag$z_2_mm_test)[-1], sep = "\n")
    }
  })

  output$z_2_mm_ci <- renderText({

    # don't show output or placeholder if no vars selected or if user changes var
    #  (until button pressed)
    if (bag$do_z_2_mm != FALSE) {
      paste0("(", round(bag$z_2_mm_ci[2], 4), ", ", round(bag$z_2_mm_ci[3], 4),
        ")"
      )
    }
  })



  ### Inferential - Two-Proportion z Procedures -
  ###  assuming measurement-group format
  ####################################################

  # update inputs
  observeEvent(input$set_z_2_mg_vars, {

    updateSelectInput(session, "z_2_mg_success", "Success Category",
      choices = unique(na.omit(bag$data_new_df[, input$z_2_select_var_1mg]))
    )

    updateSelectInput(session, "z_2_mg_group_1", "Group 1 Identifier",
      choices = unique(na.omit(bag$data_new_df[, input$z_2_select_var_2mg]))
    )

    updateSelectInput(session, "z_2_mg_group_2", "Group 2 Identifier",
      choices = unique(na.omit(bag$data_new_df[, input$z_2_select_var_2mg]))
    )


    # need this to reset inputs if new variable selected
    bag$use_z_2_mg_vars <- input$set_z_2_mg_vars
  })


  # run two-proportion z procedures
  observeEvent(input$run_z_2_mg, {

    req(input$z_2_mg_group_1, input$z_2_mg_group_2, input$z_2_mg_success)

    z_2_data <- data.frame(
      var1 = bag$data_new_df[, input$z_2_select_var_1mg],
      var2 = bag$data_new_df[, input$z_2_select_var_2mg]
    )

    z_2_grp1 <- z_2_data %>% dplyr::filter(var2 == input$z_2_mg_group_1) %>% dplyr::select(1)
    z_2_grp2 <- z_2_data %>% dplyr::filter(var2 == input$z_2_mg_group_2) %>% dplyr::select(1)


    # number of successes and sample size per sample
    z_2_x1 <- sum(z_2_grp1 == input$z_2_mg_success, na.rm = TRUE)
    z_2_x2 <- sum(z_2_grp2 == input$z_2_mg_success, na.rm = TRUE)
    z_2_n1 <- nrow(na.omit(z_2_grp1))
    z_2_n2 <- nrow(na.omit(z_2_grp2))
    z_2_p_sample_1 <- z_2_x1/z_2_n1
    z_2_p_sample_2 <- z_2_x2/z_2_n2
    z_2_p_combo <- (z_2_x1 + z_2_x2)/(z_2_n1 + z_2_n2)

    z_2_p_test <- input$z_2_mg_null_value


    # calculate Z test statistic
    z_2_test_stat <- ((z_2_p_sample_1 - z_2_p_sample_2) - z_2_p_test) /
      sqrt(z_2_p_combo*(1-z_2_p_combo)*(1/z_2_n1 + 1/z_2_n2))

    # calculate p-values
    # lt = left-tailed, rt = right-tailed, 2t = two-tailed
    z_2_pval_lt <- pnorm(z_2_test_stat)
    z_2_pval_rt <- 1 - z_2_pval_lt
    z_2_pval_2t <- ifelse(z_2_pval_lt < 0.5, 2*z_2_pval_lt, 2*z_2_pval_rt)


    # calculate wald CI
    bag$z_2_mg_ci <- BinomDiffCI(
      x1 = z_2_x1, n1 = z_2_n1,
      x2 = z_2_x2, n2 = z_2_n2,
      conf.level = input$z_2_mg_conf_level/100,
      method = "wald"
    )


    bag$z_2_mg_summaries <- data.frame(
      samp1 = formatC(c(z_2_p_sample_1, z_2_x1, z_2_n1 - z_2_x1, z_2_n1,
        sum(is.na(z_2_grp1))                                                    # FIX ?????????????????
      )),
      samp2 = formatC(c(z_2_p_sample_2, z_2_x2, z_2_n2 - z_2_x2, z_2_n2,
        sum(is.na(z_2_grp2))                                                    # FIX ?????????????????
      )),
      row.names = c("Sample Proportion", "Successes", "Failures",
        "# Non-missing Obs.", "# Missing Obs."
      )
    )

    bag$z_2_mg_test <- data.frame(
      "." = formatC(c(z_2_test_stat, z_2_pval_2t, z_2_pval_lt, z_2_pval_rt)),
      row.names = c("Test Statistic", "p-value (2-sided)  ", "p-value (<)",
        "p-value (>)"
      )
    )


    # store df w/ group names
    bag$z_2_mg_names <- data.frame(
      "." = c(input$z_2_mg_group_1, input$z_2_mg_group_2),
      row.names = c("Group 1: ", "Group 2: ")
    )


    # need this to reset output if new variable selected
    bag$do_z_2_mg <- input$run_z_2_mg
  })


  # reset output and inputs if new variable selected
  observeEvent(input$z_2_select_var_1mg, {
    bag$do_z_2_mg <- FALSE

    updateSelectInput(session, "z_2_mg_success", "Success Category",
      choices = ""
    )

    updateSelectInput(session, "z_2_mg_group_1", "Group 1 Identifier",
      choices = ""
    )

    updateSelectInput(session, "z_2_mg_group_2", "Group 2 Identifier",
      choices = ""
    )
  })

  observeEvent(input$z_2_select_var_2mg, {
    bag$do_z_2_mg <- FALSE

    updateSelectInput(session, "z_2_mg_success", "Success Category",
      choices = ""
    )

    updateSelectInput(session, "z_2_mg_group_1", "Group 1 Identifier",
      choices = ""
    )

    updateSelectInput(session, "z_2_mg_group_2", "Group 2 Identifier",
      choices = ""
    )
  })

  # reset output if different radio button clicked
  observeEvent(input$z_2_data_format, {
    bag$do_z_2_mg <- FALSE
  })


  # print group names
  output$z_2_mg_names <- renderPrint({

    # don't show output or placeholder if no vars selected or if user changes var
    #  (until button pressed)
    if (bag$do_z_2_mg != FALSE) {
      cat(
        capture.output(
          format(bag$z_2_mg_names, justify = "left")
        )[-1],
        sep = "\n"
      )
    }
  })

  # print output for procedures
  output$z_2_mg_summaries <- renderPrint({

    # don't show output or placeholder if no vars selected or if user changes var
    #  (until button pressed)
    if (bag$do_z_2_mg != FALSE) {
      names(bag$z_2_mg_summaries) <- c("Group 1", " Group 2")
      bag$z_2_mg_summaries
    }
  })

  output$z_2_mg_test <- renderPrint({

    # don't show output or placeholder if no vars selected or if user changes var
    #  (until button pressed)
    if (bag$do_z_2_mg != FALSE) {
      cat(capture.output(bag$z_2_mg_test)[-1], sep = "\n")
    }
  })

  output$z_2_mg_ci <- renderText({

    # don't show output or placeholder if no vars selected or if user changes var
    #  (until button pressed)
    if (bag$do_z_2_mg != FALSE) {
      paste0("(", round(bag$z_2_mg_ci[2], 4), ", ", round(bag$z_2_mg_ci[3], 4),
        ")"
      )
    }
  })



  ### Inferential - One-Way ANOVA -
  ###  assuming measurement-measurement format
  ####################################################
  observeEvent(input$run_anova_mm, {

    # format new data set; use more efficient code later **
    anova_values <- c(
      bag$data_new_df[, input$anova_sel_input_1],
      bag$data_new_df[, input$anova_sel_input_2]
    )
    anova_group <- c(
      rep(1, length(bag$data_new_df[, input$anova_sel_input_1])),
      rep(2, length(bag$data_new_df[, input$anova_sel_input_2]))
    )

    if (input$anova_num_grps == 3) {
      anova_values <- c(anova_values,
        bag$data_new_df[, input$anova_sel_input_3]
      )
      anova_group <- c(anova_group,
        rep(3, length(bag$data_new_df[, input$anova_sel_input_3]))
      )
    } else if (input$anova_num_grps == 4) {
      anova_values <- c(anova_values,
        bag$data_new_df[, input$anova_sel_input_3],
        bag$data_new_df[, input$anova_sel_input_4]
      )
      anova_group <- c(anova_group,
        rep(3, length(bag$data_new_df[, input$anova_sel_input_3])),
        rep(4, length(bag$data_new_df[, input$anova_sel_input_4]))
      )
    } else if (input$anova_num_grps == 5) {
      anova_values <- c(anova_values,
        bag$data_new_df[, input$anova_sel_input_3],
        bag$data_new_df[, input$anova_sel_input_4],
        bag$data_new_df[, input$anova_sel_input_5]
      )
      anova_group <- c(anova_group,
        rep(3, length(bag$data_new_df[, input$anova_sel_input_3])),
        rep(4, length(bag$data_new_df[, input$anova_sel_input_4])),
        rep(5, length(bag$data_new_df[, input$anova_sel_input_5]))
      )
    } else if (input$anova_num_grps == 6) {
      anova_values <- c(anova_values,
        bag$data_new_df[, input$anova_sel_input_3],
        bag$data_new_df[, input$anova_sel_input_4],
        bag$data_new_df[, input$anova_sel_input_5],
        bag$data_new_df[, input$anova_sel_input_6]
      )
      anova_group <- c(anova_group,
        rep(3, length(bag$data_new_df[, input$anova_sel_input_3])),
        rep(4, length(bag$data_new_df[, input$anova_sel_input_4])),
        rep(5, length(bag$data_new_df[, input$anova_sel_input_5])),
        rep(6, length(bag$data_new_df[, input$anova_sel_input_6]))
      )
    } else if (input$anova_num_grps == 7) {
      anova_values <- c(anova_values,
        bag$data_new_df[, input$anova_sel_input_3],
        bag$data_new_df[, input$anova_sel_input_4],
        bag$data_new_df[, input$anova_sel_input_5],
        bag$data_new_df[, input$anova_sel_input_6],
        bag$data_new_df[, input$anova_sel_input_7]
      )
      anova_group <- c(anova_group,
        rep(3, length(bag$data_new_df[, input$anova_sel_input_3])),
        rep(4, length(bag$data_new_df[, input$anova_sel_input_4])),
        rep(5, length(bag$data_new_df[, input$anova_sel_input_5])),
        rep(6, length(bag$data_new_df[, input$anova_sel_input_6])),
        rep(7, length(bag$data_new_df[, input$anova_sel_input_7]))
      )
    } else if (input$anova_num_grps == 8) {
      anova_values <- c(anova_values,
        bag$data_new_df[, input$anova_sel_input_3],
        bag$data_new_df[, input$anova_sel_input_4],
        bag$data_new_df[, input$anova_sel_input_5],
        bag$data_new_df[, input$anova_sel_input_6],
        bag$data_new_df[, input$anova_sel_input_7],
        bag$data_new_df[, input$anova_sel_input_8]
      )
      anova_group <- c(anova_group,
        rep(3, length(bag$data_new_df[, input$anova_sel_input_3])),
        rep(4, length(bag$data_new_df[, input$anova_sel_input_4])),
        rep(5, length(bag$data_new_df[, input$anova_sel_input_5])),
        rep(6, length(bag$data_new_df[, input$anova_sel_input_6])),
        rep(7, length(bag$data_new_df[, input$anova_sel_input_7])),
        rep(8, length(bag$data_new_df[, input$anova_sel_input_8]))
      )
    } else if (input$anova_num_grps == 9) {
      anova_values <- c(anova_values,
        bag$data_new_df[, input$anova_sel_input_3],
        bag$data_new_df[, input$anova_sel_input_4],
        bag$data_new_df[, input$anova_sel_input_5],
        bag$data_new_df[, input$anova_sel_input_6],
        bag$data_new_df[, input$anova_sel_input_7],
        bag$data_new_df[, input$anova_sel_input_8],
        bag$data_new_df[, input$anova_sel_input_9]
      )
      anova_group <- c(anova_group,
        rep(3, length(bag$data_new_df[, input$anova_sel_input_3])),
        rep(4, length(bag$data_new_df[, input$anova_sel_input_4])),
        rep(5, length(bag$data_new_df[, input$anova_sel_input_5])),
        rep(6, length(bag$data_new_df[, input$anova_sel_input_6])),
        rep(7, length(bag$data_new_df[, input$anova_sel_input_7])),
        rep(8, length(bag$data_new_df[, input$anova_sel_input_8])),
        rep(9, length(bag$data_new_df[, input$anova_sel_input_9]))
      )
    } else if (input$anova_num_grps == 10) {
      anova_values <- c(anova_values,
        bag$data_new_df[, input$anova_sel_input_3],
        bag$data_new_df[, input$anova_sel_input_4],
        bag$data_new_df[, input$anova_sel_input_5],
        bag$data_new_df[, input$anova_sel_input_6],
        bag$data_new_df[, input$anova_sel_input_7],
        bag$data_new_df[, input$anova_sel_input_8],
        bag$data_new_df[, input$anova_sel_input_9],
        bag$data_new_df[, input$anova_sel_input_10]
      )
      anova_group <- c(anova_group,
        rep(3, length(bag$data_new_df[, input$anova_sel_input_3])),
        rep(4, length(bag$data_new_df[, input$anova_sel_input_4])),
        rep(5, length(bag$data_new_df[, input$anova_sel_input_5])),
        rep(6, length(bag$data_new_df[, input$anova_sel_input_6])),
        rep(7, length(bag$data_new_df[, input$anova_sel_input_7])),
        rep(8, length(bag$data_new_df[, input$anova_sel_input_8])),
        rep(9, length(bag$data_new_df[, input$anova_sel_input_9])),
        rep(10, length(bag$data_new_df[, input$anova_sel_input_10]))
      )
    }


    # make tidy df
    bag$anova_data <- data.frame(
      response = anova_values,
      group = factor(anova_group)
    )


    # warning popup alert if at least one selected var isn't numeric
    if (!is.numeric(bag$anova_data$response)) {
      shinyalert("Warning!", "At least one of the selected variables is not
        quantitative. Please make sure all selected variables are quantitative.",
        type = "error"
      )
    }

    # keeps the app from crashing if at least one selected var isn't numeric
    validate(need(is.numeric(bag$anova_data$response), ""))


    # calculate summaries by explanatory var group
    bag$anova_summaries <- bag$anova_data %>%
      dplyr::group_by(group) %>%
      dplyr::summarize(
        Mean = mean(response, na.rm = TRUE),
        "Std. Dev." = sd(response, na.rm = TRUE),
        " # Non-missing Obs." = sum(!is.na(response)),
        " # Missing Obs." = sum(is.na(response))
      ) %>%
      dplyr::rename(Group = group) %>%
      as.data.frame()

    bag$anova_summaries[, 2:ncol(bag$anova_summaries)] <- round(
      bag$anova_summaries[, 2:ncol(bag$anova_summaries)], 4
    )


    # run one-way anova procedure
    bag$aov_out <- aov(response ~ group, data = bag$anova_data)


    # residuals and fitted df
    bag$resids_df <- data.frame(
      resid = unname(bag$aov_out$residuals),
      fitted = unname(bag$aov_out$fitted.values)
    )


    # run Levene's test
    bag$anova_levene <- LeveneTest(response ~ factor(group),
      data = bag$anova_data
    )


    # run Tukey's HSD for pairwise comparisons
    bag$anova_tukey <- PostHocTest(bag$aov_out,
      conf.level = input$anova_mm_mult_compars_level/100,
      method = "hsd"
    )


    # do pairwise comparisons w/ bonferroni adjustment
    bag$anova_bonf <- PostHocTest(bag$aov_out,
      conf.level = input$anova_mm_mult_compars_level/100,
      method = "bonferroni"
    )


    # store df w/ group names
    anova_mm_names <- list()
    for (i in 1:length(unique(anova_group))) {
      anova_mm_names[[i]] <- data.frame(
        "." = input[[paste0("anova_sel_input_", i)]],
        row.names = paste0("Group ", i, ": ")
      )
    }

    bag$anova_mm_names <- do.call(rbind, anova_mm_names)


    # need this to reset output if new variable selected
    bag$do_anova_mm <- input$run_anova_mm
  })


  # reset output if new variable selected
  observeEvent(input$anova_sel_input_1, {
    bag$do_anova_mm <- FALSE
  })

  observeEvent(input$anova_sel_input_2, {
    bag$do_anova_mm <- FALSE
  })

  observeEvent(input$anova_sel_input_3, {
    bag$do_anova_mm <- FALSE
  })

  observeEvent(input$anova_sel_input_4, {
    bag$do_anova_mm <- FALSE
  })

  observeEvent(input$anova_sel_input_5, {
    bag$do_anova_mm <- FALSE
  })

  observeEvent(input$anova_sel_input_6, {
    bag$do_anova_mm <- FALSE
  })

  # reset output if different radio button clicked
  observeEvent(input$anova_data_format, {
    bag$do_anova_mm <- FALSE
  })


  # print group names
  output$anova_mm_names <- renderPrint({

    # don't show output or placeholder if no vars selected or if user changes var
    #  (until button pressed)
    if (bag$do_anova_mm != FALSE) {
      cat(
        capture.output(
          format(bag$anova_mm_names, justify = "left")
        )[-1],
        sep = "\n"
      )
    }
  })

  # print summary statistics by group
  output$anova_mm_stats <- renderPrint({

    # don't show output or placeholder if no vars selected or if user changes var
    #  (until button pressed)
    if (bag$do_anova_mm != FALSE) {
      print(bag$anova_summaries, row.names = FALSE)
    }
  })


  # print output for anova
  output$anova_mm_output <- renderPrint({

    # don't show output or placeholder if no vars selected or if user changes var
    #  (until button pressed)
    if (bag$do_anova_mm != FALSE) {

      # store summary of aov()
      aov_summary <- summary(bag$aov_out)

      # format output
      anova_table_asdf <- as.data.frame(aov_summary[[1]])
      anova_table_asdf[, 2:4] <- round(anova_table_asdf[, 2:4], 3)
      anova_table_asdf[1, 5] <- signif(anova_table_asdf[1, 5], digits = 3)

      # add total row
      anova_table_asdf[3, ] <- c(sum(anova_table_asdf[, 1]),
        sum(anova_table_asdf[, 2]), "", "", ""
      )
      anova_table_asdf[2, 4:5] <- c("", "")

      # update row and column names
      row.names(anova_table_asdf) <- c("Group", "Error", "Total")
      names(anova_table_asdf) <- c(" df", "  Sum Sq.", " Mean Sq.", " F Stat",
        "  p-value"
      )

      anova_table_asdf
    }
  })


  # print output of Levene's test
  output$anova_mm_levene <- renderPrint({

    # don't show output or placeholder if no vars selected or if user changes var
    #  (until button pressed)
    if (bag$do_anova_mm != FALSE) {

      # format output
      levene_df <- as.data.frame(bag$anova_levene[1, 1:3])
      levene_df[1, 2] <- round(levene_df[1, 2], 3)
      levene_df[1, 3] <- signif(levene_df[1, 3], digits = 3)
      names(levene_df) <- c("df", " F Stat", " p-value")

      print.data.frame(levene_df, row.names = FALSE)
    }
  })


  # print tukey's HSD output
  output$anova_mm_tukey <- renderPrint({

    # don't show output or placeholder if no vars selected or if user changes var
    #  (until button pressed)
    if (bag$do_anova_mm != FALSE) {
      tukey_out <- as.data.frame(bag$anova_tukey[[1]])
      tukey_out[, 1:3] <- round(tukey_out[, 1:3], 3)
      tukey_out[, 4] <- signif(tukey_out[, 4], digits = 4)
      names(tukey_out) <- c(" Difference in Means", " Lower CI", " Upper CI",
        " Adjusted p-value"
      )
      tukey_out
    }
  })

  # print bonferroni output
  output$anova_mm_bonf <- renderPrint({

    # don't show output or placeholder if no vars selected or if user changes var
    #  (until button pressed)
    if (bag$do_anova_mm != FALSE) {
      bonf_out <- as.data.frame(bag$anova_bonf[[1]])
      bonf_out[, 1:3] <- round(bonf_out[, 1:3], 3)
      bonf_out[, 4] <- signif(bonf_out[, 4], digits = 4)
      names(bonf_out) <- c(" Difference in Means", " Lower CI", " Upper CI",
        " Adjusted p-value"
      )
      bonf_out
    }
  })


  # side-by-side boxplots
  output$boxplots_anova_mm <- renderPlot({

    # don't show output or placeholder if no vars selected or if user changes var
    #  (until button pressed)
    if (bag$do_anova_mm != FALSE) {
      suppressWarnings(print(
        ggplot(bag$anova_data, aes(x = group, y = response)) +
          geom_boxplot() +
          labs(x = "Group", y = "Response")
      ))
    }
  })


  # residual plot
  output$resid_plot_anova_mm <- renderPlot({

    # don't show output or placeholder if no vars selected or if user changes var
    #  (until button pressed)
    if (bag$do_anova_mm != FALSE) {
      suppressWarnings(print(
        ggplot(bag$resids_df, aes(x = fitted, y = resid)) +
          geom_point(size = 2) +
          geom_abline(slope = 0) +
          labs(x = "Fitted Value", y = "Residual")
      ))
    }
  })

  # histogram of residuals
  output$hist_anova_mm <- renderPlot({

    # don't show output or placeholder if no vars selected or if user changes var
    #  (until button pressed)
    if (bag$do_anova_mm != FALSE) {
      suppressWarnings(print(
        ggplot(bag$resids_df, aes(x = resid)) +
          geom_histogram(bins = input$hist_anova_mm_resids_bins) +
          labs(x = "Residual", y = "Count")
      ))
    }
  })


  # normal QQ plot of residuals
  output$norm_qq_anova_mm <- renderPlot({

    # don't show output or placeholder if no vars selected or if user changes var
    #  (until button pressed)
    if (bag$do_anova_mm != FALSE) {
      suppressWarnings(print(
        ggplot(bag$resids_df, aes(sample = resid)) +
          stat_qq() +
          stat_qq_line() +
          labs(x = "Theoretical Quantile", y = "Sample Quantile")
      ))
    }
  })



  ### Inferential - One-Way ANOVA -
  ###  assuming measurement-group format
  ####################################################
  observeEvent(input$run_anova_mg, {

    req(input$anova_select_var_1mg, input$anova_select_var_2mg)


    # make df w/ only response and group var's, w/ group var as a factor
    bag$anova_data <- data.frame(
      response = bag$data_new_df[, input$anova_select_var_1mg],
      group = factor(bag$data_new_df[, input$anova_select_var_2mg])
    )


    # warning popup alert if the response var isn't numeric
    if (!is.numeric(bag$anova_data$response)) {
      shinyalert("Warning!", "The selected response variable is not quantitative.
        Please select a quantitative response.",
        type = "error"
      )
    }

    # keeps the app from crashing if the response var isn't numeric
    validate(need(is.numeric(bag$anova_data$response), ""))


    bag$anova_data <- bag$anova_data %>% dplyr::filter(group != "")


    # calculate summaries by explanatory var group
    bag$anova_summaries <- bag$anova_data %>%
      dplyr::group_by(group) %>%
      dplyr::summarize(
        Mean = mean(response, na.rm = TRUE),
        "Std. Dev." = sd(response, na.rm = TRUE),
        " # Non-missing Obs." = sum(!is.na(response)),
        " # Missing Obs." = sum(is.na(response))
      ) %>%
      dplyr::rename(Group = group) %>%
      as.data.frame()

    bag$anova_summaries[, 2:ncol(bag$anova_summaries)] <- round(
      bag$anova_summaries[, 2:ncol(bag$anova_summaries)], 4
    )


    # run one-way anova procedure
    bag$aov_out <- aov(response ~ group, data = bag$anova_data)


    # residuals and fitted df
    bag$resids_df <- data.frame(
      resid = unname(bag$aov_out$residuals),
      fitted = unname(bag$aov_out$fitted.values)
    )


    # run Levene's test
    bag$anova_levene <- LeveneTest(response ~ factor(group),
      data = bag$anova_data
    )


    # run Tukey's HSD for pairwise comparisons
    bag$anova_tukey <- PostHocTest(bag$aov_out,
      conf.level = input$anova_mg_mult_compars_level/100,
      method = "hsd"
    )


    # do pairwise comparisons w/ bonferroni adjustment
    bag$anova_bonf <- PostHocTest(bag$aov_out,
      conf.level = input$anova_mg_mult_compars_level/100,
      method = "bonferroni"
    )


    # store df w/ group names
    # anova_mg_names <- list()
    # for (i in 1:length(unique(bag$anova_data$group))) {
    #   anova_mg_names[[i]] <- data.frame(
    #     "." = bag$anova_summaries$Group[i],
    #     row.names = paste0("Group ", i, ": ")
    #   )
    # }
    #
    # bag$anova_mg_names <- do.call(rbind, anova_mg_names)


    # need this to reset output if new variable selected
    bag$do_anova_mg <- input$run_anova_mg
  })


  # reset output if new variable selected
  observeEvent(input$anova_select_var_1mg, {
    bag$do_anova_mg <- FALSE
  })

  observeEvent(input$anova_select_var_2mg, {
    bag$do_anova_mg <- FALSE
  })

  # reset output if different radio button clicked
  observeEvent(input$anova_data_format, {
    bag$do_anova_mg <- FALSE
  })


  # print group names
  # output$anova_mg_names <- renderPrint({
  #
  #   # don't show output or placeholder if no vars selected or if user changes var
  #   #  (until button pressed)
  #   if (bag$do_anova_mg != FALSE) {
  #     cat(
  #       capture.output(
  #         format(bag$anova_mg_names, justify = "left")
  #       )[-1],
  #       sep = "\n"
  #     )
  #   }
  # })

  # print summary statistics by group
  output$anova_mg_stats <- renderPrint({

    # don't show output or placeholder if no vars selected or if user changes var
    #  (until button pressed)
    if (bag$do_anova_mg != FALSE) {
      print(bag$anova_summaries, row.names = FALSE)
    }
  })


  # print output for anova
  output$anova_mg_output <- renderPrint({

    # don't show output or placeholder if no vars selected or if user changes var
    #  (until button pressed)
    if (bag$do_anova_mg != FALSE) {
      # store summary of aov()
      aov_summary <- summary(bag$aov_out)

      # format output
      anova_table_asdf <- as.data.frame(aov_summary[[1]])
      anova_table_asdf[, 2:4] <- round(anova_table_asdf[, 2:4], 3)
      anova_table_asdf[1, 5] <- signif(anova_table_asdf[1, 5], digits = 4)

      # add total row
      anova_table_asdf[3, ] <- c(sum(anova_table_asdf[, 1]),
        sum(anova_table_asdf[, 2]), "", "", ""
      )
      anova_table_asdf[2, 4:5] <- c("", "")

      # update row and column names
      row.names(anova_table_asdf) <- c("Group", "Error", "Total")
      names(anova_table_asdf) <- c(" df", "  Sum Sq.", " Mean Sq.", " F Stat",
        "  p-value"
      )

      anova_table_asdf
    }
  })


  # print output of Levene's test
  output$anova_mg_levene <- renderPrint({

    # don't show output or placeholder if no vars selected or if user changes var
    #  (until button pressed)
    if (bag$do_anova_mg != FALSE) {
      # format output
      levene_df <- as.data.frame(bag$anova_levene[1, 1:3])
      levene_df[1, 2] <- round(levene_df[1, 2], 3)
      levene_df[1, 3] <- signif(levene_df[1, 3], digits = 4)
      names(levene_df) <- c("df", " F Stat", " p-value")

      print.data.frame(levene_df, row.names = FALSE)
    }
  })


  # print tukey's HSD output
  output$anova_mg_tukey <- renderPrint({

    # don't show output or placeholder if no vars selected or if user changes var
    #  (until button pressed)
    if (bag$do_anova_mg != FALSE) {
      tukey_out <- as.data.frame(bag$anova_tukey[[1]])
      tukey_out[, 1:3] <- round(tukey_out[, 1:3], 3)
      tukey_out[, 4] <- signif(tukey_out[, 4], digits = 3)
      names(tukey_out) <- c(" Difference in Means", " Lower CI", " Upper CI",
        " Adjusted p-value"
      )
      tukey_out
    }
  })


  # print bonferroni output
  output$anova_mg_bonf <- renderPrint({

    # don't show output or placeholder if no vars selected or if user changes var
    #  (until button pressed)
    if (bag$do_anova_mg != FALSE) {
      bonf_out <- as.data.frame(bag$anova_bonf[[1]])
      bonf_out[, 1:3] <- round(bonf_out[, 1:3], 3)
      bonf_out[, 4] <- signif(bonf_out[, 4], digits = 3)
      names(bonf_out) <- c(" Difference in Means", " Lower CI", " Upper CI",
        " Adjusted p-value"
      )
      bonf_out
    }
  })


  # side-by-side boxplots
  output$boxplots_anova_mg <- renderPlot({

    # don't show output or placeholder if no vars selected or if user changes var
    #  (until button pressed)
    if (bag$do_anova_mg != FALSE) {
      suppressWarnings(print(
        ggplot(data = drop_na(bag$anova_data, group), aes(x = group, y = response)) +
          geom_boxplot() +
          labs(x = "Group", y = "Response")
      ))
    }
  })


  # residual plot
  output$resid_plot_anova_mg <- renderPlot({

    # don't show output or placeholder if no vars selected or if user changes var
    #  (until button pressed)
    if (bag$do_anova_mg != FALSE) {
      suppressWarnings(print(
        ggplot(bag$resids_df, aes(x = fitted, y = resid)) +
          geom_point(size = 2) +
          geom_abline(slope = 0) +
          labs(x = "Fitted Value", y = "Residual")
      ))
    }
  })

  # histogram of residuals
  output$hist_anova_mg <- renderPlot({

    # don't show output or placeholder if no vars selected or if user changes var
    #  (until button pressed)
    if (bag$do_anova_mg != FALSE) {
      suppressWarnings(print(
        ggplot(bag$resids_df, aes(x = resid)) +
          geom_histogram(bins = input$hist_anova_mg_resids_bins) +
          labs(x = "Residual", y = "Count")
      ))
    }
  })


  # normal QQ plot of residuals
  output$norm_qq_anova_mg <- renderPlot({

    # don't show output or placeholder if no vars selected or if user changes var
    #  (until button pressed)
    if (bag$do_anova_mg != FALSE) {
      suppressWarnings(print(
        ggplot(bag$resids_df, aes(sample = resid)) +
          stat_qq() +
          stat_qq_line() +
          labs(x = "Theoretical Quantile", y = "Sample Quantile")
      ))
    }
  })



  ### Inferential - Chi-Square Goodness of Fit Test -
  ###  assuming individual observations used
  ####################################################
  observeEvent(input$set_cs_gof_obs_var, {

    req(input$cs_gof_select_var)


    # count number of categories
    bag$cs_gof_num_categories <- length(unique(
      na.omit(bag$data_new_df[, input$cs_gof_select_var])
    ))


    # make df w/ category numbers and labels
    bag$cat_identify_df <- data.frame(
      col1 = 1:bag$cs_gof_num_categories,
      col2 = bag$data_new_df[, input$cs_gof_select_var] %>%
        na.omit() %>%
        unique() %>%
        sort()  # if don't sort, categories using table() won't necessarily match
    )

    names(bag$cat_identify_df) <- c("Group #", " Group Label")


    # need to reset output if new variable selected
    bag$use_cs_gof_obs_var <- input$set_cs_gof_obs_var
  })


  # print categories w/ assigned numbers
  output$cs_gof_obs_cats <- renderPrint({

    if (bag$use_cs_gof_obs_var != FALSE) {
      print.data.frame(bag$cat_identify_df, row.names = FALSE)
    }
  })


  # update test value options based on variable selected
  output$cs_gof_obs_null_values <- renderUI({

    if (bag$use_cs_gof_obs_var != FALSE) {
      bag$cs_gof_num_categories <- length(unique(
        na.omit(bag$data_new_df[, input$cs_gof_select_var])
      ))


      props <- round(
        rep(1/bag$cs_gof_num_categories, bag$cs_gof_num_categories),
        4
      )

      # create numericInputs
      all_inputs <- list()
      all_inputs <- lapply(1:(bag$cs_gof_num_categories - 1), function(i) {
        numericInput(
          paste0("cs_gof_obs_null_value_", i),
          paste0("Test Proportion (Group ", i, ")"),
          value = props[i],
          step = 0.05
        )
      })

      all_inputs[[bag$cs_gof_num_categories]] <- numericInput(
        paste0("cs_gof_obs_null_value_", bag$cs_gof_num_categories),
        paste0("Test Proportion (Group ", bag$cs_gof_num_categories, ")"),
        value = 1-sum(props[-bag$cs_gof_num_categories]),
        step = 0.05
      )

      all_inputs
    }
  })


  # run CS GoF test
  observeEvent(input$run_cs_gof_obs, {

    req(bag$use_cs_gof_obs_var)


    # set proportions/probabilities to test
    if (bag$cs_gof_num_categories == 1) {
      # if only one count supplied, give popup warning message and keep app from
      #  crashing
      shinyalert("Warning!", "You need to input more than one observed count.",
        type = "error"
      )
      validate(need(bag$cs_gof_num_categories >= 2, ""))
    } else if (bag$cs_gof_num_categories == 2) {
      req(input$cs_gof_obs_null_value_1, input$cs_gof_obs_null_value_2)
      probs <- c(input$cs_gof_obs_null_value_1, input$cs_gof_obs_null_value_2)
    } else if (bag$cs_gof_num_categories == 3) {
      req(input$cs_gof_obs_null_value_1, input$cs_gof_obs_null_value_2,
        input$cs_gof_obs_null_value_3
      )
      probs <- c(input$cs_gof_obs_null_value_1, input$cs_gof_obs_null_value_2,
        input$cs_gof_obs_null_value_3
      )
    } else if (bag$cs_gof_num_categories == 4) {
      req(input$cs_gof_obs_null_value_1, input$cs_gof_obs_null_value_2,
        input$cs_gof_obs_null_value_3, input$cs_gof_obs_null_value_4
      )
      probs <- c(input$cs_gof_obs_null_value_1, input$cs_gof_obs_null_value_2,
        input$cs_gof_obs_null_value_3, input$cs_gof_obs_null_value_4
      )
    } else if (bag$cs_gof_num_categories == 5) {
      req(input$cs_gof_obs_null_value_1, input$cs_gof_obs_null_value_2,
        input$cs_gof_obs_null_value_3, input$cs_gof_obs_null_value_4,
        input$cs_gof_obs_null_value_5
      )
      probs <- c(input$cs_gof_obs_null_value_1, input$cs_gof_obs_null_value_2,
        input$cs_gof_obs_null_value_3, input$cs_gof_obs_null_value_4,
        input$cs_gof_obs_null_value_5
      )
    } else if (bag$cs_gof_num_categories == 6) {
      req(input$cs_gof_obs_null_value_1, input$cs_gof_obs_null_value_2,
        input$cs_gof_obs_null_value_3, input$cs_gof_obs_null_value_4,
        input$cs_gof_obs_null_value_5, input$cs_gof_obs_null_value_6
      )
      probs <- c(input$cs_gof_obs_null_value_1, input$cs_gof_obs_null_value_2,
        input$cs_gof_obs_null_value_3, input$cs_gof_obs_null_value_4,
        input$cs_gof_obs_null_value_5, input$cs_gof_obs_null_value_6
      )
    } else if (bag$cs_gof_num_categories == 7) {
      req(input$cs_gof_obs_null_value_1, input$cs_gof_obs_null_value_2,
        input$cs_gof_obs_null_value_3, input$cs_gof_obs_null_value_4,
        input$cs_gof_obs_null_value_5, input$cs_gof_obs_null_value_6,
        input$cs_gof_obs_null_value_7
      )
      probs <- c(input$cs_gof_obs_null_value_1, input$cs_gof_obs_null_value_2,
        input$cs_gof_obs_null_value_3, input$cs_gof_obs_null_value_4,
        input$cs_gof_obs_null_value_5, input$cs_gof_obs_null_value_6,
        input$cs_gof_obs_null_value_7
      )
    } else if (bag$cs_gof_num_categories == 8) {
      req(input$cs_gof_obs_null_value_1, input$cs_gof_obs_null_value_2,
        input$cs_gof_obs_null_value_3, input$cs_gof_obs_null_value_4,
        input$cs_gof_obs_null_value_5, input$cs_gof_obs_null_value_6,
        input$cs_gof_obs_null_value_7, input$cs_gof_obs_null_value_8
      )
      probs <- c(input$cs_gof_obs_null_value_1, input$cs_gof_obs_null_value_2,
        input$cs_gof_obs_null_value_3, input$cs_gof_obs_null_value_4,
        input$cs_gof_obs_null_value_5, input$cs_gof_obs_null_value_6,
        input$cs_gof_obs_null_value_7, input$cs_gof_obs_null_value_8
      )
    } else if (bag$cs_gof_num_categories == 9) {
      req(input$cs_gof_obs_null_value_1, input$cs_gof_obs_null_value_2,
        input$cs_gof_obs_null_value_3, input$cs_gof_obs_null_value_4,
        input$cs_gof_obs_null_value_5, input$cs_gof_obs_null_value_6,
        input$cs_gof_obs_null_value_7, input$cs_gof_obs_null_value_8,
        input$cs_gof_obs_null_value_9
      )
      probs <- c(input$cs_gof_obs_null_value_1, input$cs_gof_obs_null_value_2,
        input$cs_gof_obs_null_value_3, input$cs_gof_obs_null_value_4,
        input$cs_gof_obs_null_value_5, input$cs_gof_obs_null_value_6,
        input$cs_gof_obs_null_value_7, input$cs_gof_obs_null_value_8,
        input$cs_gof_obs_null_value_9
      )
    } else if (bag$cs_gof_num_categories == 10) {
      req(input$cs_gof_obs_null_value_1, input$cs_gof_obs_null_value_2,
        input$cs_gof_obs_null_value_3, input$cs_gof_obs_null_value_4,
        input$cs_gof_obs_null_value_5, input$cs_gof_obs_null_value_6,
        input$cs_gof_obs_null_value_7, input$cs_gof_obs_null_value_8,
        input$cs_gof_obs_null_value_9, input$cs_gof_obs_null_value_10
      )
      probs <- c(input$cs_gof_obs_null_value_1, input$cs_gof_obs_null_value_2,
        input$cs_gof_obs_null_value_3, input$cs_gof_obs_null_value_4,
        input$cs_gof_obs_null_value_5, input$cs_gof_obs_null_value_6,
        input$cs_gof_obs_null_value_7, input$cs_gof_obs_null_value_8,
        input$cs_gof_obs_null_value_9, input$cs_gof_obs_null_value_10
      )
    }


    # warning popup alert if test proportions don't add to 1
    if (sum(probs) != 1) {
      shinyalert("Warning!", "The test proportions must add to exactly 1.",
        type = "error"
      )
    }

    # keeps the app from crashing if the test proportions don't add to 1
    validate(need(sum(probs) == 1, ""))


    # select data
    selected_var_all <- bag$data_new_df[, input$cs_gof_select_var]


    # remove NAs
    selected_var <- na.omit(selected_var_all)


    # find observed counts
    freq_table <- table(selected_var)
    freqs <- as.vector(freq_table)


    # run cs gof test
    cs_gof_test <- chisq.test(freqs, p = probs)


    # make table of observed and expected counts
    bag$table_counts <- data.frame(
      Group = names(freq_table),
      Observed = freqs,
      Expected = round(cs_gof_test$expected, 2)
    )


    # store cs gof test output
    bag$cs_gof_test <- data.frame(
      "." = formatC(c(cs_gof_test$statistic, cs_gof_test$parameter,
        cs_gof_test$p.value)
      ),
      row.names = c("Test Statistic  ", "df", "p-value")
    )


    # count number of missing values
    bag$cs_gof_NA <- length(selected_var_all) - length(selected_var)


    # need to reset output if new variable or new radio button selected
    bag$do_cs_gof_obs <- input$run_cs_gof_obs
  })


  # chi-square gof test -- assumes individual observations used
  observeEvent(input$cs_gof_select_var, {
    bag$use_cs_gof_obs_var <- FALSE
    bag$do_cs_gof_obs <- FALSE
  })

  # reset output if different radio button clicked
  observeEvent(input$cs_gof_data_format, {
    bag$use_cs_gof_obs_var <- FALSE
    bag$do_cs_gof_obs <- FALSE
  })


  # chi-square gof test output
  output$cs_gof_obs_out <- renderPrint({

    if (bag$do_cs_gof_obs != FALSE) {
      cat(capture.output(bag$cs_gof_test)[-1], sep = "\n")
    }
  })

  # chi-square gof observed and expected counts
  output$cs_gof_obs_all_counts <- renderPrint({

    if (bag$do_cs_gof_obs != FALSE) {
      # add space before column names
      names(bag$table_counts) <- c("Group", " Observed", " Expected")
      print.data.frame(bag$table_counts, row.names = FALSE)
    }
  })

  # chi-square gof missing values
  output$cs_gof_obs_NAs <- renderPrint({

    if (bag$do_cs_gof_obs != FALSE) {
      cat(stri_dup(intToUtf8(160), 4), bag$cs_gof_NA)
    }
  })



  ### Inferential - Chi-Square Goodness of Fit Test -
  ###  assuming user manually inputs counts
  ####################################################
  output$cs_gof_counts_null_values <- renderUI({

    bag$cs_gof_user_counts <- input$cs_gof_counts %>%
      strsplit(split = "\n|,") %>%
      unlist() %>%
      as.numeric()

    # require at least 2 counts to be input; prevents app from crashing
    validate(need(length(bag$cs_gof_user_counts) >= 2, ""))


    bag$cs_gof_groups <- as.integer(length(bag$cs_gof_user_counts))

    props <- round(rep(1/bag$cs_gof_groups, bag$cs_gof_groups), 4)


    all_inputs <- list()
    all_inputs <- lapply(1:(bag$cs_gof_groups-1), function(i) {
      numericInput(
        paste0("cs_gof_counts_null_value_", i),
        paste0("Test Proportion (Group ", i, ")"),
        value = props[i],
        step = 0.05
      )
    })

    all_inputs[[bag$cs_gof_groups]] <- numericInput(
      paste0("cs_gof_counts_null_value_", bag$cs_gof_groups),
      paste0("Test Proportion (Group ", bag$cs_gof_groups, ")"),
      value = 1 - sum(props[-bag$cs_gof_groups]),
      step = 0.05
    )

    all_inputs
  })


  # run CS GoF test
  observeEvent(input$run_cs_gof_counts, {

    # set proportions/probabilities to test
    if (length(bag$cs_gof_user_counts) %in% 0:1) {
      # if only one count supplied, give popup warning message and keep app from
      #  crashing
      shinyalert("Warning!", "You need to input at least 2 observed counts.",
        type = "error"
      )
      validate(need(length(bag$cs_gof_user_counts) >= 2, ""))
    } else if (length(bag$cs_gof_user_counts) == 2) {
      req(input$cs_gof_counts_null_value_1, input$cs_gof_counts_null_value_2)
      probs <- c(input$cs_gof_counts_null_value_1,
        input$cs_gof_counts_null_value_2
      )
    } else if (length(bag$cs_gof_user_counts) == 3) {
      req(input$cs_gof_counts_null_value_1, input$cs_gof_counts_null_value_2,
        input$cs_gof_counts_null_value_3
      )
      probs <- c(input$cs_gof_counts_null_value_1,
        input$cs_gof_counts_null_value_2, input$cs_gof_counts_null_value_3
      )
    } else if (length(bag$cs_gof_user_counts) == 4) {
      req(input$cs_gof_counts_null_value_1, input$cs_gof_counts_null_value_2,
        input$cs_gof_counts_null_value_3, input$cs_gof_counts_null_value_4
      )
      probs <- c(input$cs_gof_counts_null_value_1,
        input$cs_gof_counts_null_value_2, input$cs_gof_counts_null_value_3,
        input$cs_gof_counts_null_value_4
      )
    } else if (length(bag$cs_gof_user_counts) == 5) {
      req(input$cs_gof_counts_null_value_1, input$cs_gof_counts_null_value_2,
        input$cs_gof_counts_null_value_3, input$cs_gof_counts_null_value_4,
        input$cs_gof_counts_null_value_5
      )
      probs <- c(input$cs_gof_counts_null_value_1,
        input$cs_gof_counts_null_value_2, input$cs_gof_counts_null_value_3,
        input$cs_gof_counts_null_value_4, input$cs_gof_counts_null_value_5
      )
    } else if (length(bag$cs_gof_user_counts) == 6) {
      req(input$cs_gof_counts_null_value_1, input$cs_gof_counts_null_value_2,
        input$cs_gof_counts_null_value_3, input$cs_gof_counts_null_value_4,
        input$cs_gof_counts_null_value_5, input$cs_gof_counts_null_value_6
      )
      probs <- c(input$cs_gof_counts_null_value_1,
        input$cs_gof_counts_null_value_2, input$cs_gof_counts_null_value_3,
        input$cs_gof_counts_null_value_4, input$cs_gof_counts_null_value_5,
        input$cs_gof_counts_null_value_6
      )
    } else if (length(bag$cs_gof_user_counts) == 7) {
      req(input$cs_gof_counts_null_value_1, input$cs_gof_counts_null_value_2,
        input$cs_gof_counts_null_value_3, input$cs_gof_counts_null_value_4,
        input$cs_gof_counts_null_value_5, input$cs_gof_counts_null_value_6,
        input$cs_gof_counts_null_value_7
      )
      probs <- c(input$cs_gof_counts_null_value_1,
        input$cs_gof_counts_null_value_2, input$cs_gof_counts_null_value_3,
        input$cs_gof_counts_null_value_4, input$cs_gof_counts_null_value_5,
        input$cs_gof_counts_null_value_6, input$cs_gof_counts_null_value_7
      )
    } else if (length(bag$cs_gof_user_counts) == 8) {
      req(input$cs_gof_counts_null_value_1, input$cs_gof_counts_null_value_2,
        input$cs_gof_counts_null_value_3, input$cs_gof_counts_null_value_4,
        input$cs_gof_counts_null_value_5, input$cs_gof_counts_null_value_6,
        input$cs_gof_counts_null_value_7, input$cs_gof_counts_null_value_8
      )
      probs <- c(input$cs_gof_counts_null_value_1,
        input$cs_gof_counts_null_value_2, input$cs_gof_counts_null_value_3,
        input$cs_gof_counts_null_value_4, input$cs_gof_counts_null_value_5,
        input$cs_gof_counts_null_value_6, input$cs_gof_counts_null_value_7,
        input$cs_gof_counts_null_value_8
      )
    } else if (length(bag$cs_gof_user_counts) == 9) {
      req(input$cs_gof_counts_null_value_1, input$cs_gof_counts_null_value_2,
        input$cs_gof_counts_null_value_3, input$cs_gof_counts_null_value_4,
        input$cs_gof_counts_null_value_5, input$cs_gof_counts_null_value_6,
        input$cs_gof_counts_null_value_7, input$cs_gof_counts_null_value_8,
        input$cs_gof_counts_null_value_9
      )
      probs <- c(input$cs_gof_counts_null_value_1,
        input$cs_gof_counts_null_value_2, input$cs_gof_counts_null_value_3,
        input$cs_gof_counts_null_value_4, input$cs_gof_counts_null_value_5,
        input$cs_gof_counts_null_value_6, input$cs_gof_counts_null_value_7,
        input$cs_gof_counts_null_value_8, input$cs_gof_counts_null_value_9
      )
    } else if (length(bag$cs_gof_user_counts) == 10) {
      req(input$cs_gof_counts_null_value_1, input$cs_gof_counts_null_value_2,
        input$cs_gof_counts_null_value_3, input$cs_gof_counts_null_value_4,
        input$cs_gof_counts_null_value_5, input$cs_gof_counts_null_value_6,
        input$cs_gof_counts_null_value_7, input$cs_gof_counts_null_value_8,
        input$cs_gof_counts_null_value_9, input$cs_gof_counts_null_value_10
      )
      probs <- c(input$cs_gof_counts_null_value_1,
        input$cs_gof_counts_null_value_2, input$cs_gof_counts_null_value_3,
        input$cs_gof_counts_null_value_4, input$cs_gof_counts_null_value_5,
        input$cs_gof_counts_null_value_6, input$cs_gof_counts_null_value_7,
        input$cs_gof_counts_null_value_8, input$cs_gof_counts_null_value_9,
        input$cs_gof_counts_null_value_10
      )
    }


    # warning popup alert if test proportions don't add to 1
    if (sum(probs) != 1) {
      shinyalert("Warning!", "The test proportions must add to exactly 1.",
        type = "error"
      )
    }

    # keeps the app from crashing if the test proportions don't add to 1
    validate(need(sum(probs) == 1, ""))


    # run cs gof test
    cs_gof_test <- chisq.test(bag$cs_gof_user_counts, p = probs)


    # make table of observed and expected counts
    bag$table_counts <- data.frame(
      Group = 1:length(bag$cs_gof_user_counts),
      Observed = bag$cs_gof_user_counts,
      Expected = round(cs_gof_test$expected, 2)
    )


    # format cs gof test output
    bag$cs_gof_test <- data.frame(
      "." = formatC(c(cs_gof_test$statistic, cs_gof_test$parameter,
        cs_gof_test$p.value)
      ),
      row.names = c("Test Statistic  ", "df", "p-value")
    )


    # need to reset output if new radio button clicked
    bag$do_cs_gof_counts <- input$run_cs_gof_counts
  })


  # reset output if different radio button clicked
  observeEvent(input$cs_gof_data_format, {
    bag$do_cs_gof_counts <- FALSE
    bag$do_cs_gof_obs <- FALSE
  })


  # chi-square gof test output
  output$cs_gof_counts_out <- renderPrint({

    if (bag$do_cs_gof_counts != FALSE) {
      cat(capture.output(bag$cs_gof_test)[-1], sep = "\n")
    }
  })

  # chi-square gof observed and expected counts
  output$cs_gof_counts_all_counts <- renderPrint({

    if (bag$do_cs_gof_counts != FALSE) {
      # add space before column names
      names(bag$table_counts) <- c("Group", " Observed", " Expected")
      print.data.frame(bag$table_counts, row.names = FALSE)
    }
  })



  ### Inferential - Chi-Square Test of Independence -
  ###  assuming variables from data set used
  ####################################################
  observeEvent(input$run_cs_indep_dataset, {

    req(input$cs_indep_select_var_1, input$cs_indep_select_var_2)

    # select variables
    selected_vars_all <- bag$data_new_df[,
      c(input$cs_indep_select_var_1, input$cs_indep_select_var_2)
    ]

    # remove NAs
    selected_vars <- na.omit(selected_vars_all)


    bag$table_obs <- table(selected_vars)

    bag$cs_indep_test <- chisq.test(bag$table_obs)

    bag$cs_indep_test_df <- data.frame(
      "." = formatC(c(bag$cs_indep_test$statistic, bag$cs_indep_test$parameter,
        bag$cs_indep_test$p.value)
      ),
      row.names = c("Test Statistic  ", "df", "p-value")
    )


    # count number of missing values
    bag$cs_indep_NA <- nrow(selected_vars_all) - nrow(selected_vars)


    # need this to reset output if new variable selected
    bag$do_cs_indep_dataset <- input$run_cs_indep_dataset
  })


  # reset output if new variable selected
  observeEvent(input$cs_indep_select_var_1, {
    bag$do_cs_indep_dataset <- FALSE
  })

  observeEvent(input$cs_indep_select_var_2, {
    bag$do_cs_indep_dataset <- FALSE
  })

  # reset output if new radio button clicked
  observeEvent(input$cs_indep_data_format, {
    bag$do_cs_indep_dataset <- FALSE
  })


  # chi-square test of independence output
  output$cs_indep_dataset_test_out <- renderPrint({

    # don't show output or placeholder if no vars selected or if user changes var
    #  (until button pressed)
    if (bag$do_cs_indep_dataset != FALSE) {
      cat(capture.output(bag$cs_indep_test_df)[-1], sep = "\n")
    }
  })

  # chi-square test of independence observed counts
  output$cs_indep_dataset_obs_counts <- renderPrint({

    # don't show output or placeholder if no vars selected or if user changes var
    #  (until button pressed)
    if (bag$do_cs_indep_dataset != FALSE) bag$table_obs
  })

  # chi-square test of independence expected counts
  output$cs_indep_dataset_exp_counts <- renderPrint({

    # don't show output or placeholder if no vars selected or if user changes var
    #  (until button pressed)
    if (bag$do_cs_indep_dataset != FALSE) round(bag$cs_indep_test$expected, 2)
  })

  # chi-square test of independence missing values
  output$cs_indep_obs_NAs <- renderPrint({

    # don't show output or placeholder if no vars selected or if user changes var
    #  (until button pressed)
    if (bag$do_cs_indep_dataset != FALSE) {
      cat(stri_dup(intToUtf8(160), 4), bag$cs_indep_NA)
    }
  })



  ### Inferential - Chi-Square Test of Independence -
  ###  assuming user inputs observed counts manually
  ####################################################

  # make blank table
  observeEvent(input$set_cs_indep_manual_cats, {

    # require inputs for both textAreaInputs
    req(input$cs_indep_manual_1, input$cs_indep_manual_2)


    # convert inputs to character vector
    var_1_cats <- input$cs_indep_manual_1 %>%
      strsplit(split = "\n|,") %>%  # split if see comma or new line
      unlist() %>%
      str_trim()    # remove leading or trailing whitespace

    var_2_cats <- input$cs_indep_manual_2 %>%
      strsplit(split = "\n|,") %>%  # split if see comma or new line
      unlist() %>%
      str_trim()    # remove leading or trailing whitespace

    user_table <- matrix(as.integer(NA), nrow = length(var_1_cats),
      ncol = length(var_2_cats)
    )
    user_table <- as.data.frame(user_table, row.names = var_1_cats)
    names(user_table) <- var_2_cats


    # print table
    output$cs_indep_manual_table <- renderRHandsontable({
      rhandsontable(user_table)
    })
  })


  # run CS test of independence
  observeEvent(input$run_cs_indep_manual, {

    # convert rhandsontable object to R object -- need this
    user_cs_indep_table <- hot_to_r(input$cs_indep_manual_table)


    # warning popup alert if at least one cell has no input
    if (anyNA(user_cs_indep_table)) {
      shinyalert("Warning!", "Every cell in the table must contain a count.",
        type = "error"
      )
    }

    # require all cells to have a count; prevents app from crashing
    req(anyNA(user_cs_indep_table) == FALSE)


    # run cs test of independence
    bag$cs_indep_test <- chisq.test(user_cs_indep_table)

    bag$cs_indep_test_df <- data.frame(
      "." = formatC(c(
        bag$cs_indep_test$statistic,
        bag$cs_indep_test$parameter,
        bag$cs_indep_test$p.value
      )),
      row.names = c("Test Statistic  ", "df", "p-value")
    )


    # need to reset output if new radio button clicked
    bag$do_cs_indep_manual <- input$run_cs_indep_manual
  })


  # reset output if new radio button clicked
  observeEvent(input$cs_indep_data_format, {
    bag$do_cs_indep_manual <- FALSE
  })


  # chi-square test of independence output
  output$cs_indep_manual_test_out <- renderPrint({

    if (bag$do_cs_indep_manual != FALSE) {
      cat(capture.output(bag$cs_indep_test_df)[-1], sep = "\n")
    }
  })

  # chi-square test of independence expected counts
  output$cs_indep_manual_exp_counts <- renderPrint({

    if (bag$do_cs_indep_manual != FALSE) round(bag$cs_indep_test$expected, 2)
  })



  ### Inferential - Chi-Square Test of Homogeneity -
  ###  assuming variables from data set used
  ####################################################
  observeEvent(input$run_cs_homog_dataset, {

    req(input$cs_homog_select_var_1, input$cs_homog_select_var_2)


    # select variables
    selected_vars_all <- bag$data_new_df[,
      c(input$cs_homog_select_var_1, input$cs_homog_select_var_2)
    ]

    # remove NAs
    selected_vars <- na.omit(selected_vars_all)


    bag$table_obs <- table(selected_vars)

    bag$cs_homog_test <- chisq.test(bag$table_obs)

    bag$cs_homog_test_df <- data.frame(
      "." = formatC(c(bag$cs_homog_test$statistic, bag$cs_homog_test$parameter,
        bag$cs_homog_test$p.value)
      ),
      row.names = c("Test Statistic  ", "df", "p-value")
    )


    # count number of missing values
    bag$cs_homog_NA <- nrow(selected_vars_all) - nrow(selected_vars)


    # need this to reset output if new variable or radio button selected
    bag$do_cs_homog_dataset <- input$run_cs_homog_dataset
  })


  # reset output if new variable selected
  observeEvent(input$cs_homog_select_var_1, {
    bag$do_cs_homog_dataset <- FALSE
  })

  observeEvent(input$cs_homog_select_var_2, {
    bag$do_cs_homog_dataset <- FALSE
  })

  # reset output if new radio button clicked
  observeEvent(input$cs_homog_data_format, {
    bag$do_cs_homog_dataset <- FALSE
  })


  # chi-square test of homogeneity output
  output$cs_homog_dataset_test_out <- renderPrint({

    # don't show output or placeholder if no vars selected or if user changes var
    #  (until button pressed)
    if (bag$do_cs_homog_dataset != FALSE) {
      cat(capture.output(bag$cs_homog_test_df)[-1], sep = "\n")
    }
  })

  # chi-square test of homogeneity observed counts
  output$cs_homog_dataset_obs_counts <- renderPrint({

    # don't show output or placeholder if no vars selected or if user changes var
    #  (until button pressed)
    if (bag$do_cs_homog_dataset != FALSE) bag$table_obs
  })

  # chi-square test of homogeneity expected counts
  output$cs_homog_dataset_exp_counts <- renderPrint({

    # don't show output or placeholder if no vars selected or if user changes var
    #  (until button pressed)
    if (bag$do_cs_homog_dataset != FALSE) round(bag$cs_homog_test$expected, 2)
  })

  # chi-square test of homogeneity missing values
  output$cs_homog_obs_NAs <- renderPrint({

    # don't show output or placeholder if no vars selected or if user changes var
    #  (until button pressed)
    if (bag$do_cs_homog_dataset != FALSE) {
      cat(stri_dup(intToUtf8(160), 4), bag$cs_homog_NA)
    }
  })



  ### Inferential - Chi-Square Test of Homogeneity -
  ###  assuming user inputs observed counts
  ####################################################

  # make blank table
  observeEvent(input$set_cs_homog_manual_cats, {

    # require inputs for both textAreaInputs
    req(input$cs_homog_manual_1, input$cs_homog_manual_2)


    # convert inputs to character vector
    var_1_cats <- input$cs_homog_manual_1 %>%
      strsplit(split = "\n|,") %>%  # split if see comma or new line
      unlist() %>%
      str_trim()    # remove leading or trailing whitespace

    var_2_cats <- input$cs_homog_manual_2 %>%
      strsplit(split = "\n|,") %>%  # split if see comma or new line
      unlist() %>%
      str_trim()    # remove leading or trailing whitespace

    user_table <- matrix(as.integer(NA), nrow = length(var_1_cats),
      ncol = length(var_2_cats)
    )
    user_table <- as.data.frame(user_table, row.names = var_1_cats)
    names(user_table) <- var_2_cats


    # print table
    output$cs_homog_manual_table <- renderRHandsontable({
      rhandsontable(user_table)
    })
  })


  # run CS test of homogeneity
  observeEvent(input$run_cs_homog_manual, {

    # convert rhandsontable object to R object -- need this
    user_cs_homog_table <- hot_to_r(input$cs_homog_manual_table)


    # warning popup alert if at least one cell has no input
    if (anyNA(user_cs_homog_table)) {
      shinyalert("Warning!", "Every cell in the table must contain a count.",
        type = "error"
      )
    }

    # require all cells to have a count; prevents app from crashing
    req(anyNA(user_cs_homog_table) == FALSE)


    # run cs test of homogeneity
    bag$cs_homog_test <- chisq.test(user_cs_homog_table)

    bag$cs_homog_test_df <- data.frame(
      "." = formatC(c(bag$cs_homog_test$statistic, bag$cs_homog_test$parameter,
        bag$cs_homog_test$p.value)
      ),
      row.names = c("Test Statistic  ", "df", "p-value")
    )


    # need to reset output if new radio button clicked
    bag$do_cs_homog_manual <- input$run_cs_homog_manual
  })


  # reset output if new radio button clicked
  observeEvent(input$cs_homog_data_format, {
    bag$do_cs_homog_manual <- FALSE
  })


  # chi-square test of homogeneity output
  output$cs_homog_manual_test_out <- renderPrint({

    if (bag$do_cs_homog_manual != FALSE) {
      cat(capture.output(bag$cs_homog_test_df)[-1], sep = "\n")
    }
  })

  # chi-square test of homogeneity expected counts
  output$cs_homog_manual_exp_counts <- renderPrint({

    if (bag$do_cs_homog_manual != FALSE) round(bag$cs_homog_test$expected, 2)
  })



  ### Inferential - Simple Linear Regression
  ####################################################
  observeEvent(input$run_reg_inference, {

    req(input$reg_inference_select_resp_var, input$reg_inference_select_expl_var)


    # filter data for selected variables
    bag$y <- bag$data_new_df[, input$reg_inference_select_resp_var]
    bag$x <- bag$data_new_df[, input$reg_inference_select_expl_var]


    # warning popup alert if at least one selected var isn't numeric
    if (!is.numeric(bag$y) | !is.numeric(bag$x)) {
      shinyalert("Warning!", "At least one of the variables you selected is not
        quantitative. Please make sure both selected variables are quantitative.",
        type = "error"
      )
    }

    # keeps the app from crashing if at least one selected var isn't numeric
    validate(need(is.numeric(bag$y) & is.numeric(bag$x), ""))


    # fit regression line and calculate summaries
    bag$lm_out <-  suppressWarnings(lm(bag$y ~ bag$x))
    bag$reg_summary <-  suppressWarnings(summary(bag$lm_out))


    # make df w/ regression inference output
    bag$reg_inference <- data.frame(
      "col1" = round(unname(bag$reg_summary$coefficients[, 1]), 4),
      "col2" = round(unname(bag$reg_summary$coefficients[, 2]), 4),
      "col3" = round(unname(bag$reg_summary$coefficients[, 3]), 3),
      "col4" = formatC(unname(bag$reg_summary$coefficients[, 4]))
    )
    names(bag$reg_inference) <- c(" Estimate", "  Std. Error", "  t Stat",
      " p-value (2-sided)"
    )
    row.names(bag$reg_inference) <- c("Intercept", "Slope")


    # make df w/ regression summaries
    bag$reg_summaries <- data.frame(
      "." = formatC(
        c(cor(x = bag$x, y = bag$y, use = "pairwise.complete.obs"),
          bag$reg_summary$r.squared,
          bag$reg_summary$sigma,
          length(bag$reg_summary$residuals),
          length(bag$y) - length(bag$reg_summary$residuals)
        ), digits = 4
      ),
      row.names = c("r", "r-squared", "Resid. Std. Error  ",
        "# Non-missing Obs.", "# Missing Obs."
      )
    )


    # residuals and fitted df
    bag$resid_plot_df <- data.frame(
      resid = unname(bag$lm_out$residuals),
      fitted = unname(bag$lm_out$fitted.values)
    )


    # need this to reset output if new variable selected
    bag$do_reg_inf <- input$run_reg_inference
  })


  # reset output if new variable selected
  observeEvent(input$reg_inference_select_expl_var, {
    bag$do_reg_inf <- FALSE
  })

  observeEvent(input$reg_inference_select_resp_var, {
    bag$do_reg_inf <- FALSE
  })


  # regression inference output
  output$reg_inference_out <- renderPrint({

    # don't show output or placeholder if no vars selected or if user changes var
    #  (until button pressed)
    if (bag$do_reg_inf != FALSE) bag$reg_inference
  })

  # regression summaries output
  output$reg_inference_summaries <- renderPrint({

    # don't show output or placeholder if no vars selected or if user changes var
    #  (until button pressed)
    if (bag$do_reg_inf != FALSE) {
      cat(capture.output(bag$reg_summaries)[-1], sep = "\n")
    }
  })

  # scatterplot
  output$reg_inference_scatter <- renderPlot({

    # don't show output or placeholder if no vars selected or if user changes var
    #  (until button pressed)
    if (bag$do_reg_inf != FALSE) {
      suppressWarnings(print(
        ggplot(bag$data_new_df,
          aes_string(
            x = paste0("`", input$reg_inference_select_expl_var, "`"),
            y = paste0("`", input$reg_inference_select_resp_var, "`")
          )
        ) +
          geom_abline(
            intercept = bag$lm_out$coefficients[1],
            slope = bag$lm_out$coefficients[2],
            color = "blue",
            alpha = 0.75,
            size = 1.5
          ) +
          geom_point(size = 2)
      ))
    }
  })


  # residual plot
  output$resid_plot_reg <- renderPlot({

    # don't show output or placeholder if no vars selected or if user changes var
    #  (until button pressed)
    if (bag$do_reg_inf != FALSE) {
      suppressWarnings(print(
        ggplot(bag$resid_plot_df, aes(x = fitted, y = resid)) +
          geom_point(size = 2) +
          geom_abline(slope = 0) +
          labs(x = "Fitted Value", y = "Residual")
      ))
    }
  })

  # histogram of residuals
  output$hist_reg <- renderPlot({

    # don't show output or placeholder if no vars selected or if user changes var
    #  (until button pressed)
    if (bag$do_reg_inf != FALSE) {
      suppressWarnings(print(
        ggplot(bag$resid_plot_df, aes(x = resid)) +
          geom_histogram(bins = input$hist_reg_resids_bins) +
          labs(x = "Residual", y = "Count")
      ))
    }
  })

  # normal QQ plot of residuals
  output$norm_qq_reg <- renderPlot({

    # don't show output or placeholder if no vars selected or if user changes var
    #  (until button pressed)
    if (bag$do_reg_inf != FALSE) {
      suppressWarnings(print(
        ggplot(bag$resid_plot_df, aes(sample = resid)) +
          stat_qq() +
          stat_qq_line() +
          labs(x = "Theoretical Quantile", y = "Sample Quantile")
      ))
    }
  })




  # output$inf_screenshot <- downloadHandler(
  # 	filename = "screenshot.docx", #, # sep = ".", # switch(
  # 			#input$format, PDF = "pdf", HTML = "html", Word = "docx"
  # 		#))
  # 	#},
  #
  # 	content = function(file) {
  #
  # 	  markdown_file <- "app_output.Rmd"
  #
  # 	  src <- normalizePath(markdown_file)
  #
  # 		# temporarily switch to the temp dir, in case user doesn't have write
  # 		# permission to the current working directory
  # 		owd <- setwd(tempdir())
  # 		on.exit(setwd(owd))
  # 		file.copy(src, markdown_file, overwrite = TRUE)
  #
  # 		out <- render(markdown_file, word_document()) #switch(
  # 			#input$format,
  # 			#PDF = pdf_document(), HTML = html_document(), Word = word_document()
  # 		#))
  #
  # 		file.rename(out, file)
  # 	}
  # )



  ##############################################################################
  ############################## Download Data tab #############################
  ##############################################################################

  ### convert missing values from empty/blank
  ### cells to user-specified symbol (optional)
  #################################################
  observeEvent(input$convert_missing_out, {

    # convert NAs (num or chr vectors) or "" (chr vectors) to
    #  user-specified input
    bag$data_new_df[is.na(bag$data_new_df) | bag$data_new_df == ""] <-
      input$denote_missing_output


    # print message indicating successful conversion
    output$convert_success <- renderUI({
      HTML("<b><span style = 'color: forestgreen';>Missing values converted
        successfully</b>"
      )
    })
  })



  ### sort variables in final data set
  #################################################
  output$sort_vars_ui <- renderUI({

    if (input$select_vars_output) {
      sortable_vars <- input$final_vars
    } else {
      sortable_vars <- names(bag$data_new_df)
    }


    ranked_list <- rank_list(text = "", labels = sortable_vars,
      input_id = "final_vars_sorted"
    )

    ranked_list
  })





#   observeEvent(input$save_final, {
#
#     #shinyjs::show("download_dataset")
#
#
#     	if (input$select_vars_output & input$sort_vars_output) {
#
# 		    final_data <- bag$data_new_df[, input$final_vars_sorted]
#
# 		    # force data frame in case only one variable in final data set
#   		  final_df <- as.data.frame(final_data)
#
# 		    names(final_df) <- input$final_vars_sorted
#
# 		  } else if (input$select_vars_output & (input$sort_vars_output == FALSE)) {
#
# 		    final_data <- bag$data_new_df[, input$final_vars]
#
# 		    # force data frame in case only one variable in final data set
#   		  final_df <- as.data.frame(final_data)
#
# 		    names(final_df) <- input$final_vars
#
# 		  } else if ((input$select_vars_output == FALSE) & input$sort_vars_output) {
#
# 		    final_data <- bag$data_new_df[, input$final_vars_sorted]
#
# 		    # force data frame in case only one variable in final data set
#   		  final_df <- as.data.frame(final_data)
#
# 		    names(final_df) <- input$final_vars_sorted
#
# 		  } else {
#
# 		    final_df <- bag$data_new_df
# 		  }
#
#
# 		  # prevent issue where can't have period in var name for .mat, .sas7bdat,
# 		  #  and .dta file extensions -- remove period
#       if (input$file_type_out %in% c("mat", "sas7bdat", "dta")) {
#
#         # determine if any periods in var names
#         any_periods <- any(grepl(".", names(bag$data_new_df), fixed = TRUE))
#
#
#         # convert all periods to underscores
#         if (any_periods) names(final_df) <- gsub("[.]", "_", names(final_df))
#       }
#
#
# 		  # use rmatio package instead of rio when exporting .mat files
# 		  if (input$file_type_out == "mat") {
#
# 		    write.mat(final_df, paste0(input$file_name, ".", input$file_type_out), compression = FALSE)
#
# 		  } else {
#
# 		    rio::export(final_df, paste0(input$file_name, ".", input$file_type_out))
# 		  }
#
#
#   })




	### write new data set to file and download
  #################################################
	output$download_dataset <- downloadHandler(

		filename = function() {
		  if (input$file_name == "") {
		    paste0("new_dataset", ".", input$file_type_out)
		  } else {
		    paste0(input$file_name, ".", input$file_type_out)
		  }
		},

		content = function(file) {

		  if (input$select_vars_output & input$sort_vars_output) {

		    final_data <- bag$data_new_df[, input$final_vars_sorted]

		    # force data frame in case only one variable in final data set
  		  final_df <- as.data.frame(final_data)

		    names(final_df) <- input$final_vars_sorted

		  } else if (input$select_vars_output & (input$sort_vars_output == FALSE)) {

		    final_data <- bag$data_new_df[, input$final_vars]

		    # force data frame in case only one variable in final data set
  		  final_df <- as.data.frame(final_data)

		    names(final_df) <- input$final_vars

		  } else if ((input$select_vars_output == FALSE) & input$sort_vars_output) {

		    final_data <- bag$data_new_df[, input$final_vars_sorted]

		    # force data frame in case only one variable in final data set
  		  final_df <- as.data.frame(final_data)

		    names(final_df) <- input$final_vars_sorted

		  } else {

		    final_df <- bag$data_new_df
		  }


		  # prevent issue where can't have period in var name for .mat, .sas7bdat,
		  #  and .dta file extensions -- remove period
      if (input$file_type_out %in% c("mat", "sas7bdat", "dta")) {

        # determine if any periods in var names
        any_periods <- any(grepl(".", names(bag$data_new_df), fixed = TRUE))


        # convert all periods to underscores
        if (any_periods) names(final_df) <- gsub("[.]", "_", names(final_df))
      }



		  # use rmatio package instead of rio when exporting .mat files
		  if (input$file_type_out == "mat") {

		    write.mat(final_df, file, compression = FALSE)

		  } else {

		    rio::export(final_df, file)
		  }
		}
	)
})



