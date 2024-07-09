# Main #####

fastret_ui <- function(req = NULL) {
    shiny::tagList(
        tags$head(
            tags$style(HTML(
                "#shiny-notification-panel {
                    top: 0;
                    right: 0;
                    bottom: unset;
                    left: unset;
                    margin-left: auto;
                    margin-right: auto;
                    width: auto;
                    max-width: 450px;
                }
                code {
                    color: black;
                    background-color: #f5f5f5;
                    border: 1px solid #f7f7f7;
                }
                "
            ))
        ),
        navbarPage(
            title = "FastRet",
            ui_home(),
            ui_privacy_policy(),
            ui_contact(),
            ui_about(),
        ),
        # shinythemes::themeSelector(),
        shinyjs::useShinyjs()
    )
}

# Pages #####

ui_home <- function() {
    tabPanel(
        title = "Home",
        sidebarLayout(
            ui_sidebar(),
            ui_main()
        )
    )
}

ui_privacy_policy <- function() {
    tabPanel(
        title = "Privacy Policy",
        fluidPage(
            HTML(
                "<div class='mainpanel'>",
                "    <h3>Cookies</h3>",
                "    <div>This website does not use cookies.</div>",
                "    <h3> Server Log</h3>",
                "    <div>The web server keeps a log of all requests, with the following data:</div>",
                "    <ul>",
                "        <li>The request IP adress</li>",
                "        <li>Date and Time of the request</li>",
                "        <li>request type and path</li>",
                "        <li>the User-Agent of the web browser</li>",
                "    </ul>",
                "    <div>This data is only used to diagnose technical problems.</div>",
                "    <h3>Web Analytics / Other Tracking</h3>",
                "    <div>There are no other tracking methods.</div>",
                "    <h3>Privacy Contact</h3>",
                "    <a href='http://www.uni-regensburg.de/universitaet/datenschutzbeauftragte/index.html'>",
                "        Datenschutzbeauftrage der Universit&auml;t",
                "    </a>",
                "</div>"
            )
        )
    )
}

ui_contact <- function() {
    tabPanel(
        title = "Contact",
        fluidPage(
            HTML(
                "<div class='mainpanel'>",
                "    <h3>Contact</h3>",
                "    <div>",
                "        <address>",
                "            Dr. Katja Dettmer-Wilde<br/>",
                "            Institute of Functional Genomics<br/>",
                "            University of Regensburg<br/>",
                "            Am BioPark 9<br/>",
                "            93053 Regensburg, Germany<br/>",
                "            <abbr title='Phone'>P: </abbr>+49 941 943 5051<br/>",
                "            <abbr title='Email'>M: </abbr>katja.dettmer@klinik.uni-regensburg.de",
                "        </address>",
                "    </div>",
                "</div>",
            )
        )
    )
}

ui_about <- function() {
    tabPanel(
        title = "About",
        fluidPage(
            div(
                h3("Purpose"),
                p("FastRet is an R package for predicting retention times in liquid chromatography. It can be used through the R console or through a graphical user interface (GUI)."),
                p("The package's key features include the ability to:"),
                tags$ul(
                    tags$li("Train new predictive models specific for your own chromatography column"),
                    tags$li("Use pre-trained models to predict retention times of molecules"),
                    tags$li("Adjust pre-trained models to accommodate modifications in chromatography columns")
                ),
                p("For further details see FastRets ", a(href = "https://spang-lab.github.io/FastRet/", "documentation site"))
            ),
            div(
                h3("Version Info"),
                pre(paste(capture.output(sessionInfo()), collapse = "\n"))
            )
        )
    )
}

# Homepage Sidebar #####

ui_sidebar <- function() {
    sidebarPanel(
        ui_mode_selection(),
        ui_data_upload(),
        ui_model_upload(),
        ui_train_controls(),
        ui_sm_controls(),
        ui_predict_controls(),
        ui_adjust_controls()
    )
}

ui_mode_selection <- function() {
    with_helptext(
        selectInput(
            inputId = "siMode",
            label = "Mode",
            choices = c("Train new Model", "Predict Retention Times", "Selective Measuring", "Adjust existing Model"),
            selected = getOption("FastRet.UI.startMode", "Train new Model")
        ),
        content = paste(
            "<h2>Welcome to FastRet!</h1>",
            "<p>With this R shiny tool you can choose between four modes.</p>",
            "<ul>",
            "<li>Train new Model</li>",
            "<li>Selective Measuring </li>",
            "<li>Predict Retention Times</li>",
            "<li>Adjust existing Model</li>",
            "</ul>",
            "<p>Each mode is shortly described here. For more information about a specific input click the question mark symbol next to the input.</p>",
            "<h3>Train new Model</h2>",
            "<p>This is usually the first step you take, this mode allows you to create and evaluate a Model on your own new data. Model parameters are optimized automatically using cross validation. Afterwards, the regression model as well as the predictor set can be downloaded. This step outputs one scatterplot and one boxplot showing the model's general performance.</p>",
            "<h3>Selective Measuring</h2>",
            "<p>This mode calculates on a given dataset the best k molecules to be measured for a retention time prediction. It uses a combination of Ridge Regression and k-means to determine the best representatives of your dataset. Representatives as well as their corresponding clusters can be downloaded afterwards as an excel file. This step should be used once you have a predictive model ond/or data set and want to adjust it for a new chromatography column with a different gradient or temperature etc.</p>",
            "<h3>Predict Retention Times</h2>",
            "<p>This step requires you to upload a pretrained model. After the upload, you can use your model to predict retention times of new metabolites by providing either a single SMILE ID combination or a whole list of molecules.</p>",
            "<h3>Adjust existing Model</h2>",
            "<p>This mode allows you to adapt an existing model to a new experimental design. It requires re-measuring a subset of the molecules from the original dataset under the new experimental conditions. FastRet then constructs a linear model to adjust the predictions of the original model based on these new measurements.</p>"
        )
    )
}

ui_data_upload <- function() {
    conditionalPanel(
        condition = "input.siMode == 'Selective Measuring' || input.siMode == 'Train new Model'",
        with_helptext(
            fileInput(
                inputId = "ubInpXlsx",
                label = "Data as xlsx file",
                accept = ".xlsx"
            ),
            content = paste(
                "<h2>Training data upload</h1>",
                "<p>Here you can upload your own data as Excel file. In order for this to work your file must follow a strict format guideline. If any required columns are missing, the FastRet won&#39;t work correctly. FastRet will always load in the first worksheet of an Excel file. Therefore it is suggested that you reduce your file to one sheet beforehand to avoid any errors.</p>",
                "<h3>Required columns</h2>",
                "<p>The file must consist of the following columns (case sensitive):</p>",
                "<ul>",
                "<li>RT: Retention time of your molecules. Can be any numeric input, minutes or seconds. Remember what you put in when you analyze the predictions, since those will be on the same scale as your input data.</li>",
                "<li>NAME: you can put in any characters you like. Preferably the names of your molecules.</li>",
                "<li>SMILES: Isomeric or canonical SMILES. This information is used to calculate chemical descriptors with the chemistry development kit</li>",
                "</ul>"
            )
        ),
        div(textOutput("toInpXlsxError"), style = "color: red;")
    )
}

ui_model_upload <- function() {
    conditionalPanel(
        condition = "input.siMode == 'Predict Retention Times' || input.siMode == 'Adjust existing Model'",
        with_helptext(
            fileInput(
                inputId = "ubInpFRM",
                label = "Upload a pretrained Model",
                accept = c(".rds", ".RDS")
            ),
            content = paste(
                sep = "\n",
                "<h2>Model upload</h1>",
                "<p>",
                "Here you need to upload a prediction model generated with this program in the <em>Train new Model</em> mode.",
                "This Model can also be read in and used from within R by calling",
                "<pre style='white-space: pre-wrap;'>",
                "model <- readRDS('path/to/model.rds')",
                "coef(model$model)  # show model coeffcients (LASSO only)",
                "model$df      # show the predictor set",
                "</pre>",
                "For further details see the FastRet online documentation.",
                "</p>"
            )
        ),
        div(textOutput("toInpFRMError"), style = "color: red;")
    )
}

ui_train_controls <- function() {
    conditionalPanel(
        condition = "input.siMode == 'Train new Model'",
        htmltools::tags$label("Settings"),
        checkboxInput(
            inputId = "ciShowAdvSettings",
            label = "Show advanced settings",
            value = FALSE
        ),
        checkboxInput(
            inputId = "ciShowTrainLogs",
            label = "Show console logs",
            value = FALSE
        ),
        conditionalPanel(
            condition = "input.siMode == 'Train new Model' && input.ciShowAdvSettings == true",
            with_helptext(
                radioButtons(
                    inputId = "rbMethod",
                    label = "Method",
                    choices = list("XGBoost (recommended)" = 2, "Lasso" = 1),
                    selected = 2
                ),
                content = paste(
                    "<h2>Method Selection</h1>",
                    "<p>Here you can choose by which method the regression model should be trained on. You can choose between Lasso or XGBoost. </p>",
                    "<h3>Lasso</h2>",
                    "<p>Lasso (Least absolut shrinkage and selection operator) is based on the Least Minimum Square approach with the extension of a L1 penalty norm. This leads to a selection of variables as well as a generalization of the trained model.<br>Lasso was implemented with the R-package glmnet [2].</p>",
                    "<h3>XGBoost</h2>",
                    "<p>XGBoost is a more soffisticated Machine Learning method based on Boosted Regression Trees (BRT) [3]. The main difference to random forest is, that trees are not trained independant from each other but each tree is built with a loss function based on its predecessor. It was implemented with the R-package XGBoost [4].</p>",
                    "<h3>References</h2>",
                    "<p>",
                    "[1] Santosa, Fadil; Symes, William W. (1986). &quot;Linear inversion of band-limited reflection seismograms&quot;. <em>SIAM Journal on Scientific and Statistical Computing</em>. SIAM. <strong>7</strong> (4): 1307<e2><80><93>1330",
                    "[2] Jerome Friedman, Trevor Hastie, Robert Tibshirani (2010).",
                    "Regularization Paths for Generalized Linear Models via",
                    "Coordinate Descent. Journal of Statistical Software, 33(1),",
                    "1-22.",
                    "[3] Jerome H. Friedman. &quot;Greedy function approximation: A gradient boosting machine..&quot; Ann. Statist. 29 (5) 1189 - 1232, October 2001",
                    "[4] Tianqi Chen et. Al, (2021). xgboost: Extreme Gradient Boosting. R package",
                    "version 1.4.1.1.",
                    "</p>"
                )
            ),
            with_helptext(
                checkboxGroupInput(
                    inputId = "frm_opts",
                    label = "Preprocessing Options",
                    choices = list(
                        "Include 2nd degree polynomials" = 1,
                        "Include interaction terms" = 2,
                        "Remove near-zero-variance predictors" = 3,
                        "Remove predictors with missing values" = 4,
                        "Remove not suitable descriptors" = 5
                    ),
                    selected = c(3, 4),
                    inline = FALSE
                ),
                content = paste(
                    "<h2>Preprocessing Options</h2>",
                    "<p>Here you can choose which preprocessing steps should be applied to the data before training the model.</p>",
                    "<p>Available options are:</p>",
                    "<ul>",
                    "<li><code>Include 2nd degree polynomials</code>: Adds polynomial predictors up to the degree specified by the user.</li>",
                    "<li><code>Include interaction terms</code>: Includes interaction terms between predictors in the model.</li>",
                    "<li><code>Remove near-zero-variance predictors</code>: Removes predictors with variance close to zero to improve model performance.</li>",
                    "<li><code>Remove predictors with missing values</code>: Removes predictors with missing values to prevent errors during model training.</li>",
                    "<li><code>Remove not suitable descriptors</code>: Removes chemical descriptors that were considered unsuitable for linear regression based on an analysis of the HILIC dataset from the <a href='https://www.retip.app/'>Retip package</a>.</li>",
                    "</ul>",
                    "<p>Please note that setting <code>rm_near_zero_var</code> and/or <code>rm_na</code> to TRUE can cause the cross-validation results to be overly optimistic. This is because the predictor filtering is done on the entire dataset, which means information from the validation folds is used for feature selection.</p>"
                )
            ),
            with_helptext(
                numericInput(
                    inputId = "seed",
                    label = "Seed",
                    value = as.integer(Sys.time())
                ),
                content = paste(
                    "<h2>Seed</h2>",
                    "<p>You can set the seed for the random number generator here. This is useful for reproducing model training results. By default, <code>seed</code> is set to the number of seconds since the <em>Unix epoch</em> (1970-01-01 00:00:00 GMT) when the web app is opened.</p>"
                )
            ),
        ),
        bslib::input_task_button(
            id = "btnTrain",
            label = "Train Model",
            type = "default",
            icon = icon("play")
        ),
        downloadButton(
            outputId = "dbSaveModel",
            label = "Save Model",
            style = "display: none;"
        ),
        downloadButton(
            outputId = "dbSavePredictorSet",
            label = "Save Predictor Set",
            style = "display: none;"
        ),
        conditionalPanel(
            condition = "input.ciShowTrainLogs == true",
            consoleOutput(
                divID = "divTrainLogs",
                vtoID = "vtoTrainLogs"
            )
        )
    )
}

ui_sm_controls <- function() {
    conditionalPanel(
        condition = "input.siMode == 'Selective Measuring'",
        with_helptext(
            numericInput(
                inputId = "niK", # niK for "numeric input K"
                label = "k Cluster",
                value = 25
            ),
            content = paste(
                "<h2>Cluster Calculation</h1>",
                "<p>Here you can choose how many clusters should be calculated. The programm will calculate the best k molecules to be measured for a retention time prediction. It uses a combination of Ridge Regression and k-means to determine the best representatives of your dataset. Representatives as well as their corresponding clusters can be downloaded afterwards as an excel file. This step should be used once you have a predictive model ond/or data set and want to adjust it for a new chromatography column with different gradient/temperature etc.</p>"
            )
        ),
        bslib::input_task_button(
            id = "btnSM",
            label = "Calculate clusters and medoids",
            type = "default"
        ),
        downloadButton(
            outputId = "dbSaveCluster",
            label = "Download clustering as xlsx",
            style = "display: none;"
        )
    )
}

ui_predict_controls <- function() {
    conditionalPanel(
        condition = "input.siMode == 'Predict Retention Times'",
        with_helptext(
            textInput(
                inputId = "tiPredSmiles",
                label = "Input SMILES",
                value = if ("input_smiles" %in% .Options$FastRet.mocks) "C(CC(=O)O)CN" else ""
            ),
            content = paste(
                "<h2 id='single-prediction'>Single Prediction</h1>",
                "<p>Here you can input a single SMILES string to predict the retention time of a molecule. The SMILES string has to be in the same format as the SMILES strings in the training data set. The prediction will be done with the model you uploaded.</p>"
            )
        ),
        div(textOutput("toPredSmilesError"), style = "color: red;"),
        with_helptext(
            fileInput(
                inputId = "ubPredXlsx",
                label = "Upload SMILES as xlsx",
                accept = ".xlsx"
            ),
            content = paste(
                "<h2 id='prediction-data-upload'>Prediction Data Upload</h1>",
                "<p>This file input has to be an excel file with columns NAME and SMILES</p>"
            )
        ),
        div(textOutput("toPredXlsxError"), style = "color: red;"),
        checkboxInput(
            inputId = "ciShowPredLogs",
            label = "Show console logs",
            value = FALSE
        ),
        bslib::input_task_button(
            id = "btnPred",
            label = "Predict",
            type = "default"
        ),
        downloadButton(
            outputId = "dbSavePred",
            label = "Save predictions",
            style = "display: none;"
        ),
        conditionalPanel(
            condition = "input.ciShowPredLogs == true",
            consoleOutput(
                divID = "divPredLogs",
                vtoID = "vtoPredLogs"
            )
        )
    )
}

ui_adjust_controls <- function() {
    conditionalPanel(
        condition = "input.siMode == 'Adjust existing Model'",
        with_helptext(
            fileInput(
                inputId = "ubAdjXlsx",
                label = "Data for prediction adustment as xlsx file",
                accept = ".xlsx"
            ),
            content = paste(
                "<h2>Adjustment data</h1>",
                "<p>This file input has to be an excel file with columns RT, NAME and SMILES</p>"
            )
        ),
        div(textOutput("toAdjXlsxError"), style = "color: red;"),
        with_helptext(
            checkboxGroupInput(
                inputId = "ciPredictors",
                label = "Components of linear model",
                choices = list("RT" = 1, "RT^2" = 2, "RT^3" = 3, "log(RT)" = 4, "exp(RT)" = 5, "sqrt(RT)" = 6),
                selected = 1,
                inline = TRUE
            ),
            content = paste(
                "<h2>Components of linear model</h1>",
                "<p>Model adjustment is achieved by training a linear model that predicts the Retention Times of a new column based on the Retention Times from an existing column. The model should always use the retention time (RT) as a predictor. However, it might also make sense to include additional predictors such as RT^2, RT^3, log(RT), exp(RT), and sqrt(RT). Use the checkboxes in this section to specify the predictors you wish to include in the model.</p>"
            )
        ),
        checkboxInput(
            inputId = "ciShowAdjLogs",
            label = "Show console logs",
            value = FALSE
        ),
        bslib::input_task_button(
            id = "btnAdj",
            label = "Adjust Model",
            type = "default"
        ),
        downloadButton(
            outputId = "dbSaveAdjModel",
            label = "Save adjusted model",
            style = "display: none;"
        ),
        conditionalPanel(
            condition = "input.ciShowAdjLogs == true",
            consoleOutput(
                divID = "divAdjLogs",
                vtoID = "vtoAdjLogs"
            )
        )
    )
}

# Homepage Main Area #####

ui_main <- function() {
    mainPanel(
        # shinybusy::add_busy_spinner(spin = "fading-circle"),
        uiOutput("ui_train_results"),
        uiOutput("ui_sm_results"),
        uiOutput("ui_predict_results"),
        uiOutput("ui_adjust_results")
    )
}

ui_train_results <- function(SE) {
    req(SE$RV$tblTrainResults, SE$input$siMode == "Train new Model")
    htmltools::div(
        id = "ui_train_results",
        shiny::tabsetPanel(
            shiny::tabPanel(
                title = "CV Performance",
                shiny::plotOutput("poTrainPerfCV")
            ),
            shiny::tabPanel(
                title = "Training performance",
                shiny::plotOutput("poTrainPerf")
            )
        ),
        DT::DTOutput("tblTrainResults")
    )
}

ui_sm_results <- function(SE) {
    req(SE$RV$cluster_calc, SE$input$siMode == "Selective Measuring")
    htmltools::div(
        id = "ui_sm_results",
        htmltools::h3("Medoids"),
        DT::DTOutput("tblMedoids"),
        htmltools::h3("Full clustering"),
        DT::DTOutput("tblClustering")
    )
}

ui_predict_results <- function(SE) {
    shiny::req(SE$input$siMode == "Predict Retention Times")
    catf("Showing prediction results")
    htmltools::div(
        id = "ui_predict_results",
        DT::DTOutput("tblPredResults")
    )
}

ui_adjust_results <- function(SE) {
    shiny::req(SE$input$siMode == "Adjust existing Model")
    htmltools::div(
        id = "ui_adjust_results",
        shiny::plotOutput("poAdjPerfCV"),
        shiny::plotOutput("poAdjPerf")
    )
}

# Helpers #####

with_helptext <- function(..., content) {
    shinyhelper::helper(
        ...,
        icon = "question-circle",
        colour = "#696969",
        type = "inline",
        content = content
    )
}

consoleOutput <- function(divID, vtoID) {
    vto <- verbatimTextOutput(outputId = vtoID)
    vto$attribs$style <- "
        display: block;
        line-height: 1.5em;
        height: 15em;
        overflow-y: auto;
        border: 1px solid #e3e3e3;
        resize: vertical;
        word-wrap: break-word;
        overflow-wrap: break-word;
        word-break: break-all;
        white-space: pre-wrap;
    "
    # vto$attribs$style <- paste0(vto$attribs$style, "background-color: #eeeeee;")
    div(
        id = divID,
        style = "
            margin-top: 15px;
            margin-bottom: 15px;
        ",
        # position: fixed;
        # bottom: 0;
        # width: 100%;
        htmltools::tags$label("Console Log", class = "control-label"),
        vto
    )
}
