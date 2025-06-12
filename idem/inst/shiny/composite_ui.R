##    -----------------------------------------------------------------------
##    Copyright (C) 2015  Daniel O. Scharfstein and Chenguang Wang
##    This program is free software: you can redistribute it and/or modify
##    it under the terms of the GNU General Public License as published by
##    the Free Software Foundation, either version 3 of the License, or
##    (at your option) any later version.

##    This program is distributed in the hope that it will be useful,
##    but WITHOUT ANY WARRANTY; without even the implied warranty of
##    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##    GNU General Public License for more details.

##    You should have received a copy of the GNU General Public License
##    along with this program.  If not, see <http://www.gnu.org/licenses/>.

##-------------------------------------------------------------
##           UI FUNCTIONS
##-------------------------------------------------------------

##show different type of messages
msg.box <- function(contents, type="info") {
    switch(type,
           info    = cls <- "cinfo",
           warning = cls <- "cwarning",
           success = cls <- "csuccess",
           error   = cls <- "cerror");
    rst <- '<div class="';
    rst <- paste(rst, cls, '">');
    rst <- paste(rst, contents);
    rst <- paste(rst, "</div>");
    HTML(rst);
}

##generate radio buttons for column variables
gen.radiobtn <- function(vnames) {
    rst <- list(HTML("<div class='row'>
                      <span class='smdl'></span>
                      <span class='smdl'>Treatment</span>
                      <span class='smdl'>Time to death</span>
                      <span class='smdl'>Outcome</span>
                      <span class='smdl'>Baseline outcome</span>
                      <span class='smdl'>Baseline covariates</span>
                      <span class='smdl'>Ignore</span>
                      </div>"));


    for (i in 1:length(vnames)) {
        ##remove space
        vname <- gsub("\\s","", vnames[i]);
        sel.v <- switch(vname,
                        TRT  = "trt",
                        SURV = "surv",
                        Y0   = "y0",
                        Y1   = "outcome",
                        Y2   = "outcome",
                        "cov");

        bname <- paste("inRdo", i, sep="");
        rst[[i+1]] <- fluidRow(
            column(1, h5(vname), align="center"),
            column(11,
                   radioButtons(bname, "",
                                c(" " = "trt",
                                  " " = "surv",
                                  " " = "outcome",
                                  " " = "y0",
                                  " " = "cov",
                                  " " = "ignore"),
                                selected=sel.v,
                                inline=TRUE)
                   )

            );
    }
    rst
}


##tabset for user log in
tab.login <- function(){
    div(class="well",
        textInput("inUname", "User Name", value="GUEST"),
        textInput("inPwd", "Password", value="COMPMISS"),
        br(),
        actionButton("btnLogin", "Login", styleclass="info"),
        style='width:200px; margin:40px auto; min-height:200px; border-width:0')
}


##tabset for data uploading
tab.upload <- function(){
    tabPanel("Upload Data",
             fluidPage(
               msg.box('<p>You will upload your dataset on this page.  Your dataset has to be
                          correctly formatted to ensure your analysis is completed correctly.  
                          Click the "* data upload instruction" in the text box below for explicit
                          instructions on how to format your data which included
                          proper formatting of variables and file formats for upload.</p> 
                          <p> We have provided an example dataset that you may download and use to
                          conduct an analysis. 
                          To download, right click and save as, this link: <a href="example.txt" download>
                          example file </a>. </p>
                          <p>Note that the default settings on the "Upload", "Model Specification"
                          and "Imputation" tabs are set such that the example analysis 
                          can be performed without changing any input parameters. For shorter
                          computation time, one may wish to decrease "Iterations" and 
                         "Thinning" under the "Imputation" table.</p>'),
                 ## msg.box('Please upload data file on this page. For an example of how to correctly specify an uploaded file, please see the previous tab. Right click, save as, to download an <a href="example.txt" download> example file </a>. Please see the previous tab for an example of how to perform a full data analysis using the example file. Note that the default settings on the "Upload", "Model Specification" and "Imputation" tabs are set such that the example analysis can be performed without changing any input parameters. For shorter computation time, one may wish to decrease "Iterations" and "Thinning" under the "Imputation" table.'),
                 wellPanel(h4("Upload data"),
                           fluidRow(
                               column(3, h6("Choose File"),
                                      fileInput(inputId = 'userdata', label = '',
                                                accept=c('text/csv','text/comma-separated-values,text/plain'))),
                               column(2, h6("Separator"),
                                      radioButtons('sep', '',
                                                   c(Comma=',',Semicolon=';',Tab='\t',Space=' '),
                                                   '\t')),
                               column(2, h6("Quote"),
                                      radioButtons('quote', '',
                                                   c(None='','Double Quote'='"','Single Quote'="'"),
                                                   selected = '')),
                               column(2, h6("NA string"),
                                      radioButtons('nastrings', '', c('.'='.','NA'="NA"), selected = 'NA')),
                               column(2, h6("Other"),
                                      checkboxInput(inputId='header', label='Header', value=TRUE),
                                      checkboxInput(inputId="displaydata", label = "Show Data", value = TRUE)
                                      )
                           ),
                           includeHTML("www/composite_upload.html")),
                 wellPanel(
                     h4("Try An Example"),
                     actionButton("btnExample", "Try it")
                 ),
                 conditionalPanel(condition="input.displaydata == true",
                                  wellPanel(h4("Review Data"),
                                            DT::dataTableOutput("uiData")))
                 )
             )
}

##specify model covariates
panel.model <- function(){
    list(
         msg.box("Here we will define all the relevant variables
                  for the analysis, specify the functional endpoint,
                  provide ranking rules and set imputation boundaries for
                  the functional outcome. After completing these three sections,
                  please select the 'Validate Model' button at the bottom of this page.
                  Note that the ranking rules section default settings correspond
                  to standard ranking assumptions."),
        wellPanel(id="panelmdl",
                  h4("Define Variables"),
                  uiOutput("uiModel")
                  ),
        wellPanel(fluidRow(
            column(3,
                   h4("Functional Endpoint"),
                   textInput("inTxtEnd","",value="Y2"),
                   msg.box("Please specify the analysis endpoint;
                            this may be the functional outcome measured at a single
                            time (e.g. 12-month outcome: Y12) or a function of the
                            functional outcome measured over time (e.g. change in
                            the outcome comparing 12-months to baseline: Y12-Y0)")
                   ),
            column(3, h4("Study Duration"),
                   numericInput("inDuration","",value="365", min=0),
                   msg.box("Please specify the cut off of the study.
                            Patients with survival time longer than study duration are
                            considered surviors. The study duration must be the same as the unit of
                            measurement for survival (e.g. if survival is measured in weeks,
                            and the study concluded at 1 year post-randomization,
                            the duration would be 52)")),
            column(3,
                   h4("Time Unit of Survival/Duration"),
                   selectInput("inSelUnitTime",
                               "",
                               choices=c("", "Years", "Months", "Weeks", "Days"),
                               selected="Days"))
        )),

        wellPanel(fluidRow(
            column(3,
                   h4("Boundary"),
                   msg.box("Please sepcify the lower and upper bound of the functional outcomes for
                            data transformation. Set to avoid out of boundary imputations.
                            Any imputed endpoints that exceed the range specfied here will be truncated."),
                   h5("Lower boundary for imputed functional outcomes"),
                   numericInput("inBound0","",value="0"),
                   h5("Upper boundary for imputed functional outcomes"),
                   numericInput("inBound1","",value="100")
                   ),
            column(8,
                   h4("Ranking Rules"),
                   msg.box("For each subject, the subject experiences death and we observe time to death,
                            L, or the subject survives and we observe the functional outcome of interest,
                            Z. NOTE: Z may be a specific value of the functional outcome at a
                            specified follow-up time.<br>

                            Let T(A) and T(B) be the time to death that we may observe for subjects
                            A and B, respectively. Let Z(A) and Z(B) be the functional outcome that
                            we may observe for subjects A and B, respectively.<br>

                            There are three potential scenarios for the ranking of subjects A and B."),

                   h5("Scenario I: Both subjects are alive at the end of the study.
                       Rank A better than B only if Z(A) - Z(B) bigger than "),
                   numericInput("inZdiff",
                                "",
                                value="0",
                                width="50%",
                                min=0),
                   h5("NOTE: the default value is 0. If there is a minimally
                      clinically important difference (MCID) established for
                      the functional outcome, then values of Z(A) and Z(B)
                      within the MCID may be considered the same"),
                   br(),
                   h5("Scenario II: Both subjects experience death prior to the end of the study.
                       Rank A better than B only if L(A) - L(B) bigger than"),
                   numericInput("inSdiff",
                                "",
                                value="0",
                                width="50%",
                                min=0),
                   h5("NOTE: the default value is 0. If experiencing mortality within
                       1 unit of time (for instance, 1 month) would be considered
                       an equally poor outcome for patients, then specify 1 unit of time."),
                   br(),
                   h5("Scenario III: Only subject A is alive at
                       the end of the study. Rank A better than B"),
                   h5("NOTE: We assume survival to the end of the study is a more
                       desireable outcome than experiencing death at any point prior
                       to the end of the study")
                 ))),
        wellPanel(msg.box("Please validate the configuration before proceeding to the next step."),
                  actionButton("btnValid", "Validate Model", styleclass="info"),
                  htmlOutput("uiValid")),
        uiOutput("uiLabel")
    );
}


##visulize data
panel.visual <- function(){
    navlistPanel(
        tabPanel("Missing Table",
                 msg.box("The table below summarizes mortality and missing values of the functional outcome among survivors, separately by treatment group. <br><br>

                 For the multiple imputation of the missing functional outcome among survivors, we define strata of survivors based on the missing data pattern observed. For instance, we define strata S1 to be the subset of survivors who have complete follow-up (i.e. no missing values), strata S2 is the subset of survivors who have the outcome measured at the first follow-up but who have missing values for the remainder of the study, etc. The final strata of subjects are those who have baseline variables, but whom are missing all of the follow-up outcomes."),
                 tableOutput("outTblMissing"),
                 style="height:1000px"),
        tabPanel("Missing Heatmap",
                 msg.box("Blue: Observed outcomes; Gray: Unobserved outcomes."),
                 plotOutput("outPlotMissing"),
                 align="center",
                 style="height:1000px"),
        tabPanel("Survival",
        		 h6("**NOTE: the p-value results from a log-rank test on  a difference in survival between treatment groups. This test does not adjust for other covariates."),
                 plotOutput("outPlotSurv"),
                 align="center",
                 style="height:1000px"),
        tabPanel("Survivors",
        		 h6("**NOTE: The red line/points indicate the mean value of the outcome for each treatment group. Purple lines/points indicate subjects are missing at least one functional outcome measurement."),
                 plotOutput("outPlotComp"),
                 align="center",
                 style="height:1000px"),
        widths=c(2,10)
        );
}


##visulize data
panel.fitting <- reactive({
    data.all <- get.data();
    if (is.null(data.all))
        return(NULL);

    lst.var <- data.all$lst.var;
    a.trt   <- get.trt();

    ## need to change if more than two treatment groups or the convention of TRT=0/TRT=1 changes
    ## trt.names <- c('Control','Intervention')
    trt.names <- get.data()$lst.var$trt.label;

    rst <- list(widths=c(2,10));
    for (i in 1:length(a.trt)) {
        rst[[length(rst)+1]] <- trt.names[i]
        for (j in 1:length(lst.var$outcome)) {
            lbl <- paste(lst.var$outcome[j],"|",sep="");
            if (j > 1) {
                lbl <- paste(lbl,
                             paste(lst.var$outcome[(j-1):1], collapse=","));
                lbl <- paste(lbl, ",", sep="");
            }
            lbl <- paste(lbl, "X", sep="");
            rst[[length(rst)+1]] <- lbl;
            rst[[length(rst)+1]] <- tabPanel("Fitting Results",
                                             verbatimTextOutput(paste("uifit",i,j,sep=""))
                                             );
            rst[[length(rst)+1]] <- tabPanel("Diagnosis",
                                             plotOutput(paste("uidiag",i,j,sep="")),
                                             align="center"
                                             );
        }
    }

    ##return
    rst <- do.call(navlistPanel, rst);
    list(
	    	msg.box('The multiple imputation of missing functional outcomes among
                 survivors requires us to fit a series of regression models.
                 We fit a sequence of regression models for the functional outcome
                 at the current time conditioning on the prior value of the functional
                 outcome and baseline covariates, separately for each treatment group.
                 The models are fit using data from surviving subjects that have no missing
                 functional outcomes. As an example, suppose you have a functional outcome
                 measured at 6 and 12-month follow-up. The imputation procedure requires
                 two regression models for each treatment group; a model for the 6-month
                 outcome conditioning on baseline covariates and a model for the 12-month
                 outcome conditioning on the 6-month outcome and baseline covariates.<br><br>
                 For each fitted model, we summarize the model and present basic model
                 diagnostics including a residuals vs. fitted plot and a normal quantile plot
                 to assess the distribution of residuals.'),
        rst
  )
})

##imputation panel
panel.config <- reactive({
    list(
        msg.box("On this page, you will specify inputs for the imputation and estimation procedures."),
        wellPanel(h4("General Imputation Settings"),
                  fluidRow(
                      column(3,
                             h6("Number of imputed datasets"),
                             sliderInput("inNImp", label = "", value=10, min=1, max=20, step=1),
                             h6("Normality assumption"),
                             radioButtons(inputId="inNorm", label="", c("Yes"=1, "No"=0))
                             ),
                      column(3,
                             h6("Number of bootstrap samples"),
                             sliderInput(inputId = "inNbs", label = "",
                                         value = 100, min = 5, max = 1000, step=1),
                             h6("Random seed"),
                             numericInput(inputId="inSeed", label="",
                                          value=10000, min=0)),
                      column(3,
                             h6("Number of Cores (Parallel Bootstrap)"),
                             sliderInput("inNcores", label = "", value = 1, min = 1,
                                         max = (parallel::detectCores()-1), step = 1))
                  )),
        wellPanel(
            h4("MCMC Paramters"),
            msg.box("Specify parameters for Bayesian posterior sampling. The target
                     acceptance rate and initial step-size are options for advanced
                     users to control STAN sampler's behavior. "),
            fluidRow(
                column(3,
                       h6("Number of iterations"),
                       sliderInput(inputId = "mcmciter", label = "",
                                   value = 1000, min = 200, max = 20000, step=100),
                       h6("Number of burn-in"),
                       sliderInput("mcmcburnin", label = "", value=500, min=100, max=20000, step=100)
                       ),
                column(3,
                       h6("Number of thinning"),
                       sliderInput(inputId = "mcmcthin", label = "", value=2, min=1, max=50, step=1),
                       h6("Number of chains"),
                       sliderInput("mcmcchain", label = "", value=4, min=2, max=10, step=1)
                       ),
                column(3,
                       h6("Target Acceptance Rate"),
                       sliderInput("mcmcdelta", label = "", value=0.95, min=0.05, max=1, step=0.05),
                       h6("Initial Step-size"),
                       sliderInput("mcmcstepsize", label = "", value=1, min=0.05, max=5, step=0.05)
                       ##h6("Algorithm"),
                       ##radioButtons('mcmcalg', '', c('NUTS', 'HMC', "Fixed_param"))
                       ))),
        wellPanel(h4("Sensitivity Parameters and Additional Quantile Output"),
                  fluidRow(
                      column(4,
                             h6("Imputation Sensitivity Parameters"),
                             textInput("inSensp", label="", value = get.sd.endp(outVar='ImpSens')),
                             msg.box("Imputation sensitivity parameters (separate by comma).
                                  Default values set such that the range of the sensitivity
                                  parameters is equal to one-fourth standard deviation of
                                  the distriubtion of functional endpoints among subjects
                                  who do not require their data to be imputed.")),
                      column(4,
                             h6("Quantiles of the Composite Endpoint"),
                             textInput("inExtras", label="", value=c("25,75")),
                             msg.box("The median of the composite endpoint for each treatment
                                  will be computed. Below enter additional percentiles of
                                  the composite variable you would like to obtain.")),
                      column(4,
                             h6("SACE Sensitivity Parameters"),
                             textInput("inSACE", label="", value=get.sd.endp(outVar='SACE')),
                             msg.box('SACE Sensitivity Parameter (separate by comma,
                                 values <= 0 are valid). Default values set such that
                                 the range of the sensitivity parameter ranges from 0 to
                                 minus one-half standard deviation of the distribution of
                                 functional endpoints among subjects who do not require
                                 their data to be imputed.'))
                  )),
        wellPanel(
            h4("Convergence Checking"),
            msg.box("Randomly select a subject and check the MCMC sampling convergence"),
            actionButton("btnConverge", "Check Convergence", style="info"),
            uiOutput('uiImpConv')
        )
    );
})


##panel of mcmc traceplots
panel.trace <- reactive({
    mcmc.rst <- get.imputed.mcmc();

    if (is.null(mcmc.rst))
        return(NULL);

    ## rst <- list(widths=c(2,10));
    ## for (i in 1:length(mcmc.rst$mcmc)) {
    ##     rst[[length(rst)+1]] <- tabPanel(paste("subject", i, sep=" "),
    ##                                      h4("Observed data"),
    ##                                      tableOutput(paste("uisub",i,sep="")),
    ##                                      h4("Trace plot"),
    ##                                      plotOutput(paste("uimcmc",i,sep="")),
    ##                                      align="center");
    ## }

    ## ##return
    ## div(do.call(navlistPanel, rst), style="margin-top:20px");

    div(h6("Observed data"),
        tableOutput(paste("tabSub",sep="")),
        h6("Trace plot"),
        plotOutput(paste("mcmcSub",sep="")))
})

##imputation panel
panel.impute <- reactive({
    list(
        wellPanel(
            actionButton("btnImpute", "Benchmark Assumption Imputation", style="info", width='280px'),
            uiOutput('uiImpRst')
        ),
        wellPanel(
            actionButton("btnBoot", "Hypothesis Testing by Bootstrap", style="info", width='280px'),
            uiOutput('uiBootrst')
        ))
})

##panel of imputation summary
panel.impute.summary <- reactive({
    div(navlistPanel(widths=c(3,9),
                     tabPanel('Imputed Data',
                              tabsetPanel(
                                  tabPanel("Imputed Dataset",
                                           dataTableOutput("impData")),
                                  tabPanel("Imputed Outcome",
                                        #h6("Density of imputed individual outcomes"),
                                           msg.box("Below are plots of the distribution of imputed functional
                                                  outcomes under the specified imputation sensitiy parameters.
                                                  The top and bottom rows correspond to the imputed outcomes for
                                                  the Control and Treatment arms respectively. Within each row,
                                                  from left to right, the imputed functional outcomes
                                                  are ordered chronologically."),
                                           plotOutput("outPImpY"),
                                           style="height:1000px"),
                                  tabPanel("Imputed Endpoint",
                                        #h6("Density of imputed endpoint"),
                                           msg.box("Below are plots of the distribution of imputed
                                                    functional endpoints under the specified imputation sensitiy
                                                    parameters. Note that if the endpoint is simply one of the
                                                    functional outcomes (e.g. Y12) then these plots are
                                                    identical the corresponding plot in the previous tab."),
                                           plotOutput("outPImpE"))
                              )),
                     tabPanel('Analysis Results',
                              tableOutput("outTblImpRank"),
                              br(),
                              tableOutput("outTblImpMedian"),
                              br(),
                              h5("Cumulative Distribution of the Composite Variable Under
                                  Benchmark Imputaiton Assumptions"),
                              plotOutput("outPlotImpComp"),
                              align = 'center'),
                     tabPanel("Download",
                              h6("Download the imputed full dataset"),
                              downloadButton('btnImpDload'))),
        style="margin-top:20px")
})


##panel of bootstrap results
panel.boot <- reactive({
    div(navlistPanel(widths=c(3,9),
                     tabPanel("Analysis results",
                              tabsetPanel(
                                  tabPanel("Ranks",
                                           tableOutput("outTblBootTheta"),
                                           align = 'center'),
                                  tabPanel("Quantiles",
                                           tableOutput("outTblBootQuantiles"),
                                           align = 'center'),
                                  tabPanel("Contour Plot",
                                           plotOutput("outBootContourRank"),
                                           align = 'center')
                              )),
                     tabPanel("Survivor only analysis results",
                              msg.box("PLEASE BE CAUTIOUS that survivors only analysis is only valid
                                       when the treatment has no impact on survival.", "warning"),
                              tabsetPanel(
                                  tabPanel("Treatment Effect",
                                           tableOutput("outTblBootThetaSOnly"),
                                           align = 'center'),
                                  tabPanel("Contour Plot",
                                           plotOutput("outBootContourSonly"),
                                           align = 'center')
                              )),
                     tabPanel("SACE analysis results",
                              tabsetPanel(
                                  tabPanel("Treatment Effect",
                                           tableOutput("outTblBootThetaSACE"),
                                           align = 'center'),
                                  tabPanel("Contour Plot",
                                           plotOutput("outBootContourSACE"),
                                           align = 'center')
                              ))),
        style = "margin-top:20px;  min-height: 600px;")
})

##define the main tabset for beans
tab.main <- function() {
    tabsetPanel(type = "pills",
                id="mainpanel",
                selected="Upload Data",
                tabPanel("About", includeHTML("www/composite_about.html")),
                tab.upload(),
                tabPanel("Model Specification", uiOutput("uiPanelModel")),
                tabPanel("Data Exploration", uiOutput("uiVisual")),
                tabPanel("Model Fitting", uiOutput("uiFitting")),
                tabPanel("Configuration", uiOutput("uiConfig")),
                tabPanel("Imputation", uiOutput("uiImpute")),
                tabPanel("Report", uiOutput("uiReport"))
                );
}

##-------------------------------------------------------------
##           SERVER FUNCTIONS
##-------------------------------------------------------------
observe ({
    inFile <- input$userdata;
    if (!is.null(inFile)){
        userLog$model <- -1;
        userLog$data  <- read.csv(inFile$datapath,
                                  header=input$header,
                                  sep=input$sep,
                                  quote=input$quote,
                                  na.strings=input$nastrings);
    }
})

observe ({
    if (is.null(input$btnExample))
        return(NULL);

    if (0 == input$btnExample)
        return(NULL);

    userLog$data <- idem::abc;
})


##get the list of variables in model specification
get.variables <- reactive({

    if (is.null(input$btnValid))
        return(NULL);

    if (is.null(input$inRdo1))
        return(NULL);

    isolate({
        cur.data <- userLog$data;
        if (is.null(cur.data))
            return(NULL);

        vnames <- names(cur.data);

        ##variables
        rst  <- list(endfml=input$inTxtEnd,
                     duration=input$inDuration,
                     bounds=c(input$inBound0, input$inBound1),
                     mindiff=c(input$inZdiff, input$inSdiff),
                     trt=NULL, surv=NULL, outcome=NULL,
                     y0=NULL, cov=NULL, endp=NULL,
                     unitTime=input$inSelUnitTime);

        rst$parsed.endfml <- parse(text=paste("with(d.frame, {",rst$endfml,"})"));

        for (i in 1:ncol(cur.data)) {
            cur.t <- input[[paste("inRdo", i, sep="")]];
            rst[[cur.t]] <- c(rst[[cur.t]], vnames[i]);
        }

        ##sort outcome
        if (!is.null(rst$outcome)) {
            rst$outcome <- rst$outcome[order(as.numeric(substr(rst$outcome,2,nchar(rst$outcome))))]
        }

        ##outcome in endpoint
        if (!is.null(rst$endfml)) {
            tmp <- which(sapply(rst$outcome, grepl, input$inTxtEnd));
            if (length(tmp) > 0)
                rst$endp <- rst$outcome[tmp];
        }

        ##return
        rst})
})

##get treatment labels
get.trt.labels <- reactive({
    if (is.null(input$inTxtArm0) |
        is.null(input$inTxtArm1))
        return(NULL);

    c(input$inTxtArm0, input$inTxtArm1);
})

##get mcmc setup
get.mcmc.par <- reactive({
    rst <- list(iter=input$mcmciter,
                warmup=input$mcmcburnin,
                thin=input$mcmcthin,
                chains=input$mcmcchain,
                control=list(adapt_delta = input$mcmcdelta,
                             stepsize    = input$mcmcstepsize));
    rst
})


##read data from user input
get.data <- reactive({
    lst.var <- get.variables();
    if (is.null(userLog$data) | is.null(lst.var))
        return(NULL);

    rst <- do.call(imData, c(list(userLog$data), lst.var, err.terminate = FALSE));

    if ("IDEMERROR" %in% class(rst)) {
        return(NULL);
    } else {

        ## add treatment labels
        cur.labels <- get.trt.labels();
        if (!is.null(cur.labels))
            rst$lst.var$trt.label <- cur.labels;

        return(rst);
    }
})

get.model.valid <- reactive({
    lst.var <- get.variables();

    if (is.null(userLog$data) | is.null(lst.var))
        return(null);

    rst <- do.call(imData, c(list(userLog$data), lst.var, err.terminate = FALSE));

    if ("IDEMERROR" %in% class(rst)) {
        return(rst);
    } else {
        return(NULL);
    }
})


##fit regression model to the original data
get.fit.rst <- reactive({
    if (-1 == userLog$model)
        return(NULL);

    data.all <- get.data();

    if (is.null(data.all))
        return(NULL);

    rst  <- imFitModel(data.all);
    rst
})

##get treatment arms
get.trt <- reactive({
    if (-1 == userLog$model)
        return(NULL);

    data.all <- get.data();

    if (is.null(data.all))
        return(NULL);

    rst <- summary(data.all, opt = "trt");
})

get.imp.inx <- reactive({
    data.all <- get.data();

    if (is.null(data.all))
        return(NULL);

    summary(data.all, opt = "missid");
})


##get sensivity paramters
get.deltas <- reactive({
    if (is.null(input$inSensp))
        return(NULL);

    rst <- as.numeric(strsplit(input$inSensp, ",")[[1]]);
    rst <- sort(unique(c(0, rst)));
    rst
})

## get SACE sensitivity parameters
get.SACE <- reactive({
    rst <- as.numeric(strsplit(input$inSACE, ",")[[1]]);
    rst <- sort(unique(c(0, rst)));
    rst
})

##get quantiles
get.extras <- reactive({
    rst <- as.numeric(strsplit(input$inExtras, ",")[[1]]);
    rst <- sort(unique(c(50, rst)));
    rst/100;
})

## get time of outcomes
get.TimeOut <- reactive({
    vars <- get.variables();
    rst  <- as.numeric(substr(vars$outcome, 2, nchar(vars$outcome)));
    rst
})


##get mcmc samples for convergence checking
get.imputed.mcmc <- reactive({
    if (0 == input$btnConverge)
        return(NULL);

    isolate({
        fit.rst  <- get.fit.rst();
        mcmc.par <- get.mcmc.par();
        normal   <- as.numeric(input$inNorm);

        if (any(is.null(c(fit.rst))))
            return(NULL);

        imp.inx <- get.imp.inx();
        print(imp.inx);
        if (0 == length(imp.inx))
            return(NULL);
        cur.smp <- sample(imp.inx, 1);

        rst <- imImpSingle(dsub = fit.rst$im.data$data[cur.smp,],
                           fit.rst = fit.rst,
                           normal=normal,
                           chains=mcmc.par$chains,
                           iter=mcmc.par$iter,
                           warmup=mcmc.par$warmup,
                           thin=mcmc.par$thin,
                           control=mcmc.par$control,
                           seed=as.numeric(input$inSeed));
    })
    rst
});

##get rank median for the original
get.rst.orig <- reactive({
    if (is.null(input$btnImpute))
        return(NULL);

    imp.data <- get.imputed.full();
    if (is.null(imp.data))
        return(NULL);

    rst <- imInfer(imp.data,
                   n.boot = 0,
                   effect.quantiles = get.extras(),
                   seed = as.numeric(input$inSeed));
})

##get bootstrap results
get.rst.boot <- reactive({
    if (is.null(input$btnBoot))
        return(NULL);

    isolate({
        imp.rst  <- get.imputed.full();
        n.boot   <- input$inNbs;
        nCores   <- input$inNcores;

        if (any(is.null(c(n.boot, imp.rst))))
            return(NULL);

        ##Create a Progress object
        progress <- shiny::Progress$new(session, min=0, max=1);
        progress$set(message = "Bootstrap...", value=0);
        ##Close the progress when this reactive exits (even if there's an error)
        on.exit(progress$close());

        rst <- imInfer(imp.rst,
                       n.boot=n.boot,
                       n.core=nCores,
                       update.progress=progress,
                       seed=as.numeric(input$inSeed));
    });
    rst
})


## get sace
get.rst.boot.sace <- reactive({
    cur.boot <- get.rst.boot();
    cur.sace <- get.SACE();

    if (is.null(cur.boot) | is.null(cur.sace))
        return(NULL);

    summary(cur.boot, opt = "SACE", sace.deltas = cur.sace);
})

## get standard deviation of functional endpoint -- used to populate configuration
get.sd.endp <- function(out.len = 3, sdPct = 0.1, outVar){
    data.all <- get.data();
    if (! "IDEMDATA" %in% class(data.all))
        return(NULL);

    eval(parse(text=paste("endp <- with(data.all$data, {",
                          data.all$lst.var$endfml,"})")));

    width <- sdPct*sd(endp, na.rm=TRUE);
    if(outVar == 'SACE')
        rst <- seq(-width, 0, len = out.len)
    if(outVar == 'ImpSens')
        rst <- seq(-width/2, width/2, len = out.len)

    rst <- paste(sprintf("%5.2f", rst),
                 collapse=",",
                 sep='');
    rst
}


##get imputated complete datasets using the original dataset
get.imputed.full <- reactive({

    if (is.null(input$btnImpute))
        return(NULL);

    if (0 == input$btnImpute & 0 == input$btnBoot)
        return(NULL);

    isolate({
        fit.rst  <- get.fit.rst();
        deltas   <- get.deltas();
        mcmc.par <- get.mcmc.par();
        normal   <- as.numeric(input$inNorm);

        if (any(is.null(c(deltas, fit.rst))))
            return(NULL);

        ##Create a Progress object
        progress <- shiny::Progress$new(session, min=0, max=1);
        progress$set(message = "Imputing...", value=0);
        ##Close the progress when this reactive exits (even if there's an error)
        on.exit(progress$close());

        imp.full <- imImpAll(fit.rst,
                             deltas=deltas,
                             normal=input$inNorm,
                             n.imp=input$inNImp,
                             chains=mcmc.par$chains,
                             iter=mcmc.par$iter,
                             warmup=mcmc.par$warmup,
                             thin=mcmc.par$thin,
                             control=mcmc.par$control,
                             update.progress=progress,
                             seed=as.numeric(input$inSeed));
    })

    imp.full
})

