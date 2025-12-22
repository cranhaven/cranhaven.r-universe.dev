pan_load_file <- function(id){
  tagList(
    wellPanel(
      # tags$h5("Uploading Files"),
      #____________________________________________________
      fileInput(NS(id,"file1"), "Choose CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",".csv"))
    ),#wellpanel
    wellPanel(
      tags$h5("File parameters"),
      #____________________________________________________
      # Input: Select separator ----
      radioButtons(NS(id,"sep"), "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ";"),
      radioButtons(NS(id,"dec"), "Decimal points",
                   choices = c(Comma = ",",
                               Point = "."),
                   selected = ","),
      # Input: na values
      textInput(NS(id,"na.values"), "Missing values",
                value = "-9999"),
      helpText("Note: Value representing missing values, INMET default: -9999."),
      #Input: skip lines
      uiOutput(NS(id,"skiplines")),
      #help
      uiOutput(NS(id,"help_skip")),
      tags$label(class="control-label",
                 "Column names"),
      checkboxInput(NS(id,"janitor"), "Fix column names", TRUE),
      selectInput(NS(id,"dateFormats"), "Select the date format used in the file:",
                  choices = c(
                    #choice---------
                    'dd/mm/aaaa' = "%d/%m/%Y",'mm/dd/aaaa' = "%m/%d/%Y",'aaaa/mm/dd' = "%Y/%m/%d",
                    'dd/mmm/aaaa' = "%d/%b/%Y",'aaaa/mmm/dd' = "%Y/%b/%d",'dd/mmmm/aaaa' = "%d/%B/%Y",
                    'aaaa/mmmm/dd' = "%Y/%B/%d",'dd-mm-aaaa' = "%d-%m-%Y",'mm-dd-aaaa' = "%m-%d-%Y",
                    'aaaa-mm-dd' = "%Y-%m-%d",'dd-mmm-aaaa' = "%d-%b-%Y",'aaaa-mmm-dd' = "%Y-%b-%d",
                    'dd-mmmm-aaaa' = "%d-%B-%Y",'aaaa-mmmm-dd' = "%Y-%B-%d",'dd/mm/aa' = "%d/%m/%y",
                    'mm/dd/aa' = "%m/%d/%y",'aa/mm/dd' = "%y/%m/%d",'dd/mmm/aa' = "%d/%b/%y",
                    'aa/mmm/dd' = "%y/%b/%d",'dd/mmmm/aa' = "%d/%B/%y",'aa/mmmm/dd' = "%y/%B/%d"
                    #--------
                  )
      ),
      helpText("The original date format in the file. Required for conversion to the standard R format (yyyy-mm-dd)."),
      selectInput(NS(id,"fuso"),
                  choices = OlsonNames(),
                  label = "Time zone",
                  selected = OlsonNames()[which(OlsonNames()=="America/Sao_Paulo")]
      )
    )#wellpanel
  )
  
}


#panel_config_load_file <-function(id){
  
#}
