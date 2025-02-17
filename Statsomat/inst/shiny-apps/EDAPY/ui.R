# Define UI for application 
shinyUI(fluidPage(
  
  
  # Disconnect message
  disconnectMessage(
    text = "Your session timed out or out of memory. ",
    refresh = "Reload now",
    background = "#ff9900",
    colour = "white",
    overlayColour = "grey",
    overlayOpacity = 0.3,
    refreshColour = "black"
  ),
  
  
  # Style uploading modal
  tags$head(tags$style(".modal-body {padding: 10px}
                     .modal-content  {-webkit-border-radius: 6px !important;-moz-border-radius: 6px !important;border-radius: 6px !important;}
                     .modal-dialog { width: 400px; display: inline-block; text-align: center; vertical-align: top;}
                     .modal-header {background-color: #ff9900; border-top-left-radius: 6px; border-top-right-radius: 6px}
                     .modal { text-align: center; padding-right:10px; padding-top: 24px;}
                     .close { font-size: 16px}")),
  
  
  
  tags$head(
    tags$style(HTML("
                   .shiny-notification{

                    position: fixed;
                    top: 10px;
                    left: calc(50% - 400px);;
                    width: 950px;
                    /* Make sure it draws above all Bootstrap components */
                    z-index: 2000;
                    background-color: #ff9900;
                
                   }
                
                    "))
    ),

  
  #Disable download button until check positive
  singleton(tags$head(HTML(
    '
  <script type="text/javascript">
    
    
    $(document).ready(function() {
      $("#download").attr("disabled", "true").attr("onclick", "return false;");
      Shiny.addCustomMessageHandler("check_generation", function(message) {
        $("#download").removeAttr("disabled").removeAttr("onclick").html("");
      });
    })
    
  
    
  </script>
  '
  ))),
  
  br(),
  
  tags$div(a(img(src='Logo.jpg', width=200), href="https://www.statsomat.com", target="_blank")),
  
  h1("Exploratory Data Analysis (EDA) with Python", 
     style = "font-family: 'Helvetica';
     color: #fff; text-align: center;
     background-color: #396e9f;
     padding: 20px;
     margin-bottom: 0px;"),
  h5("Statsomat/EDAPY", 
     style = "font-family: 'Helvetica';
     color: #fff; text-align: center;
     background-color: #2fa42d;
     padding: 5px;
     margin-top: 0px;"),
  
  br(),
  
  
  fluidRow( 
              
                 
      column(5, offset = 1,  
             
             
             wellPanel(style = "background: #adc7de;", 
                          
                          h3("Upload"),
                          
                          # File input
                          fileInput("file", "Choose CSV file",
                                    accept = c(
                                      "text/csv",
                                      "text/comma-separated-values",
                                      ".csv"), 
                                    buttonLabel = "Browse...",
                                    placeholder = "No file selected"),
                          
                          
                       # Input: Select encoding ----
                       radioButtons("fencoding", "Encoding",
                                    choices = c(Auto = "unknown", 
                                                "UTF-8" = "UTF-8"),
                                    selected = "unknown", inline=TRUE),
                       
                       
                       # Input: Select decimal ----
                       radioButtons("decimal", "Decimal",
                                    choices = c(Auto = "auto",
                                                Comma = ",",
                                                Dot = "."),
                                    selected = "auto", inline=TRUE),
                       
                       # Input: Select continuity ----
                       radioButtons("continuity", "Continuity",
                                    choices = c(Severe = "severe", 
                                                Relaxed = "relaxed"),
                                    selected = "severe", inline=TRUE),
                       
                       tags$small("By clicking the Browse button and uploading a file, you agree to the",
                              style="color: #808080;"),
                       
                       tags$a(href="https://statsomat.com/terms", target="_blank", "Terms of Use.", style="font-weight: bold; font-size: 9pt;")
            ),
          
            
            wellPanel(style = "background: #adc7de;", 
                      
                      h3("Select Variables (max 25)"),
                      
                      h5("Only blanks as missing values"),
                      
                      uiOutput("selection1")
                    
            ),

           
           wellPanel(style = "background: #ff9900", align="center", 
                     
                     h3("Click to Generate Results"),
                     
                     radioButtons('rcode', '', c('Data Analysis Report (PDF)', 'Python Code (HTML)'), inline = TRUE),
                     
                     
                     actionButton("generate", "", style="
                                    height:145px;
                                    width:84px;
                                    padding-top: 3px;
                                    color:#ff9900; 
                                    background-color: #ff9900; 
                                    background-image: url('Button.gif');
                                    border: none;
                                    outline: none;
                                    box-shadow: none !important;
                                   ")
                     
                     
           ), 
           

           
           wellPanel(style = "background: #ff9900", align="center", 
                     
                     h3("Click to Download"),
                     
                     downloadButton("download", "", style="
                                    height:145px;
                                    width:84px;
                                    padding-top: 3px;
                                    color:#ff9900; 
                                    background-color: #ff9900; 
                                    border-color: #ff9900;
                                    background-image: url('Button.gif');") 
                     
                     
           )
    ), # end column 

    
    column(5, 
           
          wellPanel(style = "background: #fff;", includeHTML("www/Description.html")),
          wellPanel(style = "background: #fff;", includeHTML("www/Instructions.html")),
          wellPanel(style = "background: #fff;", includeHTML("www/Secure.html")), 
          wellPanel(style = "background: #fff;", includeHTML("www/OpenSource.html")), 
          wellPanel(style = "background: #fff;", includeHTML("www/Contact.html"))
       
    ), # column 
    
    column(1,)
    
  ), # fluidrow
  
  includeHTML("www/Footer.html"),
  
  hr()
  
))
