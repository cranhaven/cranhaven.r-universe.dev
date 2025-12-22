panel_daily <- function(id){
  wellPanel(
    class = "wellprocess",
    tags$div(class = "processbtn",
             actionButton(NS(id,"diariobtn"),
                          class = "classagrupar",
                          "Grouping Data")),
    tags$div(class = "process",
      uiOutput(NS(id,"diariosum")),
      uiOutput(NS(id,"diariomean"))),
      uiOutput(NS(id,"diariomax")),
      uiOutput(NS(id,"diariomin"))
  )#wellpanel

}

panel_monthly <- function(id){
  wellPanel(
    class = "wellprocess",
    tags$div(class = "processbtn",
             actionButton(NS(id,"mensalbtn"),
                          class = "classagrupar",
                          "Grouping Data")),
    uiOutput(NS(id,"mensalsum")),
    uiOutput(NS(id,"mensalmean")),
    uiOutput(NS(id,"mensalmax")),
    uiOutput(NS(id,"mensalmin"))
  )#wellpanel
}


panel_yearly <- function(id){
  wellPanel(
    class = "wellprocess",
    tags$div(class = "processbtn",
             actionButton(NS(id,"anualbtn"),
                          class = "classagrupar",
                          "Grouping Data")),
    uiOutput(NS(id,"anualsum")),
    uiOutput(NS(id,"anualmean")),
    uiOutput(NS(id,"anualmax")),
    uiOutput(NS(id,"anualmin"))
  )#wellpanel
}
