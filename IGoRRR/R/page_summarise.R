
### Build a summary table from selected columns of current table
###   Dependencies on specific packages: none.
###   Dependencies in generated code: dplyr, Hmisc.

page_summarise <- list(

  ui = function() ..ui(page="summarise", control=TRUE),


  server = function(input, output, session) {

    ..vServer(input,output,"summarise")

    statsv <- c("n","v","sum","mean","median","q1","q3","p10","p90","sd","var","min","max","first","last") %>%
      {names(.) <- .; .} %>% ..Zrename()

    statsf  <- function(i,w,value)
      if (w=='')
        c(n="n()",
          v=glue("sum({value})"),
          sum="sum_NA2_",
          mean="mean_NA2_",
          median="median_NA2_",
          q1="quantile(.,p=.25_NA1_)",
          q3="quantile(.,p=.75_NA1_)",
          p10="quantile(.,p=.10_NA1_)",
          p90="quantile(.,p=.90_NA1_)",
          sd="sd_NA2_",
          var="var_NA2_",
          min="min_NA2_",
          max="max_NA2_",
          first="first",
          last="last")[i]
      else {
        w <- ..name(w)
        c(n=glue("sum({w})"),
          v=glue("sum({w}*{value})"),
          sum=glue("wtd.sum(.,w={w}_NA1_)"),
          mean=glue("wtd.mean(.,w={w}_NA1_)"),
          median=glue("wtd.quantile(.,p=.5,w={w}_NA1_)"),
          q1=glue("wtd.quantile(.,p=.25,w={w}_NA1_)"),
          q3=glue("wtd.quantile(.,p=.75,w={w}_NA1_)"),
          p10=glue("wtd.quantile(.,p=.10,w={w}_NA1_)"),
          p90=glue("wtd.quantile(.,p=.90,w={w}_NA1_)"),
          sd=glue("sqrt(wtd.var(.,w={w}_NA1_))"),
          var=glue("wtd.var(.,w={w}_NA1_)"),
          min="min_NA2_",
          max="max_NA2_",
          first="first",
          last="last")[i]
        }

    statna <- function(i,w,na.rm) {
      j <- case_when(i %in% c('n','v','first','last') ~ 0, # na.rm doesn't make sense
                     i %in% c('min','max') ~ 1,            # na.rm=FALSE by default
                     (w=='') ~ 1,                          # na.rm=FALSE by default
                     TRUE ~ 2)                             # na.rm=TRUE by default
      v <- case_when(((j==1)&na.rm)  ~ 'na.rm=TRUE',
                     ((j==2)&!na.rm) ~ 'na.rm=FALSE',
                                TRUE ~ '')
      u <- unique(v)
      if (length(u)==1) u else v
    }

    stat <- function (i,w,na) {
      value <-
        if (..isNotEmpty(input$summarise.value))
          if (input$summarise.value=="TRUE") "."
          else
          if (input$summarise.value=="FALSE") "!."
          else
            if (w=='') paste0(".==",input$summarise.value)
            else      paste0("(.==",input$summarise.value,")")
        else "is.na(.)"
      if (length(na)>1)
        statsf(i,w,value) %>%
          str_replace("_NA1_",ifelse(na!='',paste0(',',na),'')) %>%
          str_replace("_NA2_",ifelse(na!='',paste0('(.,',na,')'),''))
      else
        statsf(i,w,value) %>% str_replace("_NA._",'')
    }

    suffix <- function() if (..isNotEmpty(input$summarise.value)) paste0("_",input$summarise.value) else "_NA"

    output$summarise.control <- renderUI({
      .IGoR$state$meta
      if (..isNotEmpty(input$main.data))
        fluidRow(
          column(width=6,
            ..select.ui("summarise", buttons.title=..s2(.IGoR$Z$summarise$summarise)),
              box(width='100%', collapsible=TRUE,
              column(width=6, ..group.ui(input,"summarise", box=FALSE)),
              column(width=6, selectizeInput("summarise.W", ..s3(.IGoR$Z$any$weight), ..numeric(input)))
          ) ),
          column(width=6,
            ..load.ui("summarise"),
            box(width='100%',
              fluidRow(
                column(width=6,
                  selectizeInput("summarise.funs", ..s1(.IGoR$Z$summarise$funs),
                             multiple=TRUE, options=list(placeholder = .IGoR$Z$any$funs),
                             choices=statsv)),
                column(width=6, uiOutput("summarise.value"))
              ),
              fluidRow(
                column(width=6, checkboxInput("summarise.names",..s5(.IGoR$Z$summarise$names),TRUE)),
                column(width=6, checkboxInput("summarise.na.rm",..s5(.IGoR$Z$any$na.rm),TRUE))
        ) ) ) )
    })

    output$summarise.value <- renderUI(
      if ("v" %in% input$summarise.funs) ..label.ui("summarise","value",'', title=.IGoR$Z$summarise$value, suffix='')
    )

    output$summarise.command2 <- renderUI(
      ..textarea("summarise", "summarise...(...)", 3,
        if (!is.null(input$summarise.funs)) {
          na <- statna(input$summarise.funs,input$summarise.W,input$summarise.na.rm)
          f1 <- stat(input$summarise.funs,input$summarise.W,na)
          nm <- (if (..isTRUE(input$summarise.names))
                      names(statsv)[Vectorize(function (x) which(x==statsv))(input$summarise.funs)] %>%
                        ifelse(.=="Comptage",paste0("Comptage",suffix()),.)
                 else input$summarise.funs %>%
                        ifelse(.=="v",paste0("v",suffix()),.)) %>%
                        make.names()
          fn <- ..collapse0(
                  if (..isTRUE(input$summarise.names))
                       paste0('\"',nm,'\"=',f1)
                  else ifelse(str_detect(f1,"[()]"),paste0('\"',nm,'\"=',f1),f1))
          if ((length(f1)>1)||..isTRUE(input$summarise.names)||str_detect(f1,"[()]")) fn <- glue("list({fn})")
          ..command2(
            ..group_by(input,"summarise"),
            "summarise",
            if (input$summarise.type==2)
              if (input$summarise.drop)
                   glue("_if(Negate(is.{input$summarise.class}), ")
              else glue("_if(is.{input$summarise.class}, ")
            else
            if (input$summarise.type==3)
              if (input$summarise.drop)
                   "_at(c(), "
              else "_all("
            else glue("_at({..select(input,'summarise',vars=TRUE)}, "),
            fn,
            if ((length(na)==1)&&(nchar(na)>0)) paste0(', ',na),
            ")",
            ..ungroup(input,"summarise",1)
          )
        }
    ) )

    observeEvent({.IGoR$state$meta; input$summarise.command2},
      ..try(input,output,"summarise",
        function(x) isolate({
          d <- ..data(input)[,..select.columns(input,output,"summarise")]
          l <- setdiff(colnames(x),input$summarise.group)
          n <- length(l)/length(input$summarise.funs)
          paste(
           if (("character" %in% Map(class,d))&&("mean" %in% input$summarise.funs)) .IGoR$Z$summarise$msg.error1,
           sprintf(.IGoR$Z$summarise$msg.result,n),
           sep='\n'
          )
        })
    ) )

  }
)

