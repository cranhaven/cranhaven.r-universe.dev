# Project: BiclustGUI
# 
# Author: lucp8394
###############################################################################


MaxBC_WINDOW <- function(){
  
  initializeDialog(title = gettextRcmdr("Find Maximal Size BC's...")) 
  
  ### PREPARATION & BUTTON-FUNCTIONS ###
  AllResults <- .makeResultList()
  
  onSetwd <- function(){	Setwd()	}
  
  onCancel <- function() {
    if (GrabFocus()) 
      tkgrab.release(top)
    tkdestroy(top)
    tkfocus(CommanderWindow())
  }
  
  onHelp <- function() {
    tkgrab.release(window)
    print(help("MaxBC"))
  }
  
  
  
  ### GENERAL FRAMES ###
  resultfabiaFrame <- tkframe(top)
  resultFrame <- tkframe(resultfabiaFrame)
  buttons <- tkframe(top)
  
  ### BICLUSTER RESULTS FRAME ##
  
  
  resultBox <- tklistbox( resultFrame , height=5, exportselection="FALSE",
                          selectmode="single", background="white")
  for (result in AllResults) tkinsert(resultBox, "end", result)
  resultScroll <- ttkscrollbar(resultFrame,command=function(...) tkyview(resultBox, ...))
  tkconfigure(resultBox, yscrollcommand=function(...) tkset(resultScroll, ...))
  if(length(AllResults)!=0){tkselection.set(resultBox,0)}
  
  tkgrid(labelRcmdr(top,fg=getRcmdr("title.color"),font="RcmdrTitleFont",text=gettextRcmdr("Biclustering Results (select 1):")),sticky="nw")
  tkgrid(resultBox,resultScroll,sticky="nw") #,sticky="ns"
  tkgrid.configure(resultScroll,sticky="ns")
  
  optionsFrame <- tkframe(resultfabiaFrame)
  
  
  ### PARAMETER FRAME ##
  parameterFrame <- tkframe(optionsFrame)
  tkgrid(labelRcmdr(parameterFrame,fg=getRcmdr("title.color"),font="RcmdrTitleFont",text=gettextRcmdr("")),sticky="nw")
  
  
  top_entry <- tkframe(parameterFrame)
  top_vars <- tclVar("1")
  top_field <- ttkentry(top_entry,width=6,textvariable=top_vars)
  tkgrid(labelRcmdr(top_entry,text=gettextRcmdr("Top Number: ")),top_field,sticky="nw")
  tkgrid(top_entry,sticky="ne")
  
  
  ### FABIA OPTIONS FRAME ###
  
  fabiaoptionsFrame <- tkframe(optionsFrame)
  tkgrid(labelRcmdr(fabiaoptionsFrame,fg=getRcmdr("title.color"),font="RcmdrTitleFont",text=gettextRcmdr("Fabia Result Options")),sticky="nw")
  
  
  thresZ_entry <- tkframe(fabiaoptionsFrame)
  thresZ_vars <- tclVar("0.5")
  thresZ_field <- ttkentry(thresZ_entry,width=6,textvariable=thresZ_vars)
  tkgrid(labelRcmdr(thresZ_entry,text=gettextRcmdr("Threshold Bicluster Sample: ")),thresZ_field,sticky="nw")
  tkgrid(thresZ_entry,sticky="ne")
  
  thresL_entry <- tkframe(fabiaoptionsFrame)
  thresL_vars <- tclVar("NULL")
  thresL_field <- ttkentry(thresL_entry,width=6,textvariable=thresL_vars)
  tkgrid(labelRcmdr(thresL_entry,text=gettextRcmdr("Threshold Bicluster Loading: ")),thresL_field,sticky="nw")
  tkgrid(thresL_entry,sticky="ne")
  
  
  ### THE JACCARD BUTTON FUNCTION ###
  onOK <- function(){
    sel <- as.integer(tkcurselection(resultBox))+1
    if(length(AllResults)==0){
      justDoIt(paste0("warning('No available results',call.=FALSE)"))
    }
    else if(length(sel)==0){
      justDoIt(paste0("warning('No result selected',call.=FALSE)"))
    }
    else if(length(sel)>1){
      justDoIt(paste0("warning('Please select 2 results',call.=FALSE)"))
    }
    else{
      
      sel.result1 <- AllResults[sel]

      bicres1 <- .tobiclust_transf(sel.result1,thresZ=paste0(tclvalue(thresZ_vars)),thresL=paste0(tclvalue(thresL_vars)))

      
      # Correct data check
      eval(parse(text=paste0("temp.correct1 <- .correctdataforresult(",sel.result1,")")))

      
      if(temp.correct1){
        
        maxbc.command <- paste0("MaxBC(result=",bicres1,",top=",tclvalue(top_vars),")")
        doItAndPrint(maxbc.command)
      }
    }
  }
  
  
  ### WORKING DIR & EXPORT BUTTON ###
  
  buttonsleft <- tkframe(buttons)
  #setwdButton <- buttonRcmdr(buttonsleft,command=onSetwd,text=gettextRcmdr("Set Work Dir."),foreground="darkgreen",default="active",width="12",borderwidth=3)
  jaccardButton <- buttonRcmdr(buttonsleft,command=onOK,text=gettextRcmdr("MaxBC"),foreground="darkgreen",default="active",width="14",borderwidth=3)
  #tkgrid(setwdButton,exportButton)
  tkgrid(jaccardButton)
  
  
  
  ### EXIT & HELP BUTTON ###
  buttonsright <- tkframe(buttons)
  exitButton <- buttonRcmdr(buttonsright,command=onCancel,text=gettextRcmdr("Exit"),foreground="darkgreen",width="8",borderwidth=3)
  helpButton <- buttonRcmdr(buttonsright,command=onHelp,text=gettextRcmdr("Help"),foreground="darkgreen",width="8",borderwidth=3)
  tkgrid(exitButton,helpButton)
  
  
  ### FINAL FRAME CONFIGURATION ###
  tkgrid(parameterFrame,sticky="nw")
  tkgrid(fabiaoptionsFrame,sticky="nw",pady="15")
  tkgrid(resultFrame,sticky="nw")
  tkgrid(optionsFrame,sticky="nw")
  tkgrid(resultfabiaFrame,sticky="nw")
  #tkgrid.configure(optionsFrame,sticky="nw",padx="25")
  
  tkgrid(buttonsleft,buttonsright)
  tkgrid(buttons,sticky="sew",pady=8)
  tkgrid.columnconfigure(buttons, 0, weight=1)
  tkgrid.columnconfigure(buttons, 1, weight=1)
  tkgrid.configure(buttonsleft,sticky="w")
  tkgrid.configure(buttonsright,sticky="e")
  
  dialogSuffix(onOK=onOK,preventGrabFocus=TRUE)
  
}

