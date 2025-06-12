#######################################
########### Meta Menu #################
intCol <-function () 
{
dialogName<-"intCol" 
def <- list(dsname="Do_Not_Save", tfVar=100, clVar= 20, lnVar=20, fiVar=0.4, iVar=0.1, peVar=0.05, animaVar= 1)
initial <- getDialog(dialogName, defaults= def)
initializeDialog(title = gettextRcmdr("Internal Colonization"))
#### dados de entrada
dsname <- tclVar(initial$dsname)
tfVar <- tclVar(initial$tfVar)
clVar <- tclVar(initial$clVar)
lnVar <- tclVar(initial$lnVar)
fiVar <- tclVar(initial$fiVar)
iVar <- tclVar(initial$iVar) 
peVar <- tclVar(initial$peVar)
animaVar <- tclVar(initial$animaVar)
## forma de entrada
entryDsname <- tkentry(top, width="20", textvariable=dsname)
tfEntry <- tkentry(top, width = "4", textvariable = tfVar)
clEntry <- tkentry(top, width = "4", textvariable = clVar)
lnEntry <- tkentry(top, width = "4", textvariable = lnVar)
fiVarSlider <- tkscale(top, from=0, to=1, showvalue=TRUE, variable=fiVar, resolution=0.01, orient="horizontal")
iEntry <- tkscale(top, from=0, to=1, showvalue=TRUE, variable=iVar, resolution=0.01, orient="horizontal")
peEntry <- tkscale(top, from=0, to=1, showvalue=TRUE, variable=peVar, resolution=0.01, orient="horizontal")
animaBox <- tkcheckbutton(top, variable = animaVar)
##################
onOK <- function() 
	{
            closeDialog()
            tf <- round(as.numeric(tclvalue(tfVar)))
            if (is.na(tf) || tf <= 0) 
                {
                    errorCondition(message = "Number of simulations must be a positive integer")
                    return()
                }
            cl <- round(as.numeric(tclvalue(clVar)))
            if (is.na(cl) || cl <= 0) 
                {
                    errorCondition(message = "Number of columns on the simulated arena must be a positive integer.")
                    return()
                }
            ln <- round(as.numeric(tclvalue(lnVar)))
            if (is.na(ln) || ln <= 0) 
                {
                    errorCondition("Number of lines on the simulated arena must be a positive integer.")
                    return()
                }
            i <- as.numeric(tclvalue(iVar))
            if (i<0 || i > 10) 
                {
                    errorCondition(message = "Colonization constant must be between 0 and 10")
                    return()
                }
            fi <- as.numeric(tclvalue(fiVar))
            pe <- as.numeric(tclvalue(peVar))
            animaVF <- as.logical(as.numeric(tclvalue(animaVar)))
############ Data name
            dsnameValue <- trim.blanks(tclvalue(dsname))
            if (dsnameValue == "Do_Not_Save" | dsnameValue=="") 
                {
                    command <- paste("metaCi(tmax = ",tf, ", cl = ", cl,", f0 = ", fi,", rw =", ln,", ci = ", i,", pe = ", pe, ", anima = ", animaVF,")", sep = "")
                }
            else  
                {
                    command <- paste(dsnameValue,"<- metaCi(tmax = ",tf, ", cl = ", cl,", f0 = ", fi,", rw =", ln,", ci = ", i,", pe = ", pe, ", anima = ", animaVF,")", sep = "")
                }
########
            putDialog(dialogName, values = list(dsname=dsnameValue, tfVar= round(as.numeric(tclvalue(tfVar))), clVar= round(as.numeric(tclvalue(clVar))), lnVar= round(as.numeric(tclvalue(lnVar))), fiVar= as.numeric(tclvalue(fiVar)), iVar=as.numeric(tclvalue(iVar)), peVar=as.numeric(tclvalue(peVar)), animaVar=as.logical(as.numeric(tclvalue(animaVar)))),resettable = FALSE)
            doItAndPrint(command)
            tkfocus(CommanderWindow())
	}
OKCancelHelp(helpSubject = "metapopulation", reset=dialogName, apply= dialogName)
# data name
tkgrid(tklabel(top, text="Enter name for data set: "), entryDsname, sticky="e")
tkgrid(tklabel(top, text="Simulation Arena Conditions : ", fg="blue"), sticky="w")
tkgrid(tklabel(top, text = "Maximum time  "), tfEntry, sticky = "e")
tkgrid(tklabel(top, text = "Columns  "), clEntry, sticky = "e")
tkgrid(tklabel(top, text = "Rows  "), lnEntry, sticky = "e")
#
tkgrid(tklabel(top, text="Species parameters :", fg="blue"), sticky="w")
tkgrid(tklabel(top, text="Initial occupancy  "), fiVarSlider, sticky="se")
tkgrid(tklabel(top, text = "Colonization coefficient  "), iEntry, sticky = "e")
tkgrid(tklabel(top, text = "Extinction probability  "), peEntry, sticky = "se")
tkgrid(tklabel(top, text = "Show simulation frames"), animaBox, sticky = "e")
tkgrid(buttonsFrame, sticky = "w", columnspan = 2)
tkgrid.configure(entryDsname, sticky = "w")
tkgrid.configure(tfEntry, sticky = "w")
tkgrid.configure(clEntry, sticky = "w")
tkgrid.configure(lnEntry, sticky = "w")
tkgrid.configure(fiVarSlider, sticky = "w")
tkgrid.configure(iEntry, sticky = "w")
tkgrid.configure(peEntry, sticky = "w")
tkgrid.configure(animaBox, sticky = "w")
dialogSuffix(rows = 9, columns = 2, focus = tfEntry)
}
######################################
##########################################
propRain <-function() 
{
dialogName<-"propRain" 
def <- list(dsname="Do_Not_Save", tfVar=100, clVar= 20, lnVar=20, fiVar=0.4, piVar=0.1, peVar=0.05, animaVar=1)
initial <- getDialog(dialogName, defaults= def)
initializeDialog(title = gettextRcmdr("Propagulus Rain"))
####
dsname <- tclVar(initial$dsname)
tfVar <- tclVar(initial$tfVar)
clVar <- tclVar(initial$clVar)
lnVar <- tclVar(initial$lnVar)
fiVar <- tclVar(initial$fiVar)
piVar <- tclVar(initial$piVar) 
peVar <- tclVar(initial$peVar) 
animaVar <- tclVar(initial$animaVar)
###
entryDsname <- tkentry(top, width="20", textvariable=dsname)
tfEntry <- tkentry(top, width = "4", textvariable = tfVar)
clEntry <- tkentry(top, width = "4", textvariable = clVar)
lnEntry <- tkentry(top, width = "4", textvariable = lnVar)
fiEntry <- tkscale(top, from=0, to=1, showvalue=TRUE, variable=fiVar, resolution=0.01, orient="horizontal")
piEntry <- tkscale(top, from=0, to=1, showvalue=TRUE, variable=piVar, resolution=0.01, orient="horizontal")
peEntry <- tkscale(top, from=0, to=1, showvalue=TRUE, variable=peVar, resolution=0.01, orient="horizontal")
animaBox <- tkcheckbutton(top, variable = animaVar)
onOK <- function() 
    {
        closeDialog()
        tf <- round(as.numeric(tclvalue(tfVar)))
        if (is.na(tf) || tf <= 0) 
            {
                errorCondition(message = "Number of simulations must be a positive integer")
                return()
            }
        cl <- round(as.numeric(tclvalue(clVar)))
        if (is.na(cl) || cl <= 0) 
            {
                errorCondition(message = "Number of columns on the simulated arena must be a positive integer.")
                return()
            }
        ln <- round(as.numeric(tclvalue(lnVar)))
        if (is.na(ln) || ln <= 0) 
            {
                errorCondition("Number of lines on the simulated arena must be a positive integer.")
                return()
            }
        fi <- as.numeric(tclvalue(fiVar))
        pi <- as.numeric(tclvalue(piVar))
        pe <- as.numeric(tclvalue(peVar))
        animaVF <- as.logical(as.numeric(tclvalue(animaVar)))
############ Data name
        dsnameValue <- trim.blanks(tclvalue(dsname))
       if (dsnameValue == "Do_Not_Save" || dsnameValue=="") 
                {
                    command <- paste("metaPop(tmax = ",tf, ", cl = ", cl,", f0 = ", fi,", rw =", ln,", pi = ", pi,", pe = ", pe, ", anima = ", animaVF,")", sep = "")
                }
            else  
                {
                    command <- paste(dsnameValue,"<- metaPop(tmax = ",tf, ", cl = ", cl,", f0 = ", fi,", rw =", ln,", pi = ", pi,", pe = ", pe, ", anima = ", animaVF,")", sep = "")
                }
########
#	command <- paste("metaPop(tf = ",tf, ", cl = ", cl,", f0 = ", fi,", ln =", ln,", pi = ", pc,", pe = ", pe, ")", sep = "")
        putDialog(dialogName, values = list(dsname=dsnameValue, tfVar= round(as.numeric(tclvalue(tfVar))), clVar= round(as.numeric(tclvalue(clVar))), lnVar= round(as.numeric(tclvalue(lnVar))), fiVar= as.numeric(tclvalue(fiVar)), piVar=as.numeric(tclvalue(piVar)), peVar=as.numeric(tclvalue(peVar)), animaVar=as.logical(as.numeric(tclvalue(animaVar)))),resettable = FALSE)
        doItAndPrint(command)
	tkfocus(CommanderWindow())
	}
OKCancelHelp(helpSubject = "metapopulation", reset=dialogName, apply= dialogName)
tkgrid(tklabel(top, text="Enter name for data set: "), entryDsname, sticky="e")
tkgrid(tklabel(top, text="Simulation Arena Conditions : ", fg="blue"), sticky="w")
tkgrid(tklabel(top, text = "Maximum time  "), tfEntry, sticky = "e")
tkgrid(tklabel(top, text = "Columns  "), clEntry, sticky = "e")
tkgrid(tklabel(top, text = "Rows  "), lnEntry, sticky = "e")
tkgrid(tklabel(top, text="Species parameters : ", fg="blue"), sticky="w")
tkgrid(tklabel(top, text = "Initial occupancy  "), fiEntry, sticky = "se")
tkgrid(tklabel(top, text = "Colonization probability "), piEntry, sticky = "se")
tkgrid(tklabel(top, text = "Extinction probability "), peEntry, sticky = "se")
tkgrid(tklabel(top, text = "Show simulation frames"), animaBox, sticky = "e")
tkgrid(buttonsFrame, sticky = "w", columnspan = 2)
tkgrid.configure(entryDsname, sticky = "w")
tkgrid.configure(tfEntry, sticky = "w")
tkgrid.configure(clEntry, sticky = "w")
tkgrid.configure(lnEntry, sticky = "w")
tkgrid.configure(fiEntry, sticky = "w")
tkgrid.configure(piEntry, sticky = "w")
tkgrid.configure(peEntry, sticky = "w")
tkgrid.configure(animaBox, sticky = "w")
dialogSuffix(rows = 8, columns = 2, focus = tfEntry)
}
##################################################
resEff <-function () 
{
dialogName<-"resEff" 
def <- list(dsname="Do_Not_Save", tfVar=100, clVar= 20, lnVar=20, fiVar=0.4, piVar=0.1, eVar = 0.05, animaVar=1)
initial <- getDialog(dialogName, defaults= def)    
initializeDialog(title = gettextRcmdr("Rescue Effect"))
####
dsname <- tclVar(initial$dsname)
tfVar <- tclVar(initial$tfVar)
clVar <- tclVar(initial$clVar)
lnVar <- tclVar(initial$lnVar)
fiVar <- tclVar(initial$fiVar)
piVar <- tclVar(initial$piVar) 
eVar <- tclVar(initial$eVar) 
animaVar <- tclVar(initial$animaVar)
###
entryDsname <- tkentry(top, width="20", textvariable=dsname)
tfEntry <- tkentry(top, width = "4", textvariable = tfVar)
clEntry <- tkentry(top, width = "4", textvariable = clVar)
lnEntry <- tkentry(top, width = "4", textvariable = lnVar)
fiEntry <- tkscale(top, from=0, to=1, showvalue=TRUE, variable=fiVar, resolution=0.01, orient="horizontal")
piEntry <- tkscale(top, from=0, to=1, showvalue=TRUE, variable=piVar, resolution=0.01, orient="horizontal")
eEntry <- tkscale(top, from=0, to=1, showvalue=TRUE, variable = eVar, resolution=0.01, orient="horizontal")
animaBox <- tkcheckbutton(top, variable = animaVar)
onOK <- function() 
    {
        closeDialog()
        tf <- round(as.numeric(tclvalue(tfVar)))
        if (is.na(tf) || tf <= 0) 
            {
                errorCondition(message = "Number of simulations must be a positive integer")
                return()
            }
        cl <- round(as.numeric(tclvalue(clVar)))
        if (is.na(cl) || cl <= 0) 
            {
                errorCondition(message = "Number of columns on the simulated arena must be a positive integer.")
                return()
            }
        ln <- round(as.numeric(tclvalue(lnVar)))
        if (is.na(ln) || ln <= 0) 
            {
                errorCondition("Number of lines on the simulated arena must be a positive integer.")
                return()
            }
        ce <- as.numeric(tclvalue(eVar))
        fi <- as.numeric(tclvalue(fiVar))
        pi <- as.numeric(tclvalue(piVar))
        animaVF <- as.logical(as.numeric(tclvalue(animaVar)))
############ Data name
        dsnameValue <- trim.blanks(tclvalue(dsname))
        if (dsnameValue == "Do_Not_Save" | dsnameValue=="") 
        {
            command <- paste("metaEr(tmax = ",tf, ", cl = ", cl,", f0 = ", fi,", rw =", ln,", pi = ", pi,", ce = ", ce,", anima =", animaVF,  ")", sep = "")
        }
        else  
            {
                command <- paste(dsnameValue,"<-metaEr(tmax = ",tf, ", cl = ", cl,", f0 = ", fi,", rw =", ln,", pi = ", pi,", ce = ", ce, ", anima =", animaVF, ")", sep = "")
            }
########   
	 putDialog(dialogName, values = list(dsname=dsnameValue, tfVar= round(as.numeric(tclvalue(tfVar))), clVar= round(as.numeric(tclvalue(clVar))), lnVar= round(as.numeric(tclvalue(lnVar))), fiVar= as.numeric(tclvalue(fiVar)), piVar=as.numeric(tclvalue(piVar)), eVar=as.numeric(tclvalue(eVar)), animaVar=as.logical(as.numeric(tclvalue(animaVar)))),resettable = FALSE)
	doItAndPrint(command)
	tkfocus(CommanderWindow())
	}
OKCancelHelp(helpSubject = "metapopulation", reset=dialogName, apply= dialogName)
tkgrid(tklabel(top, text="Enter data set name: "), entryDsname, sticky="e")
tkgrid(tklabel(top, text="Simulation Arena Conditions : ", fg="blue"), sticky="w")
tkgrid(tklabel(top, text = "Maximum time  "), tfEntry, sticky = "e")
tkgrid(tklabel(top, text = "Columns  "), clEntry, sticky = "e")
tkgrid(tklabel(top, text = "Rows  "), lnEntry, sticky = "e")
tkgrid(tklabel(top, text="Species parameters : ", fg="blue"), sticky="w")
tkgrid(tklabel(top, text = "Initial occupancy "), fiEntry, sticky = "e")
tkgrid(tklabel(top, text = "Colonization probability  "), piEntry, sticky = "e")
tkgrid(tklabel(top, text = "Extinction coefficient  "), eEntry, sticky = "e")
tkgrid(tklabel(top, text = "Show simulation frames"), animaBox, sticky = "e")
tkgrid(buttonsFrame, sticky = "w", columnspan = 2)
tkgrid.configure(entryDsname, sticky = "w")
tkgrid.configure(tfEntry, sticky = "w")
tkgrid.configure(clEntry, sticky = "w")
tkgrid.configure(lnEntry, sticky = "w")
tkgrid.configure(fiEntry, sticky = "w")
tkgrid.configure(piEntry, sticky = "w")
tkgrid.configure(eEntry, sticky = "w")
tkgrid.configure(animaBox, sticky = "w")
dialogSuffix(rows = 11, columns = 2, focus = tfEntry)
}
################################################
resEffcol <-function () 
{
dialogName<-"resEffcol" 
def <- list(dsname="Do_Not_Save", tfVar=100, clVar= 20, lnVar=20, fiVar=0.25, iVar=0.1, eVar = 0.05, animaVar=1)
initial <- getDialog(dialogName, defaults= def)  
initializeDialog(title = gettextRcmdr("Rescue and Internal Colonization"))
####
dsname <- tclVar(initial$dsname)
tfVar <- tclVar(initial$tfVar)
clVar <- tclVar(initial$clVar)
lnVar <- tclVar(initial$lnVar)
fiVar <- tclVar(initial$fiVar)
iVar <- tclVar(initial$iVar) 
eVar <- tclVar(initial$eVar) 
animaVar <- tclVar(initial$animaVar)
###

###
entryDsname <- tkentry(top, width="20", textvariable=dsname)
tfEntry <- tkentry(top, width = "4", textvariable = tfVar)
clEntry <- tkentry(top, width = "4", textvariable = clVar)
lnEntry <- tkentry(top, width = "4", textvariable = lnVar)
fiEntry <- tkscale(top, from=0, to=1, showvalue=TRUE, variable=fiVar, resolution=0.01, orient="horizontal")
iEntry <- tkscale(top, from=0, to=1, showvalue=TRUE, variable=iVar, resolution=0.01, orient="horizontal")
eEntry <- tkscale(top, from=0, to=1, showvalue=TRUE, variable = eVar, resolution=0.01, orient="horizontal")
animaBox <- tkcheckbutton(top, variable = animaVar)
###
onOK <- function() 
    {
        closeDialog()
        tf <- round(as.numeric(tclvalue(tfVar)))
        if (is.na(tf) || tf <= 0) 
            {
                errorCondition(message = "Number of simulations must be a positive integer")
                return()
            }
        cl <- round(as.numeric(tclvalue(clVar)))
        if (is.na(cl) || cl <= 0) 
            {
                errorCondition(message = "Number of columns on the simulated arena must be a positive integer.")
                return()
            }
        ln <- round(as.numeric(tclvalue(lnVar)))
        if (is.na(ln) || ln <= 0) 
            {
                errorCondition("Number of lines on the simulated arena must be a positive integer.")
                return()
            }
        e <- as.numeric(tclvalue(eVar))
        if (e<0 || e > 1) 
        {
            errorCondition(message = "Extinction coefficient must be positive between 0 to 1")
            return()
        }
        i <- as.numeric(tclvalue(iVar))
        if (i<0 || i > 1) 
        {
            errorCondition(message = "Colonization coefficient must be positive between 0 to 1")
            return()
        }
        fi <- as.numeric(tclvalue(fiVar))
        animaVF <- as.logical(as.numeric(tclvalue(animaVar)))
############ Data name
   dsnameValue <- trim.blanks(tclvalue(dsname))
        if (dsnameValue == "Do_Not_Save" | dsnameValue=="") 
        {
        	command <- paste("metaCiEr(tmax = ",tf, ", cl = ", cl,", f0 = ", fi,", rw =", ln,", ci = ", i,", ce = ", e, ", anima =", animaVF, ")", sep = "")
        }
        else  
        {
            command <- paste(dsnameValue,"<-metaCiEr(tmax = ",tf, ", cl = ", cl,", f0 = ", fi,", rw =", ln,", ci = ", i,", ce = ", e,  ", anima = ", animaVF,")", sep = "")
        }
########
        putDialog(dialogName, values = list(dsname=dsnameValue, tfVar= round(as.numeric(tclvalue(tfVar))), clVar= round(as.numeric(tclvalue(clVar))), lnVar= round(as.numeric(tclvalue(lnVar))), fiVar= as.numeric(tclvalue(fiVar)), iVar=as.numeric(tclvalue(iVar)), eVar=as.numeric(tclvalue(eVar)), animaVar=as.logical(as.numeric(tclvalue(animaVar)))),resettable = FALSE)
        doItAndPrint(command)
	tkfocus(CommanderWindow())
    }
OKCancelHelp(helpSubject = "metapopulation", apply= dialogName, reset= dialogName)
tkgrid(tklabel(top, text="Enter data set name:"), entryDsname, sticky="e")
tkgrid(tklabel(top, text="Simulation Arena Conditions :", fg="blue"), sticky="w")
tkgrid(tklabel(top, text = "Maximum time  "), tfEntry, sticky = "e")
tkgrid(tklabel(top, text = "Columns  "), clEntry, sticky = "e")
tkgrid(tklabel(top, text = "Rows  "), lnEntry, sticky = "e")
tkgrid(tklabel(top, text="Species parameters  :", fg="blue"), sticky="w")
tkgrid(tklabel(top, text = "Initial occupancy  "), fiEntry, sticky = "e")
tkgrid(tklabel(top, text = "Colonization coefficient  "), iEntry, sticky = "e")
tkgrid(tklabel(top, text = "Extinction coefficient  "), eEntry, sticky = "e")
tkgrid(tklabel(top, text = "Show simulation frames"), animaBox, sticky = "e")
tkgrid(buttonsFrame, sticky = "w", columnspan = 2)
tkgrid.configure(entryDsname, sticky = "w")
tkgrid.configure(tfEntry, sticky = "w")
tkgrid.configure(clEntry, sticky = "w")
tkgrid.configure(lnEntry, sticky = "w")
tkgrid.configure(fiEntry, sticky = "w")
tkgrid.configure(iEntry, sticky = "w")
tkgrid.configure(eEntry, sticky = "w")
tkgrid.configure(animaBox, sticky = "w")
dialogSuffix(rows = 11, columns = 2, focus = tfEntry)
}
####################################
########################################################
metacompDb <-function () 
{
dialogName<-"metacompDb" 
def <- list(dsname="Do_Not_Save", tmaxVar=100, clVar= 20, lnVar=20, fi1Var=0.25,fi2Var= 0.25, i1Var=0.1, i2Var=0.1, peVar = 0.05, distrVar=0.00, animaVar=1)
initial <- getDialog(dialogName, defaults= def)  
initializeDialog(title = gettextRcmdr("Meta Competition"))
####  Dados de entrada
dsname <- tclVar(initial$dsname)
tmaxVar <- tclVar(initial$tmaxVar)
clVar <- tclVar(initial$clVar)
lnVar <- tclVar(initial$lnVar)
fi1Var <- tclVar(initial$fi1Var)
fi2Var <- tclVar(initial$fi2Var)
i1Var <- tclVar(initial$i1Var) ## nclassVar ->iVar
i2Var <- tclVar(initial$i2Var) ## nclassVar ->iVar
peVar <- tclVar(initial$peVar) 
distrVar <- tclVar(initial$distrVar) 
animaVar <- tclVar(initial$animaVar)
#### Formato de entrada
entryDsname <- tkentry(top, width="20", textvariable=dsname)
tmaxEntry <- tkentry(top, width = "4", textvariable = tmaxVar)
clEntry <- tkentry(top, width = "4", textvariable = clVar)
lnEntry <- tkentry(top, width = "4", textvariable = lnVar)
fi1VarSlider <- tkscale(top, from=0, to=1, showvalue=TRUE, variable=fi1Var, resolution=0.01, orient="horizontal")
fi2VarSlider <- tkscale(top, from=0, to=1, showvalue=TRUE, variable=fi2Var, resolution=0.01, orient="horizontal")
i1Entry <- tkentry(top, width = "6", textvariable = i1Var)
i2Entry <- tkentry(top, width = "6", textvariable = i2Var)
peEntry <- tkscale(top, from=0, to=1, showvalue=TRUE, variable=peVar, resolution=0.01, orient="horizontal")
distrEntry <- tkscale(top, from=0, to=1, showvalue=TRUE, variable=distrVar, resolution=0.01, orient="horizontal")
animaBox <- tkcheckbutton(top, variable = animaVar)
onOK <- function() 
    {
	closeDialog()
        tmax <- round(as.numeric(tclvalue(tmaxVar)))
        if (is.na(tmax) || tmax <= 0) 
            {
                errorCondition(message = "Number of simulations must be a positive integer")
                return()
            }
  	cl <- round(as.numeric(tclvalue(clVar)))
        if (is.na(cl) || cl <= 0) 
            {
                errorCondition(message = "Number of columns on the simulated arena must be a positive integer.")
                return()
            }
	ln <- round(as.numeric(tclvalue(lnVar)))
        if (is.na(ln) || ln <= 0) 
            {
                errorCondition("Number of lines on the simulated arena must be a positive integer.")
                return()
            }
        i1 <- as.numeric(tclvalue(i1Var))
        ## if (i1<0 ) 
        ##     {
        ##         errorCondition(message = "Colonization constant must be positive")
        ##         return()
        ##     }
        i2 <- as.numeric(tclvalue(i2Var))
        ## if (i2<0 ) 
        ##     {
        ##         errorCondition(message = "Colonization constant must be positive")
        ## return()
        ##     }
        fi1 <- as.numeric(tclvalue(fi1Var))
        fi2 <- as.numeric(tclvalue(fi2Var))
        pe <- as.numeric(tclvalue(peVar))
        distr <- as.numeric(tclvalue(distrVar))
        animaVF <- as.logical(as.numeric(tclvalue(animaVar)))
############ Data name
   dsnameValue <- trim.blanks(tclvalue(dsname))
        if (dsnameValue == "Do_Not_Save" | dsnameValue=="") 
            {
        	command <- paste("metaComp(tmax = ",tmax, ", cl = ", cl,", f01 = ", fi1,", f02 = ", fi2,", rw =", ln,", i1 = ", i1,", i2 = ", i2,", pe = ", pe,", D = ", distr,", anima = ", animaVF, ")", sep = "")
            }
        else  
            {
                command <- paste(dsnameValue,"<- metaComp(tmax = ",tmax, ", cl = ", cl,", f01 = ", fi1,", f02 = ", fi2,", rw =", ln,", i1 = ", i1,", i2 = ", i2,", pe = ", pe,", D = ", distr,", anima = ", animaVF, ")", sep = "")
            }
########
	doItAndPrint(command)
	tkfocus(CommanderWindow())
        putDialog(dialogName, values = list(dsname=dsnameValue, tmaxVar= round(as.numeric(tclvalue(tmaxVar))), clVar= round(as.numeric(tclvalue(clVar))), lnVar= round(as.numeric(tclvalue(lnVar))), fi1Var= as.numeric(tclvalue(fi1Var)), fi2Var= as.numeric(tclvalue(fi2Var)), i1Var=as.numeric(tclvalue(i1Var)), i2Var=as.numeric(tclvalue(i2Var)), peVar=as.numeric(tclvalue(peVar)), distrVar=as.numeric(tclvalue(distrVar)), animaVar=as.logical(as.numeric(tclvalue(animaVar)))),resettable = FALSE)
    }
OKCancelHelp(helpSubject = "metaComp", apply= dialogName, reset = dialogName)
# data name
tkgrid(tklabel(top, text="Enter name for data set:"), entryDsname, sticky="e")
##
tkgrid(tklabel(top, text="Simulation Arena Conditions :", fg="blue"), sticky="w")
tkgrid(tklabel(top, text = "Maximum time"), tmaxEntry, sticky = "e")
tkgrid(tklabel(top, text = "Columns"), clEntry, sticky = "e")
tkgrid(tklabel(top, text = "Rows"), lnEntry, sticky = "e")
#
tkgrid(tklabel(top, text="Best Competitor Species :", fg="blue"), sticky="w")
tkgrid(tklabel(top, text="Initial occupancy "), fi1VarSlider, sticky="se")
tkgrid(tklabel(top, text = "Colonization coefficient "), i1Entry, sticky = "e")
tkgrid(tklabel(top, text="Inferior Competitor Species :", fg="blue"), sticky="w")
tkgrid(tklabel(top, text="Initial occupancy "), fi2VarSlider, sticky="se")
tkgrid(tklabel(top, text = "Colonization coefficient  "), i2Entry, sticky = "e")
tkgrid(tklabel(top, text="Both Species :", fg="blue"), sticky="w")
tkgrid(tklabel(top, text = "Prob. Extinction  "), peEntry, sticky = "se")
tkgrid(tklabel(top, text = "Habitat Destruction  "), distrEntry, sticky = "se")
tkgrid(tklabel(top, text = "Show simulation frames"), animaBox, sticky = "e")
###
tkgrid(buttonsFrame, sticky = "w", columnspan = 2)
tkgrid.configure(entryDsname, sticky = "w")
tkgrid.configure(tmaxEntry, sticky = "w")
tkgrid.configure(clEntry, sticky = "w")
tkgrid.configure(lnEntry, sticky = "w")
tkgrid.configure(fi1VarSlider, sticky = "w")
tkgrid.configure(i1Entry, sticky = "w")
tkgrid.configure(fi2VarSlider, sticky = "w")
tkgrid.configure(i2Entry, sticky = "w")
tkgrid.configure(peEntry, sticky = "w")
tkgrid.configure(distrEntry, sticky = "w")
tkgrid.configure(animaBox, sticky = "w")
###
dialogSuffix(rows = 12, columns = 2, focus = entryDsname)
}
########################END####################################
