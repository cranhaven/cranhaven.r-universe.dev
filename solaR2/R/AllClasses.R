setOldClass('zoo')
setOldClass('loess')
setOldClass('difftime')
setOldClass('data.table')

#### Sol class ####
setClass(
         Class = 'Sol', ##Solar angles
         slots = c(
             lat = 'numeric',      #latitud in degrees, >0 if North
             solD = 'data.table',  #daily angles
             solI = 'data.table',  #intradaily angles
             sample = 'character', #sample of time
             method = 'character'  #method used for geometry calculations
         ),
    validity = function(object) {return(TRUE)}
)

#### Meteo class ####
setClass(
    Class = 'Meteo', ##radiation and temperature data
    slots = c(
        latm = 'numeric',    #latitud in degrees, >0 if North
        data = 'data.table', #data, incluying G (Wh/m2) and Ta(ºC)
        type = 'character',  #choose between 'prom', 'bd' and 'bdI'
        source = 'character' #origin of the data
    ),
    validity = function(object) {return(TRUE)}
)

#### G0 class ####
setClass(
    Class = 'G0',
    slots = c(
        G0D = 'data.table',  #result of fCompD
        G0dm = 'data.table', #monthly means
        G0y = 'data.table',  #yearly values
        G0I = 'data.table',  #result of fCompI
        Ta = 'data.table'    #Ambient temperature
    ),
    contains = c('Sol', 'Meteo'),
    validity = function(object) {return(TRUE)}
)

#### Gef class ####
setClass(
         Class = 'Gef',
         slots = c(
           GefD = 'data.table',  #daily values
           Gefdm = 'data.table', #monthly means
           Gefy = 'data.table',  #yearly values
           GefI = 'data.table',  #result of fInclin
           Theta = 'data.table', #result of fTheta
           iS = 'numeric',       #dirt index
           alb = 'numeric',      #albedo
           modeTrk = 'character',   #tracking mode
           modeShd = 'character',   #shadow mode
           angGen = 'list',         #includes alpha, beta and betaLim
           struct = 'list',         #structure dimensions
           distances = 'data.frame' #distances between structures
           ),
         contains = 'G0',
         validity = function(object) {return(TRUE)}
         )

#### ProdGCPV class ####
setClass(
         Class = 'ProdGCPV',
         slots = c(
           prodD = 'data.table',  #daily values
           prodDm = 'data.table', #monthly means
           prody = 'data.table',  #yearly values
           prodI = 'data.table',  #results of fProd
           module = 'list',       #module characteristics
           generator = 'list',    #generator characteristics
           inverter = 'list',     #inverter characteristics
           effSys = 'list'        #efficiency values of the system
           ),
         contains = 'Gef',
         validity = function(object) {return(TRUE)}
         )

#### ProdPVPS class ####
setClass(
         Class = 'ProdPVPS',
         slots = c(
           prodD = 'data.table',  #daily values
           prodDm = 'data.table', #monthly means
           prody = 'data.table',  #yearly values
           prodI = 'data.table',  #results of fPump
           Pg = 'numeric',        #generator power
           H = 'numeric',         #manometric head
           pump = 'list',         #parameters of the pump
           converter = 'list',    #inverter characteristics
           effSys = 'list'        #efficiency values of the system
           ),
         contains = 'Gef',
         validity = function(object) {return(TRUE)}
         )

#### Shade class ####
setClass(
         Class = 'Shade',
         slots = c(
           FS = 'numeric',  #shadows factor values
           GRR = 'numeric', #Ground Requirement Ratio
           Yf = 'numeric',  #final productivity
           FS.loess = 'loess', #local fitting of FS with loess
           Yf.loess = 'loess', #local fitting of Yf with loess
           modeShd = 'character', #mode of shadow
           struct = 'list',       #dimensions of the structures
           distances = 'data.frame', #distances between structures
           res = 'numeric'           #difference between the different steps of the calculations
           ),
         contains = 'ProdGCPV',##Resultado de prodGCPV sin sombras (Prod0)
         validity = function(object) {return(TRUE)}
         )
