#' A class to store the important information of an model. 
#' 
#' The slots are used to store the important information of an model. The class is used to create object for the
#' two algorithms implemented in seeds. Methods are implemented to easily calculate the nominal solution of the model and
#' change the details of the saved model.
#' The numerical solutions are calculated using the \pkg{deSolve} - package. 
#' 
#' @slot func A funtion containing the ode-equations of the model. For syntax look at the given examples of the \pkg{deSolve} package.
#' @slot times timesteps at which the model should be evaluated
#' @slot parms the parameters of the model
#' @slot input matrix containing the inputs with the time points
#' @slot measFunc function that converts the output of the ode solution
#' @slot y initial (state) values of the ODE system, has to be a vector
#' @slot meas matrix with the (experimental) measurements of the system
#' @slot sd optional standard deviations of the measurements, is used by the algorithms as weights in the costfunction
#' @slot custom customized link function
#' @slot nnStates bit vector that indicates if states should be observed by the root function
#' @slot nnTollerance tolerance at which a function is seen as zero
#' @slot resetValue value a state should be set to by an event
#' 
#' @return an object of class odeModel which defines the model
#' 
#' @export odeModel
#' @exportClass odeModel
#'
#' @import methods
#' 
odeModel <- setClass(
#name of Class
  "odeModel",
  slots = c(
    func = "function",
    times = "numeric",
    parms = "numeric",
    input = "data.frame",
    measFunc = "function",
    y = "numeric",
    meas = "data.frame",
    sd = "data.frame",
    custom = 'logical',
    nnStates = 'numeric',
    nnTollerance = 'numeric',
    resetValue = "numeric"
  ),

  prototype = list(
    func = function(x) { },
    times = numeric(),
    parms = numeric(),
    input = data.frame(matrix(numeric(0), ncol = 0)),
    measFunc = function(x) { },
    y = numeric(0),
    meas = data.frame(matrix(numeric(0), ncol = 0)),
    sd = data.frame(matrix(numeric(0), ncol = 0)),
    custom = FALSE,
    nnStates = numeric(),
    nnTollerance = numeric(),
    resetValue = numeric()
  ),
  validity = function(object) {
    # check inputs of matrix slot

    if (sum(object@times) == 0) {
      return("You have to specify the times on which the equation should be evaluated. A solution can only be calculated if the a intervall or specific timesteps are given. Set the 'times'' parameter.")
    }

    if (length(object@y) != 0 && object@custom == FALSE && sum(colSums(object@meas)) != 0) {
      m <- matrix(rep(0, length(object@y)), ncol = length(object@y))
      if (is.null(object@measFunc(m)) == FALSE) {
        testMeas <- object@measFunc(m)
        if (ncol(testMeas) != (ncol(object@meas) - 1)) {
          return("The returned results of the measurement function does not have the same
                   dimensions as the given measurements")
        }
      }
    }

    return(TRUE)
  }
)

setMethod('initialize', "odeModel", function(.Object, ...) {
  .Object <- callNextMethod()
  return(.Object)
})

checkMatrix <- function(argMatrix) {
  if (sum(argMatrix) == 0) {
    argName <- toString(deparse(substitute(argMatrix)))
    errortext <- ' has to contain values not equal to 0.'
    return(paste0(argName, errortext))
  }
}




#' Set the model equation
#' 
#' Set the model equation of the system in an odeModel object. Has to be a function that can be used with the deSolve package.
#' 
#' @param odeModel an object of the class odeModel
#' @param func function describing the ode equation of the model 
#' 
#' @return an object of odeModel
#' 
#' @examples 
#' data("uvbModel")
#' 
#' uvbModelEq <- function(t,x,parameters) {
#'   with (as.list(parameters),{
#'     
#'     dx1 = ((-2) * ((ka1 * (x[1]^2) * (x[4]^2)) - (kd1 * x[5])) + 
#'              (-2) * ((ka2 * (x[1]^2) * x[2]) - (kd2 * x[3])) + 
#'              ((ks1 *((1) + (uv * n3 * (x[11] + fhy3_s))))  - 
#'                 (kdr1 * ((1) + (n1 * uv)) * x[1])))
#'     dx2 = ((-1) * ((ka2*(x[1]^2) * x[2]) - (kd2 * x[3])) +
#'              (-1) * ((ka4 * x[2] * x[12]) - (kd4 * x[13])))
#'     dx3 = (((ka2 * (x[1]^2) * x[2]) - (kd2*  x[3]))) 
#'     dx4 = ((-2) * (k1*(x[4]^2)) + (2) * (k2 * x[6]) + 
#'              (-2) * ((ka1 * (x[1]^2)* (x[4]^2)) - (kd1 * x[5])) +
#'              (-1)* (ka3 * x[4] *x[7]))
#'     dx5 =  (((ka1 * (x[1]^2) * (x[4]^2)) -(kd1 * x[5])))
#'     dx6 = ((-1) * (k2 * x[6]) +  (k1 * (x[4]^2)) +(kd3 * (x[8]^2)))
#'     dx7 = ((-1) * (ka3 * x[4] * x[7]) + ((ks2 * ((1) + (uv * x[5]))) - 
#'                                            (kdr2 * x[7])) + (2) * (kd3 * (x[8]^2)))
#'     dx8 = ((-2) * (kd3 * x[8]^2) + (ka3 * x[4] * x[7])) 
#'     dx9  = 0 
#'     dx10 = 0
#'     dx11 =  (((ks3 * ((1) + (n2 * uv))) -(kdr3 * (((x[3] / (kdr3a + x[3])) + 
#'             (x[13] / (kdr3b + x[13]))) -(x[5] / (ksr + x[5]))) *  x[11])))
#'     dx12 = ((-1) * (ka4 * x[2] * x[12]) + (kd4 * x[13]))
#'     dx13 =((ka4 * x[2] * x[12]) - (kd4 * x[13]))
#'     
#'     list(c(dx1,dx2,dx3,dx4,dx5,dx6,dx7,dx8,dx9,dx10,dx11,dx12,dx13))
#'   })
#' }
#' 
#' setModelEquation(uvbModel,uvbModelEq)
#' 
#' @export
setGeneric(name = "setModelEquation",
           def = function(odeModel, func) {
            standardGeneric("setModelEquation")
           }
)

#' @rdname setModelEquation
setMethod(f = "setModelEquation",
          signature = "odeModel",
          definition = function(odeModel, func) {
            odeModel@func <- func
            validObject(odeModel)
            return(odeModel)
          }
)


#' Set the model parameters
#' 
#'  A method to set the model parameters of an odeModel object.
#' 
#' @param odeModel an object of the class odeModel
#' @param parms a vector containing the parameters of the model 
#' 
#' @examples 
#' data("uvbModel")
#' 
#' newParas <- c(  ks1=0.23,
#' ks2=4.0526,
#' kdr1=0.1,
#' kdr2=0.2118,
#' k1=0.0043,
#' k2=161.62,
#' ka1=0.0372,
#' ka2=0.0611,
#' ka3=4.7207,
#' kd1=94.3524,
#' kd2=50.6973,
#' kd3=0.5508,
#' ks3=0.4397,
#' kdr3=1.246,
#' uv=1,
#' ka4=10.1285,
#' kd4=1.1999,
#' n1=3,
#' n2=2,
#' n3=3.5,
#' kdr3a=0.9735,
#' kdr3b=0.406,
#' ksr=0.7537,
#' fhy3_s=5)
#' 
#' newModel <- setParms(odeModel = uvbModel, parms = newParas)
#' 
#' @return an object of odeModel
#' 
#' @export
setGeneric(name = "setParms",
           def = function(odeModel, parms) {
            standardGeneric("setParms")
           }
)

#' @rdname setParms
setMethod(f = "setParms",
          signature = c("odeModel", 'numeric'),
          definition = function(odeModel, parms) {
            odeModel@parms <- parms
            validObject(odeModel)
            return(odeModel)
          }
)

#' Set the inputs of the model. 
#' 
#' It the model has an input it can be set with this function. The inputs
#' should be a dataframe, where the first column is the timesteps of the
#' inputs in the second column.
#' 
#' @param odeModel an object of the class modelClass
#' @param input function describing the ode equation of the model 
#' 
#' @return an object of odeModel
#' 
#' @examples 
#' 
#' data("uvbModel")
#' 
#' model_times <- uvbModel@times
#' input <- rep(0,length(model_times))
#'
#' input_Dataframe <- data.frame(t = model_times, u = input)
#'
#' newModel <- setInput(odeModel = uvbModel,input = input_Dataframe)
#' 
#' @export
setGeneric(name = "setInput",
           def = function(odeModel, input) {
            standardGeneric("setInput")
           }
)

#' @rdname setInput 
setMethod(f = "setInput",
          signature = "odeModel",
          definition = function(odeModel, input) {
            odeModel@input <- input
            validObject(odeModel)
            return(odeModel)
          }
)




#' Set the measurement equation for the model
#' 
#' For a given model a measurement equation can be set. If no measurement function is set the
#' states become the output of the system. The function should be defined as in the example below.
#' 
#' @param odeModel an object of the class odeModel
#' @param measFunc measurement function of the model. Has to be a R functions.
#' @param custom custom indexing for the measurement function (used by the baysian method)
#' 
#' @return an object of odeModel
#' 
#' @examples 
#' 
#' data("uvbModel")
#' 
#' uvbMeasure <- function(x) {
#' 
#'   y1 = 2*x[,5] + x[,4] + x[,8]
#'   y2 = 2*x[,5] + 2* x[,3] + x[,1]
#'   y3 = x[,6]
#'   y4 = x[,11]
#'   y5 = x[,4]
#' 
#'   return(cbind(y1,y2,y3,y4,y5))
#'   }
#' 
#' newModel <- setMeasFunc(odeModel = uvbModel, measFunc = uvbMeasure)
#' 
#' @export
setGeneric(name = "setMeasFunc",
           def = function(odeModel, measFunc, custom) {
            standardGeneric("setMeasFunc")
           }
)

#' @rdname setMeasFunc
setMethod(f = "setMeasFunc",
          signature = c('odeModel', 'function', 'missing'),
          definition = function(odeModel, measFunc, custom) {
            odeModel@measFunc <- measFunc
            validObject(odeModel)
            return(odeModel)
          }
)


#' @rdname setMeasFunc
setMethod(f = "setMeasFunc",
          signature = c('odeModel', 'function', 'logical'),
          definition = function(odeModel, measFunc, custom) {
            odeModel@meas <- measFunc
            odeModel@custom <- custom
            validObject(odeModel)
            return(odeModel)
          }

)

#' Set the vector with the initial (state) values
#' 
#' @param odeModel an object of the class odeModel
#' @param y vector with the initial values
#' 
#' @return an object of odeModel
#' 
#' @examples 
#' 
#' data("uvbModel")
#' 
#' x0 = c(0.2,10,2,0,0,20,0,0,0,4.2,0.25,20,0)
#' 
#' newModel <- setInitState(uvbModel, y = x0)
#' 
#' @export
setGeneric(name = "setInitState",
           def = function(odeModel, y) {
            standardGeneric("setInitState")
           }
)

#' @rdname setInitState
setMethod(f = "setInitState",
          signature = "odeModel",
          definition = function(odeModel, y) {
            odeModel@y <- y
            validObject(odeModel)
            return(odeModel)
          }
)


#' set measurements of the model
#' 
#' The odeModel object stores all important information. Measurements of the objects can be set
#' directly by adressing the slot, or with this function.
#' 
#' @param odeModel an object of the class odeModel
#' @param meas measurements of the model, a matrix with measurements of the model
#' and the corresponding time values
#' 
#' @return an object of odeModel
#' 
#' @examples 
#' 
#' data(uvbData)
#' data(uvbModel)
#' 
#' measurements <- uvbData[,1:6]
#' 
#' newModel <- setMeas(odeModel = uvbModel, meas = measurements)
#' 
#' @export
setGeneric(name = "setMeas",
           def = function(odeModel, meas) {
            standardGeneric("setMeas")
           }
)

#' @rdname setMeas
setMethod(f = "setMeas",
          signature = 'odeModel',
          definition = function(odeModel, meas) {
            odeModel@meas <- meas
            validObject(odeModel)
            return(odeModel)
          }
)

#' Set the standard deviation of the measurements
#' 
#' With multiple measurements a standard deviation can be calculated for every point of
#' measurement. The standard deviation is used to weigh the estimated data points in the 
#' cost function.
#' 
#' @param odeModel an object of the class odeModel
#' @param sd a matrix with the standard deviations of the measurements
#' 
#' @return an object of odeModel
#' 
#' @examples 
#' 
#' data(uvbData)
#' data(uvbModel)
#' 
#' sd_uvb <- uvbData[,7:11]
#'
#' newModel <- setSd(odeModel = uvbModel, sd = sd_uvb)
#' 
#' @export
setGeneric(name = "setSd",
           def = function(odeModel, sd) {
            standardGeneric("setSd")
           }
)

#' @rdname setSd
setMethod(f = "setSd",
          signature = "odeModel",
          definition = function(odeModel, sd) {
            odeModel@sd <- sd
            validObject(odeModel)
            return(odeModel)
          }
)

#### generate c code (interal function)
setGeneric(name = 'genCCode',
           def = function(odeModel, bden, nnStates) {
            standardGeneric('genCCode')
           }
)


setMethod(f = 'genCCode',
          signature = c('odeModel', 'logical', 'missing'),
          definition = function(odeModel, bden, nnStates) {
            createCompModel(modelFunc = odeModel@func, parameters = odeModel@parms, bden = bden)
            return(odeModel)
          }
)

setMethod(f = 'genCCode',
          signature = c('odeModel', 'logical', 'numeric'),
          definition = function(odeModel, bden, nnStates) {
            createCompModel(modelFunc = odeModel@func, parameters = odeModel@parms, bden = bden, nnStates = nnStates)
            return(odeModel)
          }
)

setMethod(f = 'genCCode',
          signature = c('odeModel', 'missing', 'numeric'),
          definition = function(odeModel, bden, nnStates) {
            createCompModel(modelFunc = odeModel@func, parameters = odeModel@parms, nnStates = nnStates)
            return(odeModel)
          }
)

# nominal solution



#' Calculate the nominal solution of the model
#' 
#' After an model is defined it can be evaluated. This returns the numerical solution
#' for the state equation before hidden inputs are calculated.
#' 
#' @param odeModel a object of the class ode model describing the experiment
#' 
#' @return a matrix with the numeric solution to the nominal ode equation
#' 
#' @examples 
#' 
#' lotka_voltera <- function (t, x, parameters) {
#' with(as.list(c(x,parameters)), {
#'  dx1 = x[1]*(alpha - beta*x[2])
#'   dx2 = -x[2]*(gamma - delta*x[1])
#'  return(list(c(dx1, dx2)))
#' })
#' }
#' 
#' pars <- c(alpha = 2, beta = .5, gamma = .2, delta = .6)
#' init_state <- c(x1 = 10, x2 = 10)
#' time <- seq(0, 100, by = 1)
#' lotVolModel = odeModel(func = lotka_voltera, parms = pars, times = time, y = init_state)
#' nominalSol(lotVolModel) 
#' 
#' @export
setGeneric(name = 'nominalSol',
           def = function(odeModel) {
            standardGeneric('nominalSol')
           }
)

#' @rdname nominalSol
setMethod(f = 'nominalSol',
          signature = c('odeModel'),
          definition = function(odeModel) {

            x0 <- odeModel@y
            ### get the times from the measurements
            # add case for missing input

            times <- odeModel@times
            if (sum(colSums(odeModel@input)) == 0) {
              input <- rep(0, length(times))
              uList = list(cbind(times, input))
            } else {
              input <- odeModel@input
              u <- apply(X = input[, -1, drop = F], MARGIN = 2, FUN = function(x) stats::approx(x = input[, 1], y = x, xout = times, rule = 2))
              uList = list(cbind(times, u[[1]]$y))
            }

            w <- matrix(rep(0, length(x0) * length(times)), ncol = length(x0))


            if (grepl("Rtools", Sys.getenv('PATH')) || (.Platform$OS.type != "windows")) {


              createCompModel(modelFunc = odeModel@func, parameters = odeModel@parms, nnStates = odeModel@nnStates)
              temp_compiled_model <- compileModel()
              
              
              wSplit <- split(w, rep(1:ncol(w), each = nrow(w)))
              wList <- lapply(wSplit, FUN = function(x) cbind(times, x))
              forcings <- c(uList, wList)

              if (sum(odeModel@nnStates) == 0) {

                resOde <- deSolve::ode(y = odeModel@y, times = times, func = "derivsc",
                                         parms = odeModel@parms, dllname = "model", initforc = "forcc",
                                         forcings = forcings, initfunc = "parmsc")

              } else {

                eventTol <- 0.0
                resetValue <- 0.0001

                myRoot <- eval(parse(text = createRoot(rootStates = odeModel@nnStates)))
                myEvent <- eval(parse(text = createEvent(tolerance = eventTol, value = resetValue)))

                resOde <- deSolve::lsoda(y = odeModel@y, times = times, func = "derivsc",
                                         parms = odeModel@parms, dllname = "model", initforc = "forcc",
                                         forcings = forcings, initfunc = "parmsc", nroot = sum(odeModel@nnStates),
                                         rootfunc = "myroot", events = list(func = myEvent, root = TRUE))

              }


              dyn.unload(temp_compiled_model)

            } else {
              
              odeEq <- new("odeEquations")
              odeEq <- createModelEqClass(odeEq, odeModel@func)

              # !!!!!! check if the non rtools variant runs
              odeEq <- isDynElaNet(odeEq)
              odeEq <- calculateCostate(odeEq)
              createFunctions(odeEq)
              
              
              if (.Platform$OS.type != "windows"){
                temp_hidden_input_path <- paste0(tempdir(),'/','stateHiddenInput.R')
              } else {
                temp_hidden_input_path <- paste0(tempdir(),'\\','stateHiddenInput.R')
              }

              e <- new.env()
              source(temp_hidden_input_path, local = e)
              hiddenInputState <- get('hiddenInputState', envir = e)

             
              zeros_input = list(cbind(times, rep(0, length(times)))) 
              input$w <- apply(X = w, MARGIN = 2, FUN = function(x) stats::approxfun(x = times, y = x, method = 'linear', rule = 2))
              input$u <- apply(X = w[,1:2], MARGIN = 2, FUN = function(x) stats::approxfun(x = times, y = x, method = 'linear', rule = 2))
              input$optW = rep(1,length(odeModel@y))

              if (sum(odeModel@nnStates) == 0) {

                resOde <- deSolve::ode(y = odeModel@y,
                                       func = hiddenInputState,
                                       times = times,
                                       parms = odeModel@parms,
                                       input = input)



              } else {
                
                eventTol <- 0.0
                resetValue <- 0.0001

                myRoot <- eval(parse(text = createRoot(rootStates = odeModel@nnStates)))
                myEvent <- eval(parse(text = createEvent(tolerance = eventTol, value = resetValue)))

                resOde <- deSolve::ode(y = odeModel@y,
                                        times = times,
                                        func = hiddenInputState,
                                        parms = odeModel@params,
                                        input = input,
                                        events = list(func = myEvent, root = TRUE),
                                        rootfun = myRoot)
              }


            }



            return(resOde)
          }
)








