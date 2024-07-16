#' @title Fitting FCS Data
#' 
#' @description Estimates the parameters based on a given equation, on the data generated with the fcs() function.
#' @aliases fitFCS
#' @usage fitFCS(data = parent.frame(), start, low = -Inf, up = Inf,
#'               type = "D3D", model = NULL, trace = TRUE)
#' @param data data frame in which to evaluate the variables in formula and weights.
#' @param start a named list or named numeric vector of starting estimates.
#' @param low,up a named list or named numeric vector of lower and upper bounds, replicated to be as long as start. If unspecified, all parameters are assumed to be -Inf and Inf.
#' @param type specification for the equation to model, is a character string. The default value is "D3D" equation for three-dimensional free diffusion.
#' Another possibles values are: "D2D" for two-dimensional free diffusion,  "D2DT" for two-dimensional free diffusion with triplet exited state, and "D3DT" for three-dimensional free diffusion with triplet exited state
#' and D3D2S for two species in three-dimensional free diffusion.
#' @param model a character type variable, that must contain the custom equation if needed, NULL by default.
#' @param trace logical value that indicates whether the progress of the non-linear regression (nls) should be printed.
#' @details A transport model, containing physical information about the diffusive nature of the fluorophores, can be fitted to the autocorrelation data to obtain parameters such as the diffusion coeficient D and the number of molecules within the observation volume N.
#' 
#' The fitFCS() function uses the 'Non-linear Least Squares' function to fit a physical model into a data set. There are four possible models to be fit:
#' 
#' "D2D" for two-dimensional diffusion
#' 
#' "D2DT" for two-dimensional diffusion with triplet state
#' 
#' "D3D" for three-dimensional diffusion
#' 
#' "D3DT" for three-dimensional diffusion with triplet state
#' 
#' Inside the equations for each model, gamma a geometric factor that depends on the illumination profile. For confocal excitation its magnitude approaches gamma = 1/sqrt8 ??? 0.35 fl. The diffusion time is defined as tau_D = s^2/4D, where s and u are the radius and the half-length of the focal volume, respectively. The parameter u is usually expressed as u = ks, with k being the eccentricity of the focal volume; for confocal excitation k ??? 3. The fraction of molecules in the triplet state is B, and tau_B is a time constant for the triplet state.
#' 
#' @export
#' @importFrom stats nls formula
#' @return A nls object (from nls).
#' @author Raúl Pinto Cámara.
#' 
#' @seealso \code{\link{nls}}, \code{\link{fcs}}
#' 
#' @examples
#' \donttest{
#' # Load the FCSlib package
#' 
#' library(FCSlib)
#' 
#' g <- fcs(x = Cy5$f,nPoints = 2048)
#' len <- 1:length(g)
#' tau <-Cy5_100nM$t[len]
#' G<-data.frame(tau,g)
#' G<-G[-1,]
#' 
#' # Once the correlation curve 'g' has been generated,
#' # a data frame containing known parameters must be then defined
#' 
#' df<-data.frame(G, s = 0.27, k = 3)
#' head(df)
#' 
#' # The radius of the focal volume must computed experimentally.
#' # For this example, we choose a s = 0.27~ mu m
#' # Then, three lists that contain the initial values of the data,
#' # as well as the upper and lower limits of these values, must be defined.
#' # The input values here are the expected values for the real experimental data
#' # to be very similar or close to, so that the function calculates them accurately.
#' # Initial values:
#' 
#' start <- list(D = 100, G0 = 0.1)
#' up <- list(D = 1E3, G0 = 10)
#' low <- list(D = 1E-1, G0 = 1E-2)
#' 
#' # Once the known parameters are defined, we now proceed to use the fitFCS() function.
#' # The result will be a nls object
#' 
#' modelFCS <- fitFCS(df, start, low, up, type = "D3D", trace = F)
#' # summary(modelFCS)
#' 
#' # By using the predict() function, the object generated in the previous step
#' # is transformed into a vector that contains the curve fitted by the desired model.
#' 
#' fit <- predict(modelFCS, tau)
#' 
#' # Finally, use the following command to obtain the resulting graph,
#' # where the blue line corresponds to the fitted data and the black surface
#' # corresponds to the unfitted
#' 
#' plot (G, log = "x", type = "l", xlab = expression(tau(s)),
#'       ylab = expression(G(tau)), main = "Cy5")
#' lines(fit~G$tau, col = "blue")
#' 
#' # To acquire access to the physical coefficients of the model type
#' 
#' s<-summary(modelFCS)
#' s$coefficients[,1]
#' }



fitFCS <- function(data = parent.frame(), start, low = -Inf, up = Inf, type = "D3D", model = NULL, trace = TRUE){
  if(!is.list(start)){
    stop("'start' must be a list")
  }
  if (!is.list(data) && !is.environment(data)){
    stop("'data' must be a list or an environment")
  }
  GTrip <- "(1+((B*exp(-(tau)/taub))/(1-B)))"
  D2D <- "((G0) * ((1 + ((4 * D * tau)/(s ^ 2))) ^ (-1))"
  D2DT <- paste(GTrip, "*", D2D)
  D3D <- "((G0) * ((1 + ((4 * D * tau)/(s ^ 2))) ^ (-1)) * ((1 + ((4 * D * tau)/((k^2) * (s^2)))) ^ (-1 / 2)))"
  D3DT <- paste(GTrip, "*", D3D)
  D3D2S <- "(G0) * (((Y) * ((1 + ((4 * D1 * tau)/(s ^ 2))) ^ (-1)) * ((1 + ((4 * D1 * tau)/((k^2) * (s^2)))) ^ (-1 / 2))) + ((1 - Y) * ((1 + ((4 * D2 * tau)/(s ^ 2))) ^ (-1)) * ((1 + ((4 * D2 * tau)/((k^2) * (s^2)))) ^ (-1 / 2))))"
  list_type <- list("D2D", "D2DT", "D3D", "D3DT", "D3D2S"); list_model <- list(D2D, D2DT, D3D, D3DT, D3D2S)
  if(!is.null(model)){
    if(!is.character(model)){
      stop("'model' must be a character string")
    }
  } else{
    if(!type %in% list_type){
      stop(paste("The model", type, "does not exist"))
    } else{
      typePosList <- which(list_type == type); model <- paste("g ~ ", list_model[[typePosList]], " + c", sep = "")
    }
  }
  express <- formula(model)
  modelFCS <- nls(formula = express, data = data, start = start, trace = trace, algorithm = "port", lower = low, upper = up)
  return(modelFCS)
}
