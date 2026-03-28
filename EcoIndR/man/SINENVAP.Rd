\name{SINENVAP}
\alias{SINENVAP}
\encoding{latin1}
\title{SPATIAL INTERPOLATION OF VARIABLES WITHIN POLYGONS}
\description{An algorithm for finding an optimal spatial interpolation model for variables within polygons using kriging.}
\usage{
SINENVAP(data=NULL, var=NULL, dataLat=NULL, dataLon=NULL, polyLat=NULL,
polyLon=NULL, zonedata=NULL, zonepoly=NULL, convex=FALSE, alpha=0.07, ASC=NULL,
shape=NULL, shapenames=NULL, Area=NULL, validation=30, type.krige="OK",
trend.d="cte", trend.l="cte", model="AUTO", minimisation="optim", weights="npairs",
maxdist=NULL, nugget=NULL, sill=NULL, range=NULL, kappa=NULL, beta=NULL,
jitter="jitter", maxjitter=0.00001, direction=c(0,45,90,135), inside=TRUE,
error=FALSE, ResetPAR=TRUE, PAR=NULL, BOXPLOT=NULL, OUTLINE=FALSE, XLABP=NULL,
YLABP=NULL, XLAB=NULL, YLAB=NULL, XLABB="Model", YLABB="Accuracy measures",
MAIN="", XLIM=NULL, YLIM=NULL, ZLIM=NULL, COLOR="rev(heat.colors(100))",
COLORC="black", COLORB=NULL, COLORM="transparent", NLEVELS=10, LABCEX=0.6,
contour=TRUE, breaks=10, ndigits=0, xl=0, xr=0, pro=TRUE, cell=NULL,
file1="Predictions data.csv", file2="Predictions polygon.csv",
file3="Accuracy measures.csv", file4="Semivariogram.csv",
file5="Standard errors.csv", file6="Model selected.txt", na="NA",
dec=",", row.names=FALSE)
}
\arguments{
  \item{data}{Data file (CSV, RData or Excel) with the latitudes and longitudes and the values of the environmental variables. This file may also include the latitudes and longitudes of the polygons. Each polygon must be separated by a blank row.}
  \item{var}{Variable with the values of the environmental variable.}
  \item{dataLat}{Variable with the latitudes of the environmental variable.}
  \item{dataLon}{Variable with the longitudes of the environmental variable.}
  \item{polyLat}{If the geographic coordinates of the polygons are in the same file than the data of the variable, here it is indicated the variable with the latitudes of the polygons.}
  \item{polyLon}{If the geographic coordinates of the polygons are in the same file than the data of the variable, here it is indicated the variable with the latitudes of the polygons.}
  \item{zonedata}{If the latitude and longitude of the data are in UTM, it is necessary to specify the variable with the zone of each pair of coordinates in this argument.}
  \item{zonepoly}{If the latitude and longitude of the polygons are in UTM, it is necessary to specify the variable with the zone of each pair of coordinates in this argument.}
  \item{convex}{If it is TRUE, it is considered as polygon the alpha shape of the distribution of the variable. This option is useful if the variable is for instance the abundance of a species, so the spatial interpolation is performed considering the limits of the distribution of the species.}
  \item{alpha}{Alpha value of the alpha shape.}
  \item{ASC}{ASC file with the values of the variable. It is not necessary to specify the latitude and longitude of the variable, but it is mandatory to specify the polygon (in the argument \emph{data}, \emph{shape} or \emph{Area}.}
\item{shape}{It is possible to use a shape file for importing the coordinates of the polygons. In this case, it is not necessary to specify the latitude and longitude of the polygons in the arguments \emph{polyLat} and \emph{polyLon}.}
  \item{shapenames}{Variable in the shapefile with the names of the polygons.}
  \item{Area}{Only if using RWizard. It is also possible to use the polygons available in RWizard of administrative areas and river basins. A character with the name of the administrative area or a vector with several administrative areas (countries, regions, etc.) or river basins. In this case, it is not necessary to specify the latitude and longitude of the polygons in the arguments \emph{polyLat} and \emph{polyLon}. If it is "World" the entire world is plotted.}
  \item{validation}{Percentage of cases used from original data for validation. These data are not used for the estimation of the model and they are just used for evaluating the accuracy of the model (see details). If it is zero, all data are used for estimating the accuracy measures. If there are many data, a way for shortening the running time is to increase the number of data for validation, so reducing the number of data used for estimating the models.}
  \item{type.krige}{Type of kriging to be performed. Options are simple "SK" or ordinary kriging "OK". Kriging with external trend and universal kriging can be defined setting type.krige="OK" and specifying the trend model using the arguments \emph{trend.d} and \emph{trend.l}.}
  \item{trend.d}{It specifies the trend (covariate) values at the data locations (see function \link[geoR]{krige.conv} of the package geoR).}
  \item{trend.l}{It specifies the trend (covariate) values at prediction locations. It must be of the same type as for trend.d. Only used if prediction locations are provided in the
argument locations (see function \link[geoR]{krige.conv} of the package geoR).}
  \item{model}{If it is "AUTO", the algorithm tries to find the model with the highest accuracy measures (see details). It is also possible to select one or several of the following models: "exponential", "matern", "gaussian", "spherical", "circular", "cubic", "wave", "power", "linear", "cauchy", "gneiting", "powered.exponential", and/or "pure.nugget".}
  \item{minimisation}{Minimization function used to estimate the parameters of the model fitted to the semivariogram. The options are "optim", "nlm" or "nls" (see function \link[geoR]{variofit} of the package geoR).}
  \item{weights}{Type weights used in the loss function when fitting the model to the semivariogram. The options are "npairs", "cressie" or "equal" (see the function \link[geoR]{variofit} of the package geoR).}
  \item{maxdist}{Maximum distance in the semivariogram. If it is NULL, it is half of the maximum distance of the semivariogram.}
  \item{nugget,sill,range}{The value of the nugget variance parameter \eqn{\tau^2}, fixed value of the sill parameter (\eqn{\sigma^2}) and fixed value of the range parameter (\eqn{\phi}). If they are NULL (default) the algorithm tries to find the optimal values for each of the models specified in the argument \emph{model}.}
  \item{kappa}{One numerical value required for the following models: "matern", "cauchy", "gneiting.matern" and "powered.exponential", and two values for the model "gencauchy". If they are NULL (default) the algorithm tries to find the optimal values for each of the models specified in the argument \emph{model}.}
    \item{beta}{Numerical value of the mean (vector) parameter. Only used if type.krige="SK". If it is NULL, it is automatically estimated by the algorithm.}
  \item{jitter}{It may be one of these three options: "NO" means no action, "jitter" means that jitters duplicated coordinates of the environmental variable, and "mean" means that the mean of the environmental variable is estimated for those duplicated coordinates.}
  \item{maxjitter}{Maximum jittering distance in decimal degrees.}
  \item{direction}{A vector with values of 4 angles, indicating the directions for which the variograms will be computed. Default corresponds to c(0,45,90,135 (degrees).}
  \item{inside}{If it is TRUE only those geographic coordinates of the environmental variables inside the polygons are considered for the estimation of the model.}
  \item{error}{If it is TRUE, a contour map with the standard errors is depicted.}
  \item{ResetPAR}{If it is FALSE, the default condition of the function PAR is not placed and maintained those defined by the user in previous graphics.}
  \item{PAR}{It accesses the function PAR that allows to modify many different aspects of the graph.}
  \item{BOXPLOT}{It allows to specify the characteristics of the function \link[graphics]{boxplot}.}
  \item{OUTLINE}{If it is TRUE, the outliers are shown in the boxplot.}
  \item{XLABP,YLABP}{Legends of \emph{X} and \emph{Y} axes of the plot with the relationship between  observed and predicted values of the model.}
  \item{XLAB,YLAB}{Legends of \emph{X} and \emph{Y} axes of the contour plot with the spatial interpolation.}
  \item{XLABB,YLABB}{Legends of \emph{X} and \emph{Y} axes of the boxplot .}
  \item{MAIN}{Main title of the contour plot with the spatial interpolation.}
  \item{XLIM,YLIM,ZLIM}{Limits of the contour plot.}
  \item{COLOR}{Palette of colours or a vector with the colours of the contour plot.}
  \item{COLORC}{Colour of the lines in the contour plot.}
  \item{COLORB}{Vector with the colours of the models or just one colour for all models of the boxplot.}
  \item{COLORM}{Colour of the administrative areas and river basins, if any area has been specified in the argument \emph{Area} and \emph{convexhull=TRUE}.}
  \item{NLEVELS}{Numeric vector of levels at which to draw contour lines.}
  \item{LABCEX}{Size of the text in the contour lines.}
  \item{contour}{If it is TRUE, the contour lines are depicted in the contour plot.}
  \item{breaks}{Number of breakpoints of the colour legend in the contour plot.}
  \item{ndigits}{Number of decimals in the legend of the colour scale in the contour plot.}
  \item{xl,xr}{The left and right limits of the colour legend considering the \emph{X} axis of the contour plot.}
  \item{pro}{If it is TRUE, an automatic calculation is made in order to correct the aspect ratio y/x along latitude.}
  \item{cell}{Cell size in decimal degrees of the grid inside the polygons with the predictions of the model. If it is NULL, it is automatically estimated according to the limits of the polygons. To select an appropriate cell size according to the polygon size is important for shortening the running time.}
  \item{file1}{CSV FILES. Filename with the predictions of the models.}
  \item{file2}{CSV FILES. Filename with the predictions inside the polygons.}
  \item{file3}{CSV FILES. Filename with accuracy measures of the models.}
  \item{file4}{CSV FILES. Filename with values of the semivariogram.}
  \item{file5}{CSV FILES. Filename with standard errors of the predictions.}
  \item{file6}{TXT FILE. Model selected with indication of the accuracy measures.}
  \item{na}{CSV FILE. Text that is used in the cells without data.}
  \item{dec}{CSV FILE. It defines if the comma "," is used as decimal separator or the dot ".".}
  \item{row.names}{CSV FILE. Logical value that defines if identifiers are put in rows or a vector with a text for each of the rows.}
}
\value{It is obtained:

1. A CSV file, called "Predictions data.CSV" by default, contains model data and predictions. If \emph{validation=0}, the observed data are from the original dataset. Predicted values are those points inside the polygon, which are, spatially, the nearest neighbors to the observed data.

2. A CSV file, "Predictions polygon.CSV" by default, contains predictions for inside polygons.

3. A CSV file, called "Accuracy measures.CSV" by default, contains the values of the seven accuracy measures shown above, for all models.

4. A CSV file, called "Semivariogram.CSV" by default, contains semivariogram values.

5. A CSV file, called "Standard errors.CSV" by default, contains the standard prediction errors.

6. A TXT file called "Model selected.TXT" by default, contains the full details of the model selected by the algorithm.

7. A plot of the semivariogram, with the values used in the models, is depicted in green. Application of the maximum distance specified in the \emph{maxdist} argument, and the points in red, yield a semivariogram without distance limitations.

8. The directional variogram in four directions.

9. A plot with the relationship between observed and predicted values. As mentioned above, the observed values may be either randomly selected values or those from the original dataset. If \emph{validation=0}, the observed data are from the original dataset. Predicted values are those points inside the polygon, which are, spatially, the nearest neighbors to the observed data.

10. If the argument \emph{model} is "AUTO", or there is more than one model, a boxplot is depicted with the median value of the seven accuracy measures from each model.

11. The contour plot with the spatial interpolation predictions of the selected model, i.e., the model with the highest accuracy measures mean.

12. If the argument \emph{error=TRUE}, the contour plot is depicted with the selected model's standard errors.
}
\details{
\strong{SINENVAP algorithm}

The aim of this algorithm is to select a model, from a set of different models, with the nugget variance parameter \eqn{\tau^2}, fixed value of the sill parameter (\eqn{\sigma^2}) and fixed value of the range parameter (\eqn{\phi}), as close as possible to those values that generates an optimal spatial interpolation, and to validate the predictions obtained. The model and parameters selected by the algorithm may be utilized by users as references, or to make modifications for spatial interpolation prediction improvement.

The algorithm uses the package geoR (Ribeiro and Diggle, 2001; 2018) to estimate simple, ordinary, and universal kriging. The corresponding algorithm is detailed below.

\strong{1. Data with variable and polygon coordinates.}

The algorithm must be supplied with variable values (argument \emph{var}), latitude and longitude for each datum (arguments \emph{dataLat} and \emph{dataLon}), and polygon coordinates (arguments \emph{polyLat} and \emph{polyLon}), for the estimation of spatial interpolation.

Variable and polygon latitudes and longitudes may be in either decimal or UTM form. If they are in UTM form, a column with the zone of each coordinate in the \emph{zonedata} and \emph{zonepoly} arguments must be added for variable and polygon coordinates, respectively. Polygon variables and coordinates are not required to be in the same units. Therefore, variables may be in decimal form, and polygons in UTM form, vice versa, or both may be in the same units.

Variable and polygon information data may be in CSV, EXCEL, or RData files. ASC files (argument ASC) may also be used for the variable, but in this case, it is not necessary to provide latitude or longitude information for each datum in the \emph{dataLat} and \emph{dataLon} arguments. Polygon coordinates may be in the same file as variable data in CSV, EXCEL, or RData files.

If RWizard is used, any of the administrative areas available in RWizard may be chosen as polygons in the \emph{Area} argument: countries, departments, provinces, etc. River basins may also be used as polygons, in the database available in RWizard (Gonzalez-Vilas et al., 2015). 

Shape files may be used to import polygon coordinates with the \emph{shape} argument. If a shape file or the polygons available in RWizard are used, coordinate specification is unnecessary in \emph{polyLat} and \emph{polyLon} arguments.

Finally, if the argument \emph{convex=TRUE}, the alpha shape distribution is considered to be a polygon. This last option is useful when the variable is, for instance, the abundance of a species, such that spatial interpolation is performed considering the limits of species distribution. If \emph{convex=TRUE}, the specification of information in \emph{polyLat} and \emph{polyLon} arguments is unnecessary.

\strong{2. Algorithm design.}

Algorithm steps are described as follows:

1. If the argument \emph{inside=TRUE} (default option), based on all variable data available, only those data inside the polygons are used to estimate spatial interpolation. If the argument \emph{inside=FALSE}, all available variable data are used.

2. Duplicated coordinates may be treated in two ways: with the application of a jitter function (default) or with estimation of the mean value of the variable for duplicated coordinates.

3. Spatial interpolation estimates the values on a grid. Therefore, it is necessary to first create a grid with a fixed cell size, in which the spatial interpolation will be estimated. If the argument \emph{cell=NULL} (default), the algorithm estimates the optimal grid cell size, in accordance with polygon size. The output TXT provides information about the cell size chosen. The user may specify different cell sizes. Appropriate cell size selection, in accordance with polygon size, shortens running time. 

4. Only those points in polygons are selected from the grid created, for the spatial interpolation prediction.

5. By the default \emph{validation=30}, which means that 30\% of variable data are not used in the spatial interpolation model. However, they are employed to test the model. If \emph{validation=0}, all data are used in the model, and model validation is performed with all data. If validation is higher than zero, because validation data are randomly selected, spatial interpolation values may vary each time the script is implemented.

6. In order to test the model, the grid coordinates nearest to data coordinates are selected. Cross validation is performed by comparing variable data reserved for validation (see step 5) to spatial interpolation predictions on the grid, so as to verify which coordinates are nearest to the data reserved for validation.

7.Next, the semivariogram is depicted (Fig. 1). If the argument \emph{maxdist=NULL} (default), it is considered to be half of the maximum semivariogram distance for performing the models.

8. Any of the following models may be implemented: cauchy, circular, cubic, exponential, gaussian, gneiting, linear, matern, power, powered.exponential, pure.nugget, spherical, or wave. If \emph{model="AUTO"}, all models are tested, with the exception of matern. The \emph{variofit} function in the geoR package is used to find the nugget, range, and sill for each model. The data reserved for validation are compared to model predictions, and seven accuracy measures, described in the following section, are used to decide which model made the best prediction. The model that made the best prediction is chosen by the algorithm to depict variable spatial interpolation in the selected polygons.

\strong{3. Accuracy measures.}

The following accuracy measures are used in the algorithm, so as to compare and evaluate model predictions, where \emph{n} is the number of observations, and \emph{P} and \emph{O} are the predicted and observed values for each \emph{i} datum, respectively.

The measures catalogued as "normalized" are adapted to reflect values of 1 when the model is most efficient (i.e., predictions are the same as observed values). Thus, in all accuracy measures used in the algorithm, the maximum value is 1. This indicates a model with maximum predictive power. Various measures were utilized to obtain improved evaluation framework (i.e. consideration of a group of skill scores that show different result characteristics) (see Li and Head, 2011).

\emph{3.1. r-squared (\eqn{r^2})}. This is the square of the Pearson correlation coefficient (\emph{r}) between the predictions of the interpolation model and the observed values. It ranges from 0 to 1.
\deqn{r=\frac{\displaystyle\sum_{i=1}^{n}[(O_i-\bar{O})(P_i-\bar{P})]}{\sqrt{\displaystyle\sum_{i=1}^{n}{(O_i-\bar{O})}^2\displaystyle\sum_{i=1}^{n}{(P_i-\bar{P})}^2}}}
\emph{3.2. Normalized mean absolute error (NMAE)}. This is a measure of the average error between predictions and observed values. It ranges from \eqn{-\infty} to 1.
\deqn{NMAE=1-\frac{\displaystyle\sum_{i=1}^{n}|P_i-O_i|}{\displaystyle\sum_{i=1}^{n}O_i}}
\emph{3.3. Normalized root mean square error (NRMSE)}. This measure shows the distribution error variability. It ranges from \eqn{-\infty} to 1.
\deqn{NRMSE=1-\sqrt{\frac{\displaystyle\sum_{i=1}^{n}(P_i-O_i)^2}{\displaystyle\sum_{i=1}^{n}O_i}}}
\emph{3.4. Nash-Sutcliffe coefficient (E)}. Nash-Sutcliffe efficiencies may range from \eqn{-\infty} to 1 (Nash & Sutcliffe, 1970). An efficiency of 1 (E = 1) corresponds to a perfect match between model and observations. An efficiency of 0 indicates that the model predictions are as accurate as the mean of the observed data, whereas an efficiency less than zero (\eqn{-\infty} < E < 0) occurs when the observed mean is a better predictor than the model.
\deqn{E=1-\frac{\displaystyle\sum_{i=1}^{n}(O_i-P_i)^2}{\displaystyle\sum_{i=1}^{n}(O_i-\bar{O})^2}}
\emph{3.5. Index of agreement (d)}. It was developed by Willmott (1981) as a standardized measure of the degree of model prediction error and varies between 0 and 1.
\deqn{d=1-\frac{\displaystyle\sum_{i=1}^{n}(O_i-P_i)^2}{\displaystyle\sum_{i=1}^{n}(|P_i-\bar{O}|+|O_i-\bar{O}|)^2}}
\emph{3.6. Normalized relative mean absolute error (NRMAE)}. This is a modification of a measure developed by Li and Head (2011), whose maximum value is 1. According to the authors, this measure removes the effect of unit/scale and is not sensitive to changes in unit/scale. It ranges from \eqn{-\infty} to 1.
\deqn{NRMAE=1-\frac{\displaystyle\sum_{i=1}^{n}\frac{|P_i-O_i|}{O_i}*100}{\displaystyle\sum_{i=1}^{n}O_i}}
\emph{3.7. Normalized relative root mean square error (NRRMSE)}. This is a modification of a measure developed by Li and Head (2011), whose maximum value is 1. It ranges from 0 to 1.
\deqn{NRRMSE=1-\left[\frac{\displaystyle\sum_{i=1}^{n}\left(\frac{[P_i-O_i]}{O_i}\right)^2}{\displaystyle\sum_{i=1}^{n}O_i}\right]^\frac{1}{2}*100}
\strong{FUNCTIONS}

Spatial interpolation, using simple, ordinary, and universal kriging, is performed with as.geodata, variofit, krige.conv and krige.control functions, as well as the jitter of points with the jitterDupCoords function, all of which are from the geoR package (Ribeiro and Diggle, 2001; 2018).

The ASC files are loaded with the raster function from the raster package (Hijmans et al., 2018).

Points inside polygons are estimated with the in.out function from the mgcv package (Wood, 2018).

The sp package (Pebesma and Bivand, 2005; Pebesma et al., 2018) is used to process shape files.

The BreuschPagan test is performed with the bptest function from the lmtest package (Zeileis and Hothorn, 2002; Hothorn et al., 2018).

The color scale is depicted with the color.legend function from the plotrix package (Lemon, 2006; Lemon et al., 2018).

\strong{EXAMPLE}
The figure shows the contour map of the spatial interpolation of the example 1, the rainfall in the Iberian Peninsula.
\tabular{c}{
\if{html}{\figure{EcoIndRF3.jpg}{options: width="50"}}
\if{latex}{\figure{EcoIndRF3.jpg}{options: width=4.5in}}
}
}
\references{
Hijmans, R.J., van Etten, J., Cheng, J., Sumner, M., Mattiuzzi, M., Greenberg, J.A., Lamigueiro, O.P., Bevan, A., Bivand, R., Busetto, L., Canty, M., Forrest, D., Ghosh, A., Golicher, D., Gray, J., Hiemstra, P., Karney, C., Mosher, S., Nowosad, J., Pebesma, E., Racine, E.B., Rowlingson, B., Shortridge, A., Venables, B., Wueest, R. (2018) Geographic Data Analysis and Modeling. R package version 2.8-4. Available at: \url{https://CRAN.R-project.org/package=raster}.

Gonzalez-Vilas, L., Guisande, C., Vari, R. P., Pelayo-Villamil, P., Manjarres-Hernandez, A., Garcia-Rosello, E., Gonzalez-Dacosta, J., Heine, J., Perez-Costas, E., Granado-Lorencio, C., Palau-Ibars, A., Lobo, J. M. (2016) Geospatial data of freshwater habitats for macroecological studies: an example with freshwater fishes. \emph{Journal of Geographical Information Science}, 30: 126-141.

Lemon, J. (2006) Plotrix: a package in the red light district of R. \emph{R-News}, 6: 8-12.

Lemon, J., Bolker, B., Oom, S., Klein, E., Rowlingson, B.,Wickham, H., Tyagi, A., Eterradossi, O., Grothendieck, G., Toews, M., Kane, J., Turner, R., Witthoft, C., Stander, J., Petzoldt, T., Duursma, R., Biancotto, E., Levy, O., Dutang, C., Solymos, P., Engelmann, R., Hecker, M., Steinbeck, F., Borchers, H., Singmann, H., Toal, T. & Ogle, D. (2018). Various plotting functions. R package version 3.7-4. Available at: \url{https://CRAN.R-project.org/package=plotrix}.

Li, J. & Heap, A.D. (2011) A review of comparative studies of spatial interpolation methods in environmental sciences: Performance and impact factors. \emph{Ecological Informatics}, 6: 248-251.

Nash, J.E. & Sutcliffe, J.V. (1970) River flow forecasting through conceptual models part I - A discussion of principles. \emph{Journal of Hydrology}, 10, 282-290.

Pebesma, E., Bivand, R.S., Rowlingson, B., Gomez-Rubio, V., Hijmans, R., Sumner, M., MacQueen, D., Lemon, J. & O'Brien, J. (2018) Classes and Methods for Spatial Data. R package version 1.3-1. Available at: \url{https://CRAN.R-project.org/package=sp}.

Pebesma, E.J. & Bivand, R.S. (2005) Classes and methods for spatial data in R. \emph{R News}, 5: 9-13.

Ribeiro, P.A. & Diggle, P.J. (2001) geoR: a package for geostatistical analysis. R-News 1, 14-18.

Ribeiro, P.J. & Diggle, P.J. (2018) Analysis of Geostatistical Data. R package version 1.7-5.2.1. Available at: \url{https://CRAN.R-project.org/package=geoR}.

Willmott, C.J. (1981) On the validation of models. \emph{Physical Geography}, 2, 184-194.

Wood, S. (2018) Mixed GAM Computation Vehicle with Automatic Smoothness. R package version 1.8-26. Available at: \url{https://CRAN.R-project.org/package=mgcv}.
}
\examples{
\dontrun{

#Example 1.
#An example with the geographic coordinates of the polygons
#in the same file than the environmental variable

data(EnVarIP)

SINENVAP(data=EnVarIP, var="Rainfall", dataLat="dataLat",
dataLon="dataLon", polyLat="polyLat", polyLon="polyLon",
model=c("cubic", "spherical"), MAIN="Rainfall", dec=".")

#Example 2. Only to be used with RWizard
#An example using the administrative areas available in RWizard.

data(EnVarIP)
@_Build_AdWorld_
SINENVAP(data=EnVarIP, var="Temperature", dataLat="dataLat", dataLon="dataLon",
Area = c("Galicia>A Coruña", "Galicia>Lugo", "Galicia>Ourense",
"Galicia>Pontevedra"), model=c("spherical"), MAIN="Temperature", ndigits=1, dec=".")

#Example 3. Only to be used with RWizard
#An example with a virtual species using as polygon the alpha
#shape of the species (argument convex=TRUE).

data(VirtualSpecies)
@_Build_AdWorld_
SINENVAP(data=VirtualSpecies, var="Probability", dataLat="Lat",
dataLon="Lon", model=c("circular", "exponential"), convex=TRUE,
Area=c("France"), validation=90, COLORM="#DEDEDE64", ndigits=2, dec=".")

}
}
\keyword{SINENVAP}


