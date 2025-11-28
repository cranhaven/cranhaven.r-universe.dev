{smcl}
{*  16 July 2015}{...}
{cmd:help Rpovmap}
{hline}


{title:Title}

    Wrapper for running the R package ‘povmap’ for small area estimation from within Stata

	
{title:Syntax}

{p 8 17 2}
{opt Rpovmap} {depvar} {indepvars} {cmd:,}
{cmdab:smp_data(}{it:string}{cmd:)}
{cmdab:pop_data(}{it:string}{cmd:)}
{cmdab:smp_domains(}{it:string}{cmd:)}
{cmdab:pop_domains(}{it:string}{cmd:)}
[{it:options}]


{synoptset 26 tabbed}{...}
{synopthdr}
{synoptline}
{synopt:{opt pop_data(string)}} specifies the file name of the population data file. Please include the .dta extension. If no path is given, the current directory is assumed. {p_end} 
{synopt:{opt pop_domains(string)}} specifies the name of the variable that indicates domains in the population data. {p_end} 
{synopt:{opt smp_data(string)}} specifies the file name of the sample data file. If no path is given, the current directory is assumed. {p_end} 
{synopt:{opt smp_domains(string)}} specifies the name of the variable that indicates domains in the sample data. {p_end} 

{title:Options}
{synopt:{opt l(integer)}} a number determining the number of Monte-Carlo simulations that must be at least 1. Defaults to 50 {p_end}
{synopt:{opt threshold(string)}} a number defining the poverty line threshold for calculating headcount rates. Defaults to 60% of the median of the dependent variable {p_end}
{synopt:{opt transformation(string)}} specifies the transformation to be applied to the dependent variable. Options are "no","log","box.cox","dual","log.shift",and "ordernorm". Defaults to "box.cox"  {p_end} 
{synopt:{opt interval(string)}} a string equal to 'default' or a numeric vector containing a lower and upper limit determining an interval for the estimation of the optimal parameter. Defaults to "c(-1,2)"  {p_end} 
{synopt:{opt mse(string)}} if TRUE, MSE estimates using a parametric bootstrap approach are calculated. Defaults to FALSE {p_end} 
{synopt:{opt b(integer)}} a number determining the number of bootstrap populations in the parametric bootstrap approach used in the MSE estimation. The number must be greater than 1. Defaults to 50. For practical applications, values larger than 200 are recommended {p_end} 
{synopt:{opt seed(integer)}} an integer to set the seed for the random number generator. For the usage of random number generation. Defaults to 123 {p_end} 
{synopt:{opt boot_type(string)}} character string to choose between different MSE estimation procedures. The two options are "parametric" and a semi-parametric "wild" bootstrap. Defaults to "parametric". {p_end} 
{synopt:{opt cpus(integer)}} an integer determining the number of cores used when estimating the MSE. Defaults to 1. {p_end} 
{synopt:{opt na_rm(string)}} if TRUE, observations with NA values are deleted from the population and sample data. For the EBP procedure complete observations are required. Defaults to FALSE. {p_end} 
{synopt:{opt weights(string)}} specifies the name of the weight variable contained in the sample data. Default is no weights. {p_end} 
{synopt:{opt pop_weights(string)}} specifies the name of the weight variable contained in the population data to use when aggregating estimates. Default is no weights.  {p_end} 
{synopt:{opt aggregate_to(string)}} a character string specifying the name of a variable from the population data to calculate estimates for. Default is the variable specified in the smp_domain option.  {p_end} 
{synopt:{opt weights_type(string)}} a character string. Three different options for survey weights are available (i) EBP under informative sampling from Guadarrama et al. (2018) ("Guadarrama"); (ii) considering survey weights by using the weighting options of nlme from Pinheiro and Bates (2023) when estimating the linear mixed model ("nlme"); (iii) considering survey weights by using the weighting options of nlme and also using the specified weights when determining the optimal transformation parameter lambda ("nlme_lambda"). Defaults to "Guadarrama". {p_end} 
{synopt:{opt benchmark(string)}} An R vector containing the names of the indicators to be benchmarked, i,e. a value of c("Mean","Headcount") to apply internal benchmark to both the mean and the headcount rate. To apply external benchmarking, specify the name of the variable in the sample data that contains the target benchmark values.  Defaults to NULL {p_end}
{synopt:{opt benchmark_type(string)}}  A character indicating the type of benchmarking. The two options are (i) Raking ("raking") and (ii) Ratio adjustment ("ratio"). Defaults to "ratio"' {p_end}
{synopt:{opt benchmark_level(string)}}  A character specifying the variable in the sample and population data indicating the level at which the benchmarking is performed. This option must be specified if the benchmark option is selected. {p_end}
{synopt:{opt benchmark_weights(string)}}  A character specifying the variable in the sample used to weight the sample data when calculating the values to bencmhark to. Defaults to the variable specified in the "weights" option {p_end}
{synopt:{opt nlme_maxiter(integer)}} an integer indicating the maximum number of iterations the lme function from package nlme will carry out before exiting with a non-convergence error message. Defaults to 1000. {p_end}
{synopt:{opt nlme_tolerance(real)}} an integer indicating the tolerance criterium for the the lme function from package nlme. Defaults to 1e-6. {p_end}
{synopt:{opt rescale_weights(string)}}  A logical indicating if the weights in the sample data are scaled. If FALSE (default), the sample weights do not change. When TRUE, the sample weights are rescaled such that the average weight is 1 within each target domain. {p_end}
{synopt:{opt saveobject(string)}}  A string specifying the file name in which to save the emdi object created by the povmap ebp function. {p_end}
{synopt:{opt savexls(string)}}  A string indicating the file name in which to save the small area estimates. Either savexls or saveobject must be specified by the user {p_end}


{title:Description}

{pstd}
Generates small area estimates using Empirical Best Predictor models using the R package ‘povmap’ from within Stata. 


{title:Dependencies}

{pstd}
{hi:R} -- The R software system must be installed on the user's system in order for {hi:Rpovmap} to run. You can download R from https://www.r-project.org)
{hi:R} -- Rpovmap requires the haven and povmap packages to be installed in R. Executing install.packages("haven") and install.packages("povmap") in R will install these packages from CRAN 

{pstd}
{hi: rscript} -- Additionally, the Stata package rscript is required to run an R script from within Stata. You can type "ssc install rscript" to install the rscript package

{title:Examples}

{pstd}
Generating small area estimates of mean adult-equivalent income using the sample data

{pstd}
Rpovmap eqIncome gender eqsize cash self_emp unempl_ben age_ben surv_ben sick_ben dis_ben rent fam_allow house_allow cap_inv tax_adj, pop_data(eusilcA_pop.dta) smp_data(eusilcA_smp.dta) smp_domains(district) pop_domains(district) weights(weight) weights_type(Guadarrama) transformation(log) na_rm(TRUE) saveobject(emdi_model_Guadarrama) savexls(emdi_model_Guadarrama.xlsx)

{pstd}
import excel using "emdi_model_Guadarrama", sheet("Point Estimators") firstrow clear  

{phang}
list Domain Mean in 1/5, clean noobs

Note: eusilca_smp.dta and eusilcA_pop.dta must either be present in the current stata directory, or their path must be specified prior to the filename in the pop_data and smp_data options. 

{title:Saved Results}

{pstd} 
{hi:Rpovmap} The saveobject and savexls options are used to specify file names to save results 

{title:Installation Instructions}

{pstd}
{hi:1.} Ensure you have the R executable installed. If you don't have this installed, you can download and install the software from https://www.r-project.org/. 

{pstd}
{hi:2.} Install the Stata dependency rscript in Stata. {stata "ssc install Rscript"}

{pstd}
{hi:3.}  By default, rscript will search for the R executable on its own. If rscript cannot find it, the location of the Rterm executable file may need to be manually set using the global {hi:RSCRIPT_PATH}. See the rscript help file for details. 


    
	
{title:Authors}

{pstd} 
Ifeanyi Edochie, Poverty Global Practice, World Bank, Washington, D.C. 
David Newhouse, Development Economics Data Group, World Bank, Washington, D.C. 
Timo Schmid, Otto Friedrich University Bamberg, Bamberg, Germany  
Nora Wurz, Otto Friedrich University Bamberg, Bamberg, Germany 

{pstd} 
Email {browse   "mailto:iedochie@worldbank.org":iedochie@worldbank.org} 



