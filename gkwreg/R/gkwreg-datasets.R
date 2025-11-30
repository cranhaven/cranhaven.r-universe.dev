#' Gasoline Yield from Crude Oil
#'
#' Operational data on the proportion of crude oil converted to gasoline after
#' distillation and fractionation processes.
#'
#' @format A data frame with 32 observations on 6 variables:
#' \describe{
#'   \item{yield}{numeric. Proportion of crude oil converted to gasoline after
#'     distillation and fractionation (response variable).}
#'   \item{gravity}{numeric. Crude oil gravity in degrees API (American Petroleum
#'     Institute scale).}
#'   \item{pressure}{numeric. Vapor pressure of crude oil in pounds per square
#'     inch (psi).}
#'   \item{temp10}{numeric. Temperature in degrees Fahrenheit at which 10\% of
#'     crude oil has vaporized.}
#'   \item{temp}{numeric. Temperature in degrees Fahrenheit at which all gasoline
#'     has vaporized (end point).}
#'   \item{batch}{factor. Batch indicator distinguishing the 10 different crude
#'     oils used in the experiment.}
#' }
#'
#' @details
#' This dataset was collected by Prater (1956) to study gasoline yield from crude
#' oil. The dependent variable is the proportion of crude oil after distillation
#' and fractionation. Atkinson (1985) analyzed this dataset using linear
#' regression and noted that there is "indication that the error distribution is
#' not quite symmetrical, giving rise to some unduly large and small residuals".
#'
#' The dataset contains 32 observations. It has been noted (Daniel and Wood, 1971,
#' Chapter 8) that there are only ten sets of values of the first three
#' explanatory variables which correspond to ten different crudes subjected to
#' experimentally controlled distillation conditions. These conditions are captured
#' in variable `batch` and the data were ordered according to the ascending order
#' of `temp10`.
#'
#' @source
#' Taken from Prater (1956).
#'
#' @references
#' Atkinson, A.C. (1985). \emph{Plots, Transformations and Regression: An
#' Introduction to Graphical Methods of Diagnostic Regression Analysis}. New York:
#' Oxford University Press.
#'
#' Cribari-Neto, F., and Zeileis, A. (2010). Beta Regression in R.
#' \emph{Journal of Statistical Software}, \strong{34}(2), 1--24.
#' \doi{10.18637/jss.v034.i02}
#'
#' Daniel, C., and Wood, F.S. (1971). \emph{Fitting Equations to Data}. New York:
#' John Wiley and Sons.
#'
#' Ferrari, S.L.P., and Cribari-Neto, F. (2004). Beta Regression for Modeling
#' Rates and Proportions. \emph{Journal of Applied Statistics}, \strong{31}(7),
#' 799--815.
#'
#' Prater, N.H. (1956). Estimate Gasoline Yields from Crudes.
#' \emph{Petroleum Refiner}, \strong{35}(5), 236--238.
#'
#' @examples
#' \donttest{
#' require(gkwreg)
#' require(gkwdist)
#'
#' data(GasolineYield)
#'
#' # Example 1: Kumaraswamy regression with batch effects
#' # Model mean yield as function of batch and temperature
#' # Allow precision to vary with temperature (heteroscedasticity)
#' fit_kw <- gkwreg(yield ~ batch + temp | temp,
#'   data = GasolineYield,
#'   family = "kw"
#' )
#' summary(fit_kw)
#'
#' # Interpretation:
#' # - Alpha (mean): Different batches have different baseline yields
#' #   Temperature affects yield transformation
#' # - Beta (precision): Higher temperatures may produce more variable yields
#'
#' # Example 2: Full model with all physical-chemical properties
#' fit_kw_full <- gkwreg(
#'   yield ~ gravity + pressure + temp10 + temp |
#'     temp10 + temp,
#'   data = GasolineYield,
#'   family = "kw"
#' )
#' summary(fit_kw_full)
#'
#' # Interpretation:
#' # - Mean model captures effects of crude oil properties
#' # - Precision varies with vaporization temperatures
#'
#' # Example 3: Exponentiated Kumaraswamy for extreme yields
#' # Some batches may produce unusually high/low yields
#' fit_ekw <- gkwreg(
#'   yield ~ batch + temp | # alpha: batch effects
#'     temp | # beta: temperature precision
#'     batch, # lambda: batch-specific tail behavior
#'   data = GasolineYield,
#'   family = "ekw"
#' )
#' summary(fit_ekw)
#'
#' # Interpretation:
#' # - Lambda varies by batch: Some crude oils have more extreme
#' #   yield distributions (heavy tails for very high/low yields)
#'
#' # Model comparison: Does tail flexibility improve fit?
#' anova(fit_kw, fit_ekw)
#'
#' # Diagnostic plots
#' par(mfrow = c(2, 2))
#' plot(fit_kw, which = c(1, 2, 4, 5))
#' par(mfrow = c(1, 1))
#' }
"GasolineYield"


#' Proportion of Household Income Spent on Food
#'
#' Cross-section data on annual food expenditure and annual income for a random
#' sample of households in a large U.S. city. The dataset models the proportion
#' of income spent on food as a function of total income and household size.
#'
#' @format A data frame with 38 observations on 3 variables:
#' \describe{
#'   \item{food}{numeric. Annual food expenditure in U.S. dollars.}
#'   \item{income}{numeric. Annual household income in U.S. dollars.}
#'   \item{persons}{numeric. Number of persons in the household.}
#' }
#'
#' @details
#' This classic econometric dataset was taken from Griffiths et al. (1993, Table
#' 15.4) who cite Leser (1963) as the original source. The data are used to model
#' Engel curves, which describe how household expenditure on a particular good or
#' service varies with household income.
#'
#' The response variable of interest is typically `food/income`, the proportion
#' of income spent on food, which follows beta distribution properties as it is
#' bounded between 0 and 1.
#'
#' @source
#' Taken from Griffiths et al. (1993, Table 15.4).
#'
#' @references
#' Cribari-Neto, F., and Zeileis, A. (2010). Beta Regression in R.
#' \emph{Journal of Statistical Software}, \strong{34}(2), 1--24.
#' \doi{10.18637/jss.v034.i02}
#'
#' Ferrari, S.L.P., and Cribari-Neto, F. (2004). Beta Regression for Modeling
#' Rates and Proportions. \emph{Journal of Applied Statistics}, \strong{31}(7),
#' 799--815.
#'
#' Griffiths, W.E., Hill, R.C., and Judge, G.G. (1993). \emph{Learning and
#' Practicing Econometrics}. New York: John Wiley and Sons.
#'
#' Leser, C.E.V. (1963). Forms of Engel Functions. \emph{Econometrica},
#' \strong{31}(4), 694--703.
#'
#' @examples
#' \donttest{
#' require(gkwreg)
#' require(gkwdist)
#'
#' data(FoodExpenditure)
#' FoodExpenditure$prop <- FoodExpenditure$food / FoodExpenditure$income
#'
#' # Example 1: Basic Kumaraswamy regression
#' # Proportion spent on food decreases with income (Engel's law)
#' # Larger households spend more on food
#' fit_kw <- gkwreg(prop ~ income + persons,
#'   data = FoodExpenditure,
#'   family = "kw"
#' )
#' summary(fit_kw)
#'
#' # Interpretation:
#' # - Alpha: Negative income effect (Engel's law)
#' #   Positive household size effect
#' # - Beta: Constant precision (homoscedastic model)
#'
#' # Example 2: Heteroscedastic model
#' # Variability in food proportion may differ by income and household size
#' fit_kw_hetero <- gkwreg(
#'   prop ~ income + persons |
#'     income + persons,
#'   data = FoodExpenditure,
#'   family = "kw"
#' )
#' summary(fit_kw_hetero)
#'
#' # Interpretation:
#' # - Beta: Precision varies with both income and household size
#' #   Wealthier or larger households may show different spending variability
#'
#' # Test for heteroscedasticity
#' anova(fit_kw, fit_kw_hetero)
#'
#' # Example 3: Exponentiated Kumaraswamy for extreme spending patterns
#' # Some households may have unusual food spending (very frugal or lavish)
#' fit_ekw <- gkwreg(
#'   prop ~ income + persons | # alpha
#'     persons | # beta: household size affects precision
#'     income, # lambda: income affects extremity
#'   data = FoodExpenditure,
#'   family = "ekw"
#' )
#' summary(fit_ekw)
#'
#' # Interpretation:
#' # - Lambda: Income level affects tail behavior
#' #   Rich households may show more extreme (unusual) spending patterns
#'
#' # Visualization: Engel curve
#' plot(prop ~ income,
#'   data = FoodExpenditure,
#'   xlab = "Annual Income ($)", ylab = "Proportion Spent on Food",
#'   main = "Engel Curve for Food Expenditure"
#' )
#' # Add fitted values
#' FoodExpenditure$fitted_kw <- fitted(fit_kw)
#' points(FoodExpenditure$income, FoodExpenditure$fitted_kw,
#'   col = "blue", pch = 19, cex = 0.8
#' )
#' legend("topright",
#'   legend = c("Observed", "Fitted"),
#'   col = c("black", "blue"), pch = c(1, 19)
#' )
#' }
"FoodExpenditure"


#' Imprecise Probabilities for Sunday Weather and Boeing Stock Task
#'
#' Data from a cognitive psychology experiment where participants estimated upper
#' and lower probabilities for events to occur and not to occur. The study examines
#' judgment under uncertainty with imprecise probability assessments.
#'
#' @format A data frame with 242 observations on 3 variables:
#' \describe{
#'   \item{task}{factor with levels `Boeing stock` and `Sunday weather`.
#'     Indicates which task the participant performed.}
#'   \item{location}{numeric. Average of the lower estimate for the event not to
#'     occur and the upper estimate for the event to occur (proportion).}
#'   \item{difference}{numeric. Difference between upper and lower probability
#'     estimates, measuring imprecision or uncertainty.}
#' }
#'
#' @details
#' All participants in the study were either first- or second-year undergraduate
#' students in psychology at Australian universities, none of whom had a strong
#' background in probability theory or were familiar with imprecise probability
#' theories.
#'
#' For the Sunday weather task, participants were asked to estimate the probability
#' that the temperature at Canberra airport on Sunday would be higher than a
#' specified value.
#'
#' For the Boeing stock task, participants were asked to estimate the probability
#' that Boeing's stock would rise more than those in a list of 30 companies.
#'
#' For each task, participants were asked to provide lower and upper estimates for
#' the event to occur and not to occur.
#'
#' @source
#' Taken from Smithson et al. (2011) supplements.
#'
#' @references
#' Smithson, M., Merkle, E.C., and Verkuilen, J. (2011). Beta Regression Finite
#' Mixture Models of Polarization and Priming. \emph{Journal of Educational and
#' Behavioral Statistics}, \strong{36}(6), 804--831.
#' \doi{10.3102/1076998610396893}
#'
#' Smithson, M., and Segale, C. (2009). Partition Priming in Judgments of
#' Imprecise Probabilities. \emph{Journal of Statistical Theory and Practice},
#' \strong{3}(1), 169--181.
#'
#' @examples
#' \donttest{
#' require(gkwreg)
#' require(gkwdist)
#'
#' data(ImpreciseTask)
#'
#' # Example 1: Basic model with task effects
#' # Probability location varies by task type and uncertainty level
#' fit_kw <- gkwreg(location ~ task * difference,
#'   data = ImpreciseTask,
#'   family = "kw"
#' )
#' summary(fit_kw)
#'
#' # Interpretation:
#' # - Alpha: Task type and uncertainty (difference) interact to affect
#' #   probability estimates
#' # - Different tasks may have different baseline probability assessments
#'
#' # Example 2: Heteroscedastic model
#' # Precision of estimates may vary by task and uncertainty
#' fit_kw_hetero <- gkwreg(
#'   location ~ task * difference |
#'     task + difference,
#'   data = ImpreciseTask,
#'   family = "kw"
#' )
#' summary(fit_kw_hetero)
#'
#' # Interpretation:
#' # - Beta: Variability in estimates differs between tasks
#' #   Higher uncertainty (difference) may lead to less precise estimates
#'
#' # Example 3: McDonald distribution for extreme uncertainty
#' # Some participants may show very extreme probability assessments
#' fit_mc <- gkwreg(
#'   location ~ task * difference | # gamma: full interaction
#'     task * difference | # delta: full interaction
#'     task, # lambda: task affects extremity
#'   data = ImpreciseTask,
#'   family = "mc",
#'   control = gkw_control(
#'     method = "BFGS",
#'     maxit = 1500
#'   )
#' )
#' summary(fit_mc)
#'
#' # Interpretation:
#' # - Lambda varies by task: Weather vs. stock may produce
#' #   different patterns of extreme probability assessments
#' }
"ImpreciseTask"


#' (No) Myopic Loss Aversion in Adolescents
#'
#' Data from a behavioral economics experiment assessing the extent of myopic loss
#' aversion among adolescents aged 11 to 19 years. The experiment tests whether
#' short-term investment horizons lead to more conservative investment behavior.
#'
#' @format A data frame with 570 observations on 7 variables:
#' \describe{
#'   \item{invest}{numeric. Average proportion of tokens invested across all 9
#'     rounds of the experiment (response variable).}
#'   \item{gender}{factor. Gender of the player (or team of players).}
#'   \item{male}{factor. Was (at least one of) the player(s) male (in the team)?}
#'   \item{age}{numeric. Age of the player (or average age in case of team).}
#'   \item{grade}{factor. School grade of the player(s).}
#'   \item{arrangement}{factor. Investment horizon treatment with levels `short`
#'     (1 round), `medium` (3 rounds), and `long` (9 rounds).}
#'   \item{treatment}{factor. Type of treatment: long vs. short.}
#' }
#'
#' @details
#' The data were collected by Matthias Sutter and Daniela Glätzle-Rützler
#' (Universität Innsbruck) in an experiment with high-school students in Tyrol,
#' Austria (Schwaz and Innsbruck). The experiment tests the theory of myopic loss
#' aversion, which proposes that investors with shorter evaluation periods are more
#' loss-averse and thus invest less in risky assets.
#'
#' Classical theory predicts that players with short investment horizons (myopic
#' view) should invest less due to loss aversion. However, Sutter et al. (2015)
#' found no evidence of myopic loss aversion in adolescents, contrary to findings
#' in adult populations.
#'
#' The investment game structure: In each round, players could invest tokens in a
#' risky asset with 50% chance of doubling or losing the investment. The treatment
#' varied the feedback frequency (short = every round, medium = every 3 rounds,
#' long = only at the end).
#'
#' @source
#' Data collected by Matthias Sutter and Daniela Glätzle-Rützler, Universität
#' Innsbruck.
#'
#' @references
#' Sutter, M., Kocher, M.G., Glätzle-Rützler, D., and Trautmann, S.T. (2015).
#' No Myopic Loss Aversion in Adolescents? – An Experimental Note.
#' \emph{Journal of Economic Behavior & Organization}, \strong{111}, 169--176.
#' \doi{10.1016/j.jebo.2014.12.021}
#'
#' Kosmidis, I., and Zeileis, A. (2024). Extended-Support Beta Regression for
#' (0, 1) Responses. \emph{arXiv:2409.07233}.
#' \doi{10.48550/arXiv.2409.07233}
#'
#' @examples
#' \donttest{
#' require(gkwreg)
#' require(gkwdist)
#'
#' data(LossAversion)
#' # Control bounds
#'
#' LossAversion$invest <- with(
#'   LossAversion,
#'   ifelse(invest <= 0, 0.000001,
#'     ifelse(invest >= 1, 0.999999, invest)
#'   )
#' )
#' # Example 1: Test for myopic loss aversion
#' # Do short-term players invest less? (They shouldn't, per Sutter et al.)
#' fit_kw <- gkwreg(
#'   invest ~ arrangement + age + male + grade |
#'     arrangement + male,
#'   data = LossAversion,
#'   family = "kw"
#' )
#' summary(fit_kw)
#'
#' # Interpretation:
#' # - Alpha: Effect of investment horizon (arrangement) on mean investment
#' #   Age and gender effects on risk-taking
#' # - Beta: Precision varies by horizon and gender
#' #   (some groups more consistent than others)
#'
#' # Example 2: Interaction effects
#' # Does the horizon effect differ by age/grade?
#' fit_kw_interact <- gkwreg(
#'   invest ~ grade * (arrangement + age) + male |
#'     arrangement + male + grade,
#'   data = LossAversion,
#'   family = "kw"
#' )
#' summary(fit_kw_interact)
#'
#' # Interpretation:
#' # - Grade × arrangement interaction tests if myopic loss aversion
#' #   emerges differently at different developmental stages
#'
#' # Example 3: Extended-support for boundary observations
#' # Some students invest 0% or 100% of tokens
#' # Original 'invest' variable may include exact 0 and 1 values
#' fit_xbx <- gkwreg(
#'   invest ~ grade * (arrangement + age) + male |
#'     arrangement + male + grade,
#'   data = LossAversion,
#'   family = "kw" # Note: for true [0,1] support, use extended-support models
#' )
#' summary(fit_xbx)
#'
#' # Interpretation:
#' # - Model accommodates extreme risk-taking (all-in or all-out strategies)
#'
#' # Compare models
#' anova(fit_kw, fit_kw_interact)
#'
#' # Visualization: Investment by horizon
#' boxplot(invest ~ arrangement,
#'   data = LossAversion,
#'   xlab = "Investment Horizon", ylab = "Proportion Invested",
#'   main = "No Myopic Loss Aversion in Adolescents",
#'   col = c("lightblue", "lightgreen", "lightyellow")
#' )
#' }
"LossAversion"


#' Confidence of Mock Jurors in Their Verdicts
#'
#' Data from a study examining factors that influence mock juror confidence in
#' verdicts for criminal trials. The experiment manipulates verdict options
#' (two-option vs. three-option) and presence of conflicting testimonial evidence.
#'
#' @format A data frame with 104 observations on 3 variables:
#' \describe{
#'   \item{confidence}{numeric. Juror confidence in their verdict, scaled to the
#'     open unit interval (0, 1). Original scale was 0-100.}
#'   \item{verdict}{factor indicating whether a two-option verdict (guilty vs.
#'     acquittal) or three-option verdict (with Scottish 'not proven' alternative)
#'     was requested. Sum contrast coding is employed.}
#'   \item{conflict}{factor. Is there conflicting testimonial evidence? Values
#'     are `no` or `yes`. Sum contrast coding is employed.}
#' }
#'
#' @details
#' The data were collected by Deady (2004) among first-year psychology students at
#' Australian National University. The experiment examined how the availability of
#' a third verdict option ('not proven') and conflicting evidence affect juror
#' confidence.
#'
#' Smithson and Verkuilen (2006) employed the data, scaling the original confidence
#' (on a scale 0-100) to the open unit interval using the transformation:
#' \code{((original_confidence/100) * 103 - 0.5) / 104}.
#'
#' **Important note:** The original coding of `conflict` in the data provided from
#' Smithson's homepage is -1/1 which Smithson and Verkuilen (2006) describe to
#' mean no/yes. However, all their results (sample statistics, histograms, etc.)
#' suggest that it actually means yes/no, which was employed in the corrected
#' `MockJurors` dataset.
#'
#' @source
#' Data collected by Deady (2004), analyzed by Smithson and Verkuilen (2006).
#'
#' @references
#' Deady, S. (2004). \emph{The Psychological Third Verdict: 'Not Proven' or
#' 'Not Willing to Make a Decision'?} Unpublished honors thesis, The Australian
#' National University, Canberra.
#'
#' Smithson, M., and Verkuilen, J. (2006). A Better Lemon Squeezer?
#' Maximum-Likelihood Regression with Beta-Distributed Dependent Variables.
#' \emph{Psychological Methods}, \strong{11}(1), 54--71.
#'
#' @examples
#' \donttest{
#' require(gkwreg)
#' require(gkwdist)
#'
#' data(MockJurors)
#'
#' # Example 1: Main effects model with heteroscedasticity
#' # Confidence depends on verdict options and conflicting evidence
#' # Variability may also depend on these factors
#' fit_kw <- gkwreg(
#'   confidence ~ verdict + conflict |
#'     verdict * conflict,
#'   data = MockJurors,
#'   family = "kw"
#' )
#' summary(fit_kw)
#'
#' # Interpretation:
#' # - Alpha (mean): Additive effects of verdict type and conflict
#' #   Three-option verdicts may reduce confidence
#' #   Conflicting evidence reduces confidence
#' # - Beta (precision): Interaction suggests confidence variability
#' #   depends on combination of verdict options and evidence type
#'
#' # Example 2: Full interaction in mean model
#' fit_kw_interact <- gkwreg(
#'   confidence ~ verdict * conflict |
#'     verdict * conflict,
#'   data = MockJurors,
#'   family = "kw"
#' )
#' summary(fit_kw_interact)
#'
#' # Interpretation:
#' # - Full interaction: Third verdict option may have different effects
#' #   depending on whether evidence is conflicting
#'
#' # Test interaction significance
#' anova(fit_kw, fit_kw_interact)
#'
#' # Example 3: McDonald distribution for extreme confidence patterns
#' # Jurors may show very high confidence (ceiling effects) or very low
#' # confidence depending on conditions
#' fit_mc <- gkwreg(
#'   confidence ~ verdict * conflict | # gamma: full interaction
#'     verdict * conflict | # delta: full interaction
#'     verdict + conflict, # lambda: additive extremity effects
#'   data = MockJurors,
#'   family = "mc",
#'   control = gkw_control(
#'     method = "BFGS",
#'     maxit = 1500,
#'     reltol = 1e-8
#'   )
#' )
#' summary(fit_mc)
#'
#' # Interpretation:
#' # - Lambda: Models asymmetry and extreme confidence
#' #   Some conditions produce more polarized confidence (very high or very low)
#'
#' # Example 4: Exponentiated Kumaraswamy alternative
#' fit_ekw <- gkwreg(
#'   confidence ~ verdict * conflict | # alpha
#'     verdict + conflict | # beta
#'     conflict, # lambda: conflict affects extremity
#'   data = MockJurors,
#'   family = "ekw",
#'   control = gkw_control(
#'     method = "BFGS",
#'     maxit = 1500
#'   )
#' )
#' summary(fit_ekw)
#'
#' # Compare 3-parameter models
#' AIC(fit_ekw, fit_mc)
#' }
"MockJurors"


#' Dyslexia and IQ Predicting Reading Accuracy
#'
#' Data for assessing the contribution of non-verbal IQ to children's reading
#' skills in dyslexic and non-dyslexic children. This is a classic dataset
#' demonstrating beta regression with interaction effects and heteroscedasticity.
#'
#' @format A data frame with 44 observations on 4 variables:
#' \describe{
#'   \item{accuracy}{numeric. Reading accuracy score scaled to the open unit
#'     interval (0, 1). Perfect scores of 1 were replaced with 0.99.}
#'   \item{accuracy1}{numeric. Unrestricted reading accuracy score in (0, 1),
#'     including boundary observations.}
#'   \item{dyslexia}{factor. Is the child dyslexic? Levels: `no` (control group)
#'     and `yes` (dyslexic group). Sum contrast coding is employed.}
#'   \item{iq}{numeric. Non-verbal intelligence quotient transformed to z-scores
#'     (mean = 0, SD = 1).}
#' }
#'
#' @details
#' The data were collected by Pammer and Kevan (2004) and employed by Smithson and
#' Verkuilen (2006) in their seminal beta regression paper. The sample includes 19
#' dyslexic children and 25 controls recruited from primary schools in the
#' Australian Capital Territory. Children's ages ranged from 8 years 5 months to
#' 12 years 3 months.
#'
#' Mean reading accuracy was 0.606 for dyslexic readers and 0.900 for controls.
#' The study investigates whether dyslexia contributes to reading accuracy even
#' when controlling for IQ (which is on average lower for dyslexics).
#'
#' **Transformation details:** The original reading accuracy score was transformed
#' by Smithson and Verkuilen (2006) to fit beta regression requirements:
#' 1. First, the original accuracy was scaled using the minimal and maximal scores
#' (a and b) that can be obtained in the test: `accuracy1 = (original - a)/(b - a)`
#' (a and b values are not provided).
#' 2. Subsequently, `accuracy` was obtained from `accuracy1` by replacing all
#' observations with a value of 1 with 0.99 to fit the open interval (0, 1).
#'
#' The data clearly show asymmetry and heteroscedasticity (especially in the control
#' group), making beta regression more appropriate than standard linear regression.
#'
#' @source
#' Data collected by Pammer and Kevan (2004).
#'
#' @references
#' Cribari-Neto, F., and Zeileis, A. (2010). Beta Regression in R.
#' \emph{Journal of Statistical Software}, \strong{34}(2), 1--24.
#' \doi{10.18637/jss.v034.i02}
#'
#' Grün, B., Kosmidis, I., and Zeileis, A. (2012). Extended Beta Regression in R:
#' Shaken, Stirred, Mixed, and Partitioned. \emph{Journal of Statistical Software},
#' \strong{48}(11), 1--25.
#' \doi{10.18637/jss.v048.i11}
#'
#' Kosmidis, I., and Zeileis, A. (2024). Extended-Support Beta Regression for
#' (0, 1) Responses. \emph{arXiv:2409.07233}.
#' \doi{10.48550/arXiv.2409.07233}
#'
#' Pammer, K., and Kevan, A. (2004). \emph{The Contribution of Visual Sensitivity,
#' Phonological Processing and Nonverbal IQ to Children's Reading}. Unpublished
#' manuscript, The Australian National University, Canberra.
#'
#' Smithson, M., and Verkuilen, J. (2006). A Better Lemon Squeezer?
#' Maximum-Likelihood Regression with Beta-Distributed Dependent Variables.
#' \emph{Psychological Methods}, \strong{11}(1), 54--71.
#'
#' @examples
#' \donttest{
#' require(gkwreg)
#' require(gkwdist)
#'
#' data(ReadingSkills)
#'
#' # Example 1: Standard Kumaraswamy with interaction and heteroscedasticity
#' # Mean: Dyslexia × IQ interaction (do groups differ in IQ effect?)
#' # Precision: Main effects (variability differs by group and IQ level)
#' fit_kw <- gkwreg(
#'   accuracy ~ dyslexia * iq |
#'     dyslexia + iq,
#'   data = ReadingSkills,
#'   family = "kw",
#'   control = gkw_control(method = "L-BFGS-B", maxit = 2000)
#' )
#' summary(fit_kw)
#'
#' # Interpretation:
#' # - Alpha (mean): Interaction shows dyslexic children benefit less from
#' #   higher IQ compared to controls
#' # - Beta (precision): Controls show more variable accuracy (higher precision)
#' #   IQ increases consistency of performance
#'
#' # Example 2: Simpler model without interaction
#' fit_kw_simple <- gkwreg(
#'   accuracy ~ dyslexia + iq |
#'     dyslexia + iq,
#'   data = ReadingSkills,
#'   family = "kw",
#'   control = gkw_control(method = "L-BFGS-B", maxit = 2000)
#' )
#'
#' # Test if interaction is significant
#' anova(fit_kw_simple, fit_kw)
#'
#' # Example 3: Exponentiated Kumaraswamy for ceiling effects
#' # Reading accuracy often shows ceiling effects (many perfect/near-perfect scores)
#' # Lambda parameter can model this right-skewed asymmetry
#' fit_ekw <- gkwreg(
#'   accuracy ~ dyslexia * iq | # alpha
#'     dyslexia + iq | # beta
#'     dyslexia, # lambda: ceiling effect by group
#'   data = ReadingSkills,
#'   family = "ekw",
#'   control = gkw_control(method = "L-BFGS-B", maxit = 2000)
#' )
#' summary(fit_ekw)
#'
#' # Interpretation:
#' # - Lambda varies by dyslexia status: Controls have stronger ceiling effect
#' #   (more compression at high accuracy) than dyslexic children
#'
#' # Test if ceiling effect modeling improves fit
#' anova(fit_kw, fit_ekw)
#'
#' # Example 4: McDonald distribution alternative
#' # Provides different parameterization for extreme values
#' fit_mc <- gkwreg(
#'   accuracy ~ dyslexia * iq | # gamma
#'     dyslexia + iq | # delta
#'     dyslexia * iq, # lambda: interaction affects tails
#'   data = ReadingSkills,
#'   family = "mc",
#'   control = gkw_control(method = "L-BFGS-B", maxit = 2000)
#' )
#' summary(fit_mc)
#'
#' # Compare 3-parameter models
#' AIC(fit_ekw, fit_mc)
#' }
"ReadingSkills"


#' Dependency of Anxiety on Stress
#'
#' Data from a study examining the relationship between stress and anxiety levels
#' among nonclinical women in Townsville, Queensland, Australia.
#'
#' @format A data frame with 166 observations on 2 variables:
#' \describe{
#'   \item{stress}{numeric. Stress score transformed to the open unit interval
#'     (0, 1). Original scale ranged from 0 to 42 on the Depression Anxiety
#'     Stress Scales (DASS).}
#'   \item{anxiety}{numeric. Anxiety score transformed to the open unit interval
#'     (0, 1). Original scale ranged from 0 to 42 on the DASS.}
#' }
#'
#' @details
#' Both stress and anxiety were assessed using the Depression Anxiety Stress Scales
#' (DASS), ranging from 0 to 42. Smithson and Verkuilen (2006) transformed these
#' scores to the open unit interval (without providing specific details about the
#' transformation method).
#'
#' The dataset is particularly interesting for demonstrating heteroscedastic
#' relationships: not only does mean anxiety increase with stress, but the
#' variability in anxiety also changes systematically with stress levels. This
#' makes it an ideal case for beta regression with variable dispersion.
#'
#' @source
#' Data from Smithson and Verkuilen (2006) supplements. Original data source not
#' specified.
#'
#' @references
#' Smithson, M., and Verkuilen, J. (2006). A Better Lemon Squeezer?
#' Maximum-Likelihood Regression with Beta-Distributed Dependent Variables.
#' \emph{Psychological Methods}, \strong{11}(1), 54--71.
#'
#' @examples
#' \donttest{
#' require(gkwreg)
#' require(gkwdist)
#'
#' data(StressAnxiety)
#'
#' # Example 1: Basic heteroscedastic relationship
#' # Mean anxiety increases with stress
#' # Variability in anxiety also changes with stress
#' fit_kw <- gkwreg(
#'   anxiety ~ stress |
#'     stress,
#'   data = StressAnxiety,
#'   family = "kw"
#' )
#' summary(fit_kw)
#'
#' # Interpretation:
#' # - Alpha: Positive relationship between stress and mean anxiety
#' # - Beta: Precision changes with stress level
#' #   (anxiety becomes more/less variable at different stress levels)
#'
#' # Compare to homoscedastic model
#' fit_kw_homo <- gkwreg(anxiety ~ stress,
#'   data = StressAnxiety, family = "kw"
#' )
#' anova(fit_kw_homo, fit_kw)
#'
#' # Example 2: Nonlinear stress effects via polynomial
#' # Stress-anxiety relationship often shows threshold or saturation effects
#' fit_kw_poly <- gkwreg(
#'   anxiety ~ poly(stress, 2) | # quadratic mean
#'     poly(stress, 2), # quadratic precision
#'   data = StressAnxiety,
#'   family = "kw"
#' )
#' summary(fit_kw_poly)
#'
#' # Interpretation:
#' # - Quadratic terms allow for:
#' #   * Threshold effects (anxiety accelerates at high stress)
#' #   * Saturation effects (anxiety plateaus at extreme stress)
#'
#' # Test nonlinearity
#' anova(fit_kw, fit_kw_poly)
#'
#' # Example 3: Exponentiated Kumaraswamy for extreme anxiety patterns
#' # Some individuals may show very extreme anxiety responses to stress
#' fit_ekw <- gkwreg(
#'   anxiety ~ poly(stress, 2) | # alpha: quadratic mean
#'     poly(stress, 2) | # beta: quadratic precision
#'     stress, # lambda: linear tail effect
#'   data = StressAnxiety,
#'   family = "ekw"
#' )
#' summary(fit_ekw)
#'
#' # Interpretation:
#' # - Lambda: Linear component captures asymmetry at extreme stress levels
#' #   (very high stress may produce different tail behavior)
#'
#' # Example 4: McDonald distribution for highly skewed anxiety
#' # Anxiety distributions are often right-skewed (ceiling effects)
#' fit_mc <- gkwreg(
#'   anxiety ~ poly(stress, 2) | # gamma
#'     poly(stress, 2) | # delta
#'     stress, # lambda: extremity
#'   data = StressAnxiety,
#'   family = "mc",
#'   control = gkw_control(method = "BFGS", maxit = 1500)
#' )
#' summary(fit_mc)
#'
#' # Compare models
#' AIC(fit_kw, fit_kw_poly, fit_ekw, fit_mc)
#'
#' # Visualization: Stress-Anxiety relationship
#' plot(anxiety ~ stress,
#'   data = StressAnxiety,
#'   xlab = "Stress Level", ylab = "Anxiety Level",
#'   main = "Stress-Anxiety Relationship with Heteroscedasticity",
#'   pch = 19, col = rgb(0, 0, 1, 0.3)
#' )
#'
#' # Add fitted curve
#' stress_seq <- seq(min(StressAnxiety$stress), max(StressAnxiety$stress),
#'   length.out = 100
#' )
#' pred_mean <- predict(fit_kw, newdata = data.frame(stress = stress_seq))
#' lines(stress_seq, pred_mean, col = "red", lwd = 2)
#'
#' # Add lowess smooth for comparison
#' lines(lowess(StressAnxiety$stress, StressAnxiety$anxiety),
#'   col = "blue", lwd = 2, lty = 2
#' )
#' legend("topleft",
#'   legend = c("Kumaraswamy fit", "Lowess smooth"),
#'   col = c("red", "blue"), lwd = 2, lty = c(1, 2)
#' )
#' }
"StressAnxiety"


#' Weather Task with Priming and Precise and Imprecise Probabilities
#'
#' Data from a cognitive psychology experiment on probabilistic learning and
#' probability judgments. Participants estimated probabilities for weather events
#' under different priming and precision conditions.
#'
#' @format A data frame with 345 observations on 4 variables:
#' \describe{
#'   \item{agreement}{numeric. Probability indicated by participants, or the
#'     average between minimum and maximum estimates in the imprecise condition.
#'     Response variable scaled to (0, 1).}
#'   \item{priming}{factor with levels `two-fold` (case prime) and `seven-fold`
#'     (class prime). Indicates the partition priming condition.}
#'   \item{eliciting}{factor with levels `precise` and `imprecise` (lower and
#'     upper limit). Indicates whether participants gave point estimates or
#'     interval estimates.}
#' }
#'
#' @details
#' All participants in the study were either first- or second-year undergraduate
#' students in psychology, none of whom had a strong background in probability or
#' were familiar with imprecise probability theories.
#'
#' **Task description:** Participants were asked: "What is the probability that the
#' temperature at Canberra airport on Sunday will be higher than 'specified
#' temperature'?"
#'
#' **Experimental manipulations:**
#' - **Priming:** Two-fold (simple binary: above/below) vs. seven-fold (multiple
#'   temperature categories)
#' - **Eliciting:** Precise (single probability estimate) vs. imprecise (lower and
#'   upper bounds)
#'
#' The study examines how partition priming (number of response categories) and
#' elicitation format affect probability judgments. Classical findings suggest that
#' more categories (seven-fold) lead to different probability assessments than
#' binary categories (two-fold).
#'
#' @source
#' Taken from Smithson et al. (2011) supplements.
#'
#' @references
#' Smithson, M., Merkle, E.C., and Verkuilen, J. (2011). Beta Regression Finite
#' Mixture Models of Polarization and Priming. \emph{Journal of Educational and
#' Behavioral Statistics}, \strong{36}(6), 804--831.
#' \doi{10.3102/1076998610396893}
#'
#' Smithson, M., and Segale, C. (2009). Partition Priming in Judgments of
#' Imprecise Probabilities. \emph{Journal of Statistical Theory and Practice},
#' \strong{3}(1), 169--181.
#'
#' @examples
#' \donttest{
#' require(gkwreg)
#' require(gkwdist)
#'
#' data(WeatherTask)
#'
#' # Example 1: Main effects model
#' # Probability judgments affected by priming and elicitation format
#' fit_kw <- gkwreg(
#'   agreement ~ priming + eliciting,
#'   data = WeatherTask,
#'   family = "kw"
#' )
#' summary(fit_kw)
#'
#' # Interpretation:
#' # - Alpha: Seven-fold priming may shift probability estimates
#' #   Imprecise elicitation may produce different mean estimates
#'
#' # Example 2: Interaction model with heteroscedasticity
#' # Priming effects may differ by elicitation format
#' # Variability may also depend on conditions
#' fit_kw_interact <- gkwreg(
#'   agreement ~ priming * eliciting |
#'     priming + eliciting,
#'   data = WeatherTask,
#'   family = "kw"
#' )
#' summary(fit_kw_interact)
#'
#' # Interpretation:
#' # - Alpha: Interaction tests if partition priming works differently
#' #   for precise vs. imprecise probability judgments
#' # - Beta: Precision varies by experimental condition
#'
#' # Test interaction
#' anova(fit_kw, fit_kw_interact)
#'
#' # Example 3: McDonald distribution for polarized responses
#' # Probability judgments often show polarization (clustering at extremes)
#' # particularly under certain priming conditions
#' fit_mc <- gkwreg(
#'   agreement ~ priming * eliciting | # gamma
#'     priming * eliciting | # delta
#'     priming, # lambda: priming affects polarization
#'   data = WeatherTask,
#'   family = "mc",
#'   control = gkw_control(method = "BFGS", maxit = 1500)
#' )
#' summary(fit_mc)
#'
#' # Interpretation:
#' # - Lambda varies by priming: Seven-fold priming may produce more
#' #   extreme/polarized probability judgments
#' }
"WeatherTask"


#' Partition-Primed Probability Judgement Task for Car Dealership
#'
#' Data from a cognitive experiment examining how partition priming affects
#' probability judgments in a car dealership context. Participants judged
#' probabilities under different framing conditions.
#'
#' @format A data frame with 155 observations on 3 variables:
#' \describe{
#'   \item{probability}{numeric. Estimated probability (response variable).}
#'   \item{task}{factor with levels `Car` and `Salesperson` indicating the
#'     condition/question type.}
#'   \item{NFCCscale}{numeric. Combined score from the Need for Closure (NFC) and
#'     Need for Certainty (NCC) scales, which are strongly correlated.}
#' }
#'
#' @details
#' All participants in the study were undergraduate students at The Australian
#' National University, some of whom obtained course credit in first-year Psychology
#' for their participation.
#'
#' **Task questions:**
#' - **Car condition:** "What is the probability that a customer trades in a coupe?"
#' - **Salesperson condition:** "What is the probability that a customer buys a car
#'   from Carlos?" (out of four possible salespersons)
#'
#' The key manipulation is the implicit partition: In the Car condition, there are
#' multiple car types (binary: coupe vs. not coupe), while in the Salesperson
#' condition, there are four specific salespersons. Classical findings suggest that
#' different partition structures lead to different probability estimates even when
#' the actual probabilities are equivalent.
#'
#' The NFCC scale (Need for Closure and Certainty) measures individual differences
#' in tolerance for ambiguity. Higher scores indicate greater need for definitive
#' answers and discomfort with uncertainty.
#'
#' @source
#' Taken from Smithson et al. (2011) supplements.
#'
#' @references
#' Smithson, M., Merkle, E.C., and Verkuilen, J. (2011). Beta Regression Finite
#' Mixture Models of Polarization and Priming. \emph{Journal of Educational and
#' Behavioral Statistics}, \strong{36}(6), 804--831.
#' \doi{10.3102/1076998610396893}
#'
#' Smithson, M., and Segale, C. (2009). Partition Priming in Judgments of
#' Imprecise Probabilities. \emph{Journal of Statistical Theory and Practice},
#' \strong{3}(1), 169--181.
#'
#' @examples
#' \donttest{
#' require(gkwreg)
#' require(gkwdist)
#'
#' data(CarTask)
#'
#' # Example 1: Task effects on probability judgments
#' # Do people judge probabilities differently for car vs. salesperson?
#' fit_kw <- gkwreg(
#'   probability ~ task,
#'   data = CarTask,
#'   family = "kw"
#' )
#' summary(fit_kw)
#'
#' # Interpretation:
#' # - Alpha: Task type affects mean probability estimate
#' #   Salesperson condition (1/4 = 0.25) vs. car type (unclear baseline)
#'
#' # Example 2: Individual differences model
#' # Need for Closure/Certainty may moderate probability judgments
#' fit_kw_nfcc <- gkwreg(
#'   probability ~ task * NFCCscale |
#'     task,
#'   data = CarTask,
#'   family = "kw"
#' )
#' summary(fit_kw_nfcc)
#'
#' # Interpretation:
#' # - Interaction: NFCC may have different effects depending on task
#' #   People high in need for certainty may respond differently to
#' #   explicit partitions (4 salespersons) vs. implicit partitions (car types)
#' # - Beta: Precision varies by task type
#'
#' # Example 3: Exponentiated Kumaraswamy for extreme estimates
#' # Some participants may give very extreme probability estimates
#' fit_ekw <- gkwreg(
#'   probability ~ task * NFCCscale | # alpha
#'     task | # beta
#'     task, # lambda: extremity differs by task
#'   data = CarTask,
#'   family = "ekw"
#' )
#' summary(fit_ekw)
#'
#' # Interpretation:
#' # - Lambda varies by task: Salesperson condition (explicit partition)
#' #   may produce more extreme estimates (closer to 0 or 1)
#'
#' # Visualization: Probability by task and NFCC
#' plot(probability ~ NFCCscale,
#'   data = CarTask,
#'   col = c("blue", "red")[task], pch = 19,
#'   xlab = "Need for Closure/Certainty", ylab = "Probability Estimate",
#'   main = "Car Task: Individual Differences in Probability Judgment"
#' )
#' legend("topright",
#'   legend = levels(CarTask$task),
#'   col = c("blue", "red"), pch = 19
#' )
#'
#' # Distribution comparison
#' boxplot(probability ~ task,
#'   data = CarTask,
#'   xlab = "Task Condition", ylab = "Probability Estimate",
#'   main = "Partition Priming Effects",
#'   col = c("lightblue", "lightcoral")
#' )
#' abline(h = 0.25, lty = 2, col = "gray")
#' text(1.5, 0.27, "Uniform (1/4)", col = "gray")
#' }
"CarTask"


#' Autologous Peripheral Blood Stem Cell Transplants Data
#'
#' Data on Autologous Peripheral Blood Stem Cell Transplants from the Stem Cell
#' Lab in the Cross Cancer Institute, Alberta Health Services. The dataset
#' examines recovery rates of CD34+ cells after peripheral blood stem cell (PBSC)
#' transplants.
#'
#' @format A data frame with 60 observations on 5 variables:
#' \describe{
#'   \item{rcd}{numeric. Recovery rate of CD34+ cells (proportion in (0, 1)).
#'     Response variable measuring the proportion of CD34+ cells recovered after
#'     PBSC transplant.}
#'   \item{age}{numeric. Patient age in years (range: 18-71 years).}
#'   \item{ageadj}{numeric. Age-adjusted covariate. Centered and scaled version
#'     of age for improved numerical stability in regression models.}
#'   \item{chemo}{factor. Type of chemotherapy protocol used for stem cell
#'     mobilization. Levels include: `1-day`, `3-day`, `G-CSF only`, and `other`.}
#'   \item{gender}{factor. Patient gender. Most patients in the study are male.}
#' }
#'
#' @details
#' This dataset contains clinical data from autologous peripheral blood stem cell
#' (PBSC) transplant patients treated at the Cross Cancer Institute, Alberta Health
#' Services. CD34+ cells are hematopoietic stem and progenitor cells critical for
#' successful transplantation and hematopoietic recovery.
#'
#' **Clinical context:** Autologous PBSC transplantation is used to treat various
#' hematological malignancies including multiple myeloma, non-Hodgkin's lymphoma,
#' acute leukemia, and some solid tumors. The recovery rate of CD34+ cells is a
#' crucial predictor of engraftment success and patient outcomes.
#'
#' **Chemotherapy protocols:**
#' - **1-day protocol:** Single-day high-dose chemotherapy for mobilization
#' - **3-day protocol:** Multi-day chemotherapy regimen
#' - **G-CSF only:** Granulocyte colony-stimulating factor without chemotherapy
#' - **Other:** Alternative or combined protocols
#'
#' The proportion of recovered CD34+ cells naturally falls in the interval (0, 1),
#' making it ideal for proportional data regression modeling. Age effects are
#' particularly important as older patients may show different recovery patterns.
#'
#' This dataset is particularly suitable for:
#' - Simplex regression (original application by Zhang et al. 2016)
#' - Beta regression with variable dispersion
#' - Kumaraswamy regression for flexible distributional modeling
#'
#' @source
#' Stem Cell Lab, Cross Cancer Institute, Alberta Health Services, Canada.
#'
#' @references
#' Zhang, P., Qiu, Z., and Shi, C. (2016). simplexreg: An R Package for
#' Regression Analysis of Proportional Data Using the Simplex Distribution.
#' \emph{Journal of Statistical Software}, \strong{71}(11), 1--21.
#' \doi{10.18637/jss.v071.i11}
#'
#' @examples
#' \donttest{
#' require(gkwreg)
#' require(gkwdist)
#'
#' data(sdac)
#'
#' # Example 1: Basic Kumaraswamy regression
#' # Mean recovery depends on age and chemotherapy protocol
#' # Precision varies with age (older patients more variable)
#' fit_kw <- gkwreg(
#'   rcd ~ ageadj + chemo |
#'     age,
#'   data = sdac,
#'   family = "kw"
#' )
#' summary(fit_kw)
#'
#' # Interpretation:
#' # - Alpha (mean recovery): Depends on age-adjusted covariate and chemo protocol
#' #   Different protocols show different baseline recovery rates
#' #   G-CSF-only may differ from multi-day chemotherapy protocols
#' # - Beta (precision): Raw age affects recovery variability
#' #   Hypothesis: Older patients show more heterogeneous responses
#'
#' # Example 2: Include gender effects
#' # Gender may influence stem cell recovery rates
#' fit_kw_gender <- gkwreg(
#'   rcd ~ ageadj + chemo + gender |
#'     age + gender,
#'   data = sdac,
#'   family = "kw"
#' )
#' summary(fit_kw_gender)
#'
#' # Interpretation:
#' # - Gender effects in both mean and precision
#' # - Precision may differ between males and females
#'
#' # Test gender significance
#' anova(fit_kw, fit_kw_gender)
#'
#' # Example 3: Exponentiated Kumaraswamy for extreme recovery patterns
#' # Some patients show unusually high or low recovery (outliers)
#' # Lambda parameter captures tail heaviness
#' fit_ekw <- gkwreg(
#'   rcd ~ ageadj + chemo + gender | # alpha: mean model
#'     age + chemo | # beta: precision varies with age and protocol
#'     chemo, # lambda: protocol affects extremity
#'   data = sdac,
#'   family = "ekw"
#' )
#' summary(fit_ekw)
#'
#' # Clinical interpretation:
#' # - Lambda varies by chemotherapy protocol: Some protocols produce more
#' #   extreme recovery patterns (very high or very low CD34+ counts)
#' # - G-CSF-only vs multi-day protocols may differ in tail behavior
#' # - Important for risk stratification and clinical decision-making
#'
#' # Test if extreme patterns differ by protocol
#' anova(fit_kw_gender, fit_ekw)
#'
#' # Example 4: Interaction between age and protocol
#' # Protocol effectiveness may vary with patient age
#' fit_kw_interact <- gkwreg(
#'   rcd ~ ageadj * chemo |
#'     age * chemo,
#'   data = sdac,
#'   family = "kw"
#' )
#' summary(fit_kw_interact)
#'
#' # Interpretation:
#' # - Interaction: Does protocol effectiveness decline with age?
#' # - Critical for personalized treatment selection
#' }
"sdac"


#' Intraocular Gas Decay in Retinal Surgery
#'
#' Longitudinal data on the recorded decay of intraocular gas (perfluoropropane)
#' in complex retinal surgeries. The dataset tracks the proportion of gas remaining
#' over time following vitrectomy procedures.
#'
#' @format A data frame with 40 observations on 7 variables:
#' \describe{
#'   \item{ID}{integer. Patient identification number for longitudinal tracking.}
#'   \item{Gas}{numeric. Proportion of intraocular gas remaining (0-1 scale).
#'     Response variable measuring the fraction of perfluoropropane gas still
#'     present in the vitreous cavity.}
#'   \item{Time}{numeric. Time point of measurement (days or weeks post-surgery).}
#'   \item{LogT}{numeric. Logarithm of time, log(Time). Used to linearize the
#'     exponential decay pattern.}
#'   \item{LogT2}{numeric. Squared logarithm of time, (log(Time))^2. Captures
#'     nonlinear decay patterns.}
#'   \item{Level}{factor. Initial gas concentration level at the time of injection.
#'     Different starting concentrations affect decay kinetics.}
#' }
#'
#' @details
#' This longitudinal dataset comes from a study of gas decay following vitreoretinal
#' surgery. Perfluoropropane (C3F8) is commonly used as a temporary tamponade agent
#' in retinal detachment repair and other complex vitreoretinal procedures.
#'
#' **Clinical background:** During vitrectomy for retinal detachment, gas bubbles
#' are injected into the vitreous cavity to help reattach the retina by providing
#' internal tamponade. The gas gradually absorbs and dissipates over time. Understanding
#' the decay rate is important for:
#' - Predicting when patients can resume normal activities (esp. air travel)
#' - Assessing treatment efficacy
#' - Planning follow-up examinations
#'
#' **Decay kinetics:** Gas decay typically follows a nonlinear pattern that can be
#' approximated by exponential or power-law functions. The log transformation
#' (LogT, LogT2) helps linearize these relationships for regression modeling.
#'
#' **Data structure:** This is a longitudinal/panel dataset with repeated measurements
#' on the same patients over time. Correlation structures (exchangeable, AR(1), etc.)
#' should be considered when modeling.
#'
#' The proportional nature of the gas variable (bounded between 0 and 1) makes this
#' dataset ideal for:
#' - Simplex marginal models (original application by Song & Tan 2000)
#' - Beta regression with longitudinal correlation structures
#' - Kumaraswamy regression with heteroscedastic errors
#'
#' @source
#' Based on clinical data from vitreoretinal surgery patients. Originally analyzed
#' in Song and Tan (2000).
#'
#' @references
#' Meyers, S.M., Ambler, J.S., Tan, M., Werner, J.C., and Huang, S.S. (1992).
#' Variation of Perfluoropropane Disappearance After Vitrectomy. \emph{Retina},
#' \strong{12}, 359--363.
#'
#' Song, P.X.-K., and Tan, M. (2000). Marginal Models for Longitudinal Continuous
#' Proportional Data. \emph{Biometrics}, \strong{56}, 496--502.
#' \doi{10.1111/j.0006-341x.2000.00496.x}
#'
#' Song, P.X.-K., Qiu, Z., and Tan, M. (2004). Modelling Heterogeneous Dispersion
#' in Marginal Models for Longitudinal Proportional Data. \emph{Biometrical Journal},
#' \strong{46}, 540--553.
#'
#' Qiu, Z., Song, P.X.-K., and Tan, M. (2008). Simplex Mixed-Effects Models for
#' Longitudinal Proportional Data. \emph{Scandinavian Journal of Statistics},
#' \strong{35}, 577--596.
#' \doi{10.1111/j.1467-9469.2008.00603.x}
#'
#' Zhang, P., Qiu, Z., and Shi, C. (2016). simplexreg: An R Package for
#' Regression Analysis of Proportional Data Using the Simplex Distribution.
#' \emph{Journal of Statistical Software}, \strong{71}(11), 1--21.
#' \doi{10.18637/jss.v071.i11}
#'
#' @examples
#' \donttest{
#' require(gkwreg)
#' require(gkwdist)
#'
#' data(retinal)
#'
#' # Example 1: Nonlinear time decay with level effects
#' # Model gas decay as quadratic function of log-time
#' # Allow precision to vary by initial gas concentration
#' fit_kw <- gkwreg(
#'   Gas ~ LogT + LogT2 + Level |
#'     Level,
#'   data = retinal,
#'   family = "kw"
#' )
#' summary(fit_kw)
#'
#' # Interpretation:
#' # - Alpha: Decay curve shape varies by initial gas concentration
#' #   LogT + LogT2 capture nonlinear exponential-like decay
#' # - Beta: Precision differs by concentration level
#' #   Higher concentration may produce more/less variable decay
#'
#' # Example 2: Heteroscedastic model
#' # Variability in gas proportion may change over time
#' fit_kw_hetero <- gkwreg(
#'   Gas ~ LogT + LogT2 + Level |
#'     Level + LogT,
#'   data = retinal,
#'   family = "kw"
#' )
#' summary(fit_kw_hetero)
#'
#' # Interpretation:
#' # - Beta: Precision varies with both level and time
#' #   Early measurements may be more variable than late measurements
#'
#' # Test heteroscedasticity
#' anova(fit_kw, fit_kw_hetero)
#'
#' # Example 3: Exponentiated Kumaraswamy for decay tails
#' # Gas decay may show different tail behavior at extreme time points
#' # (very fast initial decay or very slow residual decay)
#' fit_ekw <- gkwreg(
#'   Gas ~ LogT + LogT2 + Level | # alpha: decay curve
#'     Level + LogT | # beta: heteroscedasticity
#'     Level, # lambda: tail heaviness by level
#'   data = retinal,
#'   family = "ekw"
#' )
#' summary(fit_ekw)
#'
#' # Interpretation:
#' # - Lambda varies by level: Different initial concentrations may have
#' #   different rates of extreme decay (very fast or very slow residual gas)
#' # - Important for predicting complete absorption time
#'
#' # Example 4: McDonald distribution for asymmetric decay
#' # Alternative parameterization for skewed decay patterns
#' fit_mc <- gkwreg(
#'   Gas ~ LogT + LogT2 + Level | # gamma
#'     LogT + Level | # delta
#'     Level, # lambda
#'   data = retinal,
#'   family = "mc",
#'   control = gkw_control(
#'     method = "BFGS",
#'     maxit = 1500,
#'     reltol = 1e-8
#'   )
#' )
#' summary(fit_mc)
#'
#' # Model comparison
#' AIC(fit_kw, fit_kw_hetero, fit_ekw, fit_mc)
#' }
"retinal"
