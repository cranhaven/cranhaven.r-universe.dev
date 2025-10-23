#' @title Ratkowsky equation of development rate as a function of temperature (Shi 2016 modification).
#'
#' @description Ratkowsky, D.A., Olley, J., McMeekin, T.A., and Ball, A. (1982) Relationship between
#' temperature and growth rate of bacterial cultures. Journal of Bacteriology 149(1): 1-5.
#' @description Ratkowsky, D.A., R.K. Lowry, T.A. McMeekin, A.N. Stokes, and R.E. Chandler. 1983.
#' Model for bacterial culture growth rate throughout the entire biokinetic temperature range.
#' Journal of Bacteriology 154: 1222-1226.
#' @description Shi, P. J., Reddy, G. V., Chen, L., and Ge, F. (2015). Comparison of thermal performance
#' equations in describing temperature-dependent developmental rates of insects: (I) empirical models.
#' Annals of the Entomological Society of America, 109(2), 211-215.
#'
#' @details Equation:
#' \deqn{rT = (cc * (T - T1) * (1 - e^{k * (T - T2)}))^2}{%
#'       rT = (cc * (T - T1) * (1 - exp(k * (T - T2))))^2}
#'
#' @details where rT is the development rate, T the temperature, T1 and T2 the minimum
#' and maximum temperatures at which rate of growth is zero, cc the slope of the
#' regression as in the rootsq_82 equation, and k a constant. The Ratkowsky model designed
#' for microorganisms has been modified by Shi et al. 2016 to describe the temperature-dependent
#' development rates of insects.
#'
#' @format A list of eight elements describing the equation.
#' \describe{
#'   \item{eq}{The equation (formula object).}
#'   \item{eqAlt}{The equation (string).}
#'   \item{name}{The name of the equation.}
#'   \item{ref}{The equation reference.}
#'   \item{refShort}{The equation reference shortened.}
#'   \item{startVal}{The parameters found in the literature with their references.}
#'   \item{com}{An optional comment about the equation use.}
#'   \item{id}{An id to identify the equation.}
#' }
#' @references \doi{10.1093/aesa/sav121}
#' @docType data
#' @keywords datasets
"ratkowsky_83"

#' @title Beta2 equation of development rate as a function of temperature.
#'
#' @description Yin, X., Kropff, M.J., McLaren, G., and Visperas, R.M. (1995) A nonlinear model for crop
#'   development as a function of temperature. Agricultural and Forest Meteorology 77(1): 1-16.
#' @description Shi, P. J., Chen, L., Hui, C., & Grissino-Mayer, H. D. (2016). Capture the time when
#' plants reach their maximum body size by using the beta sigmoid growth equation. Ecological Modelling,
#' 320, 177-181.
#' @description Shi, P. J., Reddy, G. V., Chen, L., and Ge, F. (2015). Comparison of thermal performance
#' equations in describing temperature-dependent developmental rates of insects: (I) empirical models.
#' Annals of the Entomological Society of America, 109(2), 211-215.
#'
#' @details Equation:
#' \deqn{rT = rm * (\frac{T2 - T}/{T2 - Tm}) * (\frac{T - T1}/{Tm - T1})^{\frac{Tm - T1}/{T2 - Tm}}}{%
#'       rT = rm * (T2 - T)/(T2 - Tm) * ((T - T1)/(Tm - T1))^((Tm - T1)/(T2 - Tm))}
#'
#' @details where rT is the development rate, T the temperature, T1, T2,
#' and Tm the model parameters.
#'
#' @format A list of eight elements describing the equation.
#' \describe{
#'   \item{eq}{The equation (formula object).}
#'   \item{eqAlt}{The equation (string).}
#'   \item{name}{The name of the equation.}
#'   \item{ref}{The equation reference.}
#'   \item{refShort}{The equation reference shortened.}
#'   \item{startVal}{The parameters found in the literature with their references.}
#'   \item{com}{An optional comment about the equation use.}
#'   \item{id}{An id to identify the equation.}
#' }
#' @references \doi{10.1016/j.ecolmodel.2015.09.012}
#' @docType data
#' @keywords datasets
"beta_16"

#' @title Beta equation of development rate as a function of temperature.
#'
#' @description Yin, X., Kropff, M.J., McLaren, G., and Visperas, R.M. (1995) A nonlinear model for crop
#'   development as a function of temperature. Agricultural and Forest Meteorology 77(1): 1-16.
#'
#' @details Equation:
#' \deqn{rT = e^{mu} * (T - Tb)^{aa} * (Tc - T)^{bb}}{%
#'       rT = exp(mu) * (T - Tb)^aa * (Tc - T)^bb}
#'
#' @details where rT is the development rate, T the temperature, mu, aa,
#' and bb the model parameters, Tb the base temperature, and Tc the ceiling
#' temperature.
#'
#' @format A list of eight elements describing the equation.
#' \describe{
#'   \item{eq}{The equation (formula object).}
#'   \item{eqAlt}{The equation (string).}
#'   \item{name}{The name of the equation.}
#'   \item{ref}{The equation reference.}
#'   \item{refShort}{The equation reference shortened.}
#'   \item{startVal}{The parameters found in the literature with their references.}
#'   \item{com}{An optional comment about the equation use.}
#'   \item{id}{An id to identify the equation.}
#' }
#' @references \doi{10.1016/0168-1923(95)02236-Q}
#' @docType data
#' @keywords datasets
"beta_95"

#' @title Performance-2 equation of development rate as a function of temperature.
#'
#' @description Shi, P., Ge, F., Sun, Y., and Chen, C. (2011) A simple model for describing the effect of
#' temperature on insect developmental rate. Journal of Asia-Pacific Entomology 14(1): 15-20.
#' @description Wang, L., P. Shi, C. Chen, and F. Xue. 2013. Effect of temperature on the development
#' of Laodelphax striatellus (Homoptera: Delphacidae). J. Econ. Entomol. 106: 107-114.
#' @description Shi, P. J., Reddy, G. V., Chen, L., and Ge, F. (2016). Comparison of Thermal
#' Performance Equations in Describing Temperature-Dependent Developmental Rates
#' of Insects:(I) Empirical Models. Annals of the Entomological Society of America, 109(2), 211-215.
#'
#' @details Equation:
#' \deqn{rT = cc * (T - T1) * (1 - e^{k * (T - T2)})}{%
#'       rT = cc * (T - T1) * (1 - exp(k * (T - T2)))}
#'
#' @details where rT is the development rate, T the temperature, T1 and T2 the conceptual
#' lower and upper developmental thresholds at which development rates equal zero,
#' and cc and k constants.
#'
#' @format A list of eight elements describing the equation.
#' \describe{
#'   \item{eq}{The equation (formula object).}
#'   \item{eqAlt}{The equation (string).}
#'   \item{name}{The name of the equation.}
#'   \item{ref}{The equation reference.}
#'   \item{refShort}{The equation reference shortened.}
#'   \item{startVal}{The parameters found in the literature with their references.}
#'   \item{com}{An optional comment about the equation use.}
#'   \item{id}{An id to identify the equation.}
#' }
#' @references \doi{10.1016/j.aspen.2010.11.008}
#' @docType data
#' @keywords datasets
"perf2_11"

#' @title Root square equation of development rate as a function of temperature.
#'
#' @description Ratkowsky, D.A., Olley, J., McMeekin, T.A., and Ball, A. (1982) Relationship between
#' temperature and growth rate of bacterial cultures. Journal of Bacteriology 149(1): 1-5.
#'
#' @details Equation:
#' \deqn{rT = (bb * (T - Tb))^{2}}{%
#'       rT = (bb * (T - Tb))^2}
#'
#' @details where rT is the development rate, T the temperature, bb the slope of the
#' regression line, and Tb a conceptual temperature of no metabolic significance.
#'
#' @format A list of eight elements describing the equation.
#' \describe{
#'   \item{eq}{The equation (formula object).}
#'   \item{eqAlt}{The equation (string).}
#'   \item{name}{The name of the equation.}
#'   \item{ref}{The equation reference.}
#'   \item{refShort}{The equation reference shortened.}
#'   \item{startVal}{The parameters found in the literature with their references.}
#'   \item{com}{An optional comment about the equation use.}
#'   \item{id}{An id to identify the equation.}
#' }
#' @references \doi{10.1128/jb.149.1.1-5.1982}
#' @docType data
#' @keywords datasets
"rootsq_82"

#' @title Wang and Engel equation of development rate as a function of temperature.
#'
#' @description Wang, E., and Engel, T. (1998) Simulation of phenological development of wheat crops.
#' Agricultural systems 58(1): 1-24.
#'
#' @details Equation:
#' \deqn{rT = \frac{2 * (T - Tmin)^{aa} * (Topt - Tmin)^{aa} - (T - Tmin)^{2 * aa}}{(Topt - Tmin)^{2 * aa}}}{%
#'       rT = (2 * (T - Tmin)^aa * (Topt - Tmin)^aa - (T - Tmin)^(2 * aa)) / ((Topt - Tmin)^(2 * aa))}
#'
#' @details where rT is the development rate, T the temperature, Tmin the minimum temperature,
#' Topt the optimum temperature, and aa a constant.
#'
#' @format A list of eight elements describing the equation.
#' \describe{
#'   \item{eq}{The equation (formula object).}
#'   \item{eqAlt}{The equation (string).}
#'   \item{name}{The name of the equation.}
#'   \item{ref}{The equation reference.}
#'   \item{refShort}{The equation reference shortened.}
#'   \item{startVal}{The parameters found in the literature with their references.}
#'   \item{com}{An optional comment about the equation use.}
#'   \item{id}{An id to identify the equation.}
#' }
#' @references \doi{10.1016/S0308-521X(98)00028-6}
#' @docType data
#' @keywords datasets
"wangengel_98"

#' @title Ratkowsky equation of development rate as a function of temperature (Shi modification).
#'
#' @description Ratkowsky, D.A., Olley, J., McMeekin, T.A., and Ball, A. (1982) Relationship between
#' temperature and growth rate of bacterial cultures. Journal of Bacteriology 149(1): 1-5.
#' @description Ratkowsky, D.A., R.K. Lowry, T.A. McMeekin, A.N. Stokes, and R.E. Chandler. 1983.
#' Model for bacterial culture growth rate throughout the entire biokinetic temperature range.
#' Journal of Bacteriology 154: 1222-1226.
#' @description Shi, P., Ge, F., Sun, Y., and Chen, C. (2011) A simple model for describing the effect of
#' temperature on insect developmental rate. Journal of Asia-Pacific Entomology 14(1): 15-20.
#'
#' @details Equation:
#' \deqn{rT = (\sqrt{cc} * k1 * (T - T1) * (1 - e^{k2 * (T - T2)}))^{2}}{%
#'       rT = (sqrt(cc) * k1 * (T - T1) * (1 - exp(k2 * (T - T2))))^2}
#'
#' @details where rT is the development rate, T the temperature, T1 and T2 the minimum
#' and maximum temperatures at which rate of growth is zero, sqrt(cc) * k1 the slope of the
#' regression as in the rootsq_82 equation, and k2 a constant. The Ratkowsky model designed
#' for microorganisms has been modified by Shi et al. 2011 to describe the temperature-dependent
#' development rates of insects.
#'
#' @format A list of eight elements describing the equation.
#' \describe{
#'   \item{eq}{The equation (formula object).}
#'   \item{eqAlt}{The equation (string).}
#'   \item{name}{The name of the equation.}
#'   \item{ref}{The equation reference.}
#'   \item{refShort}{The equation reference shortened.}
#'   \item{startVal}{The parameters found in the literature with their references.}
#'   \item{com}{An optional comment about the equation use.}
#'   \item{id}{An id to identify the equation.}
#' }
#' @references \doi{10.1128/jb.149.1.1-5.1982}
#' @references \doi{10.1128/jb.154.3.1222-1226.1983}
#' @docType data
#' @keywords datasets
"ratkowsky_82"

#' @title Regniere equation of development rate as a function of temperature.
#'
#' @description Regniere, J., Powell, J., Bentz, B., and Nealis, V. (2012) Effects of temperature on
#' development, survival and reproduction of insects: experimental design, data analysis
#' and modeling. Journal of Insect Physiology 58(5): 634-47.
#'
#' @details Equation:
#' \deqn{rT = phi * (e^{bb * (T - Tb)} - \frac{Tm - T}{Tm - Tb} * e^{-bb * \frac{T - Tb}{deltab}} - \frac{T - Tb}{Tm - Tb} * e^{\frac{bb * (Tm - Tb) - (Tm - T)}{deltam}})}{%
#'       rT = phi * (exp(bb * (T - Tb)) - ((Tm - T) / (Tm - Tb)) * exp(-bb * (T - Tb) / deltab) - ((T - Tb)/(Tm - Tb)) * exp(bb * (Tm - Tb) - (Tm - T)/deltam))}
#'
#' @details where rT is the development rate, T the temperature, Tb the minimum
#' temperature, Tm the maximum temperature and phi, bb, deltab,
#' and deltam constants (see source for more details).
#'
#' @format A list of eight elements describing the equation.
#' \describe{
#'   \item{eq}{The equation (formula object).}
#'   \item{eqAlt}{The equation (string).}
#'   \item{name}{The name of the equation.}
#'   \item{ref}{The equation reference.}
#'   \item{refShort}{The equation reference shortened.}
#'   \item{startVal}{The parameters found in the literature with their references.}
#'   \item{com}{An optional comment about the equation use.}
#'   \item{id}{An id to identify the equation.}
#' }
#' @references \doi{10.1016/j.jinsphys.2012.01.010}
#' @docType data
#' @keywords datasets
"regniere_12"

#' @title Shi equation of development rate as a function of temperature.
#'
#' @description Shi, P., Ge, F., Sun, Y., and Chen, C. (2011) A simple model for describing the effect of
#' temperature on insect developmental rate. Journal of Asia-Pacific Entomology 14(1): 15-20.
#'
#' @details Equation:
#' \deqn{rT = cc * (1 - e^{-k1 * (T - T1)}) * (1 - e^{k2 * (T - T2)})}{%
#'       rT = cc * (1 - exp(-k1 * (T - T1))) * (1 - exp(k2 * (T - T2)))}
#'
#' @details where rT is the development rate, T the temperature, T1 and T2 the conceptual
#' lower and upper developmental thresholds at which development rates equal zero,
#' and cc k1, and k2 constants.
#'
#' @format A list of eight elements describing the equation.
#' \describe{
#'   \item{eq}{The equation (formula object).}
#'   \item{eqAlt}{The equation (string).}
#'   \item{name}{The name of the equation.}
#'   \item{ref}{The equation reference.}
#'   \item{refShort}{The equation reference shortened.}
#'   \item{startVal}{The parameters found in the literature with their references.}
#'   \item{com}{An optional comment about the equation use.}
#'   \item{id}{An id to identify the equation.}
#' }
#' @references \doi{10.1016/j.aspen.2010.11.008}
#' @docType data
#' @keywords datasets
"shi_11"

# #' @title Hansen equation of development rate as a function of temperature.
# #'
# #' @description Hansen, E.M., Bentz, B.J., Powell, J.A., Gray, D.R., and Vandygriff, J.C. (2011) Prepupal
# #' diapause and instar IV developmental rates of the spruce beetle, Dendroctonus rufipennis
# #' (Coleoptera: Curculionidae, Scolytinae). Journal of insect physiology 57(10): 1347-57.
# #'
# #' @details Equation:
# #' rT ~ p2 * ( (exp(p3 * (T - p1)) - 1) * (exp(p3 * (p5 - p1)) - 1) * exp((T - p5) / p4) )
# #'
# #' @details where rT is the development rate, T the temperature, p1 the lower developmental
# #' treshold, p2 the peak rate control parameter, p3 the low temperature acceleration of
# #' rates, p4 the width of upper thermal boundary layer, and p5 the upper developmental
# #' threshold.
# #'
# #' @format A list of eight elements describing the equation.
# #' \describe{
# #'   \item{eq}{The equation (formula object).}
# #'   \item{eqAlt}{The equation (string).}
# #'   \item{name}{The name of the equation.}
# #'   \item{ref}{The equation reference.}
# #'   \item{refShort}{The equation reference shortened.}
# #'   \item{startVal}{The parameters found in the literature with their references.}
# #'   \item{com}{An optional comment about the equation use.}
# #'   \item{id}{An id to identify the equation.}
# #' }
# #' @references \doi{10.1016/j.jinsphys.2011.06.011}
# "hansen_11"

#' @title Janisch equation of development rate as a function of temperature (Analytis modification).
#'
#' @description Janisch, E. (1932) The influence of temperature on the life-history of insects.
#' Transactions of the Royal Entomological Society of London 80(2): 137-68.
#' @description Analytis, S. (1977) Uber die Relation zwischen biologischer Entwicklung und
#' Temperatur bei phytopathogenen Pilzen. Journal of Phytopathology 90(1): 64-76.
#' @description Analytis, S. (1981). Relationship between temperature and development
#' times in phytopathogenic fungus and in plant pests: a mathematical model. Agric.
#' Res.(Athens), 5, 133-159.
#' @description Kontodimas, D.C., Eliopoulos, P.A., Stathas, G.J. and Economou, L.P. (2004) Comparative
#' temperature-dependent development of Nephus includens (Kirsch) and Nephus bisignatus
#' (Boheman)(Coleoptera: Coccinellidae) preying on Planococcus citri
#' (Risso)(Homoptera: Pseudococcidae): evaluation of a linear and various nonlinear models
#' using specific criteria. Environmental Entomology 33(1): 1-11.
#'
#' @details Equation:
#' \deqn{rT = (\frac{Dmin}{2} * (e^{aa*(T - Topt)} + e^{-bb*(T - Topt)}))^{-1}}{%
#'       rT = (Dmin/2 * (exp(aa*(T - Topt)) + exp(-bb*(T - Topt))))^(-1)}
#'
#' @details where rT is the development rate, T the temperature, Topt the optimum temperature,
#' Dmin, aa, and bb constants.
#'
#' @format A list of eight elements describing the equation.
#' \describe{
#'   \item{eq}{The equation (formula object).}
#'   \item{eqAlt}{The equation (string).}
#'   \item{name}{The name of the equation.}
#'   \item{ref}{The equation reference.}
#'   \item{refShort}{The equation reference shortened.}
#'   \item{startVal}{The parameters found in the literature with their references.}
#'   \item{com}{An optional comment about the equation use.}
#'   \item{id}{An id to identify the equation.}
#' }
#' @references \doi{10.1111/j.1365-2311.1932.tb03305.x}
#' @docType data
#' @keywords datasets
"janisch_32"

#' @title Davidson equation of development rate as a function of temperature.
#'
#' @description Davidson, J. (1944). On the relationship between temperature and rate of development of insects
#' at constant temperatures. The Journal of Animal Ecology:26-38. <doi:10.2307/1326>
#'
#' @details Equation:
#' \deqn{rT = \frac{K}{1 + e^{aa + bb * T}}}{%
#'       rT = K / (1 + exp(aa + bb * T))}
#'
#' @details where rT is the development rate, T the temperature, K the distance between
#' the upper and lower asymptote of the curve, aa the relative position of the origin of
#' the curve on the abscissa, bb the degree of acceleration of development of the life stage
#' in relation to temperature.
#'
#' @format A list of eight elements describing the equation.
#' \describe{
#'   \item{eq}{The equation (formula object).}
#'   \item{eqAlt}{The equation (string).}
#'   \item{name}{The name of the equation.}
#'   \item{ref}{The equation reference.}
#'   \item{refShort}{The equation reference shortened.}
#'   \item{startVal}{The parameters found in the literature with their references.}
#'   \item{com}{An optional comment about the equation use.}
#'   \item{id}{An id to identify the equation.}
#' }
#' @docType data
#' @keywords datasets
"davidson_44"

#' @title Campbell et al. equation of development rate as a function of temperature.
#'
#' @description Campbell, A., Frazer, B. D., Gilbert, N. G. A. P., Gutierrez, A. P., & Mackauer, M. (1974).
#' Temperature requirements of some aphids and their parasites. Journal of applied ecology, 431-438. <doi:10.2307/2402197>
#'
#' @details Equation:
#' \deqn{rT = aa + bb * T}{%
#'       rT = aa + bb * T}
#'
#' @details where rT is the development rate, T the temperature, bb the slope, and aa
#' the point at which the line crosses the rT axis when T = 0.
#'
#' @format A list of eight elements describing the equation.
#' \describe{
#'   \item{eq}{The equation (formula object).}
#'   \item{eqAlt}{The equation (string).}
#'   \item{name}{The name of the equation.}
#'   \item{ref}{The equation reference.}
#'   \item{refShort}{The equation reference shortened.}
#'   \item{startVal}{The parameters found in the literature with their references.}
#'   \item{com}{An optional comment about the equation use.}
#'   \item{id}{An id to identify the equation.}
#' }
#' @docType data
#' @keywords datasets
"campbell_74"

#' @title Stinner et al equation of development rate as a function of temperature.
#'
#' @description Stinner, R., Gutierrez, A. & Butler, G. (1974) An algorithm for temperature-dependent growth
#' rate simulation. The Canadian Entomologist, 106, 519-524.
#'
#' @details Equation:
#' \deqn{rT = \frac{C}{1 + e^{k1 + k2 * T}}}{%
#'       rT = C / (1 + exp(k1 + k2 * T))}
#' and
#' \deqn{rT = \frac{C}{1 + e^{k1 + k2 * (2 * Topt - T)}}}{%
#'       rT = C / (1 + exp(k1 + k2 * (2 * Topt - T)))}
#'
#' @details where rT is the development rate, T the temperature, Topt the optimum temperature,
#' k1 and k2 constants. "[...] the relationship [is] inverted when the temperature is above an
#' optimum [...] T = 2 * Topt - T for T >= Topt." Stinner et al. 1974.
#'
#' @format A list of eight elements describing the equation.
#' \describe{
#'   \item{eq}{The equation (formula object).}
#'   \item{eqAlt}{The equation (string).}
#'   \item{name}{The name of the equation.}
#'   \item{ref}{The equation reference.}
#'   \item{refShort}{The equation reference shortened.}
#'   \item{startVal}{The parameters found in the literature with their references.}
#'   \item{com}{An optional comment about the equation use.}
#'   \item{id}{An id to identify the equation.}
#' }
#' @references \doi{10.4039/Ent106519-5}
#' @docType data
#' @keywords datasets
"stinner_74"

#' @title Logan et al. equation 6 of development rate as a function of temperature.
#'
#' @description Logan, J. A., Wollkind, D. J., Hoyt, S. C., and Tanigoshi, L. K. (1976). An analytic model
#' for description of temperature dependent rate phenomena in arthropods. Environmental
#' Entomology, 5(6), 1133-1140.
#'
#' @details Equation:
#' \deqn{rT = phi * (e^{bb * T} - e^{bb * Tmax - \frac{Tmax - T}{deltaT}})}{%
#'       rT = phi * (exp(bb * T) - exp(bb * Tmax - (Tmax - T)/deltaT))}
#'
#' @details where rT is the development rate, T the temperature, Tmax the maximum temperature,
#' deltaT the width of the high temperature boundary layer, phi the developmental rate at some
#' base temperature above developmental threshold, and bb a constant.
#'
#' @format A list of eight elements describing the equation.
#' \describe{
#'   \item{eq}{The equation (formula object).}
#'   \item{eqAlt}{The equation (string).}
#'   \item{name}{The name of the equation.}
#'   \item{ref}{The equation reference.}
#'   \item{refShort}{The equation reference shortened.}
#'   \item{startVal}{The parameters found in the literature with their references.}
#'   \item{com}{An optional comment about the equation use.}
#'   \item{id}{An id to identify the equation.}
#' }
#' @references \doi{10.1093/ee/5.6.1133}
#' @docType data
#' @keywords datasets
"logan6_76"

#' @title Logan et al. equation 10 of development rate as a function of temperature.
#'
#' @description Logan, J. A., Wollkind, D. J., Hoyt, S. C., and Tanigoshi, L. K. (1976). An analytic model
#' for description of temperature dependent rate phenomena in arthropods. Environmental
#' Entomology, 5(6), 1133-1140.
#'
#' @details Equation:
#' \deqn{rT = alpha * (\frac{1}{1 + cc * e^{- bb * T}} - e^{-\frac{Tmax - T}{deltaT}})}{%
#'       rT = alpha * (1/(1 + cc * exp(- bb * T)) - exp(-((Tmax - T)/deltaT)))}
#'
#' @details where rT is the development rate, T the temperature, Tmax the maximum temperature,
#' deltaT the width of the high temperature boundary layer, and alpha and bb constants.
#'
#' @format A list of eight elements describing the equation.
#' \describe{
#'   \item{eq}{The equation (formula object).}
#'   \item{eqAlt}{The equation (string).}
#'   \item{name}{The name of the equation.}
#'   \item{ref}{The equation reference.}
#'   \item{refShort}{The equation reference shortened.}
#'   \item{startVal}{The parameters found in the literature with their references.}
#'   \item{com}{An optional comment about the equation use.}
#'   \item{id}{An id to identify the equation.}
#' }
#' @references \doi{10.1093/ee/5.6.1133}
#' @docType data
#' @keywords datasets
"logan10_76"

#' @title Sharpe and DeMichele equation of development rate as a function of temperature.
#'
#' @description Sharpe, P.J. & DeMichele, D.W. (1977) Reaction kinetics of poikilotherm development.
#' Journal of Theoretical Biology, 64, 649-670.
#'
#' @details Equation:
#' \deqn{rT = \frac{(T + 273.16) * e^{\frac{aa - \frac{bb}{T + 273.16}}{1.987}}}{1 + e^{\frac{cc - \frac{dd}{T + 273.16}}{1.987}} + e^{\frac{ff - \frac{gg}{T + 273.16}}{1.987}}}}{%
#'       rT = ((T + 273.16) * exp((aa - bb/(T + 273.16))/1.987)) / (1 + exp((cc - dd/(T + 273.16))/1.987) + exp((ff - gg/(T + 273.16))/1.987))}
#'
#' @details where rT is the development rate, T the temperature, and aa, bb, cc,
#' dd, ff, and gg thermodynamic parameters.
#'
#' @format A list of eight elements describing the equation.
#' \describe{
#'   \item{eq}{The equation (formula object).}
#'   \item{eqAlt}{The equation (string).}
#'   \item{name}{The name of the equation.}
#'   \item{ref}{The equation reference.}
#'   \item{refShort}{The equation reference shortened.}
#'   \item{startVal}{The parameters found in the literature with their references.}
#'   \item{com}{An optional comment about the equation use.}
#'   \item{id}{An id to identify the equation.}
#' }
#' @references \doi{10.1016/0022-5193(77)90265-X}
#' @docType data
#' @keywords datasets
"sharpeDeMichele_77"

#' @title Analytis equation of development rate as a function of temperature.
#'
#' @description Analytis, S. (1977) Uber die Relation zwischen biologischer Entwicklung und Temperatur bei
#' phytopathogenen Pilzen. Journal of Phytopathology 90(1): 64-76.
#'
#' @details Equation:
#' \deqn{rT = aa * (T - Tmin)^{bb} * (Tmax - T)^{cc}}{%
#'       rT = aa * (T - Tmin)^bb * (Tmax - T)^cc}
#'
#' @details where rT is the development rate, T the temperature, Tmin the minimum
#' temperature, Tmax the maximum temperature, and aa, bb, and cc constants.
#'
#' @format A list of eight elements describing the equation.
#' \describe{
#'   \item{eq}{The equation (formula object).}
#'   \item{eqAlt}{The equation (string).}
#'   \item{name}{The name of the equation.}
#'   \item{ref}{The equation reference.}
#'   \item{refShort}{The equation reference shortened.}
#'   \item{startVal}{The parameters found in the literature with their references.}
#'   \item{com}{An optional comment about the equation use.}
#'   \item{id}{An id to identify the equation.}
#' }
#' @references \doi{10.1111/j.1439-0434.1977.tb02886.x}
#' @docType data
#' @keywords datasets
"analytis_77"

#' @title Schoolfield et al. equation of development rate as a function of temperature.
#'
#' @description Schoolfield, R., Sharpe, P. & Magnuson, C. (1981) Non-linear regression of biological
#' temperature-dependent rate models based on absolute reaction-rate theory.
#' Journal of theoretical biology, 88, 719-731.
#'
#' @details Equation:
#' \deqn{rT = \frac{p25 * \frac{T + 273.16}{298} * e^{\frac{aa}{1.987} * (\frac{1}{298} - \frac{1}{T + 273.16})}}{1 + e^{\frac{bb}{1.987} * (\frac{1}{cc} - \frac{1}{T + 273.16})} + e^{\frac{dd}{1.987} * (\frac{1}{ee} - \frac{1}{T + 273.16})}}}{%
#'       rT = (p25 * (T + 273.16)/298 * exp(aa/1.987 * (1/298 - 1/(T + 273.16)))) / (1 + exp(bb/1.987 * (1/cc - 1/(T + 273.16))) + exp(dd/1.987 * (1/ee - 1/(T + 273.16))))}
#'
#' @details where rT is the development rate, T the temperature, p25 the development
#' rate at 25 degree Celsius assuming no enzyme inactivation, aa the enthalpy of
#' activation of the reaction that is catalyzed by the enzyme, bb the change in
#' enthalpy associated with low temperature inactivation of the enzyme, cc the
#' temperature at which the enzyme is 1/2 active and 1/2 low temperature inactive,
#' dd the change in enthalpy associated with high temperature inactivation of the enzyme,
#' and ee the temperature at which the enzyme is 1/2 active and 1/2 high temperature
#' inactive.
#'
#' @format A list of eight elements describing the equation.
#' \describe{
#'   \item{eq}{The equation (formula object).}
#'   \item{eqAlt}{The equation (string).}
#'   \item{name}{The name of the equation.}
#'   \item{ref}{The equation reference.}
#'   \item{refShort}{The equation reference shortened.}
#'   \item{startVal}{The parameters found in the literature with their references.}
#'   \item{com}{An optional comment about the equation use.}
#'   \item{id}{An id to identify the equation.}
#' }
#' @references \doi{10.1016/0022-5193(81)90246-0}
#' @docType data
#' @keywords datasets
"schoolfield_81"

#' @title Schoolfield et al. equation of development rate as a function of temperature for
#' intermediate to high temperatures only.
#'
#' @description Schoolfield, R., Sharpe, P. & Magnuson, C. (1981) Non-linear regression of biological
#' temperature-dependent rate models based on absolute reaction-rate theory.
#' Journal of theoretical biology, 88, 719-731.
#' Wagner, T.L., Wu, H.I., Sharpe, P.S.H., Schoolfield, R.M., Coulson, R.N. (1984) Modeling
#' insect development rates: a literature review and application of a biophysical model.
#' Annals of the Entomological Society of America 77(2): 208-20.
#'
#' @details Equation:
#' \deqn{rT = \frac{p25 * \frac{T + 273.16}{298} * e^{\frac{aa}{1.987} * (\frac{1}{298} - \frac{1}{T + 273.16})}}{1 + e^{\frac{dd}{1.987} * (\frac{1}{ee} - \frac{1}{T + 273.16})}}}{%
#'       rT = (p25 * (T + 273.16)/298 * exp(aa/1.987 * (1/298 - 1/(T + 273.16)))) / (1 + exp(dd/1.987 * (1/ee - 1/(T + 273.16))))}
#'
#' @details where rT is the development rate, T the temperature, p25 the development
#' rate at 25 degrees Celsius assuming no enzyme inactivation, aa the enthalpy of
#' activation of the reaction that is catalyzed by the enzyme, bb the change in
#' enthalpy associated with low temperature inactivation of the enzyme, cc the
#' the temperature at which the enzyme is 1/2 active and 1/2 low temperature inactive,
#' dd the cange in enthalpy associated with high temperature inactivation of the enzyme,
#' and ee the temperature at which the enzyme is 1/2 active and 1/2 high temperature
#' inactive.
#'
#' @format A list of eight elements describing the equation.
#' \describe{
#'   \item{eq}{The equation (formula object).}
#'   \item{eqAlt}{The equation (string).}
#'   \item{name}{The name of the equation.}
#'   \item{ref}{The equation reference.}
#'   \item{refShort}{The equation reference shortened.}
#'   \item{startVal}{The parameters found in the literature with their references.}
#'   \item{com}{An optional comment about the equation use.}
#'   \item{id}{An id to identify the equation.}
#' }
#' @references \doi{10.1016/0022-5193(81)90246-0}
#' @docType data
#' @keywords datasets
"schoolfieldHigh_81"

#' @title Schoolfield et al. equation of development rate as a function of temperature for
#' intermediate to low temperatures only.
#'
#' @description Schoolfield, R., Sharpe, P. & Magnuson, C. (1981) Non-linear regression of biological
#' temperature-dependent rate models based on absolute reaction-rate theory.
#' Journal of theoretical biology, 88, 719-731.
#' Wagner, T.L., Wu, H.I., Sharpe, P.S.H., Schoolfield, R.M., Coulson, R.N. (1984) Modeling
#' insect development rates: a literature review and application of a biophysical model.
#' Annals of the Entomological Society of America 77(2): 208-20.
#'
#' @details Equation:
#' \deqn{rT = \frac{p25 * \frac{T + 273.16}{298} * e^{\frac{aa}{1.987} * (\frac{1}{298} - \frac{1}{T + 273.16})}}{1 + e^{\frac{bb}{1.987} * (\frac{1}{cc} - \frac{1}{T + 273.16})}}}{%
#'       rT = (p25 * (T + 273.16)/298 * exp(aa/1.987 * (1/298 - 1/(T + 273.16)))) / (1 + exp(bb/1.987 * (1/cc - 1/(T + 273.16))))}
#'
#' @details where rT is the development rate, T the temperature, p25 the development
#' rate at 25 degrees Celsius assuming no enzyme inactivation, aa the enthalpy of
#' activation of the reaction that is catalyzed by the enzyme, bb the change in
#' enthalpy associated with low temperature inactivation of the enzyme, cc the
#' the temperature at which the enzyme is 1/2 active and 1/2 low temperature inactive,
#' dd the cange in enthalpy associated with high temperature inactivation of the enzyme,
#' and ee the temperature at which the enzyme is 1/2 active and 1/2 high temperature
#' inactive.
#'
#' @format A list of eight elements describing the equation.
#' \describe{
#'   \item{eq}{The equation (formula object).}
#'   \item{eqAlt}{The equation (string).}
#'   \item{name}{The name of the equation.}
#'   \item{ref}{The equation reference.}
#'   \item{refShort}{The equation reference shortened.}
#'   \item{startVal}{The parameters found in the literature with their references.}
#'   \item{com}{An optional comment about the equation use.}
#'   \item{id}{An id to identify the equation.}
#' }
#' @references \doi{10.1016/0022-5193(81)90246-0}
#' @docType data
#' @keywords datasets
"schoolfieldLow_81"

#' @title Taylor equation of development rate as a function of temperature.
#'
#' @description Taylor, F. (1981) Ecology and evolution of physiological time in insects.
#' American Naturalist, 1-23.
#' @description Lamb, RJ. (1992) Developmental rate of Acyrthosiphon pisum (Homoptera: Aphididae) at low
#' temperatures: implications for estimating rate parameters for insects.
#' Environmental Entomology 21(1): 10-19.
#'
#' @details Equation:
#' \deqn{rT = Rm * e^{-\frac{1}{2} * (\frac{T - Tm}{To})^{2}}}{%
#'       rT = Rm * exp(-1/2 * ((T - Tm)/To)^2)}
#'
#' @details where rT is the development rate, T the temperature, Rm the maximum
#' development rate, Tm the optimum temperature, and To the rate at which development
#' rate falls away from Tm.
#'
#' @format A list of eight elements describing the equation.
#' \describe{
#'   \item{eq}{The equation (formula object).}
#'   \item{eqAlt}{The equation (string).}
#'   \item{name}{The name of the equation.}
#'   \item{ref}{The equation reference.}
#'   \item{refShort}{The equation reference shortened.}
#'   \item{startVal}{The parameters found in the literature with their references.}
#'   \item{com}{An optional comment about the equation use.}
#'   \item{id}{An id to identify the equation.}
#' }
#' @docType data
#' @keywords datasets
"taylor_81"

#' @title Second-order polynomial equation of development rate as a function of temperature.
#'
#' @description A simple second-order polynomial equation.
#'
#' @details Equation:
#' \deqn{rT = a0 + a1 * T + a2 * T^{2}}{%
#'       rT = a0 + a1 * T + a2 * T^2}
#'
#' @details where rT is the development rate, T the temperature, and a0, a1, and a2 are
#' constants.
#'
#' @format A list of eight elements describing the equation.
#' \describe{
#'   \item{eq}{The equation (formula object).}
#'   \item{eqAlt}{The equation (string).}
#'   \item{name}{The name of the equation.}
#'   \item{ref}{The equation reference.}
#'   \item{refShort}{The equation reference shortened.}
#'   \item{startVal}{The parameters found in the literature with their references.}
#'   \item{com}{An optional comment about the equation use.}
#'   \item{id}{An id to identify the equation.}
#' }
#' @docType data
#' @keywords datasets
"poly2"

#' @title Harcourt and Yee equation of development rate as a function of temperature.
#'
#' @description Harcourt, D. and Yee, J. (1982) Polynomial algorithm for predicting the duration of insect
#' life stages. Environmental Entomology, 11, 581-584.
#'
#' @details Equation:
#' \deqn{rT = a0 + a1 * T + a2 * T^{2} + a3 * T^{3}}{%
#'       rT = a0 + a1 * T + a2 * T^2 + a3 * T^3}
#'
#' @details where rT is the development rate, T the temperature, and a0, a1, a2, and a3 are
#' constants.
#'
#' @format A list of eight elements describing the equation.
#' \describe{
#'   \item{eq}{The equation (formula object).}
#'   \item{eqAlt}{The equation (string).}
#'   \item{name}{The name of the equation.}
#'   \item{ref}{The equation reference.}
#'   \item{refShort}{The equation reference shortened.}
#'   \item{startVal}{The parameters found in the literature with their references.}
#'   \item{com}{An optional comment about the equation use.}
#'   \item{id}{An id to identify the equation.}
#' }
#' @references \doi{10.1093/ee/11.3.581}
#' @docType data
#' @keywords datasets
"harcourtYee_82"

#' @title Fourth-order polynomial equation of development rate as a function of temperature.
#'
#' @description A simple fourth-order polynomial equation.
#'
#' @details Equation:
#' \deqn{rT = a0 + a1 * T + a2 * T^{2} + a3 * T^{3} + a4 * T^{4}}{%
#'       rT = a0 + a1 * T + a2 * T^2 + a3 * T^3 + a4 * T^4}
#'
#' @details where rT is the development rate, T the temperature, and a0, a1, a2, a3, and a4 are
#' constants.
#'
#' @format A list of eight elements describing the equation.
#' \describe{
#'   \item{eq}{The equation (formula object).}
#'   \item{eqAlt}{The equation (string).}
#'   \item{name}{The name of the equation.}
#'   \item{ref}{The equation reference.}
#'   \item{refShort}{The equation reference shortened.}
#'   \item{startVal}{The parameters found in the literature with their references.}
#'   \item{com}{An optional comment about the equation use.}
#'   \item{id}{An id to identify the equation.}
#' }
#' @docType data
#' @keywords datasets
"poly4"

#' @title Holling type III equation of development rate as a function of temperature.
#'
#' @description Hilbert, DW, y JA Logan (1983) Empirical model of nymphal development for the migratory
#' grasshopper, Melanoplus sanguinipes (Orthoptera: Acrididae).
#' Environmental Entomology 12(1): 1-5.
#'
#' @details Equation:
#' \deqn{rT = phi * ((\frac{(T-Tb)^{2}}{(T-Tb)^{2} + aa^{2}}) - e^{-\frac{Tmax - (T-Tb)}{deltaT}})}{%
#'       rT = phi * (((T-Tb)^2 / ((T-Tb)^2 + aa^2)) - exp(-(Tmax - (T-Tb))/deltaT))}
#'
#' @details where rT is the development rate, T the temperature, Tb the minimum
#' temperature for development, deltaT the width of high temperature boundary area,
#' Tmax the maximum temperature, and aa a constant.
#'
#' @format A list of eight elements describing the equation.
#' \describe{
#'   \item{eq}{The equation (formula object).}
#'   \item{eqAlt}{The equation (string).}
#'   \item{name}{The name of the equation.}
#'   \item{ref}{The equation reference.}
#'   \item{refShort}{The equation reference shortened.}
#'   \item{startVal}{The parameters found in the literature with their references.}
#'   \item{com}{An optional comment about the equation use.}
#'   \item{id}{An id to identify the equation.}
#' }
#' @references \doi{10.1093/ee/12.1.1}
#' @docType data
#' @keywords datasets
"hilbertLogan_83"

#' @title Lamb equation of development rate as a function of temperature.
#'
#' @description Lamb, R. J., Gerber, G. H., & Atkinson, G. F. (1984). Comparison of developmental rate curves
#' applied to egg hatching data of Entomoscelis americana Brown (Coleoptera: Chrysomelidae).
#' Environmental entomology, 13(3), 868-872.
#' @description Lamb, RJ. (1992) Developmental rate of Acyrthosiphon pisum (Homoptera: Aphididae) at low
#' temperatures: implications for estimating rate parameters for insects.
#' Environmental Entomology 21(1): 10-19.
#'
#' @details Equation:
#' \deqn{rT = Rm * e^{-\frac{1}{2} * (\frac{T - Tmax}{To})^{2}}}{%
#'       rT = Rm * exp(-1/2 * ((T - Tmax)/To)^2)}
#' and
#' \deqn{rT = Rm * e^{-\frac{1}{2} * (\frac{T - Tmax}{T1})^{2}}}{%
#'       rT = Rm * exp(-1/2 * ((T - Tmax)/T1)^2)}
#'
#' @details where rT is the development rate, T the temperature, Rm the maximum
#' development rate, Tmax the optimum temperature, and To and T1 the shape parameter giving
#' the spread of the curve.
#'
#' @format A list of eight elements describing the equation.
#' \describe{
#'   \item{eq}{The equation (formula object).}
#'   \item{eqAlt}{The equation (string).}
#'   \item{name}{The name of the equation.}
#'   \item{ref}{The equation reference.}
#'   \item{refShort}{The equation reference shortened.}
#'   \item{startVal}{The parameters found in the literature with their references.}
#'   \item{com}{An optional comment about the equation use.}
#'   \item{id}{An id to identify the equation.}
#' }
#' @references \doi{10.1093/ee/21.1.10}
#' @docType data
#' @keywords datasets
"lamb_92"

#' @title Lactin et al. equation 1 of development rate as a function of temperature.
#'
#' @description Lactin, Derek J, NJ Holliday, DL Johnson, y R Craigen (995) Improved rate model of
#' temperature-dependent development by arthropods. Environmental Entomology 24(1): 68-75.
#'
#' @details Equation:
#' \deqn{rT = e^{aa * T} - e^{aa * Tmax - \frac{Tmax - T}{deltaT}}}{%
#'       rT = exp(aa * T) - exp(aa * Tmax - (Tmax - T)/deltaT)}
#'
#' @details where rT is the development rate, T the temperature, and aa, Tmax,
#' and deltaT fitted parameters.
#'
#' @format A list of eight elements describing the equation.
#' \describe{
#'   \item{eq}{The equation (formula object).}
#'   \item{eqAlt}{The equation (string).}
#'   \item{name}{The name of the equation.}
#'   \item{ref}{The equation reference.}
#'   \item{refShort}{The equation reference shortened.}
#'   \item{startVal}{The parameters found in the literature with their references.}
#'   \item{com}{An optional comment about the equation use.}
#'   \item{id}{An id to identify the equation.}
#' }
#' @references \doi{10.1093/ee/24.1.68}
#' @docType data
#' @keywords datasets
"lactin1_95"

#' @title Lactin et al. equation 2 of development rate as a function of temperature.
#'
#' @description Lactin, Derek J, NJ Holliday, DL Johnson, y R Craigen (995) Improved rate model of
#' temperature-dependent development by arthropods. Environmental Entomology 24(1): 68-75.
#'
#' @details Equation:
#' \deqn{rT = e^{aa * T} - e^{aa * Tmax - \frac{Tmax - T}{deltaT}} + bb}{%
#'       rT = exp(aa * T) - exp(aa * Tmax - (Tmax - T)/deltaT) + bb}
#'
#' @details where rT is the development rate, T the temperature, and aa, bb, Tmax,
#' and deltaT fitted parameters.
#'
#' @format A list of eight elements describing the equation.
#' \describe{
#'   \item{eq}{The equation (formula object).}
#'   \item{eqAlt}{The equation (string).}
#'   \item{name}{The name of the equation.}
#'   \item{ref}{The equation reference.}
#'   \item{refShort}{The equation reference shortened.}
#'   \item{startVal}{The parameters found in the literature with their references.}
#'   \item{com}{An optional comment about the equation use.}
#'   \item{id}{An id to identify the equation.}
#' }
#' @references \doi{10.1093/ee/24.1.68}
#' @docType data
#' @keywords datasets
"lactin2_95"

#' @title Briere et al equation 1 of development rate as a function of temperature.
#'
#' @description Briere, J.F., Pracros, P., le Roux, A.Y. and Pierre, S. (1999) A novel rate model of
#' temperature-dependent development for arthropods. Environmental Entomology, 28, 22-29.
#'
#' @details Equation:
#' \deqn{rT = aa * T * (T - Tmin) * (Tmax - T)^{\frac{1}{2}}}{%
#'       rT = aa * T * (T - Tmin) * (Tmax - T)^(1 / 2)}
#'
#' @details where rT is the development rate, T the temperature, Tmin the low
#' temperature developmental threshold, Tmax the lethal temperature, and aa
#' an empirical constant.
#'
#' @format A list of eight elements describing the equation.
#' \describe{
#'   \item{eq}{The equation (formula object).}
#'   \item{eqAlt}{The equation (string).}
#'   \item{name}{The name of the equation.}
#'   \item{ref}{The equation reference.}
#'   \item{refShort}{The equation reference shortened.}
#'   \item{startVal}{The parameters found in the literature with their references.}
#'   \item{com}{An optional comment about the equation use.}
#'   \item{id}{An id to identify the equation.}
#' }
#' @references \doi{10.1093/ee/28.1.22}
#' @docType data
#' @keywords datasets
"briere1_99"

#' @title Briere et al equation 2 of development rate as a function of temperature.
#'
#' @description Briere, J.F., Pracros, P., le Roux, A.Y. and Pierre, S. (1999) A novel rate model of
#' temperature-dependent development for arthropods. Environmental Entomology, 28, 22-29.
#'
#' @details Equation:
#' \deqn{rT = aa * T * (T - Tmin) * (Tmax - T)^{\frac{1}{bb}}}{%
#'       rT = aa * T * (T - Tmin) * (Tmax - T)^(1 / bb)}
#'
#' @details where rT is the development rate, T the temperature, Tmin the low
#' temperature developmental threshold, Tmax the lethal temperature, and aa and
#' bb empirical constants.
#'
#' @format A list of eight elements describing the equation.
#' \describe{
#'   \item{eq}{The equation (formula object).}
#'   \item{eqAlt}{The equation (string).}
#'   \item{name}{The name of the equation.}
#'   \item{ref}{The equation reference.}
#'   \item{refShort}{The equation reference shortened.}
#'   \item{startVal}{The parameters found in the literature with their references.}
#'   \item{com}{An optional comment about the equation use.}
#'   \item{id}{An id to identify the equation.}
#' }
#' @references \doi{10.1093/ee/28.1.22}
#' @docType data
#' @keywords datasets
"briere2_99"

#' @title Kontodimas et al. equation of development rate as a function of temperature.
#'
#' @description Kontodimas, D.C., Eliopoulos, P.A., Stathas, G.J. and Economou, L.P. (2004) Comparative
#' temperature-dependent development of Nephus includens (Kirsch) and Nephus bisignatus
#' (Boheman)(Coleoptera: Coccinellidae) preying on Planococcus citri
#' (Risso)(Homoptera: Pseudococcidae): evaluation of a linear and various nonlinear models
#' using specific criteria. Environmental Entomology 33(1): 1-11.
#'
#' @details Equation:
#' \deqn{rT = aa * (T - Tmin)^{2} * (Tmax - T)}{%
#'       rT = aa * (T - Tmin)^2 * (Tmax - T)}
#'
#' @details where rT is the development rate, T the temperature, Tmin the minimum
#' temperature, Tmax the maximum temperature, and aa a constant.
#'
#' @format A list of eight elements describing the equation.
#' \describe{
#'   \item{eq}{The equation (formula object).}
#'   \item{eqAlt}{The equation (string).}
#'   \item{name}{The name of the equation.}
#'   \item{ref}{The equation reference.}
#'   \item{refShort}{The equation reference shortened.}
#'   \item{startVal}{The parameters found in the literature with their references.}
#'   \item{com}{An optional comment about the equation use.}
#'   \item{id}{An id to identify the equation.}
#' }
#' @references \doi{10.1603/0046-225X-33.1.1}
#' @docType data
#' @keywords datasets
"kontodimas_04"

#' @title Simplified beta type equation of development rate as a function of temperature.
#'
#' @description Damos, P.T., and Savopoulou-Soultani, M. (2008). Temperature-dependent bionomics and modeling
#' of Anarsia lineatella (Lepidoptera: Gelechiidae) in the laboratory.
#' Journal of economic entomology, 101(5), 1557-1567.
#'
#' @details Equation:
#' \deqn{rT = aa * (bb - \frac{T}{10}) * (\frac{T}{10})^{cc}}{%
#'       rT = aa * (bb - T / 10) * (T / 10)^cc}
#'
#' @details where rT is the development rate, T the temperature, and aa, bb, and
#' cc empirical constant parameters.
#'
#' @format A list of eight elements describing the equation.
#' \describe{
#'   \item{eq}{The equation (formula object).}
#'   \item{eqAlt}{The equation (string).}
#'   \item{name}{The name of the equation.}
#'   \item{ref}{The equation reference.}
#'   \item{refShort}{The equation reference shortened.}
#'   \item{startVal}{The parameters found in the literature with their references.}
#'   \item{com}{An optional comment about the equation use.}
#'   \item{id}{An id to identify the equation.}
#' }
#' @references \doi{10.1093/jee/101.5.1557}
#' @docType data
#' @keywords datasets
"damos_08"

#' @title Inverse second-order polynomial equation of development rate as a function of temperature.
#'
#' @description Damos, P., and Savopoulou-Soultani, M. (2011) Temperature-driven models for insect
#' development and vital thermal requirements. Psyche: A Journal of Entomology, 2012.
#'
#' @details Equation:
#' \deqn{rT = \frac{aa}{1 + bb * T + cc * T^{2}}}{%
#'       rT = aa / (1 + bb * T + cc * T^2)}
#'
#' @details where rT is the development rate, T the temperature, and aa, bb, and
#' cc empirical constant parameters.
#'
#' @format A list of eight elements describing the equation.
#' \describe{
#'   \item{eq}{The equation (formula object).}
#'   \item{eqAlt}{The equation (string).}
#'   \item{name}{The name of the equation.}
#'   \item{ref}{The equation reference.}
#'   \item{refShort}{The equation reference shortened.}
#'   \item{startVal}{The parameters found in the literature with their references.}
#'   \item{com}{An optional comment about the equation use.}
#'   \item{id}{An id to identify the equation.}
#' }
#' @references \doi{10.1155/2012/123405}
#' @docType data
#' @keywords datasets
"damos_11"

#' @title Wang et al. equation of development rate as a function of temperature.
#'
#' @description Wang, R., Lan, Z. and Ding, Y. (1982) Studies on mathematical models of the relationship
#' between insect development and temperature. Acta Ecol. Sin, 2, 47-57.
#'
#' @details Equation:
#' \deqn{rT = \frac{K}{1 + e^{-r*(T - T0)}} * (1 - e^{-\frac{T - TL}{aa}}) * (1 - e^{-\frac{TH - T}{aa}})}{%
#'       rT = (K / (1 + exp(-r*(T - T0)))) * (1 - exp(-(T - TL)/aa)) * (1 - exp(-(TH - T)/aa))}
#'
#' @details where rT is the development rate, T the temperature, and K, r, T0, TH, and TL constants.
#'
#' @format A list of eight elements describing the equation.
#' \describe{
#'   \item{eq}{The equation (formula object).}
#'   \item{eqAlt}{The equation (string).}
#'   \item{name}{The name of the equation.}
#'   \item{ref}{The equation reference.}
#'   \item{refShort}{The equation reference shortened.}
#'   \item{startVal}{The parameters found in the literature with their references.}
#'   \item{com}{An optional comment about the equation use.}
#'   \item{id}{An id to identify the equation.}
#' }
#' @references \url{http://en.cnki.com.cn}
#' @docType data
#' @keywords datasets
"wang_82"

#' @title Bayoh and Lindsay equation of development rate as a function of temperature.
#'
#' @description Bayoh, M.N., Lindsay, S.W. (2003) Effect of temperature on the development of the aquatic
#' stages of Anopheles gambiae sensu stricto (Diptera: Culicidae). Bulletin of entomological
#' research 93(5): 375-81.
#'
#' @details Equation:
#' \deqn{rT = aa + bb * T + cc * e^{T} + dd * e^{-T}}{%
#'       rT = aa + bb * T + cc * exp(T) + dd * exp(-T)}
#'
#' @details where rT is the development rate, T the temperature, and aa, bb,
#' cc, and dd empirical constant parameters.
#'
#' @format A list of eight elements describing the equation.
#' \describe{
#'   \item{eq}{The equation (formula object).}
#'   \item{eqAlt}{The equation (string).}
#'   \item{name}{The name of the equation.}
#'   \item{ref}{The equation reference.}
#'   \item{refShort}{The equation reference shortened.}
#'   \item{startVal}{The parameters found in the literature with their references.}
#'   \item{com}{An optional comment about the equation use.}
#'   \item{id}{An id to identify the equation.}
#' }
#' @references \doi{10.1079/BER2003259}
#' @docType data
#' @keywords datasets
"bayoh_03"

#' @title Hagstrum et Milliken equation of development rate as a function of temperature retrieved
#' from Wagner 1984.
#'
#' @description Hagstrum, D.W., Milliken, G.A. (1988) Quantitative analysis of temperature, moisture, and
#' diet factors affecting insect development. Annals of the Entomological Society of America
#' 81(4): 539-46.
#' @description Wagner, T.L., Wu, H.I., Sharpe, P.S.H., Schoolfield, R.M., Coulson, R.N. (1984) Modeling
#' insect development rates: a literature review and application of a biophysical model.
#' Annals of the Entomological Society of America 77(2): 208-20.
#'
#' @details Equation:
#' \deqn{rT = \frac{1}{\frac{1 + e^{\frac{cc}{1.987} * (\frac{1}{dd} - \frac{1}{T + 273.16})}}{aa*\frac{T + 273.16}{298.15}*e^{\frac{bb}{1.987}*(\frac{1}{298.15} - \frac{1}{T + 273.16})}}}}{%
#'       rT = 1/( (1 + exp((cc/1.987) * ((1/dd) - (1/(T + 273.16))) )) / (aa * (T + 273.16)/298.15 * exp( (bb/1.987) * ((1/298.15) - 1/(T + 273.16)) ) ) )}
#'
#' @details where rT is the development rate, T the temperature, and aa, bb, cc,
#' and dd are thermodynamic parameters.
#'
#' @format A list of eight elements describing the equation.
#' \describe{
#'   \item{eq}{The equation (formula object).}
#'   \item{eqAlt}{The equation (string).}
#'   \item{name}{The name of the equation.}
#'   \item{ref}{The equation reference.}
#'   \item{refShort}{The equation reference shortened.}
#'   \item{startVal}{The parameters found in the literature with their references.}
#'   \item{com}{An optional comment about the equation use.}
#'   \item{id}{An id to identify the equation.}
#' }
#'
#' @references \doi{10.1093/aesa/77.2.208}
#' @references \doi{10.1093/aesa/81.4.539}
#' @docType data
#' @keywords datasets
"wagner_88"

#' @title Bieri equation 1 of development rate as a function of temperature.
#'
#' @description Bieri, M., Baumgartner, J., Bianchi, G., Delucchi, V., Arx, R. von. (1983)
#' Development and fecundity of pea aphid (Acyrthosiphon pisum Harris) as
#' affected by constant temperatures and by pea varieties. Mitteilungen der
#' Schweizerischen Entomologischen Gesellschaft, 56, 163-171.
#' @description Kumar, S., and Kontodimas, D.C. (2012). Temperature-dependent
#' development of Phenacoccus solenopsis under laboratory conditions.
#' Entomologia Hellenica, 21, 25-38.
#'
#' @details Equation:
#' \deqn{rT = aa * (T - Tmin) - (bb * e^{T - Tm})}{%
#'       rT = aa * (T - Tmin) - (bb * exp(T - Tm))}
#'
#' @details where rT is the development rate, T the temperature, Tmin the minimum
#' temperature, and aa, bb, and Tm fitted coefficients.
#'
#' @format A list of eight elements describing the equation.
#' \describe{
#'   \item{eq}{The equation (formula object).}
#'   \item{eqAlt}{The equation (string).}
#'   \item{name}{The name of the equation.}
#'   \item{ref}{The equation reference.}
#'   \item{refShort}{The equation reference shortened.}
#'   \item{startVal}{The parameters found in the literature with their references.}
#'   \item{com}{An optional comment about the equation use.}
#'   \item{id}{An id to identify the equation.}
#' }
#' @references \url{http://www.e-periodica.ch}
#' @docType data
#' @keywords datasets
"bieri1_83"

#' The list of all available equations of development rate as a function of temperature.
"devRateEqList"

#' Default starting values for each equation listed in the devRateEqList object.
"devRateEqStartVal"
