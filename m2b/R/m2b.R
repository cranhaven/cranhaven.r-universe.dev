#' Movement to behaviour package
#'
#' A package to infer behaviour and social interaction from
#' movement data
#'
#'Animal behaviour, including social interactions, are fundamental to the field of
#'ecology. Whereas the direct observation of animal behaviour is often limited due
#'to logistical constraints, collection of movement data have been greatly
#'facilitated through the development of bio-logging. Animal movement data
#'obtained through tracking instrumentation may potentially constitute a relevant
#'proxy to infer animal behaviour. This is, however, based on the premise that a
#'range of movement patterns can be linked to specific behaviours.
#'
#'Statistical learning constitutes a number of methods that can be used to
#'assess the link between given variables from a fully informed training
#'dataset and then predict the values on a non-informed variable. We chose the
#'random forest algorithm for its capacity to deal with imbalanced data
#'(particularly relevant for behavioural data), its high prediction accuracy
#'and its ease of implementation (@breiman2001b, @chen2004). 
#'The strength of random forest partly relies
#'in its ability to handle a very large number of variables. Hence, our
#'methodology is based on the derivation of multiple predictor variables from
#'the movement data over various temporal scales, in order to capture as much
#'information as possible on the changes and variations of movement.
#'
#'In this package we developed a method to link the movement patterns of animals
#'with their behaviour, using the random forest algorithm. The specificity of this
#'method relies on the derivation of multiple predictor variables from the
#'movement data over a range of temporal windows. This procedure allows to capture
#'as much information as possible on the changes and variations of movement and
#'ensures the use of the random forest algorithm to its best capacity. The method
#'is very generic, applicable to any dataset providing movement data together with
#'observation of behaviour.
#'
#' @docType package
#' @name m2b 
NULL

