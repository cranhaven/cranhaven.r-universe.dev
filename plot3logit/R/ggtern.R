
Stat3Logit <- ggplot2::ggproto('StatIdentity', Stat,
  compute_group = function(data, scales) {
  	data %>%
  	  dplyr::filter(type == 'arrow') %>%
  	  return()
  }
)

Conf3Logit <- ggplot2::ggproto('StatConfidenceTern', Stat,
  compute_group = function(data, scales) {
  	data %>%
  	  dplyr::filter(type == 'region') %>%
  	  return()
  }
)





#' Create a new gg3logit
#'
#' `gg3logit` initialises a [`ggplot`][ggplot2::ggplot] object through
#' [`ggtern`][ggtern::ggtern]. If a `field3logit` or a `multifield3logit`
#' object is passed to argument `data`, the mandatory aesthetics of the ternary
#' plot are automatically set. See \insertCite{santi2022;textual}{plot3logit}
#' for details and examples.
#'
#' @param data a `field3logit` object, a `multifield3logit` object, or
#'   a `data.frame` structured like a fortified `field3logit` or a
#'   `multifield3logit` object.
#' @param mapping list of aesthetic mappings to be used for plot. If a
#'   `field3logit` or a `multifield3logit` is passed to `data`, none of the
#'   aesthetics mappings listed in section *Aesthetic mappings* below has to be
#'   specified (if specified, they will be overwritten). 
#' @param ... additional arguments passed through to [`ggtern`][ggtern::ggtern].
#'
#' @section Aesthetic mappings:
#'
#' The following aesthetics are required by at least one of the available stats.
#' None of them should be specified if a `field3logit` or a `multifield3logit`
#' is passed to the argument `data` of [gg3logit()], [stat_field3logit()] or
#' [stat_conf3logit()]:
#' * `x`, `y`, `z` are required by:
#'   + [stat_field3logit()] as ternary coordinates of the starting points of the
#'     arrows;
#'   + [stat_conf3logit()] ternary coordinates of the points on the border of
#'     confidence regions;
#' * `xend`, `yend`, `zend`: required by [stat_field3logit()] as ternary
#'   coordinates of the ending points of the arrows;
#' * `group`: identifier of groups of graphical objects (arrows and their confidence
#'   regions);
#' * `type`: type of graphical object (arrows or confidence regions).
#'
#' The following variables of a fortified `field3logit` or a `multifield3logit`
#' object may be useful for defining other standard aesthetics (such as `fill`,
#' `colour`, ...):
#' * `label` identifies a field through a label, thus it is useful for
#'   distinguishing the fields in a `multifield3logit` object.
#' * `idarrow` identifies each group of graphical objects (arrows and their
#'   confidence regions) *within* every field. Unlike variable `group`,
#'  `idarrow` is not a global identifier of graphical objects.
#' 
#' @returns Object of class `ggplot`.
#' 
#' @family gg functions
#'
#' @examples
#' \donttest{
#' data(cross_1year)
#'
#' mod0 <- nnet::multinom(employment_sit ~ gender + finalgrade, data = cross_1year)
#' field0 <- field3logit(mod0, 'genderFemale')
#'
#' gg3logit(field0) + stat_field3logit()
#' }
#'
#' @export
gg3logit <- function (data = NULL, mapping = aes(), ...) {

  if (!is.null(data)) {
    if (inherits(data, c('field3logit', 'multifield3logit'))) {
    	  data %<>% fortify
      
      mapping %<>%
        modifyList(ggplot2::aes_(
          x     = as.symbol(colnames(data)[5]),
          y     = as.symbol(colnames(data)[6]),
          z     = as.symbol(colnames(data)[7]),
          xend  = as.symbol(colnames(data)[8]),
          yend  = as.symbol(colnames(data)[9]),
          zend  = as.symbol(colnames(data)[10]),
          group = as.symbol('group'),
          type  = as.symbol('type')
        ))
    }
  }

  ggtern(data = data, mapping = mapping, ...) +
    limit_tern(breaks = (0:5) / 5, labels = (0:5) / 5) +
    theme_showarrows()
}





#' Add a field to a `gg3logit` plot
#'
#' [stat_field3logit()] adds a field to a [`gg3logit`] plot.
#'
#' @inheritParams gg3logit
#' @inheritParams ggplot2::geom_segment
#' @inheritParams ggplot2::stat_identity
#' @param mapping list of aesthetic mappings to be used for plot. Mandatory
#'   aesthetics should not be specified if `field3loglit` or `multifield3logit`
#'   object is passed to `data`. See secion **Aesthetic mappings** of
#'   [gg3logit()] for details.
#' @param arrow. specification for arrow heads, as created by
#'   function [`arrow`][grid::arrow] of package [`grid`][grid::grid-package].
#'
#' @returns Layer of `ggplot2` package, object of class `LayerInstance`.
#'
#' @family gg functions
#'
#' @examples
#' \donttest{
#' data(cross_1year)
#'
#' mod0 <- nnet::multinom(employment_sit ~ gender + finalgrade, data = cross_1year)
#' field0 <- field3logit(mod0, 'genderFemale', conf = 0.95)
#'
#' gg3logit(field0) + stat_field3logit()
#' gg3logit(field0) + stat_field3logit() + stat_conf3logit()
#' }
#'
#' @export
stat_field3logit <- function(mapping = aes(), data = NULL, geom = 'segment',
  position = 'identity', show.legend = NA, inherit.aes = TRUE,
  arrow. = arrow(length = unit(0.2, 'cm')), ...) {

  params <- list(arrow = arrow., ...)
  
  if (!is.null(data)) {
    if (inherits(data, c('field3logit', 'multifield3logit'))) {
      data %<>% fortify
      
      mapping %<>%
        modifyList(ggplot2::aes_(
          x     = as.symbol(colnames(data)[5]),
          y     = as.symbol(colnames(data)[6]),
          z     = as.symbol(colnames(data)[7]),
          xend  = as.symbol(colnames(data)[8]),
          yend  = as.symbol(colnames(data)[9]),
          zend  = as.symbol(colnames(data)[10]),
          group = as.symbol('group'),
          type  = as.symbol('type')
        ))
    }
  } else {
  	mapping %<>% utils::modifyList(list(
  	  x = NULL, y = NULL, z = NULL,
  	  xend = NULL, yend = NULL, zend = NULL,
  	  group = NULL, type = NULL
  	))
  }

  geom <- quote(ifelse(any(is.na(data[ , mapping$xend])), 'point', 'segment'))
  
  ggplot2::layer(
    stat = Stat3Logit, data = data, mapping = mapping, geom = eval(geom),
    position = position, show.legend = show.legend,
    inherit.aes = inherit.aes, params = params
  )
}





#' Add the confidence regions of a field to a `gg3logit` plot
#'
#' [stat_conf3logit()] adds a field to a [`gg3logit`] plot.
#'
#' @inheritParams stat_field3logit
#' 
#' @returns Layer of `ggplot2` package, object of class `LayerInstance`.
#'
#' @family gg functions
#'
#' @examples
#' \donttest{
#' data(cross_1year)
#'
#' mod0 <- nnet::multinom(employment_sit ~ gender + finalgrade, data = cross_1year)
#' field0 <- field3logit(mod0, 'genderFemale', conf = 0.95)
#'
#' gg3logit(field0) + stat_conf3logit()
#' gg3logit(field0) + stat_field3logit() + stat_conf3logit()
#' }
#'
#' @export
stat_conf3logit <- function(mapping = aes(), data = NULL, geom = 'polygon',
  position = 'identity', show.legend = NA, inherit.aes = TRUE, ...) {

  list(fill = 'blue', alpha = 0.2) %>%
    modifyList(list(...)) -> params
    
  if (!is.null(data)) {
  	if (inherits(data, c('field3logit', 'multifield3logit'))) { data %<>% fortify }
  	mapping %<>%
      modifyList(ggplot2::aes_(
          x     = as.symbol(colnames(data)[5]),
          y     = as.symbol(colnames(data)[6]),
          z     = as.symbol(colnames(data)[7]),
          xend  = as.symbol(colnames(data)[8]),
          yend  = as.symbol(colnames(data)[9]),
          zend  = as.symbol(colnames(data)[10]),
          group = as.symbol('group'),
          type  = as.symbol('type')
        ))
  } else {
  	mapping %<>% utils::modifyList(list(
  	  x = NULL, y = NULL, z = NULL,
  	  xend = NULL, yend = NULL, zend = NULL,
  	  group = NULL, type = NULL
  	))
  }
  
  if (!is.null(mapping$fill))  { params$fill  <- NULL }
  if (!is.null(mapping$alpha)) { params$alpha <- NULL }

  ggplot2::layer(
    stat = Conf3Logit, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend,
    inherit.aes = inherit.aes, params = params
  )
}





#' Add a field and confidence regions to a `gg3logit` plot
#'
#' [stat_3logit()] adds a field and confidence regions to a [`gg3logit`]
#' plot. [stat_3logit()] is a wrapper for stats [stat_field3logit()] and
#' [stat_conf3logit()] which are jointly applied.
#'
#' @inheritParams stat_field3logit
#' @param mapping_field,mapping_conf aesthetic mappings passed to argument
#'   `mapping` of [stat_field3logit()] and [stat_conf3logit()].
#' @param params_field,params_conf graphical parameters passed to argument
#'   `mapping` of [stat_field3logit()] and [stat_conf3logit()].
#' @param conf if `TRUE` and if confidence regions are available, the layer of
#'   [stat_conf3logit()] is added, otherwise only the layer of
#'   [stat_field3logit()] is returned.
#'
#' @returns If `conf` is set to `FALSE` a layer of `ggplot` package is returned
#' (object of class `LayerInstance`), otherwise, if `conf` is set to `TRUE`,
#' `stat_3logit` returns a list of two `ggplot2` layers (class `LayerInstance`).
#' 
#' @family gg functions
#'
#' @examples
#' \donttest{
#' data(cross_1year)
#'
#' mod0 <- nnet::multinom(employment_sit ~ gender + finalgrade, data = cross_1year)
#' field0 <- field3logit(mod0, 'genderFemale', conf = 0.95)
#'
#' gg3logit(field0) + stat_3logit()
#' gg3logit(field0) + stat_3logit(conf = TRUE)
#' }
#'
#' @export
stat_3logit <- function(mapping_field = aes(), mapping_conf = aes(),
  data = NULL, params_field = list(), params_conf = list(),
  show.legend = NA, inherit.aes = TRUE, conf = TRUE) {

  list(
    mapping = mapping_field, data = data,
    show.legend = show.legend, inherit.aes = inherit.aes
  ) %>%
    modifyList(params_field) %>%
    do.call('stat_field3logit', .) -> out
  
  if (conf) {
    list(
      mapping = mapping_conf, data = data,
      show.legend = show.legend, inherit.aes = inherit.aes
    ) %>%
      modifyList(params_conf) %>%
      do.call('stat_conf3logit', .) %>%
      list(out) -> out
  }
    
  return(out)
}





#' Create a `gg3logit` plot with field and confidence regions
#'
#' [autoplot()] creates a [`gg3logit`] plot and adds a field and its confidence
#' regions. [autoplot()] is a wrapper for [gg3logit()] and [stat_3logit()].
#'
#' @inheritParams stat_3logit
#' @inheritParams add_confregions
#' @inheritParams ggplot2::autoplot
#' @param conf if `TRUE` and if confidence regions are available, the layer of
#'   [stat_conf3logit()] is added, otherwise only a [gg3logit()] object with the
#'   layer of [stat_field3logit()] is returned.
#' @param object an object of class `field3logit` or `multifield3logit`.
#' 
#' @returns Object of class `ggplot`.
#' 
#' @family gg functions
#'
#' @examples
#' \donttest{
#' data(cross_1year)
#'
#' mod0 <- nnet::multinom(employment_sit ~ gender + finalgrade, data = cross_1year)
#' field0 <- field3logit(mod0, 'genderFemale', conf = 0.95)
#'
#' autoplot(field0)
#' }
#'
#' @export
autoplot.Hfield3logit <- function(object, ..., mapping_field = aes(),
  mapping_conf = aes(), data = NULL, params_field = list(),
  params_conf = list(), show.legend = NA, conf = TRUE) {
  
  if (!inherits(object, 'Hfield3logit')) {
  	stop('Only objects of class "field3logit" and "multifield3logit" are allowed')
  }
  
  gg3logit(object) +
    stat_3logit(
      mapping_field = mapping_field, mapping_conf = mapping_conf,
      params_field = params_field, params_conf = params_conf,
      show.legend = show.legend, conf = conf
    )
}


