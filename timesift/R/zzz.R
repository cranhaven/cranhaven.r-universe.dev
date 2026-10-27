# The learners, response heads and metrics that ship are registered here, through the same public
# calls a user registers their own with. There is no second, privileged path into the registries.

# `self` is bound by torch inside a module's own methods rather than by this package, so the code
# checker has nothing to resolve it against.
utils::globalVariables("self")

.onLoad <- function(libname, pkgname) {
  register_metric("tss", tss)
  register_metric("roc_auc", roc_auc)
  register_metric("average_precision", average_precision)
  register_metric("kappa", function(y, p) kappa_score(y, p, "prevalence"))
  register_metric("kappa_youden", function(y, p) kappa_score(y, p, "youden"))

  register_response("presence_absence", .presence_absence)

  register_learner("elasticnet", elasticnet)
  register_learner("stepwise", stepwise)
  register_learner("forest", forest)
  register_learner("mlp", mlp)
  register_learner("cnn", cnn)
  register_learner("rescnn", rescnn)
  invisible(NULL)
}
