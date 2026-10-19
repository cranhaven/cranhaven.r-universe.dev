# EconCausal 1.0.4

* `bsts` and `BoomSpikeSlab` are now suggested rather than imported, following CRAN's
  request regarding the archival of `Boom`. `bsts_model()` requires both packages and
  stops with an informative message when they are not installed.
* Improved the estimates for the `ecm_mars()` and `bsts_model()` models.
