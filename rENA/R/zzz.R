.onLoad <- function(libname, pkgname) {
  globalVariables(c(
    ".","ENA_ROW_IDX","ENA_UNIT","V1","V2","V3",
    "ci.x","ci.y","e","handle","name","unit.groups",
    "V","graph_from_data_frame","%>%","%<>%","X1","X2",
    "dfDT.points","points.raw","lines","KEYCOL","ENA_CONV",
    "..groupCol","..units","..metadata", "..codes", "..conversation","ENA_GROUP_NAME",
    "label.font.color","label.font.family","label.font.size",
    "label.offset","legend.include.edges","legend.name",
    "network.edges.shapes","nodes","rows.to.keep","show.legend",
    "..connection_name", "..dimension_names"
  ))
#   op <- options()
#   op.rENA <- list(
#     UNIT_NAMES = "ena.unit.names",
#     TRAJ_TYPES = c("accumulated","non-accumulated")
#   );
#
#   toset <- !("rENA" %in% names(op))
#   print(paste("ToSet:", toset));
#
#   if(toset) {
#     options(rENA = op.rENA)
#   }
#
#   invisible()
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("For the latest features and updates, install from https://cran.qe-libs.org");
  invisible();
}
