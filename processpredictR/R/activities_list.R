activities_list <- function(log) {
  UseMethod("activities_list")
}

activities_list.log <- function(log) {
  activities <- as.character(activity_labels(log))
  activities
}
