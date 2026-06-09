.radian2degree <- function(x) x * 180 / pi
.degree2radian <- function(x) x * pi / 180

.get_max <- function(r) max(r[], na.rm = TRUE)
.get_min <- function(r) min(r[], na.rm = TRUE)

.decode_label <- function(label) {
  sector_ID <- trunc(label / 1000)
  ring_ID <- label - sector_ID * 1000
  ds <- data.frame(sector_ID, ring_ID)
  names(ds) <- c("sector_ID", "ring_ID")
  ds
}

.calc_rmse <- function(x) sqrt(mean(x^2))

.extension <- function(file_name, new_extension = "tif") {
  file_name <- filenamer::as.filename(file_name)
  file_name <- filenamer::insert(file_name, ext = new_extension, replace = TRUE)
  as.character(file_name)
}

.make_fake_las <- function(X, Y, Z){
  data_template_names <- c("X", "Y", "Z", "gpstime","Intensity", "ReturnNumber",
                           "NumberOfReturns", "ScanDirectionFlag",
                           "EdgeOfFlightline",  "Classification",
                           "Synthetic_flag", "Keypoint_flag", "Withheld_flag",
                           "ScanAngleRank", "UserData", "PointSourceID",
                           "R",  "G",  "B")
  data_template <- matrix(ncol = length(data_template_names), nrow = length(X))
  data_template <- data.frame(data_template)
  names(data_template) <- data_template_names
  data_template[] <- 0

  fake_las <- new("LAS")
  fake_las@data <- as(data_template, "data.table")
  fake_las@data$X <- X
  fake_las@data$Y <- Y
  fake_las@data$Z <- Z
  fake_las
}

.show_popup <- function(message) {
  # Create the main window
  win <- tcltk::tktoplevel()
  tcltk::tkwm.title(win, "Popup")

  # Create a label with the message
  label <- tcltk::tklabel(win, text = message, padx = 10,
                          pady = 10, justify = "left")
  tcltk::tkpack(label)

  # Create a button to close the window
  button <- tcltk::tkbutton(win, text = "OK",
                            command = function() tcltk::tkdestroy(win))
  tcltk::tkpack(button, padx = 10, pady = 10)

  # Set the focus on the window
  tcltk::tkfocus(win)

  # Run the event loop
  tcltk::tkwait.window(win)
}


.cores <- function() {
  cores <- parallel::detectCores(logical = FALSE)
  if (is.na(cores) || cores < 1) cores <- 1
  cores
}
.with_cluster <- function(cores, expr) {
  if (!requireNamespace("doParallel", quietly = TRUE)) {
    stop("The 'doParallel' package is required but not available.", call. = FALSE)
  }

  cl <- parallel::makeCluster(cores)
  on.exit(parallel::stopCluster(cl), add = TRUE)
  doParallel::registerDoParallel(cl)

  force(expr)
}




