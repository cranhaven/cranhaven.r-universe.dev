#' Panel component for shiny panels layout
#'
#' @param names A vector or data.frame with names or full names
#' @param result_as A named vector with names c("male", "female")
#'   values can be used to override the results
#' @param lang Use "es" for Spanish (default), "pt" for Portuguese.
#' @param col The name of the column with the names or full names
#'   when the input is a data frame.
#' @param na String to be used when there is not match for gender
#' @param rev_weights Boolean to indicate if weights should be
#'   reversed when input names have the format Last Name First Name
#'
#' @return A vector of data frame with the estimated gender for the
#'   input. When the input is data.frame a column is attached next
#'   to the column used for the input names with the result.
#'
#' @examples
#' genero(c("Juan", "Pablo", "Camila", "Mariana"))
#'
#'
#' @export
#' @importFrom utils read.csv
genero <- function(names,
                   result_as = c(male = "male", female = "female"),
                   lang = "es",
                   col = NULL, na = NA,
                   rev_weights = FALSE
                   ){

  if(lang == "es"){
    nms_gender <-names_gender_es
  }
  if(lang == "pt"){
    nms_gender <- names_gender_pt
  }

  if(class(names) %in% c("factor", "character")) {
    names <- remove_accents(tolower(names))
    gender <- match_replace(names, nms_gender, na = na)
    if(length(gender) == 1 && is.na(gender)) return(gender)
    if(na_proportion(gender) > 0.7 || many_words_proportion(names) > 0.5){
      # Try splitting names
      lnames <- strsplit(names, " ")
      gender <- unlist(lapply(lnames, function(x){
        x <- na_to_chr(genero(x), "NA")
        # weight by position
        w <- length(x):1/sum(1:length(x))
        if(rev_weights) w <- rev(w)
        names(w) <- x
        ws <- c(female = sum(w[names(w) == "female"]),
                male = sum(w[names(w) == "male"]),
                na = sum(w[names(w) == "na"]))
        maxima <- which(ws == max(ws))
        if(length(maxima) > 1 || names(maxima) == "NA") return(NA)
        names(ws[maxima])
        #ws
      }))
    }

    if(any(names(result_as) != unname(result_as))){
      gender <- match_replace(gender, data.frame(match = names(result_as),
                                                 replace = result_as, stringsAsFactors = FALSE))
    }
  }

  if("data.frame" %in% class(names)){
    if(is.null(col)){
      name_cols <- c("name", "names", "first_name", "first name", "nombre", "nombres",
                     "nombres y apellidos", "nombres_apellidos", "nombre_apellidos", "nome",
                     "prenom")

      col <- which_in(name_cols, names(names))
      message("Guessed names column: ", col)
      if(length(col) == 0) stop("Please provide a column with the names to estimate gender to")
      if(length(col) > 1) warning("Using first names column found: ", col[1])
    } else{
      if(!col %in% names(names)) stop("Provided col not found. Please provide a column with the names to estimate gender to")
    }
    gender <- genero(names[,col[1]], result_as = result_as)
    target <- match(col, names(names))
    gender <- insert_column(names, gender, target, col_name = paste0(col,"_gender_guess")
    )
  }
  gender

}
