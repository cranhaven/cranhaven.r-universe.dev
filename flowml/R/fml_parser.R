#' @name create_parser
#' @author Sebastian Malkusch
#' @title create_parser
#' @description Creates an object that defines and handles command line arguments.
#' @details A parser that organizes the communication between the user and th function.
#' It also provides a help message.
#'
#' @importFrom optparse OptionParser parse_args make_option
#'
#' @return An instance of type 'optparse::OptionParser'.
#'
#' @export
#'
create_parser = function(){
  option_list = list(
    optparse::make_option("--pipeline_segment", type = "character", default=NULL,
                          help="pipeline segment to run. Opeions are: train,
                          validate, bootstrap, interpret", metavar = "character"),
    optparse::make_option("--config", type="character", default=NULL,
                          help="config file name", metavar="config file"),
    optparse::make_option("--data", type="character", default=NULL,
                          help="Data to analyze."),
    optparse::make_option("--samples_train", type="character", default=NULL,
                          help="Training samples"),
    optparse::make_option("--samples_test", type="character", default=NULL,
                          help="Training samples"),
    optparse::make_option("--features", type="character", default=NULL,
                          help="Training features"),
    optparse::make_option("--extended_features", type="character", default=NULL,
                          help="Attribute pool for permutation experiment"),
    optparse::make_option("--permutation", type="character", default="none",
                          help="method of permutation experiment. Options are (none, features, response). [default= %default]", metavar="character"),
    optparse::make_option("--interpretation", type="character", default="shap",
                          help="method for model interpretation. Options are (permutation, shap, internal). [default= %default]", metavar="character"),
    optparse::make_option("--trained", type="character", default = NULL,
                          help = "trained caret object (*.rds).", metavar="character"),
    optparse::make_option("--cores", type="integer", default=1,
                          help="available cores for multi-processing [default= %default]", metavar="character"),
    optparse::make_option("--result_dir", type="character", default = NULL,
                          help="directory to store results")
  )

  opt_parser = optparse::OptionParser(option_list=option_list)
  opt = optparse::parse_args(opt_parser)
  return(opt)
}
