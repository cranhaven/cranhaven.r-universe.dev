config <- function() {
  config_path <- system.file("config.yaml", package = "Pandora")
  yaml.load_file(config_path)
}
