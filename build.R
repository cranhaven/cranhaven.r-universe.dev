# Proof of concept
pkgs <- c("dse", "eiCompare", "EvalEst", "survivalmodels", "survivalSL")
df <- data.frame(package = pkgs, url = paste0("https://github.com/cran/", pkgs))
jsonlite::write_json(df, "packages.json", pretty = TRUE)

