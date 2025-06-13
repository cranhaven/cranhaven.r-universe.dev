# utility script to create function skeletons for package
library(httr)
library(xml2)
library(magrittr)
library(rvest)
library(data.table)
library(rapportools)
library(RoxygenReady) #https://github.com/vertesy/roxygenready devtools::install_github(repo = "vertesy/RoxygenReady/RoxygenReady", ref = "cdf661e79f3a75405f4e30a3c2f8e6c39158d67f")
library(whisker)
slURL <- list(
  accountMethods = "https://wiki.saucelabs.com/display/DOCS/Account+Methods",
  infoMethods = "https://wiki.saucelabs.com/display/DOCS/Information+Methods",
  jobMethods = "https://wiki.saucelabs.com/display/DOCS/Job+Methods",
  actUsageMethods = "https://wiki.saucelabs.com/display/DOCS/Test+Activity+and+Usage+Methods",
  tunnelMethods = "https://wiki.saucelabs.com/display/DOCS/Tunnel+Methods",
  tempStorageMethods = "https://wiki.saucelabs.com/display/DOCS/Temporary+Storage+Methods",
  jsUnitTestMethods = "https://wiki.saucelabs.com/display/DOCS/JavaScript+Unit+Testing+Methods"
)

readSLUrl <- function(appURL){
  appTable <- (doc <- appURL %>% read_html %>% xml_find_all("//table")) %>%
    html_table(header = TRUE)
  funcArgs <- lapply(doc, function(docT){
    fArgs <- docT %>% xml_find_all(".//td[@class='confluenceTd'][5]")
    fArgs <- lapply(fArgs, function(x){
      xml_text(xml_find_all(x, ".//li"))
    })
  })
  Map(function(aT, fA){
    aT$`Request Fields` <- fA
    aT
  }
  , aT = appTable
  , fA = funcArgs
  )
}

appMethods <- lapply(slURL, readSLUrl)
# remove jobs 1 for now as they are all one method
appMethods[[3]] <- appMethods[[3]][2]

appMethods <- setNames(lapply(names(appMethods), function(x){
  rbindlist(
    lapply(appMethods[[x]], function(y){
      setDT(y)
      y[, group := x]
    })
  )
}), names(appMethods))
appMethods <- rbindlist(appMethods, fill = TRUE)
setnames(appMethods, tocamel(tolower(names(appMethods))))

orgVars <- list(":username", ":automation_api", ":job_id",
     ":file_name", ":tunnel_id", ":your_file_name", "^/rest/v1/", "https://saucelabs.com/rest/v1/")
newVars <- list("{{username}}", "{{automation_api}}", "{{job_id}}",
                   "{{file_name}}", "{{tunnel_id}}", "{{your_file_name}}", "", "")
temp <- appMethods$url
for(i in seq_along(orgVars)) temp <- gsub(orgVars[i], newVars[i], temp)
appMethods[, version := ifelse(grepl("/rest/v1.1/", temp), "v1.1", "v1")]
temp <- gsub("^(info/platforms/\\{\\{automation_api\\}\\}).*", "\\1", temp)
temp <- gsub("^(\\{\\{username\\}\\}/jobs/\\{\\{job_id\\}\\}/assets/\\{\\{file_name\\}\\}).*", "\\1", temp)
appMethods$url <- gsub("https://saucelabs.com/rest/v1.1/", "", temp)


appMethods[, args := sapply(requestFields, function(x){
  rF <- sub("(.*):(.*)", "\\1", x)
  rF <- sub("-", "_", rF)
  rF <- paste0(paste(trimws(sub("(.*)\\(.*", "\\1", rF)), collapse =", "), ", ...")
  sub("^([^,].*)", ", \\1", rF)
})]
appMethods[, method := tocamel(tolower(method))]

funcTemp <- list(
  account = "
{{method}} <- function(account{{{args}}}){
  # {{description}}
  obj <- list()
  pathTemplate <- whisker.render(\"https://saucelabs.com/rest/{{version}}/{{url}}\", data = obj)
  pathURL <- parse_url(pathTemplate)
  res <- queryAPI(verb = {{methodType}}, account = account, url = build_url(pathURL), source = \"{{method}}\", json = body,...)
  res
}
")

appMethods[, RSLFuncs := sapply(rowSplit(appMethods), function(x){
  whisker.render(funcTemp[["account"]], x)
}
)]

appMethods[, RSLFuncs := gsub("js tests, js tests", "js_tests", RSLFuncs)]

# write templates to file based on group
appMethods[,write(file = paste0("R/", group, ".R"), paste(RSLFuncs, collapse = "")), by = group]

# create roxygen skeletons for files:
lapply(appMethods[, unique(paste0("R/", group, ".R"))], function(x){
  RoxygenReady(x, overview =  FALSE)
})

appFiles <- appMethods[, unique(paste0("R/", group, ".R"))]
appFiles <- c(appFiles, paste0(appFiles, ".annot.R"))
annotInd <- grepl("annot", appFiles)
file.remove(appFiles[!annotInd])
file.rename(appFiles[annotInd], appFiles[!annotInd])


