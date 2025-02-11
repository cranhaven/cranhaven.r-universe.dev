cArgs = commandArgs(TRUE)

params = yaml::read_yaml(cArgs[1L])
parentDir = cArgs[2L]

seeker::seekerArray(
  study = params$study, geneIdType = params$geneIdType,
  platform = params$platform, parentDir)
