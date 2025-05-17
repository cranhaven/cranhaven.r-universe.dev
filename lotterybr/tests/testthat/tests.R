

library(lotterybr)

games <- c("maismilionaria", "megasena", "lotofacil", "quina", "lotomania",
         "duplasena", "diadesorte")
types <- c("numbers", "winners")
language <- c("ptbr","eng")
for(game in games){
  for(type in types){
    for(language in language){
      data <- get_data(game = game, type = type, language = language)
    }
  }
}

