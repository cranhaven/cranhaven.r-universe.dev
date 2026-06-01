## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  eval = FALSE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
#  install.packages("devtools")
#  library(devtools)
#  
#  # После чего можно устанавливать пакет rmytarget.
#  install_github('selesnow/rmytarget')
#  library(rmytarget)

## -----------------------------------------------------------------------------
#  library(rmytarget)
#  
#  # ================
#  # пример работы с клиентским аккаунтом
#  # авторизация
#  # если вы работаете через агенский аккаунт то в браузере выберите пункт
#  # предоставить доступ к аккаунту клиента или менеджера
#  
#  myTarAuth(login = "seleznev", token_path = "tokens")
#  
#  # загрузка списка рекламных кампаний и объявлений
#  campaing <- myTarGetCampaignList(login = "seleznev", token_path = "tokens")
#  ads      <- myTarGetAdList(login = "seleznev", token_path = "tokens")
#  
#  # загрузка статистики по рекламным кампанийм
#  camp_data    <- myTarGetStats(date_from   = Sys.Date() - 7,
#                                date_to     = Sys.Date(),
#                                object_type = "campaigns",
#                                object_id   = campaing$id,
#                                stat_type   = "day",
#                                login       = "seleznev",
#                                token_path  = "tokens")
#  
#  # загрузка списка метрик входящих в группы "base", "tps", "viral" по объявлениям
#  custom_data <- myTarGetStats(date_from   = Sys.Date() - 7,
#                               date_to     = Sys.Date(),
#                               object_type = "banners",
#                               metrics     = c("base", "tps", "viral"),
#                               stat_type   = "day",
#                               login       = "seleznev",
#                               token_path  = "tokens")
#  
#  # загрузка всех возможных метрик с группировкой по рекламным кампаниям
#  all_data <- myTarGetStats(date_from   = Sys.Date() - 7,
#                            date_to     = Sys.Date(),
#                            object_type = "campaigns",
#                            metrics     = "all",
#                            login       = "seleznev",
#                            token_path  = "tokens")

## -----------------------------------------------------------------------------
#  library(rmytarget)
#  
#  # авторизация
#  # в браузере необходимо выбрать пункт предоставить доступ к аккаунту "логин агенсткого аккаунта"
#  myTarAuth(login = "agency", token_path = "tokens")
#  
#  # загрузка списка клиентов
#  clients <- myTarGetClientList(login = "agency",
#                                token_path = "tokens")
#  
#  # загрузка статистики с группировкой по клиентам агентского аккаунта
#  client_stat <-  myTarGetStats(date_from   = Sys.Date() - 7,
#                                date_to     = Sys.Date(),
#                                object_id   = clients$id,
#                                object_type = "users",
#                                metrics     = "all",
#                                login       = "agency",
#  			      token_path = "tokens")

