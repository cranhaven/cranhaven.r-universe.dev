
<!-- README.md is generated from README.Rmd. Please edit that file -->
# rmytarget - R пакет для работы с API MyTarget v2 <a href='https://selesnow.github.io/rmytarget/'><img src='https://raw.githubusercontent.com/selesnow/rmytarget/master/inst/logo/rmytarget.png' align="right" height="139" /></a>
================================================

Подробная русскоязычная документация находится по [ссылке](https://selesnow.github.io/rmytarget/).

Виньетки
========

Помимо основной документации пакет содержит 2 виньетки:

-   rmytarget-auth - Автооризация в API MyTarget
-   rmytarget-intro - Введение в работу с пакетом rmytarget

Для просмотра виньеток запустите в R одну из следующих команд:

`vignette('rmytarget-auth', package = 'rmytarget')`

`vignette('rmytarget-intro', package = 'rmytarget')`

Поддержать проект
========================
Вы можете поддержать проект любой произвольной суммой перейдя по этой [ссылке](https://secure.wayforpay.com/button/b6dd4a7083fe0).

Функции пакета rmytarget
========================

На данный момент в пакете rmytarget доступно 5 функций:

-   myTarAuth - Авторизация
-   myTarGetCampaignList - Загрузка списка кампаний
-   myTarGetAdList - Загрузка списка объявлений
-   myTarGetClientList - Загрузка списка клиентов из агентского аккаунта
-   myTarGetStats - Загрузка статистики по объявениям, рекламным кампаниям или клиентам

Пример работы с пакетом rmytarget
=================================

``` r
# work with simple client account
# authorize by ad account
myTarAuth(login = "client_login")

# get campaing list
campaing <- myTarGetCampaignList(login = "client_login")

# get ads list
ads      <- myTarGetAdList(login = "client_login")

# load statistic
# load base stat by ads
old_data    <- myTarGetStats(date_from   = Sys.Date() - 7,
                             date_to     = Sys.Date(),
                             object_type = "banners",
                             object_id   = campaing$id,
                             login       = "client_login")

# load base, tps, and viral metrics by campaings
custom_data <- myTarGetStats(date_from   = Sys.Date() - 7,
                             date_to     = Sys.Date(),
                             object_type = "campaigns",
                             metrics     = c("base", "tps", "viral"),
                             stat_type   = "day",
                             login       = "client_login")

# load all metrics by campaigns
all_data <- myTarGetStats(date_from   = Sys.Date() - 7,
                          date_to     = Sys.Date(),
                          object_type = "campaigns",
                          metrics     = "all",
                          login       = "client_login")

# ========================
# work with agency account
# authorize by agency account
myTarAuth(login = "agency_login")

# load client list from agency account 
clients <- myTarGetClientList(login = "agency_login")

# load statistic by agency clients
client_stat <-  myTarGetStats(date_from   = Sys.Date() - 7,
                              date_to     = Sys.Date(),
                              object_id   = clients$id,
                              object_type = "users",
                              metrics     = "all",
                              login       = "agency_login")
```
