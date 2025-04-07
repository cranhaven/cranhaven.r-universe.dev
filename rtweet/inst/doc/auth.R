## ----include=FALSE------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, eval = FALSE, comment = "#>", collapse = TRUE)

## -----------------------------------------------------------------------------
#  library(rtweet)

## ----client, eval = FALSE-----------------------------------------------------
#  client <- rtweet_client(app = "my_app")
#  client_as(client)

## ----client_load, eval = FALSE------------------------------------------------
#  client_as("my_app")

## ----eval = FALSE-------------------------------------------------------------
#  auth_sitrep()
#  ## Tokens from rtweet version < 1.0.0 found on /home/user:
#  ## Empty tokens were found.
#  ## Choose which is the best path of action for the tokens:
#  ##                              user_id  key
#  ## .rtweet_token.rds      My app         <NA>
#  ## .rtweet_token1.rds My account            A
#  ## Tokens found on /home/user/.config/R/rtweet:
#  ##             token
#  ## my-app2.rds     A
#  ## Multiple authentications with the same app found!
#  ## Choose which is the best path of action for the tokens:
#  ##                       app    user_id key
#  ## default.rds        rtweet 9951053384   A
#  ## testing_rtweet.rds rtweet              B
#  ## All tokens should be moved to /home/user/.config/R/rtweet

## ----eval = FALSE-------------------------------------------------------------
#  auth <- rtweet_app()

## ----eval = FALSE-------------------------------------------------------------
#  auth_as(auth)

## ----eval = FALSE-------------------------------------------------------------
#  client_as(client)

## ----eval = FALSE-------------------------------------------------------------
#  auth_save(auth, "some-name")

## ----eval = FALSE-------------------------------------------------------------
#  auth_as("some-name")

## ----eval=FALSE---------------------------------------------------------------
#  client_save(client)
#  client_as("myapp")

