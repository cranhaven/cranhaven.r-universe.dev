## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(tabulog)
library(readr)
library(lubridate)

## ------------------------------------------------------------------------
parser('[0-9]+', f = as.integer, name = 'int') # Name is optional

## ----show_logs, comment=''-----------------------------------------------
log_file <- 'accesslog.txt'
cat(readr::read_file(log_file))

## ----build_template------------------------------------------------------
template <- '{{ ip ip_address }} - - [{{ date date_time }}] "{{ url URL }}"'

## ----date_class----------------------------------------------------------
date_parser <- parser(
  '[0-9]{4}\\-[0-9]{2}\\-[0-9]{2}:[0-9]{2}:[0-9]{2}:[0-9]{2}[ ][\\-\\+][0-9]{4}',
  function(x) lubridate::as_datetime(x, format = '%Y-%m-%d:%H:%M:%S %z'),
  name = 'date'
)
date_parser

default_classes()[c('ip', 'url')]

## ----parse_logs----------------------------------------------------------
# Naming the date_parser 'date' in the list tells Tabulog to use it to parse
# the field with class 'date' in the template.
parse_logs(readLines(log_file), template, classes = list(date = date_parser))

## ----yaml_template, comment=''-------------------------------------------
template_file <- 'accesslog_template.yml'
cat(readr::read_file(template_file))

## ----custom_formatters---------------------------------------------------
formatters <- list(
  date = function(x) lubridate::as_datetime(x, format = '%Y-%m-%d:%H:%M:%S %z')
)

## ----parse_logs_file-----------------------------------------------------
parse_logs_file(log_file, template_file, formatters)

