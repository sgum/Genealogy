`%!in%` <- Negate(`%in%`)
libs <- c("visNetwork",
          "tidyr",
          "shiny",
          "magrittr",
          "bs4Dash",
          "data.table",
          "stringr",
          "RColorBrewer",
          "shinythemes",
          "qs",
          "dplyr")

# Инсталляция отсутствующих не гитхаб библиотек
new.packages <- libs[!(libs %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# Включение библиотек
suppressMessages(lapply(libs, require, character.only = TRUE))




