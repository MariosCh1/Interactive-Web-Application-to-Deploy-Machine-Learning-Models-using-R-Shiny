if(!("installr" %in% rownames(installed.packages()))){
  install.packages("installr")
}

if(!("shiny" %in% rownames(installed.packages()))){
  install.packages("shiny")
}

if(!("shinythemes" %in% rownames(installed.packages()))){
  install.packages("shinythemes")
}

if(!("auth0" %in% rownames(installed.packages()))){
  install.packages("auth0")
}

if(!("blob" %in% rownames(installed.packages()))){
  install.packages("blob")
}

if(!("vroom" %in% rownames(installed.packages()))){
  install.packages("vroom")
}

if(!("foreign" %in% rownames(installed.packages()))){
  install.packages("foreign")
}

if(!("DT" %in% rownames(installed.packages()))){
  install.packages("DT")
}

if(!("shinydashboard" %in% rownames(installed.packages()))){
  install.packages("shinydashboard")
}

if(!("shinydashboardPlus" %in% rownames(installed.packages()))){
  install.packages("shinydashboardPlus")
}

if(!("styler" %in% rownames(installed.packages()))){
  install.packages("styler")
}

if(!("shinyEffects" %in% rownames(installed.packages()))){
  install.packages("shinyEffects")
}

if(!("shinyAce" %in% rownames(installed.packages()))){
  install.packages("shinyAce")
}

if(!("shinyjqui" %in% rownames(installed.packages()))){
  install.packages("shinyjqui")
}

if(!("RMySQL" %in% rownames(installed.packages()))){
  install.packages("RMySQL")
}

if(!("DBI" %in% rownames(installed.packages()))){
  install.packages("DBI")
}

if(!("RSQLite" %in% rownames(installed.packages()))){
  install.packages("RSQLite")
}

if(!("caroline" %in% rownames(installed.packages()))){
  install.packages("caroline")
}

if(!("tidyverse" %in% rownames(installed.packages()))){
  install.packages("tidyverse")
}

if(!("cli" %in% rownames(installed.packages()))){
  install.packages("cli")
}

if(!("auth0" %in% rownames(installed.packages()))){
  install.packages("auth0")
}

if(!("rlang" %in% rownames(installed.packages()))){
  install.packages("rlang")
}

if(!("httr" %in% rownames(installed.packages()))){
  install.packages("httr")
}

if(!("jsonlite" %in% rownames(installed.packages()))){
  install.packages("jsonlite")
}

if(!("png" %in% rownames(installed.packages()))){
  install.packages("png")
}

if(!("reticulate" %in% rownames(installed.packages()))){
  install.packages("reticulate")
}

if(!("shinyWidgets" %in% rownames(installed.packages()))){
  install.packages("shinyWidgets")
}

if(!("usethis" %in% rownames(installed.packages()))){
  install.packages("usethis")
}


if(!("ExPanDaR" %in% rownames(installed.packages()))){
  install.packages("ExPanDaR")
}

if(!("DataExplorer" %in% rownames(installed.packages()))){
  install.packages("DataExplorer")
}

if(!("readxl" %in% rownames(installed.packages()))){
  install.packages("readxl")
}

if(!"caret" %in% rownames(installed.packages())){
  install.packages("caret")
}

if(!("shinyalert" %in% rownames(installed.packages()))){
  install.packages("shinyalert")
}

if(!("shinyjs" %in% rownames(installed.packages()))){
  install.packages("shinyjs")
}

if(!("blob" %in% rownames(installed.packages()))){
  install.packages("blob")
}

if(!("dbplyr" %in% rownames(installed.packages()))){
  install.packages("dbplyr")
}

if(!("hablar" %in% rownames(installed.packages()))){
  install.packages("hablar")
}

if(!("ggplot2" %in% rownames(installed.packages()))){
  install.packages("ggplot2")
}

if(!("plotly" %in% rownames(installed.packages()))){
  install.packages("plotly")
}

if(!("plyr" %in% rownames(installed.packages()))){
  install.packages("plyr")
}

if(!("shinyglide" %in% rownames(installed.packages()))){
install.packages("shinyglide")
}

if(!("xgboost" %in% rownames(installed.packages()))){
  install.packages("xgboost")
}

if(!("Matrix" %in% rownames(installed.packages()))){
  install.packages("Matrix")
}

if(!("waiter" %in% rownames(installed.packages()))){
  install.packages("waiter")
}

# 
# if(!("remotes" %in% rownames(installed.packages()))){
#   install.packages("remotes")
# }
# 
# remotes::install_github("lgnbhl/scroller")



# list all packages where an update is available
#old.packages()

# update, without prompts for permission/clarification
# update.packages(ask = FALSE, checkBuilt=TRUE)
# 
# install.packages(
#   c(
#     "installr", 
#     "shiny",
#     "shinythemes",
#     "auth0",
#     "blob",
#     "vroom",
#     "foreign",
#     "DT",
#     "shinydashboard",
#     "shinydashboardPlus",
#     "styler",
#     "shinyEffects"
#     "shinyAce",
#     "shinyjqui",
#     "RMySQL",
#     "DBI",
#     "RSQLite" ,
#     "caroline",
#     "tidyverse",
#     "auth0",
#     "rlang",
#     "httr",
#     "jsonlite",
#     "png",
#     "reticulate",
#     "shinyWidgets",
#     "usethis",
#     "ExPanDaR",
#     "DataExplorer",
#     "readxl",
#     "shinyalert",
#     "shinyjs",
#     "blob",
#     "dbplyr",
#     "hablar",
#     "ggplot2",
#     "plotly",
#     "plyr",
#     "xgboost",
#     "Matrix",
#     "waiter"
#    )
#  )

library(installr)
library(shiny)# web app framework
library(shinythemes)# themes for shiny
library(auth0)
library(blob)
library(vroom) # data management tool
library(foreign) #SPSS file reading
library(DT) #Data Tables
library(shinydashboard)
library(shinydashboardPlus)
library(styler)
library(shinyEffects)
library(shinyAce)
library(shinyjqui)
library(RMySQL)
library(DBI)
library(caroline)
library(tidyverse)
library(cli)
library(auth0)
library(rlang)
library(httr)
library(jsonlite)
library(png)
library(reticulate)#python
library(shinyWidgets)
library(usethis)
library(ExPanDaR)#EDA
library(DataExplorer)#EDA
library(networkD3)
library(dplyr)#MySQL_Datatype
library(lubridate)#MySQL_Datatype
library(data.table)#MySQL_Datatype
library(readxl)
library(caret) #knn
library(class)#knn
library(shinyalert) #popups
library(shinyjs) #reset
library(blob)
library(dbplyr)
library(hablar)
library(ggplot2)
library(plotly)
library(plyr)
library(shinyglide)
library(xgboost)
library(Matrix)
library(waiter)
# library(remotes)
# library(scroller)
  

#updateR()