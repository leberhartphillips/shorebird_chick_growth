# libraries
library(readxl)
library(xlsx)
library(rJava)
library(tidyverse)
library(DBI)
library(RSQLite)

# connect to CeutaCLOSED
CeutaCLOSED <- 
  dbConnect(SQLite(), 
            dbname = "/Users/Luke/Documents/Academic_Projects/Postdoc_Seewiesen/Ceuta_Open/Ceuta_CLOSED/data/Ceuta_CLOSED_version_releases/Ceuta_CLOSED_v2-0-0.sqlite")
