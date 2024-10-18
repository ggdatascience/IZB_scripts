# Script om de datadump snel in te laden.
# Let op: maar 1x uitvoeren!

suppressPackageStartupMessages(library(tidyverse))
library(DBI)
library(odbc)
library(this.path)

setwd(dirname(this.path()))

# aanpassen naar eigen omgeving
conn = dbConnect(odbc::odbc(), Driver="SQL Server", Server="az-odb0\\odbnog", Database="HPZone")

csvs = list.files("./export/", ".*?.csv")

for (filename in csvs) {
  tbl_name = str_sub(filename, end=-5)
  dbExecute(conn, paste0("SET IDENTITY INSERT ", tbl_name, " ON"))
  
  data = read.csv(paste0("export/", filename))
  dbAppendTable(conn, tbl_name, data)
  
  dbExecute(conn, paste0("SET IDENTITY INSERT ", tbl_name, " OFF"))
}

print("Alle tabellen ingevoegd.")