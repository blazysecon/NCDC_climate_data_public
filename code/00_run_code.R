## Reading climate indicators from NOAA dataset at NCDS website via API at http://www.ncdc.noaa.gov/cdo-web/webservices/v2
# Use this script to run main code and save output to a log file

rm ( list = ls())

Sys.setlocale ( category = "LC_ALL", locale = "English_United States.1252" )

## Specify the directory of project files

# RworkingFolder <- "D:/GB_files/Modelling_and_Analytics/Working_files/"
RworkingFolder <- "E:/GB_files/Modelling_and_Analytics/Working_files/"

## Set a month and project folder
month <- ""
projectFolder <-"Climate"

myfiledir <- file.path(RworkingFolder,month,projectFolder)

setwd(myfiledir)

# turn off scientific notation in most output
options(scipen = 15)
# increase the max size of output file
options(max.print=10E6) 

# script.name <- "11_read_noaa_data_90cn_regions"
script.name <- "11_read_noaa_data_daily"

log.file <- paste0 ( "log/", script.name, Sys.Date(), ".rlog" )

log.con <- file(log.file)
sink(log.con, append=FALSE)
sink(log.con, append=TRUE, type="message")

# This will echo all input and not truncate 150+ character lines...
source(paste0("code/",script.name,".R"), echo=TRUE, max.deparse.length=10000)

message("Estimation finished")

# Restore output to console
sink() 
sink(type="message")

unlink(log.con)
