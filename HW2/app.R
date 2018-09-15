# Author: Dominic Contreras.1
# Title: HW2
# Date: September 14 2018
# Class: R Shiny for Operations Management

library(shiny)
library(reshape2)
library(dplyr)
library(plotly)
library(shinythemes)
library(stringr)
library(shinyWidgets)

crime <- read.csv("https://github.com/themarshallproject/city-crime/raw/master/data/ucr_crime_1975_2015.csv", 
                  header = T, sep = ",")  # read in data
crime <- subset(crime, year == 2015)  # subset data to 2015
crime <- crime[, -c(1, 2, 5:10, 16:17)]  # remove extra columns
crime <- na.omit(crime)  # remove NA rows 
crime$City <- NULL  # create new column for cleaned city names
crime$City <- gsub("(.*),.*", "\\1", crime$department_name)  # remove state abbreviations
crime[2, 8] <- "Arlington TX"  # clean up confusion city
crime[64, 8] <- "Washington DC"  # clean up confusion city
crime <- crime[- grep("County", crime$City),]  # remove county-level data
crime$department_name <- NULL  # delete old city name column
crime <- crime[,c(7, 1:6)]  # rearrange data frame to have city be first column
crime_types <- c("Violent_Crime", "Homicide", "Rape", "Robbery", "Aggravated_Assault")  # rename columns
colnames(crime)[2] <- "Population"
colnames(crime)[3:7] <- crime_types  # create vector of crime types
cities <- unique(crime$City) # create vector of city names
rownames(crime) <- NULL  # renumber rows
crime[2:7] <- round(crime[2:7], digits = 2)  # round all numbers to 2 digits

pdf(NULL)