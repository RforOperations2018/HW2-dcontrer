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

# Define UI for application that draws a histogram
ui <- navbarPage("Major City Crime Stats (2015)", # set page name
                 tabPanel("Scatterplot", # set tab name
                          sidebarLayout(
                            sidebarPanel(
                              # City Selection
                              pickerInput("citySelect", # create city selection slider, default to 10 selections
                                          "City:", 
                                          choices = sort(unique(crime$City)), 
                                          multiple = TRUE,
                                          selected = cities[1:10],
                                          options = list(`max-options` = 58)),
                              # Crime Selection
                              pickerInput("crimeSelect", # create crime selection slider, limit to 2 selections for scatterplot
                                          "Crimes:",
                                          choices = crime_types,
                                          multiple = TRUE,
                                          selected = crime_types[1:2],
                                          options = list(`max-options` = 2)),
                              # Population Selection
                              sliderInput("popSelect", # create population size slider
                                          "Population Size:",
                                          min = min(crime$Population, na.rm = T),
                                          max = max(crime$Population, na.rm = T),
                                          value = c(min(crime$Population, na.rm = T), max(crime$Population, na.rm = T)),
                                          step = 1),
                              actionButton("reset", "Reset Filters", icon = icon("refresh")) # add button to reset filters
                            ),
                            # Output plot
                            mainPanel(
                              plotlyOutput("plot")
                            )
                          )
                 ),
                 # Data Table
                 tabPanel("Table",
                          inputPanel(
                            downloadButton("downloadData","Download Crime Data") # add button to download table as csv
                          ),
                          fluidPage(DT::dataTableOutput("table"))
                 )
)
