# List of packages to check and install if necessary
packages <- c("tidyverse", "dplyr", "tidyr", "readxl", "readr")
# Function to check and install missing packages
install_if_missing <- function(packages) {
  for (pkg in packages) {
    if (!require(pkg, character.only = TRUE)) {
      install.packages(pkg, dependencies = TRUE)
      library(pkg, character.only = TRUE)}}}

install_if_missing(packages)

library(tidyverse)
library(dplyr)
library(tidyr)
library(readxl)
library(readr)

'##############################data loading####################################'

#change the working directory to the folder "data_AEER" in the next line:
setwd("C:/Users/bashu/Desktop/Uni Master/Semester 4/AEER/data_AEER")


#load the data 
freisetzungen<- read_excel("2023-12-08_PRTR-Deutschland_Freisetzungen.xlsx")

CampusFile_WK_cities <- read_csv("CampusFile_WK_cities.csv")

number_observations_panel <- read_excel("number_observations_panel.xlsx")


#due to long waiting time (between 5-8 hours) the fitted logistic regression 
#and the estimated Propensity Score (PS) were saved localy. 

#if you want to load the estimated PS from the fitted logit model please 
#execute the following line: 

PS <- readRDS("PS.rds")

#if you want to fit the model again instead an reestimate the PS please
#uncomment the lines 87-98 in section 'estimation.R'. In that case when
#executing the section the logit model will be fitted again and the then PS 
#will be estimated. Both will be saved automatically under geven working 
#directory.

'###########################END OF DATA LOADING################################'
'##############################################################################'