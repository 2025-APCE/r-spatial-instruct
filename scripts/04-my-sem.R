#------------------------00 HEADER -----------
# SEM to predict woody cover for my study area in the Serengeti 

#--------------------------01 Set up the environment ----
rm(list = ls()) # clear environment
# run the setup script for user-defined functions and Google Sheets authentication
source("scripts/01-setup.R")
# authenticate Google Sheets access
gsheets_auth(email='h.olff@rug.nl')  # change in your own email address
# restore the libraries of the project 
renv::restore()

library(here)      # for getting your project start directory
library(tidyverse)
library(lavaan) # for SEM, install when needed
library(semPlot)
library(psych)
here()

#------------------READ THE DATA ----------------------
# these are the data as generated in script 02-r-spatial.R
pointdata<-readr::read_csv("./_MyData/pointdata.csv")
pointdata