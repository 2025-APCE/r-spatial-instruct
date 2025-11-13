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
library(vegan)
here()

#------------------READ THE DATA ----------------------
# these are the data as generated in script 02-r-spatial.R
pointdata<-readr::read_csv("G:/Shared drives/_Org OlffLab/Teaching/APCE/_general/APCE_GIS/_MyData/pointdata.csv")
pointdata<-pointdata[complete.cases(pointdata),] # remove cases with missing values
pointdata
#-------------------DO A PCA---------------------------
# Load the vegan package
library(vegan)
# Perform PCA using the rda() function
pca_result <- vegan::pca(pointdata,
                         scale = TRUE)
# Display a summary of the PCA
summary(pca_result)
pca
# Plot the PCA
plot(pca_result, scaling = 2, type="n", xlab="",ylab="")  # Use scaling = 1 for distance preservation, scaling = 2 for correlations
# Add points for samples
points(pca_result, display = "sites", pch=pointdata$CorProtAr+1, col = pointdata$hills+1, bg = "blue", cex = 1)
# Add arrows for variables
arrows(0, 0, scores(pca_result, display = "species")[, 1], scores(pca_result, display = "species")[, 2], 
       length = 0.1, col = "red")
# Label the variables with arrows
text(scores(pca_result, display = "species")[, 1], scores(pca_result, display = "species")[, 2], 
     labels = colnames(pointdata), col = "red", cex = 0.8, pos = 4)
# Add axis labels and a title
title(main = "PCA Biplot")
xlabel <- paste("PC1 (", round(pca_result$CA$eig[1] / sum(pca_result$CA$eig) * 100, 1), "%)", sep = "")
ylabel <- paste("PC2 (", round(pca_result$CA$eig[2] / sum(pca_result$CA$eig) * 100, 1), "%)", sep = "")
title(xlab=xlabel)
title(ylab=ylabel)
# add contours for woody cover
vegan::ordisurf(pca_result, pointdata$woody, add = TRUE, col = "green4")

