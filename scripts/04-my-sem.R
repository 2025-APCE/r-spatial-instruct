#------------------------00 HEADER -----------
# SEM to predict woody cover for my study area in the Serengeti 

#--------------------------01 Set up the environment ----
rm(list = ls()) # clear environment
# run the setup script for user-defined functions and Google Sheets authentication
source("C:/Users/holff/Github/2025-APCE/r-spatial-instruct/scripts/01-setup.R")
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

#------------------READ THE DATA ----------------------
# these are the data as generated in script 02-r-spatial.R
pointdata<-readr::read_csv("G:/Shared drives/_Org OlffLab/Teaching/APCE/_general/APCE_GIS/_MyData/pointdata.csv")
pointdata<-pointdata[complete.cases(pointdata),] # remove cases with missing values
pointdata
#-------------------DO A PCA---------------------------
# Perform PCA 
pca_result <- vegan::pca(pointdata,
                         scale = TRUE)
# Display a summary of the PCA
summary(pca_result)
# Plot the PCA
plot(pca_result, scaling = 2, type="n", xlab="",ylab="")  # Use scaling = 1 for distance preservation, scaling = 2 for correlations
# Add points for samples
points(pca_result, display = "sites", pch=pointdata$CorProtAr+1, bg = "blue", cex = 1)
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


#-------------------SEM with lavaan ---------------------------
pointdata_std <- pointdata |>
  mutate_all(~(scale(.) %>% as.vector)) |>
  as_tibble()
pointdata_std

names(pointdata_std)

# define the structural equation model to be fitted
model1 <- 'woody ~ rainfall + firefreq  + plains
                firefreq ~ rainfall + CorProtAr + plains
                '
# fit the model to the data
model1.fit <- lavaan::sem(model1, data=pointdata_std)

# show the model results
# goodness of fit (should be >0.9): CFI and TLI
# badness of fit: ( should be <0.1): RMSEA, SRMR
lavaan::summary(model1.fit, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)


# plot the sem model 

# Your desired coordinates of the variables (x = column, y = vertical position)
coords <- rbind(
  plains     = c(0,0),
  CorProtAr  = c(0,1),
  rainfall   = c(0,2),
  firefreq   = c(1,1),
  woody     = c(2,1)
)

# Get internal node order (S4: use @, not $)
g <- semPlot::semPlotModel(model1.fit)
node_order <- as.character(g@Vars$name)

# Reorder coords to that exact order; sanity-check all nodes exist
missing <- setdiff(node_order, rownames(coords))
if (length(missing)) stop("Missing coordinates for: ", paste(missing, collapse = ", "))
coords_ord <- coords[node_order, , drop = FALSE]

# Plot with semPlot
semPlot::semPaths(model1.fit, 
                  what="std",                   # use standardized coefficients
                  layout=coords_ord,            # use the specified layout
                  edge.label.cex=1.2, 
                  edge.label.position = 0.66,  # ← move labels 2/3 along each arrow
                  sizeMan=12,                   # size of the boxes
                  sizeLat=10,                    # size of the ovals
                  nCharNodes=0,                 # do not abbreviate variable names
                  residuals=FALSE,              # do not show residuals
                  edge.color="black", 
                  fade=FALSE, 
                  curve = 4,        # <-- stronger curvature for covariances
                  curvature = 1.5,     
                  mar = c(8, 8, 8, 8)            # ← increase bottom, left, top, right margins
)

#-------------------Piecewise sem -----------------------------------------

library(piecewiseSEM)

# 1. Specify each component model separately
mod_woody <- lm(
  woody ~ rainfall + firefreq + plains,
  data = pointdata
)

mod_firefreq <- glm(
  firefreq ~ rainfall + CorProtAr + plains,
  data = pointdata,
  family = poisson()
)

# 2. Combine them into a piecewise SEM
model1_pw <- psem(
  mod_woody,
  mod_firefreq
)

# 3. Model summaries
summary(model1_pw)

