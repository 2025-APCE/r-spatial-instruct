#### STRUCTURAL EQUATION MODELLING USING LAVAAN

# analysis of Anderson 2007 dataset
# Paper:
# browseURL("https://drive.google.com/file/d/1aasI7uIFj7zcjuu0A3QhPDU4X-oQtcnx/view?usp=sharing")

# course Advanced Population and Community Ecology (APCE) -  2025
# Spatial analysis in R
# Han Olff nov 2025


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
#--------------------------02 Load and prepare the data ----
# Anderson et al 2007 - effects of large herbivores and fire on savanna
# key variables of interest in the dataset: 
# predictors:
# ALL_LHU - total large herbivore density
# RES_LHU - density resident herbivores 
# FIRE_FRQ - fire frequency
# NMS - plant species composition (NMDS ordination axis score)
# response: 
# LF_N - plant leaf nitrogen content
# other variables:
# PRECIP - annual rainfall
# THETRI - biomass of Themada triandra, a tall grass positively responding to fire
# BIOMASS - total aboveground plant biomass
# SOIL_RN - total soil reactive nitrogen (ammonium+nitrate)
# LF_NA - plant leaf sodium content

# define and read the dataset:
dataset_link<-"https://docs.google.com/spreadsheets/d/1wk3UTAN7Cp7ZeoB0wpfW2C2eE_VoyKnJQpJ0Zrjk3yM/"
# browseURL(dataset_link)
Anderson2007<-read_gsdb(dataset_link,sheets = c("FactAndersonData"))
Anderson2007$FactAndersonData <- Anderson2007$FactAndersonData |>
  dplyr::mutate(SOIL_RN = SOIL_NO3 + SOIL_NH4)
names(Anderson2007$FactAndersonData)
# standardize all variables to mean 0 and standard deviation 1
Anderson2007std <- Anderson2007$FactAndersonData |>
  mutate_all(~(scale(.) %>% as.vector)) |>
  as_tibble()
Anderson2007std
# note that this does not affect the relations between the variables, only the scales  

# make a pairs panel to inspect linearity of relations and expected normality of residuals
psych::pairs.panels(Anderson2007std |> select(RES_LHU, FIRE_FRQ,PRECIP,NMS,
                                            THETRI,BIOMASS,SOIL_RN,LF_N, LF_Na),
                    stars = T, ellipses = F)

#--------------------------03 Structural Equation Modelling ----

# analyse the model (response ~ predictors) with first with a multiple regression approach 
# so not assuming any causal relations between predictors
# multiple regression approach 
multreg<-lm(LF_N~BIOMASS + RES_LHU + FIRE_FRQ + NMS, data=Anderson2007std)
summary(multreg)

# define the structural equation model to be fitted
leaf_n.model <- 'LF_N ~  BIOMASS + RES_LHU + FIRE_FRQ + NMS
                    BIOMASS ~ FIRE_FRQ + RES_LHU
                    NMS ~  FIRE_FRQ'
# fit the model to the data
leaf_n.fit <- lavaan::sem(leaf_n.model, data=Anderson2007std)
leaf_n.fit
# show the model results
# goodness of fit (should be >0.9): CFI and TLI
# badness of fit: ( should be <0.1): RMSEA, SRMR
lavaan::summary(leaf_n.fit, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)


# plot the sem model leaf_n.fit:

# Your desired coordinates of the variables (x = column, y = vertical position)
coords <- rbind(
  FIRE_FRQ = c(0,  1.0),
  RES_LHU  = c(0,  0.0),
  BIOMASS  = c(1,  1.0),
  NMS      = c(1,  0.0),
  LF_N     = c(2,  0.5)
)

# Get internal node order (S4: use @, not $)
g <- semPlot::semPlotModel(leaf_n.fit)
node_order <- as.character(g@Vars$name)

# Reorder coords to that exact order; sanity-check all nodes exist
missing <- setdiff(node_order, rownames(coords))
if (length(missing)) stop("Missing coordinates for: ", paste(missing, collapse = ", "))
coords_ord <- coords[node_order, , drop = FALSE]

# Plot — do NOT use rotation when supplying explicit coordinates
semPlot::semPaths(leaf_n.fit, 
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
                  mar = c(8, 8, 8, 8)            # ← increase bottom, left, top, right margins
                  )

# Make a lavaan model as hypothesized in the Anderson et al 2007 paper and fit the model 


# show the model results
# goodness of fit (should be >0.9): CFI and TLI
# badness of fit: ( should be <0.1): RMSEA, SRMR

# visualize the model

# also explore the models as shown in fig 5b and 5c of the Anderson2007 paper
# so repeat the model for leaf P content

