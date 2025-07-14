



#### Setup: Run this script before running any of the others ####

# All other project scripts depend on packages and functions that are loaded in this one. 


# Order to run files

##############################################################################################################





#### Load libraries ####
# packages for data wrangling
library(tidyverse)
library(purrr) # for working with lists

# packages for survival analysis
library(survival)
library(survminer)
library(survRM2) # for calculating restricted mean survival time

# packages for data visualization
# for pretty plots
library(ggplot2)
library(cowplot)
# for pretty tables
library(knitr)



#### Load named Siler functions ####
# Fast mortality
CoaleDemenyWestF3 <- data.frame(
  a1= 0.558,
  b1= 1.05,
  a2= 0.01225, 
  a3= 0.000520, 
  b3= 0.0727,
  name="CoaleDemenyWestF3") 

CoaleDemenyWestF5 <- data.frame(
  a1= 0.457,
  b1= 1.07,
  a2= 0.01037, 
  a3= 0.000359, 
  b3= 0.0763,
  name="CoaleDemenyWestF5") 

CoaleDemenyWestF11 <- data.frame(
  a1= 0.256,
  b1= 1.17,
  a2= 0.00596, 
  a3= 0.000133, 
  b3= 0.086,
  name="CoaleDemenyWestF11") 

CoaleDemenyWestF15 <- data.frame(
  a1= 0.175,
  b1= 1.40,
  a2= 0.00368, 
  a3= 0.000075, 
  b3= 0.0917,
  name="CoaleDemenyWestF15") 

CoaleDemenyWestF17 <- data.frame(
  a1= 0.14,
  b1= 1.57,
  a2= 0.00265, 
  a3= 0.000056, 
  b3= 0.0949,
  name="CoaleDemenyWestF17") 

# Slow mortality
CoaleDemenyWestF21 <- data.frame(
  a1= 0.091,
  b1= 2.78,
  a2= 0.00092, 
  a3= 0.000025, 
  b3= 0.1033,
  name="CoaleDemenyWestF21") 




#### Load Anderson's individual-based model and functions for handling model output ####
source("./Known Unknowns/Functions/Simulate_Cemetery.R") # <- See this file for model details. 
source("./Known Unknowns/Functions/Workflow_Functions.R")





