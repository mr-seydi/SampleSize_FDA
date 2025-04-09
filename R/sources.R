#############Loading Library#####################
library(fda)
library(GET)
library(openxlsx)
library(doParallel)
library(plyr)

#############Loading utilities#####################
source("R/utilities.R")

########Loading Parallel#####################
source("R/Parallel.R")
source("R/FWER_parallel.R")
#############Loading Data#####################
source("R/Data_functions.R")

#############Loading Functions#####################

# TWT function
source("R/TWT2.R")
# Fmax function
source("R/Fmax.R")
# IWT function
source("R/IWT.R")
# SPM function
source("R/SPM.R")