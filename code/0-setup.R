# SETUP ####


# Library ----------------------------------------------------------------------

library(rstudioapi) # setup
library(tidyverse)  # data processing
library(tidyr)
library(magrittr)
# library(reshape2)
library(sf)         # spatial analysis
library(sp)
library(gstat)
library(geosphere)
library(tidymodels) # machine learning
library(ranger)
library(parallel)   # parallel computing
library(foreach)
library(doParallel)
library(cowplot)    # plotting
library(egg)
# library(png)
# library(plotly)


# Set Working Directory --------------------------------------------------------

# set current directory as working directory
dir_path <- dirname(getActiveDocumentContext()$path)
setwd(dir_path)


# Functions --------------------------------------------------------------------

# list of functions
functions <- list.files(path = '../../functions/', pattern = "[.]R$", 
                        full.names=TRUE, recursive = TRUE)
# load
for (i in (1:length(functions))) {source(functions[i])}

