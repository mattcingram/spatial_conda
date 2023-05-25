################################################################################
#
# Ingram, M. 
# Reproduction of Brass et al. (2020) in Political Geography
# last update: 2023-05-25
# steps here: setup
#
################################################################################

################################
# clear any objects from memory
rm(list = ls(all = TRUE))

################################
# directory structure
################################
getwd()
# if script opened directly from code subfolder, may only need to move up one level
# setwd("../")

# otherwise, hardcode filepath to working directory
# note: this is my directory; change to filepath on your own computer
path <- "B:/book_gisspatialanalysis/replications/brassetal2020"
setwd(path)
getwd()

###################################
# environment
###################################

options(scipen=999, digits=3)

#install.packages("pacman")
library(pacman)

# note: in original work, authors used packages that later were deprecated and
# archived; these packages include spdep and maptools
p_load(sf,              # sf (simple features); is replacing sp and spdep
       spdep, 
       spatialreg, rgdal,
       terra,      # terra is replacing raster
       ggmap,      # quick way of getting raster maps from web
       foreign, haven, xtable,
       stargazer, ggplot2, broom, tictoc, plyr, dplyr,
       CARBayes, miceadds, tidyverse, sandwich, 
       GWmodel                                  # for GWR analysis
)

#end
