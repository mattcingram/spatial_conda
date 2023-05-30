################################################################################
#
# Ingram, M. 
# Reproduction of Brass et al. (2020) in Political Geography
# last update: 2023-04-23
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

# otherwise, hardcode filepath to working directory (wd)
# note: this is my directory; change to filepath on your own computer
path <- "B:/book_gisspatialanalysis/replications/brassetal2020"
# on mybinder, use this path for wd
path <- "/home/jovyan"
setwd(path)
getwd()

# build folder structure for sub-directories

dir.create("./code",showWarnings = TRUE)
dir.create("./data",showWarnings = TRUE)
dir.create("./data/original",showWarnings = TRUE)
dir.create("./data/working",showWarnings = TRUE)
dir.create("./figures",showWarnings = TRUE)
dir.create("./reports",showWarnings = TRUE)
dir.create("./publication",showWarnings = TRUE)
dir.create("./tables",showWarnings = TRUE)


###################################
# environment
###################################

options(scipen=999, digits=3)

#install.packages("pacman")
library(pacman)

# note: in original work, authors used packages that later were deprecated and
# archived; these packages include spdep and maptools
p_load(sf,              # sf (simple features); is replacing sp and spdep
       sp, spdep,       # parts of sp and spdep are increasingly deprecated
       spatialreg, # spatialreg replaces parts of sp and spdep
       rgdal, rgeos, maptools,     # retiring in 2023
       terra,      # replaced raster
       ggmap,      # quick way of getting raster maps from web
       tmap,       # quick thematic maps
       foreign, haven, xtable,
       stargazer, ggplot2, broom, tictoc, plyr, dplyr,
       CARBayes, miceadds, tidyverse, sandwich, 
       GWmodel,                                  # for GWR analysis
       igraph, network, shp2graph       # network tools
       )

#end
