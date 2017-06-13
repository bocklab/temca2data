
install_packages <- function() {
  # only run it if these packages have not been installed
  # Two usually need to be installed:
  # 1) a fork (and slightly modified) version of elmr from "zhihaozheng/elmr" (ZZ_TODO to ellimiate that fragile dependency)
  # 2) "jefferis/nat.as" and therefore "jefferis/AnalysisSuite"
  message("Installing packages!")
  if(!interactive())
    options(repos="http://cran.rstudio.com")
  
  # install standard packages
  install.packages(c('reshape2','grid', 'doMC', 'magrittr', 
                     'dendroextras', 'rgl', 'ggplot2', 'dplyr', 
                     'dendextend','igraph', 'xlsx', 'tidyr'))
  
  if(!require(devtools)) install.packages('devtools')
  devtools::install_github(c("jefferis/nat",
                             "jefferislab/nat.nblast",
                             "jefferislab/nat.templatebrains",
                             "jefferislab/nat.flybrains",
                             "jefferis/flycircuit",
                             "jefferis/rcatmaid",
                             "jefferis/nat.as"))
  
  nat.as::install_analysis_suite()
  nat.as::reload_analysis_suite()
  
  # note that downloading the fork elmr is needed (ZZ_TODO)
  devtools::install_github("zhihaozheng/elmr")
  devtools::install_github("zhihaozheng/rmushroom")
}

library(elmr)
library(flycircuit)
library(nat)
library(catmaid)
library(mushroom)
library(xlsx)
library(magrittr)
library(reshape2)
library(dplyr)
library(ggplot2)
library(igraph)
library(nat.flybrains)
library(grid)
library(reshape2)
library(rgl)

# laod funtions, data, and meta-data
source(file.path(getwd(),"functions.R"))
