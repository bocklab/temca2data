
if(!interactive())
  options(repos="http://cran.rstudio.com")

# install standard packages
install.packages(c('doMC', 'magrittr', 'dendroextras', 'rgl', 'ggplot2', 'dplyr', 'dendextend','igraph', 'xlsx'))

if(!require(devtools)) install.packages('devtools')
devtools::install_github(c("jefferis/nat",
                           "jefferislab/nat.nblast",
                           "jefferislab/nat.templatebrains",
                           "jefferislab/nat.flybrains",
                           "jefferis/flycircuit",
                           "jefferis/rcatmaid"))

# note that downloading the fork elmr is needed
devtools::install_github("zhihaozheng/elmr")
devtools::install_github("zhihaozheng/rmushroom")

# set up CATMAID log in or load pre-saved data
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

source(file.path(getwd(),"functions.R"))
