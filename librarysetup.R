## Setup and install libraries locally
## As a general rule, we want the working directory to be the directory containing the command file.

## This next is fast so it doesn't hurt to set it even if it is already set by virtue of the way that we are using R (i.e. if you start R at the command line in this directory, it will have the correct working directory set).

### First create a local library directory so that we do not mess with global R packages (especially important if we do not have permission, also polite)
dir.create("libraries", showWarnings = FALSE)
.libPaths("libraries")
firstpkgs <- c("here","devtools")
pkgs <- installed.packages(lib.loc="libraries")

toinstallfirst <- firstpkgs[!(firstpkgs %in% pkgs[,"Package"])]
if(length(toinstallfirst)>0){
  install.packages(toinstallfirst, repos="http://cran.rstudio.org")
}

library(here)
thisdir <- here()
setwd(thisdir)

## Devtools install_github will only reinstall RItools if something changes on github
library(devtools)
## We are using the development branch of RItools for cluster and stratification options.
##remotes::install_github("markmfredrickson/RItools@randomization-distribution",, lib="libraries")
remotes::install_github("markmfredrickson/RItools@issue-111-balanceTest",, lib="libraries")
library(RItools,lib="libraries")

## Install our local package. Eventually make its own repo and use the code like that for RItools above.
remotes::install_local(path=here("manytestsr"),build_manual=TRUE)


secondpkgs <- c("ggplot2", "dplyr", "clubSandwich", "lmtest", "sandwich", "knitr","tidyr","broom","coin",
"optmatch","estimatr","arm")


toinstallsecond <- secondpkgs[!(secondpkgs %in% pkgs[,"Package"])]
if(length(toinstallsecond)>0){
  install.packages(toinstallsecond, repos="http://cran.rstudio.org")
}

## load the second set of packages
for(nm in secondpkgs){
  message("Loading package ",nm,sep="")
  require(nm,character.only = TRUE)
}

knitr::opts_chunk$set(strip.white=TRUE,
               width.cutoff=132,
               size='\\scriptsize',
               out.width='.8\\textwidth',
               message=FALSE,
               comment=NA)

options(digits=4,scipen=10)

system("touch libraries/librarysetup_done.txt")
