#!/bin/bash

#SBATCH --job-name=find_design_soldvsnot
#SBATCH --partition=node
#SBATCH --ntasks-per-node=24
#SBATCH --nodes=1
#SBATCH --time=168:00:00
#SBATCH --mem-per-cpu=8048
#SBATCH --mail-type=FAIL
#SBATCH --mail-type=END
#SBATCH --mail-user=jwbowers@illinois.edu
#SBATCH --export=PATH,R_LIBS,CLUSTER,CORES,LD_LIBRARY_PATH

set InputDir=/data/keeling/a/jwbowers/Documents/PROJECTS/socialcontacturuguay-crime/Analysis
set RunDir=/data/keeling/a/jwbowers/Documents/PROJECTS/socialcontacturuguay-crime/Analysis

cd /data/keeling/a/jwbowers/Documents/PROJECTS/socialcontacturuguay-crime/Analysis

## For now, just use one node and multicore. Having trouble with the alternative
##export CLUSTER="keeling-future"
export CLUSTER=""
export CORES=24
export R_LIBS=/data/keeling/a/jwbowers/R/x86_64-redhat-linux-gnu-library/3.5
export HOME=/data/keeling/a/jwbowers

##mpirun -n 1 -x HOME  -x LD_LIBRARY_PATH -x CLUSTER -x CORES -x R_LIBS Rscript -e "library(rmarkdown);render('design_soldvsnot.Rmd',quiet=FALSE)"
mpirun -n 1 -x HOME  -x LD_LIBRARY_PATH -x CLUSTER -x CORES -x R_LIBS R --vanilla --file=designsearch_soldvsnot.R

