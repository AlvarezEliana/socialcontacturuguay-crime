# This is a Makefile that records the relationships among the files in this project.
# For example the following example says that the file, outputdata.rda depends in inputdata.rda and the datawork.R file.
# To create outputdata.rda we will need to run the R CMD BATCH datawork.R command.
# To use the makefile at the unix command line, we would type make outputdata.rda .

## outputdata.rda: inputdata.rda datawork.R
## 	R CMD BATCH datawork.R

SHELL = /bin/sh

.PHONY: all

all: main.pdf

## Paper

main.pdf: main.tex  \
	media/initial_balance_plot.pdf \
	media/fm2_balance_plot.pdf \
	Analysis/outcome_analysis_soldvsnot.pdf
	latexmk -pdf main.tex

## Analysis

Analysis/outcome_analysis_soldvsnot.pdf: Analysis/outcome_analysis_soldvsnot.Rmd Analysis/design_soldvsnot.rda
	cd Analysis && Rscript -e "library(rmarkdown);render('outcome_analysis_soldvsnot.Rmd')"

Analysis/describe_design_soldvsnot.pdf: Analysis/describe_design_soldvsnot.Rmd Analysis/design_soldvsnot.rda
	cd Analysis && Rscript -e "library(rmarkdown);render('describe_design_soldvsnot.Rmd')"

media/fm2_balance_plot.pdf: Analysis/describe_design_soldvsnot.pdf

Analysis/design_soldvsnot.rda: Analysis/design_soldvsnot.Rmd Analysis/match_data_prep.rda \
	Analysis/initial_balance.rda
	cd Analysis && Rscript -e "library(rmarkdown);render('design_soldvsnot.Rmd')"

Analysis/design_soldvsnot.pdf: Analysis/design_soldvsnot.rda

Analysis/initial_balance.rda: Analysis/initial_balance.Rmd Analysis/match_data_prep.rda
	cd Analysis && Rscript -e "library(rmarkdown);render('initial_balance.Rmd')"

media/initial_balance_plot.pdf: Analysis/initial_balance.rda

Analysis/matchingresults.rda: Analysis/balanceAndMatching.Rmd Data/wrkdat.rda
	cd Analysis && Rscript -e "library(rmarkdown);render('balanceAndMatching.Rmd')"

Analysis/outcomeresults.rda: Analysis/outcome_analysis.Rmd Analysis/matchingresults.rda
	cd Analysis && Rscript -e "library(rmarkdown);render('outcome_analysis.Rmd')"


### Data

Analysis/match_data_prep.rda: Analysis/match_data_prep.Rmd Data/wrkdat.rda Data/finaldat.rda \
	Analysis/rmarkdownsetup.R
	cd Analysis && Rscript -e "library(rmarkdown);render('match_data_prep.Rmd')"

Data/finaldat.rda : Data/stata2R.R Data/basefinal1718.dta
	cd Data && R --vanilla --file=stata2R.R

Data/wrkdat.rda : Data/finaldat.rda Data/datasetup.R libraries/librarysetup_done.txt
	cd Data && R --vanilla --file=datasetup.R

Data/basefinal1718.dta:


## Data/basefinal1718.rda : Data/basefinal1718.dta Data/basefinal1718toR.R
##	cd Data && R --vanilla --file=basefinal1718toR.R

## I don't think this next is used to create Final_database.dta
## Data/merged_ip.csv : Data/Mergescript.R \
## 	Data/base\ vecinos\ con\ factores\ PUBLICS\ y\ STIGM.dta \
## 	Data/base\ farmacias.dta
## 	cd Data && R CMD BATCH Data/Mergescript.R

## Local libraries

libraries/librarysetup_done.txt: librarysetup.R
	R --vanilla --file=librarysetup.R

## Other tasks

makefile.png: Makefile
	make -Bnd | make2graph > makefile.dot;
	gsed -i '2i graph [dpi =   600]; \n size="7.5,9"; \n ratio="fill" ' makefile.dot;
	dot -Tpng makefile.dot -o makefile.png

## see also https://stackoverflow.com/questions/14784405/how-to-set-the-output-size-in-graphviz-for-the-dot-format/20536144
## dot -Tpng -Gsize=9,15\! -Gdpi=100 -ofoo.png foo.gv
