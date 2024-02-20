## Main file for data analysis

# Run r_install_packages.R first if you need to install some of the
# packages below

rm(list=ls())

library(data.table)
library(ggplot2)
library(Hmisc)
library(readstata13)
library(dplyr)
library(psych)
library(R.matlab)
library(statar)
library(tidyr)
library(splitstackshape)
library(speedglm)
library(XNomial)
library(mitools)
library(survey)		
library(downloader)	
library(foreign) 	
library(margins)
library(brglm)
library(mfx)
library(stringr)
library(ipumsr)
library(scales)
library(plm)
library(lattice)
library(zoo)
library(weights)
library(moments)
library(RColorBrewer)


main.folder <- "~/Dropbox/Research/Portfolio/182663-2/"
# This is the ONLY path that needs to be set for this code to work
# Independent pieces of this code can be run just by setting this
# path first, too

da.folder <- paste0(main.folder,"code/data_analysis/")

# If wanting to derive variables from PSID-provided files run
# source(paste0(main.folder,"code/data_prep/load_psid_data_1.R"))
# This is not necessary if downloading raw_psid_data from my repository
# as described in the README

# Similarly, if wanting to derive variables from SCF original files run
# source(paste0(main.folder,"code/data_prep/load_scf_data.R"))

# Else 

source(paste0(da.folder,"load_psid_data_2.R"),echo=TRUE)

source(paste0(da.folder,"psid_outputs.R"),echo=TRUE)

source(paste0(da.folder,"psid_outputs_more.R"),echo=TRUE)

source(paste0(da.folder,"data_asset_returns.R"),echo=TRUE)
source(paste0(da.folder,"scf_pre80.R"),echo=TRUE)

# if wanting to reproduce results from original SCF data:
#source(paste0(da.folder,"load_scf_data.R"),echo=TRUE)

source(paste0(da.folder,"scf_participation.R"),echo=TRUE)
source(paste0(da.folder,"ipums_rev.R"),echo=TRUE)
