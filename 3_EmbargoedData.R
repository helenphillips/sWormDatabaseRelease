## This script removes data where we had an explicit 'no' from
## the providers about making their data open access

## For clarity, the 'no' was for a variety of reasons
## For example, the data were still being used for other analyses
## and therefore couldn't be made open yet


## WORKING DIRECTORY ----------------------------------
if(Sys.info()["nodename"] == "IDIVNB193"){
  setwd("C:\\restore2\\hp39wasi\\sWormDatabaseRelease")
}

if(Sys.info()["nodename"] == "IDIVNB179"){
  setwd("C:\\Users\\hp39wasi\\WORK\\sWormDatabaseRelease\\")
}

if(Sys.info()["nodename"] == "LAPTOP-I0JSR1DL"){
  setwd("~/WORK/sWormDatabaseRelease")
}



## VARS AND LIBRARIES --------------------------------
if(!dir.exists("3_Data")){
  dir.create("3_Data")
}
data_out <- "3_Data"

data_in <- "2_Data"

source("Functions/loadMostRecent.R")

## LOAD DATA ------------------------------------


# One day I will make the function better
loadinbib <- loadMostRecent_2("MetaData_sWorm", data_in)
bib <- read.csv(file.path(data_in, loadinbib))


loadinsites <- loadMostRecent_2("SitesData_sWorm", data_in)
sites <- read.csv(file.path(data_in, loadinsites))


loadinspp <- loadMostRecent_2("SppOccData_sWorm", data_in)
spp <- read.csv(file.path(data_in, loadinspp))



## WHICH ARE EMBARGOED NOW -----------------------------

embargoedFiles <- c("000_Briones1991")

bib <- bib[-which(bib$file == embargoedFiles),]

sites <- sites[-which(sites$file == embargoedFiles),]

spp <- spp[-which(spp$file == embargoedFiles),]

## SAVE ALL FILES AGAIN ---------------------------------

write.csv(bib, file = file.path(data_out, paste0("MetaData_sWorm_", Sys.Date(), ".csv")), row.names = FALSE)
write.csv(sites, file = file.path(data_out, paste0("SiteData_sWorm_", Sys.Date(), ".csv")), row.names = FALSE)
write.csv(spp, file = file.path(data_out, paste0("SppOccData_sWorm_", Sys.Date(), ".csv")), row.names = FALSE)
