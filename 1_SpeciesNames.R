# This script makes a list of all species names in the dataset. 
# This list was given to the earthworm experts in sWorm to correct the
# names and functional groups. 




########################################################
# 1. Set Working Directory
########################################################


if(Sys.info()["nodename"] == "IDIVNB179"){
  setwd("C:\\Users\\hp39wasi\\WORK\\sWormDatabaseRelease\\")
}
########################################################
# 2. Create folder if it doesn't exist to save data into
########################################################

if(!dir.exists("1_Data")){
  dir.create("1_Data")
}
data_out <- "1_Data"


########################################################
# 3. Libraries
########################################################

source(file.path("Functions", "FormatData.R"))
library(dplyr)

#################################################
# 4. Loading in variables
#################################################

data_in <-"0_2_Data"
files <- list.files(file.path(data_in))
file_dates <- sapply(strsplit(files, "_"), "[", 2) ## Split the string by date, which produces a list, then take second element of each list i.e. the date
file_dates <- sapply(strsplit(file_dates, "\\."), "[", 1) ## Split the string by date, which produces a list, then take first element of each list i.e. the date

file_dates <- as.Date(file_dates)
date <- max(file_dates, na.rm = TRUE)
loadin <- files[grep(date, files)]
loadinsites <- loadin[grep("species_", loadin)]

loadinbib <- loadin[grep("Metadata_", loadin)]
#################################################
# 5. Load in data
#################################################

dat <- read.csv(file.path(data_in, loadinsites))
bib <- read.csv(file.path(data_in, loadinbib))

#################################################
# 6. Quick investigation
#################################################

length(unique(dat$SpeciesBinomial)) ##   316

table(dat$Functional_Type) ## Unknowns are also blank

## Check that all are binomials
which(
  (sapply(gregexpr("\\W+", dat$SpeciesBinomial), length) + 1) 
  < 2) ## This counts the number of words in the string
#################################################
# 6. Merge bib info
#################################################

bib <- bib[,which(names(bib) == "file" | names(bib) == "DataProvider_Surname")]


dat <- merge(dat, bib, by.x = "file", by.y = "file")

#################################################
# 7. Create dataframe for new morphospecies
#################################################

result <- dat %>%
  filter(!is.na(SpeciesBinomial)) %>% # to remove NA categories that we are grouping by
   group_by(SpeciesBinomial) %>%
   summarise(fg = toString(unique(Functional_Type)), 
             Country = toString(unique(Country)), 
             file = toString(unique(file)),
             provider = toString(unique(DataProvider_Surname)))

spp <- as.data.frame(result) # 315

# Drilobase sometimes contains multiple functional groups for each earthworm species
# Previous function merged them all together
# Some contained NAs and unknowns, so this removes them from the string
spp$fg <- gsub("NA, ", "", spp$fg)
spp$fg <- gsub(", NA", "", spp$fg)
spp$fg <- gsub("Unknown, ", "", spp$fg)
spp$fg <- gsub(", Unknown", "", spp$fg)

names(spp)[2] <- "FunctionalGroup"



## Remove Microdriles and Megadriles
# This have been included by accident
spp <- spp[-grep("microdrile", spp$SpeciesBinomial, ignore.case = TRUE),]
spp <- spp[-grep("Megadrile", spp$SpeciesBinomial, ignore.case = TRUE)]


# ###################################################
# # 8. Add blank columns for people to fill in
# ###############################################
# 
# 
# spp$Revised <- NA
# spp$Revised_fg <- NA
# spp$Revised_Authority <- NA
# spp$sWormMember <- NA

###################################################
# 11. Save the data
###############################################

write.csv(spp, file.path(data_out, paste("UniqueSpecies_", Sys.Date(), ".csv", sep ="")), row.names = FALSE)
