
########################################################
# 1. Set Working Directory
########################################################

if(Sys.info()["nodename"] == "IDIVNB193"){
  setwd("C:\\restore2\\hp39wasi\\sWormDatabaseRelease")
}

source(file.path("Functions", "FormatData.R"))
library(plyr)
library(dplyr)
library(reshape2)

########################################################
# 2. Create folder if it doesn't exist to save data into
########################################################

if(!dir.exists("3_Data")){
  dir.create("3_Data")
}
data_out <- "3_Data"


#################################################
# 3. Loading in variables
#################################################
## Species data


## This is a file I made manually, by combining data from George and Maria (also in this folder)
loadinfg <- "UniqueSpeciestoSend_2019-06-19_Final.csv"
# Drilobase was also used to check for synonyms and add in functional groups where possible

## Some species identifications are in press - look at George's final file to identify. 
# Guillaume Rosseau would need to be contacted to get the ecological categories


## Site data


data_in_sites <-"2_Data"

files <- list.files(file.path(data_in_sites))
file_dates <- sapply(strsplit(files, "_"), "[", 2) ## Split the string by date, which produces a list, then take second element of each list i.e. the date
file_dates <- sapply(strsplit(file_dates, "\\."), "[", 1) ## Split the string by date, which produces a list, then take first element of each list i.e. the date

file_dates <- as.Date(file_dates)
date <- max(file_dates, na.rm = TRUE)
loadin <- files[grep(date, files)]

rm(files)
rm(date)

### Species data

data_in_spp <-"0_Data"
files <- list.files(file.path(data_in_spp))
files <- files[grep("species_", files)]
file_dates <- sapply(strsplit(files, "_"), "[", 2) ## Split the string by date, which produces a list, then take second element of each list i.e. the date
file_dates <- sapply(strsplit(file_dates, "\\."), "[", 1) ## Split the string by date, which produces a list, then take first element of each list i.e. the date

file_dates <- as.Date(file_dates)
date <- max(file_dates, na.rm = TRUE)
loadin_spp <- files[grep(date, files)]

#################################################
# 4. Load in data
#################################################

sites <- read.csv(file.path(data_in_sites, loadin))
spp <- read.csv(file.path("RevisedSpeciesNames", loadinfg))
spp_dat <- read.csv(file.path(data_in_spp, loadin_spp)) # 20726

spp_dat$newID <- paste(spp_dat$file.x, spp_dat$Study_Name, spp_dat$Site_Name.x)
sites$newID <- paste(sites$file, sites$Study_Name, sites$Site_Name)


#################################################
# 5. Tidy up categories in spp
#################################################

levels(spp$Revised_fg)
levels(spp$Revised_fg)[levels(spp$Revised_fg) == "anecic"] <- "Anecic"
levels(spp$Revised_fg)[levels(spp$Revised_fg) == "endogeic"] <- "Endogeic"
levels(spp$Revised_fg)[levels(spp$Revised_fg) == "endogeic?"] <- "Endogeic"
levels(spp$Revised_fg)[levels(spp$Revised_fg) == "Epi-endogeic"] <- "Epi-Endogeic"
levels(spp$Revised_fg)[levels(spp$Revised_fg) == "epigeic"] <- "Epigeic"
levels(spp$Revised_fg)[levels(spp$Revised_fg) == "N/A"] <- "Unknown"
levels(spp$Revised_fg)[levels(spp$Revised_fg) == "endogeic or epiendogeic"] <- "Epi-Endogeic"
levels(spp$Revised_fg)[levels(spp$Revised_fg) == "Endo-anecic"] <- "Anecic"
levels(spp$Revised_fg)[levels(spp$Revised_fg) == "Endogeic, Anecic"] <- "Anecic"
levels(spp$Revised_fg)[levels(spp$Revised_fg) == "epi-anecic"] <- "Anecic"

#################################################
# 6. Remove unwanted columns
#################################################
keep <- c("SpeciesBinomial","Revised", "Revised_fg")      

spp <- spp[,names(spp) %in% keep]

#################################################
# 7. Merge with species level dataset
#################################################

spp_dat <- merge(spp_dat, spp, by.x = "SpeciesBinomial", by.y = "SpeciesBinomial", all.x = TRUE)

#################################################
# 8. Use FG given if no other (especially as some non-species will have them)
#################################################

missing <- which(is.na(spp_dat$Revised_fg))
given <- which(!(is.na(spp_dat$Functional_Type)))
toFill <- intersect(missing, given)

spp_dat$Revised_fg[toFill] <- spp_dat$Functional_Type[toFill]

## And fill NAs with Unknown
missing <- which(is.na(spp_dat$Revised_fg))
spp_dat$Revised_fg[missing] <- "Unknown"

names(spp_dat)[names(spp_dat) == "Revised"] <- "Revised_Binomial"
#################################################
# 9 Save this dataset
#################################################

write.csv(spp_dat, file = file.path(data_out, paste("SpecieswithFunctionalGroups_", Sys.Date(), ".csv", sep = "")), row.names = FALSE)

#################################################
# 10. Create the dataframe
#################################################
# This create site-level abundances and biomasses

detach(package:plyr) 
Summary.div <- spp_dat %>% # Start by defining the original dataframe, AND THEN...
  group_by(newID) %>% # Define the grouping variable, AND THEN...
  summarize( # Now you define your summary variables with a name and a function...
    Epi_biomass = sum(Biomass_fromspecies[which(Revised_fg == "Epigeic")], na.rm = TRUE),
    Endo_biomass = sum(Biomass_fromspecies[which(Revised_fg == "Endogeic")], na.rm = TRUE),
    Ane_biomass = sum(Biomass_fromspecies[which(Revised_fg == "Anecic")], na.rm = TRUE),
    EpiEndo_biomass = sum(Biomass_fromspecies[which(Revised_fg == "Epi-Endogeic")], na.rm = TRUE),
    Unknown_biomass = sum(Biomass_fromspecies[which(Revised_fg == "Unknown")], na.rm = TRUE),
    FG_biomass_units = unique(Biomass_fromspeciesUnits),
    Epi_abundance = sum(Individuals_fromspecies[which(Revised_fg == "Epigeic")], na.rm = TRUE),
    Endo_abundance = sum(Individuals_fromspecies[which(Revised_fg == "Endogeic")], na.rm = TRUE),
    Ane_abundance = sum(Individuals_fromspecies[which(Revised_fg == "Anecic")], na.rm = TRUE),
    EpiEndo_abundance = sum(Individuals_fromspecies[which(Revised_fg == "Epi-Endogeic")], na.rm = TRUE),
    Unknown_abundance = sum(Individuals_fromspecies[which(Revised_fg == "Unknown")], na.rm = TRUE),
    FG_abundance_units = unique(Individuals_fromspeciesUnits)
  )

summary.div <- as.data.frame(Summary.div)
str(summary.div)


#######################################################
## SPECIES RICHNESS
########################################################

juvs <- which(spp_dat$LifeStage == "Juvenile")
notSpecies <- which(is.na(spp_dat$Revised_Binomial) & is.na(spp_dat$MorphospeciesID))

notSp <- union(juvs, notSpecies)
spR <- spp_dat[-notSp,]
library(plyr)
t <- ddply(spR, c("newID", "Revised_fg"), summarise, nrows = length(Revised_fg))
t2 <- dcast(t, newID ~ Revised_fg, value.var = "nrows")
names(t2)[2:6] <- c("Ane_richness", "Endo_richness", "EpiEndo_richness", "Epi_richness", "Unknown_richness")

###################
## Calculate the functional group richness as well
###################

t3 <- t2
t3$Unknown_richness <- NULL
t3$Ane_richness <- ifelse(is.na(t2$Ane_richness), 0, 1)
t3$Endo_richness <- ifelse(is.na(t2$Endo_richness), 0, 1)
t3$EpiEndo_richness <- ifelse(is.na(t2$EpiEndo_richness), 0, 1)
t3$Epi_richness <- ifelse(is.na(t2$Epi_richness), 0, 1)

t3$FGRichness <- rowSums(t3[,2:5])
t3 <- t3[,c('newID', 'FGRichness')]
t2 <- merge(t2, t3, by = "newID")

##########################################################
## Match with site level dataset
##########################################################

sites_fg <- merge(sites, summary.div, by.x = "newID", by.y = "newID", all.x = TRUE)
sites_fg <- merge(sites_fg, t2, by.x = "newID", by.y = "newID", all.x = TRUE)
##########################################################
## Save the data
##########################################################


write.csv(sites_fg, file = file.path(data_out, paste("SiteswithFunctionalGroups_", Sys.Date(), ".csv", sep = "")), row.names = FALSE)

