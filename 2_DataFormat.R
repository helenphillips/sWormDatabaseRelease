## SCRIPT INFO ----------------------------
# This script takes the three files (bib, sites and species)
# and formats them. for example, adding in revised species names
# renaming columns, making sure columns are in a good order
# removing redundant columns

## GENERAL VARS ---------------------------

if(Sys.info()["nodename"] == "IDIVNB193"){
  setwd("C:\\restore2\\hp39wasi\\sWormDatabaseRelease")
}

if(Sys.info()["nodename"] == "IDIVNB179"){
  setwd("C:\\Users\\hp39wasi\\WORK\\sWormDatabaseRelease\\")
}


data_in <-"0_Data"

if(!dir.exists("2_Data")){
  dir.create("2_Data")
}
data_out <- "2_Data"


## FUNCTIONS AND PACKAGES --------------

source("Functions/loadMostRecent.R")
source("Functions/orderDF.R")


## LOAD BIB DATA ------------------------------------


loadinbib <- loadMostRecent("Metadata_", data_in)
bib <- read.csv(file.path(data_in, loadinbib))

## REMOVE BIB COLUMNS---------------------------------

names(bib)

colsToRemove <- c("DataProvider_Title","DataProvider_Surname",
                 "DataProvider_FirstName","DataProvider_MiddleInitials" ,
                 "DataProvider_Email","DataProvider_Institute",
                 "DataProvider_Department","Additional_Authors","Notes","BibKey")

bib <- bib[,-(which(names(bib) %in% colsToRemove))]


## REORDER BIB FILES ---------------------------------

bib <- arrange.vars(bib, c("file"=1))

                           
## SAVE NEW BIB FILE -------------------------------------

write.csv(bib, file = file.path(data_out, paste0("MetaData_sWorm_", Sys.Date(), ".csv")), row.names = FALSE)


## LOAD SITE DATA ------------------------------------------

loadinsites <- loadMostRecent("sites_", data_in)
sites <- read.csv(file.path(data_in, loadinsites))

## REMOVE SITE COLUMNS ------------------------------------
names(sites)

colsToRemove <- c("IPBES_Habitat_Units", "Stocking_rate", "Study_site", "ID")

sites <- sites[,-(which(names(sites) %in% colsToRemove))]

## RENAME SOME SITE COLUMNS ------------------------------

names(sites)[names(sites) == "Longitude__Decimal_Degrees"] <- "Longitude__decimal_degrees"

sites$ExtractionMethod <- as.character(sites$ExtractionMethod)
sites$ExtractionMethod[which(sites$ExtractionMethod == "Hand sorting + Liquid Extraction (e.g. Formaldehyde)")] <- 
  "Hand sorting + Chemical extraction (e.g. Formalin)"
sites$ExtractionMethod[which(sites$ExtractionMethod == "Liquid Extraction (e.g. Formaldehyde)")] <- 
  "Chemical extraction (e.g. Formalin)"

sites$ExtractionMethod[which(sites$ExtractionMethod == "Hand sorting + Liquid extraction (e.g. Mustard)")] <- 
  "Hand sorting + Chemical extraction (e.g. Mustard)"
sites$ExtractionMethod[which(sites$ExtractionMethod == "Liquid extraction (e.g. Mustard)")] <- 
  "Chemical extraction (e.g. Mustard)"


sites$PH_Collection_Method <- as.character(sites$PH_Collection_Method)
sites$PH_Collection_Method[which(sites$PH_Collection_Method == "H20")] <- "H2O"


sites$Management_System <- as.character(sites$Management_System)
sites$Management_System[which(is.na(sites$Management_System))] <- "None"

sites$LandUse <- as.character(sites$LandUse)
sites$LandUse[which(is.na(sites$LandUse))] <- "Unknown"

sites$HabitatCover <- as.character(sites$HabitatCover)
sites$HabitatCover[which(is.na(sites$HabitatCover))] <- "Unknown"



## FIXING COUNTRY NAMES -----------------------------------



## CHANGE THE ORDER OF SITE COLUMNS --------------------------

## Actually, they are in the correct order

## SAVE NEW SITE FILE -------------------------------------

write.csv(sites, file = file.path(data_out, paste0("SitesData_sWorm_", Sys.Date(), ".csv")), row.names = FALSE)


## LOAD THE SPECIES DATA ---------------------------------------

loadinspp <- loadMostRecent("species_", data_in)
spp <- read.csv(file.path(data_in, loadinspp))

## REMOVE COLUMNS -----------------------------------------

spp$Study_site <- NULL

## MERGE WITH FUNCTIONAL GROUPS -----------------------------

## This is a file I made manually, by combining data from George and Maria (also in this folder)
loadinfg <- "UniqueSpeciestoSend_2019-06-19_Final.csv"
# Drilobase was also used to check for synonyms and add in functional groups where possible

## Some species identifications are in press - look at George's final file to identify. 
# Guillaume Rosseau would need to be contacted to get the ecological categories
fg <- read.csv(file.path("RevisedSpeciesNames", loadinfg))
keep <- c("SpeciesBinomial","Revised", "Revised_fg")      

fg <- fg[,names(fg) %in% keep]

levels(fg$Revised_fg)
levels(fg$Revised_fg)[levels(fg$Revised_fg) == "anecic"] <- "Anecic"
levels(fg$Revised_fg)[levels(fg$Revised_fg) == "endogeic"] <- "Endogeic"
levels(fg$Revised_fg)[levels(fg$Revised_fg) == "endogeic?"] <- "Endogeic"
levels(fg$Revised_fg)[levels(fg$Revised_fg) == "Epi-endogeic"] <- "Epi-Endogeic"
levels(fg$Revised_fg)[levels(fg$Revised_fg) == "epigeic"] <- "Epigeic"
levels(fg$Revised_fg)[levels(fg$Revised_fg) == "N/A"] <- "Unknown"
levels(fg$Revised_fg)[levels(fg$Revised_fg) == "endogeic or epiendogeic"] <- "Epi-Endogeic"
levels(fg$Revised_fg)[levels(fg$Revised_fg) == "Endo-anecic"] <- "Anecic"
levels(fg$Revised_fg)[levels(fg$Revised_fg) == "Endogeic, Anecic"] <- "Anecic"
levels(fg$Revised_fg)[levels(fg$Revised_fg) == "epi-anecic"] <- "Anecic"

spp <- merge(spp, fg, by.x = "SpeciesBinomial", by.y = "SpeciesBinomial", all.x = TRUE)

missing <- which(is.na(spp$Revised_fg))
given <- which(!(is.na(spp$Functional_Type)))
toFill <- intersect(missing, given)

spp$Revised_fg[toFill] <- spp$Functional_Type[toFill]

## And fill NAs with Unknown
missing <- which(is.na(spp$Revised_fg))
spp$Revised_fg[missing] <- "Unknown"

names(spp)[names(spp) == "Revised"] <- "Revised_Binomial"

## REMOVE AND RENAME COLUMNS ------------------------------

spp$SpeciesBinomial <- NULL
spp$Functional_Type <- NULL

spp <- spp[,c("file","Study_ID","Site_Name",
              "Revised_Binomial", "MorphospeciesID","Genus","Family",
              "Revised_fg","LifeStage","Native.Nonnative","Abundance",
              "Abundance_Unit","WetBiomass","WetBiomassUnits")]
              
names(spp)[which(names(spp) == "Revised_Binomial")] <- "SpeciesBinomial"
names(spp)[which(names(spp) == "Revised_fg")] <- "Ecological_group"

## SAVE SPECIES FILE --------------------------------------

write.csv(spp, file = file.path(data_out, paste0("SppData_sWorm_", Sys.Date(), ".csv")), row.names = FALSE)

