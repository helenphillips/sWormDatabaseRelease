## SCRIPT INFO ----------------------------
# This script takes the three files (bib, sites and species)
# and formats them. for example, adding in revised species names
# renaming columns, making sure columns are in a good order
# removing redundant columns

## GENERAL VARS ---------------------------

if(Sys.info()["nodename"] == "IDIVNB179"){
  setwd("C:\\Users\\hp39wasi\\WORK\\sWormDatabaseRelease\\")
}

if(Sys.info()["nodename"] == "LAPTOP-I0JSR1DL"){
  setwd("~/WORK/sWormDatabaseRelease")
}



data_in <-"0_2_Data"

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

colsToRemove <- c("PaperContact_Email", "DataProvider_Title","DataProvider_Surname",
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

colsToRemove <- c("Study_site", "ID")

sites <- sites[,-(which(names(sites) %in% colsToRemove))]

## RENAME SOME SITE COLUMNS ------------------------------

names(sites)[names(sites) == "Longitude__Decimal_Degrees"] <- "Longitude_decimal_degrees"
names(sites)[names(sites) == "Latitude__decimal_degrees"] <- "Latitude_decimal_degrees"


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

levels(sites$Country)[which(levels(sites$Country) == "MEXICO")] <- "Mexico"
  levels(sites$Country)[which(levels(sites$Country) == "Pueto Rico")] <- "Puerto Rico"
  levels(sites$Country)[which(levels(sites$Country) == "UK")] <- "United Kingdom"
  levels(sites$Country)[which(levels(sites$Country) == "Wales")] <- "United Kingdom"
  levels(sites$Country)[which(levels(sites$Country) == "USA")] <- "United States"
## CHANGE THE ORDER OF SITE COLUMNS --------------------------

## Actually, they are in the correct order

## SAVE NEW SITE FILE -------------------------------------

write.csv(sites, file = file.path(data_out, paste0("SitesData_sWorm_", Sys.Date(), ".csv")), row.names = FALSE)


## LOAD THE SPECIES DATA ---------------------------------------

loadinspp <- loadMostRecent("species_", data_in)
spp <- read.csv(file.path(data_in, loadinspp))


### CLEAN THE DATA -----------------------

spp$SpeciesBinomial  <- trimws(spp$SpeciesBinomial, which = c("both"))



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

# spp$SpeciesBinomial <- NULL
spp$Functional_Type <- NULL

spp <- spp[,c("file","Study_ID","Site_Name", "SpeciesBinomial",
              "Revised_Binomial", "MorphospeciesID","Genus","Family",
              "Revised_fg","LifeStage","Native.Nonnative","Abundance",
              "Abundance_Unit","WetBiomass","WetBiomassUnits")]

names(spp)[which(names(spp) == "SpeciesBinomial")] <- "OriginalSpeciesBinomial"

names(spp)[which(names(spp) == "Revised_Binomial")] <- "SpeciesBinomial"
names(spp)[which(names(spp) == "Revised_fg")] <- "Ecological_group"

## GENUS NAMES -------------------------
# Some binomials are actually subspecies
# which makes this harder...
# So I am doing it the easy way

allGenus <- c()
for(i in 1:nrow(spp)){
  allGenus[i] <- strsplit(spp$SpeciesBinomial[i], "\\s+")[[1]][1]
}

allGenus <- ifelse(is.na(allGenus), spp$Genus, allGenus)

allGenus<- trimws(allGenus, which = c("both"))


unique(allGenus)[order(unique(allGenus))]

allGenus[which(allGenus == "Euty")] <- "Eutyphoeus"
allGenus[which(allGenus == "genYuc")] <- ""
allGenus[which(allGenus == "Lombricus")] <- "Lumbricus"

allGenus[which(allGenus == "Amynthus")] <- "Amynthas"

allGenus[which(allGenus == "Firzingeria")] <- "Fitzingeria"

allGenus[which(allGenus == "Urubenus")] <- "Urobenus"




spp$Family[which(allGenus == "Lumbricidae")] <- "Lumbricidae"
allGenus[which(allGenus == "Lumbricidae")] <- ""

allGenus[which(allGenus == "Megascolecida")] <- "Megascolecidae"
spp$Family[which(allGenus == "Megascolecidae")] <- "Megascolecidae"
allGenus[which(allGenus == "Megascolecidae")] <- ""

spp$Family[which(allGenus == "Acanthodrilidae")] <- "Acanthodrilidae"
allGenus[which(allGenus == "Acanthodrilidae")] <- ""

spp$Family[which(allGenus == "Glossoscolecidae")] <- "Glossoscolecidae"
allGenus[which(allGenus == "Glossoscolecidae")] <- ""

allGenus[which(allGenus == "Ocnerod")] <- "Ocnerodrilidae" # this matches their other data
spp$Family[which(allGenus == "Ocnerodrilidae")] <- "Ocnerodrilidae"
allGenus[which(allGenus == "Ocnerodrilidae")] <- ""

spp$Family[which(allGenus == "Eudrilidae")] <- "Eudrilidae"
allGenus[which(allGenus == "Eudrilidae")] <- ""


allGenus[which(allGenus == "Aporectodea")] <- "Aporrectodea"
allGenus[which(allGenus == "Apporectodea")] <- "Aporrectodea"


spp$Genus2 <- allGenus

## FAMILY NAMES -----------------------------

spp$Family2 <- spp$Family

spp$Family2[spp$Genus2 %in% c(
"Agastrodrilus"  ,
  "Balanteodrilus",
"Dichogaster",
"Diplocardia",
"Diplotrema",
"Eutyphoeus",
"Larsonidrilus",
"Lavellodrilus",
"Lennogaster",
"Mayadrilus",
"Microscolex",
"Millsonia",
"Neotrigaster",
"Octochaetona",
"Reginaldia"
)] <- "Acanthodrilidae"


spp$Family2[spp$Genus2 %in% c(
  "Allobophoridella",
"Allolobophora",
"Allolobophoridella",
"Aporrectodea",
"Bimastos",
"Dendrobaena",
"Dendrodrilus",
"Eisenia",
"Eiseniella",
"Eiseniona",
"Fitzingeria",
"Helodrilus",
"Iberoscolex",
"Kritodrilus",
"Lumbricus",
"Murchieona",
"Octolasion",
"Octodriloides",
"Octodrilus",
"Perelia",
"Proctodrilus",
"Prosellodrilus",
"Rhiphaeodrilus",
"Satchellius",
"Scherotheca",
"Zophoscolex"
)] <- "Lumbricidae"


spp$Family2[spp$Genus2 %in% c(
  "Amynthas",
"Anisochaeta",
"Gemascolex",
"Heteroporodrilus",
"Lampito",
"Metaphire",
"Perionyx",
"Pithemera",
"Polypheretima",
"Spenceriella"
)] <- "Megascolecidae"

spp$Family2[spp$Genus2 %in% c(
  "Fimoscolex",
  "Glossodrilus",
  "Glossoscolex",
  "Holoscolex",
  "Righiodrilus"

)] <- "Glossoscolecidae" 

spp$Family2[spp$Genus2 %in% c(
  "Arraia",
  "Belladrilus",
  "Brasilisia",
  "Dorgiodrilus",
  "Eukerria",
  "Gordiodrilus",
  "Ilyogenia",
  "Nematogenia",
  "Ocnerodrilus",
  "Phoenicodrilus"
  
)] <- "Ocnerodrilidae"  


spp$Family2[spp$Genus2 %in% c(
  "Criodrilus"

)] <- "Criodrilidae"



spp$Family2[spp$Genus2 %in% c(
  "Drawida"
  
)] <- "Moniligastridae"


spp$Family2[spp$Genus2 %in% c(
  "Eminoscolex",
  "Ephyriodrilus",
  "Eudrilus",
  "Hyperiodrilus",
  "Lavellea",
  "Legonodrilus",
  "Malodrilus",
  "Polytoreutus",
  "Rosadrilus",
  "Stuhlmannia"
  
)] <- "Eudrilidae"

spp$Family2[spp$Genus2 %in% c(
  "Hormogaster"
  
)] <- "Hormogastridae"



spp$Family2[spp$Genus2 %in% c(
  "Andiorrhinus",
  "Martiodrilus",
  "Onychochaeta",
  "Periscolex",
  "Pontoscolex",
  "Rhinodrilus",
  "Urobenus"
  
)] <- "Rhinodrilidae"
## 


spp$Genus <- spp$Genus2
spp$Genus2 <- NULL

spp$Family <- spp$Family2
spp$Family2 <- NULL


spp <- spp[,c("file","Study_ID","Site_Name", "OriginalSpeciesBinomial",
              "SpeciesBinomial", "MorphospeciesID","Genus","Family",
              "Ecological_group","LifeStage","Native.Nonnative","Abundance",
              "Abundance_Unit","WetBiomass","WetBiomassUnits")]

## SAVE SPECIES FILE --------------------------------------

write.csv(spp, file = file.path(data_out, paste0("SppOccData_sWorm_", Sys.Date(), ".csv")), row.names = FALSE)

