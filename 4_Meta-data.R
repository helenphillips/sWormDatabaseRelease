## To get the numbers for the manuscript
## i.e. number of sites

## setwd----------------------------------

if(Sys.info()["nodename"] == "IDIVNB179"){
  setwd("C:\\Users\\hp39wasi\\WORK\\sWormDatabaseRelease\\")
}

## FUNCTIONS AND PACKAGES --------------

source("Functions/loadMostRecent.R")

## VARS ---------------------------------


data_in <-"3_Data"

if(!dir.exists("4_Data")){
  dir.create("4_Data")
}
data_out <- "4_Data"


## LOAD ----------------------------------
loadinSite <- loadMostRecent_2("SiteData_sWorm", data_in)
sites <- read.csv(file.path(data_in, loadinSite))

loadinSpecies <- loadMostRecent_2("SppData_sWorm", data_in)
spp <- read.csv(file.path(data_in, loadinSpecies))

loadinBib <- loadMostRecent_2("MetaData_sWorm", data_in)
bib <- read.csv(file.path(data_in, loadinBib))
## SITES ----------------------------------

nrow(sites) # 8148 sites
length(unique(sites$Study_Name)) # 277 studies
length(unique(sites$file))

unique(sites$Country)

levels(sites$Country)[which(levels(sites$Country) == "MEXICO")] <- "Mexico"
levels(sites$Country)[which(levels(sites$Country) == "Pueto Rico")] <- "Puerto Rico"
levels(sites$Country)[which(levels(sites$Country) == "UK")] <- "United Kingdom"
levels(sites$Country)[which(levels(sites$Country) == "Wales")] <- "United Kingdom"
levels(sites$Country)[which(levels(sites$Country) == "USA")] <- "United States"

sites <- droplevels(sites)

length(unique(sites$Country)) - 1 ## Minus one for "border"



min(sites$Sample_StartDate_Year, na.rm = TRUE)
max(sites$Sample_StartDate_Year, na.rm = TRUE)

## Number with environmental information

temp <- sites
temp <- temp[temp$HabitatCover != "Unknown",]
temp <- temp[temp$LandUse != "Unknown",]
soilProps <- c("PH","CEC","Base_Saturation_percent","Organic_Carbon__percent",
  "Soil_Organic_Matter__percent","C.N_ratio","Sand__percent","Silt__percent",
  "Clay__percent","USDA_SoilTexture","Soil_Moisture_percent")

temp <- temp[rowSums(is.na(temp[, names(temp) %in% soilProps])) != length(soilProps), ]

nrow(temp) / nrow(sites)

## SPECIES --------------------------------
length(unique(spp$SpeciesBinomial))




## published or not? ---------------------------------
one <- bib[grep("unpublished", bib$Article_Title, ignore.case = TRUE),]
two <- bib[grep("thesis", bib$Article_Title, ignore.case = TRUE),]
three <- bib[is.na(bib$Article_Title),]
unique(bib$Article_Year) ## there's no unpublished in the article year

t <- rbind(one, two, three)

length(unique(t$file)) # 17

length(unique(sites$file)) - length(unique(t$file)) # 186
