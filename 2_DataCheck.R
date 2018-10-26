
########################################################
# 1. Set Working Directory
########################################################

if(Sys.info()["nodename"] == "IDIVNB193"){
  setwd("C:\\Users\\hp39wasi\\sWormDatabaseRelease")
}

source(file.path("Functions", "FormatData.R"))
########################################################
# 2. Create folder if it doesn't exist to save data into
########################################################

if(!dir.exists("2_Data")){
  dir.create("2_Data")
}
data_out <- "2_Data"

#################################################
# 3. Loading in variables
#################################################

data_in <-"0_Data"
files <- list.files(file.path(data_in))
files <- files[grep("sites_", files)]


file_dates <- sapply(strsplit(files, "_"), "[", 2) ## Split the string by date, which produces a list, then take second element of each list i.e. the date
file_dates <- sapply(strsplit(file_dates, "\\."), "[", 1) ## Split the string by date, which produces a list, then take first element of each list i.e. the date

file_dates <- as.Date(file_dates)
date <- max(file_dates, na.rm = TRUE)
loadin <- files[grep(date, files)]
loadinsites <- loadin[grep("sites_", loadin)]


files <- list.files(file.path(data_in))
files <- files[grep("Metadata_", files)]
file_dates <- sapply(strsplit(files, "_"), "[", 2) ## Split the string by date, which produces a list, then take second element of each list i.e. the date
file_dates <- sapply(strsplit(file_dates, "\\."), "[", 1) ## Split the string by date, which produces a list, then take first element of each list i.e. the date

file_dates <- as.Date(file_dates)
date <- max(file_dates, na.rm = TRUE)
loadin <- files[grep(date, files)]
loadinbib <- loadin[grep("Metadata_", loadin)]


#################################################
# 4. Load in data
#################################################

sites <- read.csv(file.path(data_in, loadinsites))
bib <- read.csv(file.path(data_in, loadinbib))

#################################################
# 5. Rename factor levels
#################################################
sites$Management_System <- as.character(sites$Management_System)
sites$Management_System[which(is.na(sites$Management_System))] <- "None"
sites$Management_System <- as.factor(sites$Management_System)

sites$LandUse <- as.character(sites$LandUse)
sites$LandUse[which(is.na(sites$LandUse))] <- "Unknown"
sites$LandUse <- as.factor(sites$LandUse)

sites$HabitatCover <- as.character(sites$HabitatCover)
sites$HabitatCover[which(is.na(sites$HabitatCover))] <- "Unknown"
sites$HabitatCover <- as.factor(sites$HabitatCover)


## The ESA habitat cover variable allowed additional levels
## that we weren't using originally
## This section of code adds them in, by using information
## in other fields

#################################################
# 6 Create a new ESA variable
#################################################
sites$ESA <- sites$HabitatCover

sites$ESA <- as.character(sites$ESA)

prod_herb <- which(sites$LandUse == "Production - Arable" | sites$Management_System == "Annual crop")
sites$ESA[prod_herb] <- "Production - Herbaceous"

prod_planta <- which(sites$LandUse == "Production - Crop plantations" | sites$LandUse == "Production - Wood plantation" | sites$Management_System == "Tree plantations")
sites$ESA[prod_planta] <- "Production - Plantation"

integratessys <- which(sites$Management_System == "Integrated systems")
sites$ESA[integratessys] <- "Cropland/Other vegetation mosaic"

pastures <- which(sites$Management_System == "Pastures (grazed lands)")
sites$ESA[pastures] <- "Herbaceous"

# unique(sites$ESA[sites$LandUse == "Pasture"])

table(sites$ESA)



#### There are some empty cells
## And at the moment, can't do anything with them

sites$ESA[is.na(sites$ESA)] <- "Unknown"


#################################################
# 7 Set reference levels
#################################################

sites <- SiteLevels(sites) 

#################################################
# 8. Check that all biomass and abundance values have units
#################################################

any(!(is.na(sites$Site_WetBiomass)) && is.na(sites$Site_WetBiomassUnits)) ## If true, there's no units for samples

# nrow(sites[!(is.na(sites$Site_WetBiomass)) && is.na(sites$Site_WetBiomassUnits),])

any(!(is.na(sites$Site_Abundance)) && is.na(sites$Site_AbundanceUnits))



#################################################
#  Save file
#################################################

write.csv(sites, file = file.path(data_out, paste("sites_", Sys.Date(), ".csv", sep = "")), row.names = FALSE)

