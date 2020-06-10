########################################################
# 1. Set Working Directory
########################################################

if(Sys.info()["nodename"] == "IDIVNB179"){
  setwd("C:/Users/hp39wasi/WORK/futureProjections_earthworms")
}


########################################################
# 2. Create folder if it doesn't exist to save data into
########################################################

if(!dir.exists("Data")){
  dir.create("Data")
}



if(!dir.exists(file.path("Data","0_2_Data"))){
  dir.create(file.path("Data", "0_2_Data"))
}
data_out <- file.path("Data", "0_2_Data")


data_in <- file.path("Data", "0_1_Data", "2020-06-03")

########################################################
# 3. Libraries
########################################################

source("Scripts/Functions/FormatData.R")
source("Scripts/Functions/CalculateSitelevelMetrics.R")

library(xlsx)




########################################################
# 5. Create dataframe for bibliographic and author info
########################################################

meta <- read.xlsx(file.path(data_in, "DatasetTemplateoriginal.xlsx"), sheetName = "MetaData", header = FALSE)
 
bib_names <- as.character(meta[,1])

bib <- data.frame(matrix(ncol = length(bib_names) + 1, nrow = 0)) ## plus one so I can add a file name
colnames(bib) <- c(bib_names, "file")



######################################################
# 5.5. Create template of site info for checking
#######################################################



sitetemplate <- read.xlsx(file.path(data_in, "DatasetTemplateoriginal.xlsx"), sheetName = "Site-LevelData")
speciestemplate <- read.xlsx(file.path(data_in, "DatasetTemplateoriginal.xlsx"), sheetName = "Species-LevelData")


sitetemplate[grep("NA", names(sitetemplate))] <- NULL
speciestemplate[grep("NA", names(speciestemplate))] <- NULL

########################################################
# 6. Get files from directory
########################################################

filelist <- dir(file.path(data_in))
# All files
# remove the template
filelist <- filelist[-which(filelist == "DatasetTemplateoriginal.xlsx")]

# And as a second check
all_files <- filelist[grep("^\\d*\\_", filelist, perl = TRUE)]

cat(paste("\nFound", length(all_files), "datasheets"))

options( warn = 2 )

#all_bib <- list(length = length(bib_names))
#names(all_bib) <- bib_names
all_sites <- list()
all_species <- list()

########################################################
# 7. Start processing data
########################################################


## create temporary dataframe

StudyLines <- data.frame(file = rep(NA, length = length(all_files)),
                         n_studies = rep(NA, length = length(all_files)),
                         total_sites = rep(NA, length = length(all_files)),
                         sites_per_study = rep(NA, length = length(all_files)),
                         n_studies_post = rep(NA, length = length(all_files)),
                         n_sites_post = rep(NA, length = length(all_files)))

count <- 0

for(file in all_files){
  count <- count + 1
  
  StudyLines$file[count] <- file
  
  print(file)
  
  meta <- read.xlsx(file.path(data_in, file), sheetName = "MetaData", header = FALSE)
  sites <- read.xlsx(file.path(data_in, file), sheetName = "Site-LevelData")
  species <- read.xlsx(file.path(data_in, file), sheetName = "Species-LevelData")
  
  ## should only be two columns
  meta <- meta[,c(1,2)]
  ## Add file info to metadata
  meta[,1] <- as.character(meta[,1])
  meta[,2] <- as.character(meta[,2])
  meta[nrow(meta) + 1,] <- c("file", file)

  ## Remove blank rows (added because of downloading as excel file)
  sites <- sites[!(is.na(sites$Site_Name)),]
  species <- species[!(is.na(species$Site_Name)),]
  
  
  
  ### Check that most recent version of template, else skip it
  if(!(all(names(bib) %in% meta[,1]))) {
    print(paste(file, ": failed on meta sheet"))
    next }
  if(!(all(names(sitetemplate) %in% names(sites)))){
    print(paste(file, ": failed on sites sheet"))
    next }
  if(!(all(names(speciestemplate) %in% names(species)))) {
    print(paste(file, ": failed on species sheet"))
    next }
  
  ## Sorting out bib dataframe
  meta <- meta[meta[,1] %in% names(bib),] ## To remove unnessecary rows
  meta <- meta[match(names(bib), meta[,1]),] ## To put it in the same order
  bib[count,] <- meta[,2]
  
  
  
  ## Adding article ID
  sites <- cbind(file, sites)
  if(nrow(species) > 0){species <- cbind(file, species)}
  
  ## Formatting
  sites <- formatSites(sites)
  if(nrow(species) > 0){species <- formatSpecies(species)}
  
  
  
  ## CHECKS
  ## all site names in 'sites' should be unique
  
  if(nrow(sites) != length(sites$Study_site)){
    print(paste(file,": some sites are not unique"))
    next
  }
  
  ## all study names in 'species' should be in 'sites'
  
  if( !(all(species$Study_ID %in% sites$Study_Name))){
    print(paste(file, ": some studys in 'species' are not in 'sites'"))
    next
  }
  
  
  ## all site names in 'species' should be in 'sites'
  
  if( !(all(species$Study_site %in% sites$Study_site))){
    print(paste(file, ": some sites in 'species' are not in 'sites'"))
    next
  }
  
  ## only one site in a study
  if(any(table(sites$Study_Name) < 2)){
    print(paste(file, ": a study has less than 2 sites"))
    next
  }
  
  
  ## three files have sites in species file that doesn't have corresponding info in sites file
#  [1] "000_DavalosUnpublished.xlsx"
 # [1] "113_Davalos2015.xlsx"
  # [1] "7601_GutierrezLopez2016.xlsx"

  # This is an issue from the underlying data. Only a couple of data points that will be missed. 
  # this way round is less of an issue than the other way round
  
  StudyLines$total_sites[count] <- nrow(sites)
  StudyLines$n_studies[count] <- length(unique(sites$Study_Name))
  StudyLines$sites_per_study[count] <- table(sites$Study_Name)[1]
  
  
  sites$NumberofSpecies <- NA
  sites$Individuals_fromspecies <- NA
  sites$Individuals_fromspeciesUnits <- NA
  sites$Biomass_fromspecies <- NA
  sites$Biomass_fromspeciesUnits <- NA
  
  
  #### Calculate site level species richness from species list & Check the values there
  if(nrow(species) > 0){
    
    
    ## Is there are species richness measures
    
    potentialSpR <- c("SpeciesBinomial", "MorphospeciesID", "Genus", "Family")
    
    
    len_study <- length(unique(species$Study_ID))
    uni_study <- unique(species$Study_ID)
    
    try(if(any(!(uni_study %in% unique(sites$Study_Name)))) stop ("Differing study names"))
    
    for(s in 1:len_study){
      
      tempSpp <- droplevels(species[species$Study_ID == uni_study[s],])
      
      if(any(!(is.na(tempSpp[,potentialSpR])))){ # If any have some values in
        
        
        
        juvs <- which(tempSpp$LifeStage == "Juvenile")
        notSpecies <- which(is.na(tempSpp$SpeciesBinomial) & is.na(tempSpp$MorphospeciesID))
        
        if(length(c(juvs, notSpecies)) > 0){
          spR <- as.data.frame(table(tempSpp$Study_site[-c(juvs, notSpecies)]))
        } else {spR <- as.data.frame(table(tempSpp$Study_site))}
        rm(juvs)
        rm(notSpecies)
        names(spR)[2] <- "NumberofSpecies"

        
        ## Not pretty, but the simplest way
        
        for(r in 1:nrow(spR)){
          sites$NumberofSpecies[which(spR$Var1[r] == sites$Study_site)] <- spR$NumberofSpecies[r]
        }
        
        # NAs in studies where there are other numbers, are actually zeros
        
        sites$NumberofSpecies[which(is.na(sites$NumberofSpecies) & sites$Study_Name == as.character(uni_study[s]))] <- 0
        
        rm(spR)
        
      } 
      
      
      ##
      ## Calculate site level abundance
      if(any(!(is.na(tempSpp$Abundance)))){ # Any abundance values in this study 
        ta <- as.data.frame(tapply(tempSpp$Abundance, tempSpp$Study_site, sum, na.rm = TRUE))
        names(ta) <- "Individuals_fromspecies"
        ta$Individuals_fromspeciesUnits <- tempSpp$Abundance_Unit[1]
        ta$SS <- rownames(ta)
        ta$Individuals_fromspecies <- unname(ta$Individuals_fromspecies)
        
        ## Not pretty, but the simplest way
        
        for(r in 1:nrow(ta)){
          sites$Individuals_fromspecies[which(ta$SS[r] == sites$Study_site)] <- ta$Individuals_fromspecies[r]
          sites$Individuals_fromspeciesUnits[which(ta$SS[r] == sites$Study_site)] <- as.character(ta$Individuals_fromspeciesUnits[r])
          
        }
        
        # NAs in studies where there are other numbers, are actually zeros
        sites$Individuals_fromspecies[which(is.na(sites$Individuals_fromspecies))] <- 0
        sites$Individuals_fromspeciesUnits[which(is.na(sites$Individuals_fromspeciesUnits))] <- as.character(ta$Individuals_fromspeciesUnits[1])
        
        rm(ta)
      }
      
      ## 
      ## Calculate site level biomass
      if(any(!(is.na(tempSpp$WetBiomass)))){ # Any biomass values in this study 
        
        bm <- as.data.frame(tapply(tempSpp$WetBiomass, tempSpp$Study_site, sum))
        names(bm) <- "Biomass_fromspecies"
        bm$Biomass_fromspeciesUnits <- tempSpp$WetBiomassUnits[1]
        bm$SS <- rownames(bm)
        bm$Biomass_fromspecies <- unname(bm$Biomass_fromspecies)

        
        for(r in 1:nrow(bm)){
          sites$Biomass_fromspecies[which(bm$SS[r] == sites$Study_site)] <- bm$Biomass_fromspecies[r]
          sites$Biomass_fromspeciesUnits[which(bm$SS[r] == sites$Study_site)] <- as.character(bm$Biomass_fromspeciesUnits[r])
          
        }
        
        # NAs in studies where there are other numbers, are actually zeros
        sites$Biomass_fromspecies[which(is.na(sites$Biomass_fromspecies))] <- 0
        sites$Biomass_fromspeciesUnits[which(is.na(sites$Biomass_fromspeciesUnits))] <- as.character(bm$Biomass_fromspeciesUnits[1])
        
        
        rm(bm)
      }
    }
    
  }  


  StudyLines$n_studies_post[count] <- length(unique(sites$Study_Name))
  StudyLines$n_sites_post[count] <- nrow(sites)
  
  ## Some checks to make sure that we haven't lost sites
  StudyLines$diff_sites[count] <- StudyLines$total_sites[count] - StudyLines$n_sites_post[count]
  try(if(StudyLines$diff_sites[count] > 0) stop("Number of sites has changed."))
  ## Or studies
  StudyLines$diff_studies[count] <- StudyLines$n_studies[count] - StudyLines$n_studies_post[count]
  try(if(StudyLines$diff_studies[count] > 0) stop("Number of sites has changed."))
  
    
## Now to make a species level dataframe with all the variables in
  if(nrow(species) > 0){

    # Some files only have species data for some studies
    
    actual_studies <- unique(species$Study_ID)
    sites_temp <- sites[which(sites$Study_Name %in% actual_studies) ,]

    
    
    site_species <- merge(sites_temp, species, by.x = "Study_site", by.y = "Study_site", all.x = TRUE) # Still want sites with zero
    
  
  
    # Make sure blanks are filled in where nessecary
    site_species$Study_ID <- site_species$Study_Name # Maybe multiple studies
    
    # If any of the two diversity metrics have some value, the blanks need zero

    for(s in 1:length(unique(site_species$Study_Name))){
      
      # Abundance
      if(any(!(is.na(site_species$Abundance[site_species$Study_Name == unique(site_species$Study_Name)[s]])))){
        # If any are not NA in the abundance col for the study, then
        
        the_nas <- which(is.na(site_species$Abundance) & site_species$Study_Name == unique(site_species$Study_Name)[s] )
        
        site_species$Abundance[the_nas] <- 0
        
        # site_species$Abundance_Unit <-  as.character(site_species$Abundance_Unit)
        units <- unique(site_species$Abundance_Unit[site_species$Study_Name == unique(site_species$Study_Name)[s]])
        units <- units[!(is.na(units))]
        site_species$Abundance_Unit[the_nas] <- units
              }
      
      # Biomass
      if(any(!(is.na(site_species$WetBiomass[site_species$Study_Name == unique(site_species$Study_Name)[s]])))){
        # If any are not NA in the biomass col for the study, then
        
        the_nas <- which(is.na(site_species$WetBiomass) & site_species$Study_Name == unique(site_species$Study_Name)[s] )
        
        site_species$WetBiomass[the_nas] <- 0
        
        units <- unique(site_species$WetBiomassUnits[site_species$Study_Name == unique(site_species$Study_Name)[s]])
        units <- units[!(is.na(units))]
        site_species$WetBiomassUnits[the_nas] <- units
      }
      
      
      
    }
  
  
    all_species[[count]] <- site_species
  }


  all_sites[[count]] <- sites

}

########################################################
# 8. Create two complete dataframes
########################################################


sites <- do.call("rbind", all_sites)
species <- do.call("rbind", all_species)

species$file.y <- NULL
species$Site_Name.y <- NULL

names(species)[which(names(species) == "file.x")] <- "file"
names(species)[which(names(species) == "Site_Name.x")] <- "Site_Name"
########################################################
# 9. Amalgamte site level values of species richness, biomass and abundance
########################################################

sitelevel_spR <- which(is.na(sites$SpeciesRichness) & !(is.na(sites$NumberofSpecies)))
sites$SpeciesRichness[sitelevel_spR] <-sites$NumberofSpecies[sitelevel_spR]
sites$SpeciesRichnessUnit[sitelevel_spR] <- "Number of species"
rm(sitelevel_spR)

sites$Site_WetBiomassUnits <- as.character(sites$Site_WetBiomassUnits)

str(which(!(is.na(sites$Biomass_fromspecies))))

sitelevel_biom <- which(is.na(sites$Site_WetBiomass) & !(is.na(sites$Biomass_fromspecies)))
if(length(sitelevel_biom ) > 0) {
  sites$Site_WetBiomass[sitelevel_biom] <-sites$Biomass_fromspecies[sitelevel_biom]
  sites$Site_WetBiomassUnits[sitelevel_biom] <- as.character(sites$Biomass_fromspeciesUnits[sitelevel_biom])
}
rm(sitelevel_biom)

sites$Site_AbundanceUnits <- as.character(sites$Site_AbundanceUnits)


sitelevel_abund <- which(is.na(sites$Site_Abundance) & !(is.na(sites$Individuals_fromspecies)))
if(length(sitelevel_abund ) > 0) {
  sites$Site_Abundance[sitelevel_abund] <-sites$Individuals_fromspecies[sitelevel_abund]
  sites$Site_AbundanceUnits[sitelevel_abund] <- as.character(sites$Individuals_fromspeciesUnits[sitelevel_abund])
}
rm(sitelevel_abund)


colsToRemove <- c("NumberofSpecies", "Individuals_fromspecies", "Individuals_fromspeciesUnits", "Biomass_fromspecies", "Biomass_fromspeciesUnits")
sites[,names(sites) %in% colsToRemove] <- NULL


## Another check

if(length(unique(sites$file)) != length(unique(all_files))){stop ("Files are missing!!")}

########################################################
# 10. Save the data
########################################################
sites$ID <- 1:nrow(sites)
sites$ID <- paste(sites$file, sites$Study_Name, sites$ID, sep = "_")

write.csv(sites, file = file.path(data_out, paste("sites_", Sys.Date(), ".csv", sep = "")), row.names = FALSE)
write.csv(species, file = file.path(data_out, paste("species_", Sys.Date(), ".csv", sep = "")), row.names = FALSE)
write.csv(bib, file = file.path(data_out, paste("Metadata_", Sys.Date(), ".csv", sep = "")), row.names = FALSE)

write.csv(data.frame(unique(sites$file)), file = file.path(data_out, paste("listofFiles_", Sys.Date(), ".csv", sep = "")), row.names = FALSE)

options( warn = 0 )
