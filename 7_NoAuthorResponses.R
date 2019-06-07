setwd("C:/restore2/hp39wasi/sWormDatabaseRelease")


#############################################
## Load Data
############################################

# Those who never filled in teh form
noresponse <- read.csv(file.path("Authorship", "toResend_2019-06-04.csv"))
noresponse$fullname <- paste(noresponse$First.Name, noresponse$Last.Name)

# The original file - includes info on why they were beign offered authorship
datasource <- read.csv(file.path("Authorship", "ListofDataProviders_HPEdit.csv"))
datasource$fullname <- paste(datasource$firstname, datasource$surname)



all <- merge(noresponse, datasource, by = "fullname", all.x = TRUE)


#############################################
## Data from paper
############################################

# Remove, as not an issue if they didn't response

all <- all[all$source != "data from paper",]


#############################################
## Additional author
############################################

additional <- all[all$source == "additional author",]

## Manually check that the original data provider has given permission



#############################################
## Data Providers
############################################

provider <- all[all$source == "Data Provider",]



## print out the list
## And manually go through
all$Address <- NULL
all$Phone.Number <- NULL
all$Member.Rating <- NULL
all$fullname <- NULL
write.csv(all, file = file.path("Authorship", "NeverResponded_2019-06-04.csv"), row.names = FALSE)
