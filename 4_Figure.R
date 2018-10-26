
########################################################
# 1. Set Working Directory
########################################################

if(Sys.info()["nodename"] == "IDIVNB193"){
  setwd("C:\\Users\\hp39wasi\\sWormDatabaseRelease")
}



library(maps)
library(maptools)
########################################################
# 2. Create folder if it doesn't exist to save data into
########################################################

if(!dir.exists("Figures")){
  dir.create("Figures")
}
figures <- "Figures"

########################################################
# 3. Load data
########################################################
data_in <-"3_Data"
files <- list.files(file.path(data_in))
files <- files[grep("SiteswithFunctionalGroups_", files)]
file_dates <- sapply(strsplit(files, "_"), "[", 2) ## Split the string by date, which produces a list, then take second element of each list i.e. the date
file_dates <- sapply(strsplit(file_dates, "\\."), "[", 1) ## Split the string by date, which produces a list, then take first element of each list i.e. the date

file_dates <- as.Date(file_dates)
date <- max(file_dates, na.rm = TRUE)
loadin <- files[grep(date, files)]


sites <- read.csv(file.path(data_in, loadin))

#######################################################
# 4. Create map of all studies
########################################################

coord<-aggregate(cbind(sites$Longitude__Decimal_Degrees, sites$Latitude__decimal_degrees), list(sites$Study_Name), mean)

coord$X<-coord$Group.1
coord<-coord[2:4]
names(coord)<-c("Long", "Lat", "X")

# Some studies are missing coordinates
# Remove them
coord <- coord[!(is.na(coord$Lat)),]

dsSPDF<-SpatialPointsDataFrame(coord[,1:2], data.frame(coord[,1:3]))
proj4string(dsSPDF)<-CRS("+proj=longlat")


jpeg(filename = file.path(figures, "Map_allData.jpg"), quality = 100, res = 300, width = 2000, height = 1000)
mar=c(0,0,0,0)
map("world",border="gray87",fill=TRUE, col="gray87",mar=rep(0,4))
points(dsSPDF, col="black", bg="black", cex= 1, pch=19)
dev.off()

#######################################################
# 5. Bar chart of all methods
########################################################
labs <- names(table(sites$ExtractionMethod))
labs <- gsub("Formaldehyde", "Formalin", labs)

labs <- c("Hand sorting",                         
"Hand sorting + \n Liquid Extraction (e.g. Formalin)",
"Hand sorting + \n Liquid extraction (e.g. Mustard)",
"Liquid Extraction \n (e.g. Formalin)",      
"Liquid extraction \n (e.g. Mustard)",
"Octet Method \n (electric shock)",              
"Other",                          
"Other Multiple",                       
"Unknown")     

jpeg(filename = file.path(figures, "ExtractionMethods.jpg"), quality = 100, res = 300, width = 2000, height = 1500)
par(mar = c(13, 4, 1, 1))
b <- barplot(table(sites$ExtractionMethod), axisnames = FALSE)
axis(1, at = b, labels = labs, las = 2)
mtext("Number of sites", line =  2.5, side = 2)
dev.off()
