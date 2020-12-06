

## WORKING DIRECTORY ----------------------------


if(Sys.info()["nodename"] == "IDIVNB193"){
  setwd("C:\\restore2\\hp39wasi\\sWormDatabaseRelease")
}


if(Sys.info()["nodename"] == "IDIVNB179"){
  setwd("C:\\Users\\hp39wasi\\WORK\\sWormDatabaseRelease\\")
}

if(Sys.info()["nodename"] == "LAPTOP-I0JSR1DL"){
  setwd("~/WORK/sWormDatabaseRelease")
}




## PACKAGES AND VARS ---------------------------------------

library(maps)
library(maptools)
library(dplyr)
library( plotrix )

if(!dir.exists("Figures")){
  dir.create("Figures")
}
figures <- "Figures"

data_in <-"3_Data"

source('Functions/loadMostRecent.R')

## LOAD DATA -----------------------------------------

sites <- loadMostRecent_2("SiteData_", data_in)
sites <- read.csv(file.path(data_in, sites))

#######################################################
# 4. Create map of all studies
########################################################

sitecoords <- sites[!(is.na(sites$Latitude_decimal_degrees)),]
coord<-aggregate(cbind(sitecoords$Longitude_decimal_degrees, sitecoords$Latitude_decimal_degrees), list(sitecoords$Study_Name), mean)


# coord<-coord[2:4]
 
all(coord$Group.1 == names(table(sites$Study_Name)))
coord <- cbind(coord, table(sites$Study_Name))
 
coord$Var1 <- NULL
names(coord)<-c("X", "Long", "Lat",  "nSites")


coord$size <- ((coord$nSites-min(coord$nSites))/(max(coord$nSites)-min(coord$nSites)) * 2) + 0.5


dsSPDF<-SpatialPointsDataFrame(coord[,2:3], data.frame(coord[,1:5]))
proj4string(dsSPDF)<-CRS("+proj=longlat")

transpBlack <- rgb(0, 0, 0, alpha = 0.4, names = NULL, maxColorValue = 1)

# jpeg(filename = file.path(figures, "Map_allData.jpg"), quality = 100, res = 300, width = 2000, height = 1000)

pdf(file = file.path(figures, "Map_allData.pdf"), 
   width = 10, height = 5)



mar=c(0,0,0,0)
map("world",border="gray87",fill=TRUE, col="gray87",mar=rep(0,4))
points(dsSPDF, col=transpBlack, bg = transpBlack, cex= coord$size, pch=19)


sizes <- c(2, 100, 200, 300, 400, 500)
cexsizes <- ((sizes-min(coord$nSites))/(max(coord$nSites)-min(coord$nSites)) * 2) + 0.5

legend(x = -170, y = 2, legend = sizes, pch = 19, pt.cex =cexsizes, bty="n", cex = 0.7, 
       y.intersp = c(1, 1, 1, 1.05, 1.1, 1.18),
       x.intersp = c(1.19),
       title = "Number Of Sites")
dev.off()



#######################################################
# 5. Bar chart of all methods
########################################################
labs <- names(table(sites$ExtractionMethod))

labs <- c(
  "Chemical extraction \n (Formalin)",      
  "Chemical extraction \n (Mustard)",
  "Hand sorting",                         
"Hand sorting + \n Chemical extraction (Formalin)",
"Hand sorting + \n Chemical extraction (Mustard)",
"Octet method \n (electric shock)",              
"Other",                          
"Other Multiple",                       
"Unknown")     

# jpeg(filename = file.path(figures, "ExtractionMethods.jpg"), quality = 100, res = 300, width = 2000, height = 1500)
pdf(file = file.path(figures, "ExtractionMethods.pdf"), 
    width = 7, height = 6)

par(mar = c(13, 4, 1, 1))
b <- barplot(table(sites$ExtractionMethod), axisnames = FALSE)
axis(1, at = b, labels = labs, las = 2)
mtext("Number of sites", line =  2.5, side = 2)
dev.off()


#######################################################
# 6. Bar chart of all land uses/habitat covers
########################################################
# jpeg(filename = file.path(figures, "LUandHCFreq.jpg"), quality = 100, res = 300, width = 2200, height = 4000)
pdf(file = file.path(figures, "LUandHCFreq.pdf"), 
   width = 7, height = 14)

par(mfrow = c(2, 1))
sites$LandUse <- factor(sites$LandUse, levels = c("Primary vegetation", "Secondary vegetation", 
"Pasture",
"Production - Arable","Production - Crop plantations",
"Production - Wood plantation",
"Urban", "Unknown"))

LULabs <- c("Primary vegetation", "Secondary vegetation", 
            "Pasture",
            "Production System\n(Arable)","Production System\n(Crop plantations)",
            "Production System\n(Wood plantation)",
            "Urban", "Unknown")

par(mar = c(9, 4, 1, 4))
b <- barplot(table(sites$LandUse), axisnames = FALSE, xlim = c(0, 10),  xaxs = "i")
axis(1, at = b, labels = LULabs, las = 2)
# mtext("(a)", side = 3, line = 0, at = 0, adj = 0.5)

mtext("Number of sites", line =  2.5, side = 2)

### Number of studies

nStudies <- sites %>% 
  group_by(LandUse) %>%
  summarise(no_rows = length(unique(Study_Name)))

nStudies <- as.data.frame(nStudies)
nStudies <- nStudies[!(is.na(nStudies$LandUse)), ] 

par(new=TRUE)
plot(b,nStudies[,2],xaxs = "i", xlim=c(0,10),pch = 19,col="red",axes=FALSE,ylim=c(0,100),ann=FALSE)
axis(4,at=seq(0,100,10), las = 2)
mtext("Number of studies (red dots)", line =  2.5, side = 4)


sites$HabitatCover[which(sites$HabitatCover == "Wetland")] <- "Wetland/Herbaceous"

sites$HabitatCover <- factor(sites$HabitatCover, levels = c(
  "Broadleaf deciduous forest", "Broadleaf evergreen forest", "Needleleaf deciduous forest",
  "Needleleaf evergreen forest", "Mixed forest", "Tree open", "Herbaceous with spare tree/shrub",
  "Shrub", "Herbaceous", "Sparse vegetation", 
  "Cropland/Other vegetation mosaic",  "Urban", "Bare area (consolidated",
  "Bare area (unconsolidated", "Paddy field", "Wetland/Herbaceous", "Water bodies", "Unknown"))

HCLabs <- c(
  "Broadleaf deciduous forest", "Broadleaf evergreen forest", "Needleleaf deciduous forest",
  "Needleleaf evergreen forest", "Mixed forest", "Tree open", "Herbaceous \nwith spare tree/shrub",
  "Shrub", "Herbaceous", "Sparse vegetation", 
  "Cropland/Other \nvegetation mosaic",  "Urban",  "Bare area (consolidated)",
  "Bare area (unconsolidated)",
  "Paddy field", "Wetland/Herbaceous", "Water bodies", "Unknown")




sites$HabitatCover <- droplevels(sites$HabitatCover)
par(mar = c(12, 4, 1, 4))
b <- barplot(table(sites$HabitatCover), axisnames = FALSE, xaxs = "i", xlim=c(0, 22))
axis(1, at = b, labels = HCLabs, las = 2)
# mtext("(b)", side = 3, line = 0, at = 0, adj = 0.5)

mtext("Number of sites", line =  2.5, side = 2)


### Number of studies

nStudies <- sites %>% 
  group_by(HabitatCover) %>%
  summarise(no_rows = length(unique(Study_Name)))

nStudies <- as.data.frame(nStudies)
nStudies <- nStudies[!(is.na(nStudies$HabitatCover)), ] 

par(new=TRUE)
plot(b,nStudies[,2],xaxs = "i",pch = 19,col="red",axes=FALSE,ylim=c(0,125),ann=FALSE, xlim=c(0, 22))
axis(4,at=seq(0,100,10), las = 2)
mtext("Number of studies (red dots)", line =  2.5, side = 4)



dev.off()

#######################################################
# 7. Venn Diagram
########################################################

## Studies
Summary.df <- sites %>% # Start by defining the original dataframe, AND THEN...
  group_by(Study_Name) %>% # Define the grouping variable, AND THEN...
  summarise( # Now you define your summary variables with a name and a function...
    N_sites = n(),  ## The function n() in dlpyr gives you the number of observations
    richness = sum(SpeciesRichness, na.rm = TRUE), # Because some sites were missing observations, others not, in 4 studies
    biomass = sum(Site_WetBiomass, na.rm = TRUE),
    abundance = sum(Site_Abundance, na.rm = TRUE)

  )

head(Summary.df)
summary.df <- as.data.frame(Summary.df)

summary.df$richness <- ifelse(summary.df$richness == 0, NA, summary.df$richness)
summary.df$biomass <- ifelse(summary.df$biomass == 0, NA, summary.df$biomass)
summary.df$abundance <- ifelse(summary.df$abundance == 0, NA, summary.df$abundance)
# Because if all sites had no observations its a 0, not NA

summary.df$RandB <- summary.df$richness + summary.df$biomass
summary.df$RandA <- summary.df$richness + summary.df$abundance
summary.df$AandB <- summary.df$abundance + summary.df$biomass
summary.df$all3 <- summary.df$abundance + summary.df$biomass + summary.df$richness

##### Where studies have all 3
all3 <- summary.df$all3
all3 <- all3[complete.cases(all3)]
all3 <- length(all3)

summaryonly2 <- droplevels(summary.df[which(is.na(summary.df$all3)),])
RandB <- nrow(summaryonly2[complete.cases(summaryonly2$RandB),])
RandA <- nrow(summaryonly2[complete.cases(summaryonly2$RandA),])
AandB <- nrow(summaryonly2[complete.cases(summaryonly2$AandB),])

## Remove studies that had two
summary1 <- summaryonly2[which(is.na(summaryonly2$RandB)),]
summary1 <- summary1[which(is.na(summary1$RandA)),]
summary1 <- summary1[which(is.na(summary1$AandB)),]

R <- nrow(summary1[complete.cases(summary1$richness),])
B <- nrow(summary1[complete.cases(summary1$biomass),])
A <- nrow(summary1[complete.cases(summary1$abundance),])


#----------
# Data 
#----------
# R <- 9
# B <- 37
# A <- 137
# 
# RandB <- 7
# RandA <- 78
# AandB <- 33
# 
# all3 <- 88
# 
# nrowSummary <-  253

#------------
# parameters
#------------

nrowSummary <- nrow(summary.df)
res.circle = 0.001 # if you need to increase the circle resolution
a <- 2/(nrowSummary*sqrt(pi)) # size of the circle radius
big1 <-  1.5 #size of the background shape around the circles
big2 <-  4 
#--------------
# plot
#--------------

# jpeg(filename = file.path(figures, "N_Metrics.jpg"), quality = 100, res = 500, width = 7, height = 7, units = "in")
# jpeg(filename = file.path(figures, "N_Metrics_both.jpg"), quality = 100, res = 500, width = 14, height = 7, units = "in")

pdf(file = file.path(figures, "N_Metrics_both.pdf"),
     width = 14, height = 7)

par(mfrow=c(1,2))
plot(0, xaxt = 'n', yaxt = 'n', bty = 'n', pch = '', ylab = '', xlab = '', ylim = c(1,8), xlim = c(1,10))

#-------------------------
# Backgrounds
#-------------------------

# Background shape colors
pol.col.B = rgb(0, 0, 1, alpha = 0.2)
pol.col.A = rgb(1, 1, 0, alpha = 0.3)
pol.col.R <- rgb(1, 0, 0, alpha = 0.2)

# plot shape 

## B background
g = all3
angle1 = seq(0.6*pi, 1.05*pi, by =res.circle)
x1 = cos(angle1)*g*a*ifelse(g>10,big1,big2) + 5
y1 = sin(angle1)*g*a*ifelse(g>10,1.4,4) + (5*(sqrt(6.5) + 2)) / 6.5

g=AandB
angle2 = seq(1.1*pi, 9.5*pi/6,by =res.circle)
x2 = cos(angle2)*g*a*ifelse(g>10,big1,big2) + 5
y2 = sin(angle2)*g*a*ifelse(g>10,big1,big2) + 2

g=B
angle3 = seq(3*pi/2, 0.5*pi/3 +2*pi, by =res.circle)
x3 = cos(angle3)*g*a*ifelse(g>10,big1,big2) + 8
y3 = sin(angle3)*g*a*ifelse(g>10,big1,big2) + 2

angle4 = seq(0, pi/2, by =res.circle)
g=RandB
x4 = cos(angle4)*g*a*ifelse(g>10,big1,big2) + 6.5
y4 = sin(angle4)*g*a*ifelse(g>10,big1,big2) + sqrt(6.75)+2

polygon(x = c(x1,x2,x3,x4), y = c(y1,y2,y3,y4), border=NA, col = pol.col.B)

## A background
g = all3
angle1 = seq(3.7*pi/2, 9*pi/4,by =res.circle)
x1 = cos(angle1)*g*a*ifelse(g>10,big1,big2) + 5
y1 = sin(angle1)*g*a*ifelse(g>10,big1,big2) + (5*(sqrt(6.5) + 2)) / 6.5

g=RandA
angle2 = seq(0.2*pi, 0.9*pi,by =res.circle)
x2 = cos(angle2)*g*a*ifelse(g>10,big1,big2) + 3.5
y2 = sin(angle2)*g*a*ifelse(g>10,big1,big2) + sqrt(6.75) + 2

g=A
angle3 = seq(0.8*pi, 1.5*pi, by =res.circle)
x3 = cos(angle3)*g*a*ifelse(g>10,big1,big2) + 2
y3 = sin(angle3)*g*a*ifelse(g>10,big1,big2) + 2

g=AandB
angle4 = seq(1.5*pi, 2*pi, by =res.circle)
x4 = cos(angle4)*g*a*ifelse(g>10,big1,big2) + 5
y4 = sin(angle4)*g*a*ifelse(g>10,big1,big2) + 2

polygon(x = c(x1,x2,x3,x4), y = c(y1,y2,y3,y4), border=NA, col = pol.col.A)

## R background
g=all3
angle1 = seq(1.35*pi,1.8*pi,by =res.circle)
x1 = cos(angle1)*g*a*ifelse(g>10,big1,big2) + 5
y1 = sin(angle1)*g*a*ifelse(g>10,big1,big2) + (5*(sqrt(6.5) + 2)) / 6.5

g=RandB
angle2 = seq(1.65*pi,2.4*pi,by =res.circle)
x2 = cos(angle2)*g*a*ifelse(g>10,big1,big2) + 6.5
y2 = sin(angle2)*g*a*ifelse(g>10,big1,big2) + sqrt(6.75)+2

g=R
angle3 = seq(0.2*pi, 0.7*pi , by =res.circle)
x3 = cos(angle3)*g*a*ifelse(g>10,big1,big2) + 5
y3 = sin(angle3)*g*a*ifelse(g>10,big1,big2) + sqrt(27) + 2

g=RandA
angle4 = seq(0.8*pi, 1.23*pi, by =res.circle)
x4 = cos(angle4)*g*a*ifelse(g>10,big1,big2) + 3.5
y4 = sin(angle4)*g*a*ifelse(g>10,big1,big2) + sqrt(6.75)+2

polygon(x = c(x1,x2,x3,x4), y = c(y1,y2,y3,y4), border=NA, col = pol.col.R)

#---------------------
# circles
#---------------------

# Sequence of points
c.x = cos(seq(0,2*pi,res.circle))
c.y = sin(seq(0,2*pi,res.circle))

# plotting
## all3
polygon(x = c.x * a * all3 + 5, y = c.y * a * all3 + (5*(sqrt(6.5) + 2)) / 6.5, col = "black", border = "black")
## A and B
polygon(x= c.x * a * AandB + 5, y= c.y * a * AandB + 2,col = "#32CD32", border = "#32CD32")
## A and R
polygon(x= c.x * a * RandA + 3.5, y= c.y * a * RandA + sqrt(6.75) + 2, border = "orange", col = "orange")
## R and B
polygon(x= c.x * a * RandB + 6.5, y= c.y * a * RandB + sqrt(6.75) + 2, border = "purple" ,col = "purple") 
## A 
polygon(x= c.x * a * A + 2, y= c.y *a *A + 2, border = "yellow" ,col = "yellow")
## B 
polygon(x=c.x*a*B+8, y=c.y*a*B+2, border = "blue", col = "blue")
## R
polygon(x=c.x*a*R+5, y=c.y*a*R+sqrt(27) + 2, border = "red", col = "red")


### Text

textadj <- 0.3

text(x=5, y=(5*(sqrt(6.5) + 2)) / 6.5, labels = paste("n =", all3), col = "white")

text(x=5, y=2 + textadj, labels = paste("n =", AandB), col = "black") # A and B
text(x=3.5, y=sqrt(6.75) + 2 + textadj + 0.15, labels = paste("n =", RandA), col = "black") # A and R
text(x=6.5, y=sqrt(6.75) + 2 + textadj, labels = paste("n =", RandB), col = "black") # R and B

text(x=2, y=2 + textadj,labels = paste("n =", A), col = "black") # A
text(x=8, y=2 + textadj, labels = paste("n =", B), col = "black") # B
text(x=5, y=sqrt(27) + 2 + textadj, labels = paste("n =", R), col = "black") # R


text(x = 1.7, y = 1.4, labels = "Abundance", cex = 1.5)
text(x = 5, y = 8, labels = "Richness", cex = 1.5)
text(x = 8.7, y = 1.4, labels = "Biomass", cex = 1.5)

# mtext("(a)", side = 3, line = 0, at = 0, adj = 0.1, cex = 2)



# dev.off()



#######################################################
# 7. Venn Diagram
########################################################

## Sites
colsNeeded <- c("Site_Name", "SpeciesRichness", "Site_WetBiomass", "Site_Abundance")
summary.df <- sites[,which(names(sites) %in% colsNeeded)]
names(summary.df) <- c("Site_Name", "richness", "biomass", "abundance")

summary.df$RandB <- summary.df$richness + summary.df$biomass
summary.df$RandA <- summary.df$richness + summary.df$abundance
summary.df$AandB <- summary.df$abundance + summary.df$biomass
summary.df$all3 <- summary.df$abundance + summary.df$biomass + summary.df$richness

##### Where studies have all 3
all3 <- summary.df$all3
all3 <- all3[complete.cases(all3)]
all3 <- length(all3)

summaryonly2 <- summary.df[which(is.na(summary.df$all3)),]
RandB <- nrow(summaryonly2[complete.cases(summaryonly2$RandB),])
RandA <- nrow(summaryonly2[complete.cases(summaryonly2$RandA),])
AandB <- nrow(summaryonly2[complete.cases(summaryonly2$AandB),])

## Remove studies that had two
summary1 <- summaryonly2[which(is.na(summaryonly2$RandB)),]
summary1 <- summary1[which(is.na(summary1$RandA)),]
summary1 <- summary1[which(is.na(summary1$AandB)),]

R <- nrow(summary1[complete.cases(summary1$richness),])
B <- nrow(summary1[complete.cases(summary1$biomass),])
A <- nrow(summary1[complete.cases(summary1$abundance),])


#------------
# parameters
#------------
nrowSummary <- nrow(summary.df)
res.circle = 0.001 # if you need to increase the circle resolution
a <- 2/(nrowSummary*sqrt(pi)) # size of the circle radius
big1 <-  1.5 #size of the background shape around the circles
big2 <-  4 
#--------------
# plot
#--------------

# jpeg(filename = file.path(figures, "NSites_Metrics.jpg"), quality = 100, res = 500, width = 7, height = 7, units = "in")

plot(0, xaxt = 'n', yaxt = 'n', bty = 'n', pch = '', ylab = '', xlab = '', ylim = c(1,8), xlim = c(1,10))

#-------------------------
# Backgrounds
#-------------------------

# Background shape colors
pol.col.B = rgb(0, 0, 1, alpha = 0.2)
pol.col.A = rgb(1, 1, 0, alpha = 0.3)
pol.col.R <- rgb(1, 0, 0, alpha = 0.2)

# plot shape 

## B background
g = all3
angle1 = seq(0.6*pi, 1.05*pi, by =res.circle)
x1 = cos(angle1)*g*a*ifelse(g>10,big1,big2) + 5
y1 = sin(angle1)*g*a*ifelse(g>10,1.4,4) + (5*(sqrt(6.5) + 2)) / 6.5

g=AandB
angle2 = seq(1.1*pi, 9.5*pi/6,by =res.circle)
x2 = cos(angle2)*g*a*ifelse(g>10,big1,big2) + 5
y2 = sin(angle2)*g*a*ifelse(g>10,big1,big2) + 2

g=B
angle3 = seq(3*pi/2, 0.5*pi/3 +2*pi, by =res.circle)
x3 = cos(angle3)*g*a*ifelse(g>10,big1,big2) + 8
y3 = sin(angle3)*g*a*ifelse(g>10,big1,big2) + 2

angle4 = seq(0, pi/2, by =res.circle)
g=RandB
x4 = cos(angle4)*g*a*ifelse(g>10,big1,big2) + 6.5
y4 = sin(angle4)*g*a*ifelse(g>10,big1,big2) + sqrt(6.75)+2

polygon(x = c(x1,x2,x3,x4), y = c(y1,y2,y3,y4), border=NA, col = pol.col.B)

## A background
g = all3
angle1 = seq(3.7*pi/2, 9*pi/4,by =res.circle)
x1 = cos(angle1)*g*a*ifelse(g>10,big1,big2) + 5
y1 = sin(angle1)*g*a*ifelse(g>10,big1,big2) + (5*(sqrt(6.5) + 2)) / 6.5

g=RandA
angle2 = seq(0.2*pi, 0.9*pi,by =res.circle)
x2 = cos(angle2)*g*a*ifelse(g>10,big1,big2) + 3.5
y2 = sin(angle2)*g*a*ifelse(g>10,big1,big2) + sqrt(6.75) + 2

g=A
angle3 = seq(0.8*pi, 1.5*pi, by =res.circle)
x3 = cos(angle3)*g*a*ifelse(g>10,big1,big2) + 2
y3 = sin(angle3)*g*a*ifelse(g>10,big1,big2) + 2

g=AandB
angle4 = seq(1.5*pi, 2*pi, by =res.circle)
x4 = cos(angle4)*g*a*ifelse(g>10,big1,big2) + 5
y4 = sin(angle4)*g*a*ifelse(g>10,big1,big2) + 2

polygon(x = c(x1,x2,x3,x4), y = c(y1,y2,y3,y4), border=NA, col = pol.col.A)

## R background
g=all3
angle1 = seq(1.35*pi,1.8*pi,by =res.circle)
x1 = cos(angle1)*g*a*ifelse(g>10,big1,big2) + 5
y1 = sin(angle1)*g*a*ifelse(g>10,big1,big2) + (5*(sqrt(6.5) + 2)) / 6.5

g=RandB
angle2 = seq(1.65*pi,2.4*pi,by =res.circle)
x2 = cos(angle2)*g*a*ifelse(g>10,big1,big2) + 6.5
y2 = sin(angle2)*g*a*ifelse(g>10,big1,big2) + sqrt(6.75)+2

g=R
angle3 = seq(0.2*pi, 0.7*pi , by =res.circle)
x3 = cos(angle3)*g*a*ifelse(g>10,big1,big2) + 5
y3 = sin(angle3)*g*a*ifelse(g>10,big1,big2) + sqrt(27) + 2

g=RandA
angle4 = seq(0.8*pi, 1.23*pi, by =res.circle)
x4 = cos(angle4)*g*a*ifelse(g>10,big1,big2) + 3.5
y4 = sin(angle4)*g*a*ifelse(g>10,big1,big2) + sqrt(6.75)+2

polygon(x = c(x1,x2,x3,x4), y = c(y1,y2,y3,y4), border=NA, col = pol.col.R)

#---------------------
# circles
#---------------------

# Sequence of points
c.x = cos(seq(0,2*pi,res.circle))
c.y = sin(seq(0,2*pi,res.circle))

# plotting
## all3
polygon(x = c.x * a * all3 + 5, y = c.y * a * all3 + (5*(sqrt(6.5) + 2)) / 6.5, col = "black", border = "black")
## A and B
polygon(x= c.x * a * AandB + 5, y= c.y * a * AandB + 2,col = "#32CD32", border = "#32CD32")
## A and R
polygon(x= c.x * a * RandA + 3.5, y= c.y * a * RandA + sqrt(6.75) + 2, border = "orange", col = "orange")
## R and B
polygon(x= c.x * a * RandB + 6.5, y= c.y * a * RandB + sqrt(6.75) + 2, border = "purple" ,col = "purple") 
## A 
polygon(x= c.x * a * A + 2, y= c.y *a *A + 2, border = "yellow" ,col = "yellow")
## B 
polygon(x=c.x*a*B+8, y=c.y*a*B+2, border = "blue", col = "blue")
## R
polygon(x=c.x*a*R+5, y=c.y*a*R+sqrt(27) + 2, border = "red", col = "red")


### Text

textadj <- 0.3

text(x=5, y=(5*(sqrt(6.5) + 2)) / 6.5, labels = paste("n =", all3), col = "white")

text(x=5, y=2 + textadj, labels = paste("n =", AandB), col = "black") # A and B
text(x=3.5, y=sqrt(6.75) + 2 + textadj + 0.25, labels = paste("n =", RandA), col = "black") # A and R
text(x=6.5, y=sqrt(6.75) + 2 + textadj, labels = paste("n =", RandB), col = "black") # R and B

text(x=2, y=2 + textadj,labels = paste("n =", A), col = "black") # A
text(x=8, y=2 + textadj, labels = paste("n =", B), col = "black") # B
text(x=5, y=sqrt(27) + 2 + textadj, labels = paste("n =", R), col = "black") # R


text(x = 1.7, y = 1.4, labels = "Abundance", cex = 1.5)
text(x = 5, y = 8, labels = "Richness", cex = 1.5)
text(x = 8.7, y = 1.4, labels = "Biomass", cex = 1.5)


# mtext("(b)", side = 3, line = 0, at = 0, adj = 0.1, cex = 2)

dev.off()

