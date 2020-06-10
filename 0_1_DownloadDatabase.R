
## This script downloads all the data from drive
## and just saves it as a excel as is

########################################################
# 1. Set Working Directory
########################################################

if(Sys.info()["nodename"] == "IDIVNB179"){
  setwd("C:/Users/hp39wasi/WORK/sWormDatabaseRelease")
}


########################################################
# 2. Create folder if it doesn't exist to save data into
########################################################

if(!dir.exists("0_1_Data")){
  dir.create("0_1_Data")
}




if(!dir.exists(file.path("0_1_Data", Sys.Date()))){
  dir.create(file.path("0_1_Data", Sys.Date()))
}


data_out <- file.path("0_1_Data", Sys.Date())

########################################################
# 3. Libraries
########################################################


library("googledrive")


########################################################
# 4. Download files
########################################################

t <-drive_find(type = "spreadsheet")
# Find every spreadsheet


## Download the template
template_ID <- t$id[grep("DatasetTemplateoriginal", t$name)]
template_ID <- as_id(template_ID)

drive_download(
  file = template_ID,
  path = file.path(data_out, "DatasetTemplateoriginal"),
  type = NULL,
  overwrite = TRUE,
  verbose = TRUE
)
  
## Then download every file
all_files <- t$name[grep("^\\d*\\_", t$name, perl = TRUE)]

cat(paste("\nFound", length(all_files), "datasheets"))

for(file in 1:length(all_files)){
  
  file_name <- all_files[file]
  file_name <- paste("^",file_name,"$", sep="")
  
  file_ID <- t$id[grep(file_name, t$name)]
  file_ID <- as_id(file_ID)
  
  drive_download(
    file = file_ID,
    path = file.path(data_out, all_files[file]),
    type = NULL,
    overwrite = TRUE,
    verbose = TRUE
  )
  
  
}

print("Done!")
########################################################
# 5. Checking files
########################################################

## Does downloaded file number match list
length(dir(file.path(data_out))) == length(all_files) + 1



