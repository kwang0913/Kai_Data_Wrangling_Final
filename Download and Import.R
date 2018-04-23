library(tidyverse)
library(furrr) #To use parallel computing version of purrr

# Main directory for downloading data
baseURL <- "https://s3.amazonaws.com/tripdata/"

# Specific what date to download
monthsToDownload <- 1:12
yearMonth <- 2013:2017 %>% map(rep, 12) %>% 
  map(paste0, ifelse(monthsToDownload < 10, yes = paste0(0,monthsToDownload), no = monthsToDownload)) %>% 
  unlist() %>% .[-c(1:6)]

# Compound filenames
fileNames <- paste0(yearMonth,
                    ifelse(yearMonth < 201700, 
                           yes = "-citibike-tripdata.zip", 
                           no = "-citibike-tripdata.csv.zip"
                    )
) 

#Write function to download, unzip, and remove zip files  
saveTripData <- function(fileName, baseURL){
  fileURL <- paste0(baseURL, fileName)
  download.file(url = fileURL, destfile = fileName)
  unzip(zipfile = fileName)
  file.remove(fileName)
}

# To use parallel computing version of purrr
plan(multiprocess)

# Map function to all files
future_map2(fileNames, baseURL, saveTripData)
# if your computer doesn't support such high volumn computation
#map2(fileNames, baseURL, saveTripData) 

#Import csv files
temp <- list.files(pattern="*.csv")
tempnames <- temp %>% str_sub(1,7) %>% make.names() %>% str_replace_all("X|[.]","")
temp %>% setNames(paste0("citibike", tempnames)) %>% future_map(read.csv) %>% list2env(envir = .GlobalEnv)
# if your computer doesn't support such high volumn computation
#temp %>% setNames(paste0("citibike", tempnames)) %>% map(read.csv) %>% list2env(envir = .GlobalEnv)