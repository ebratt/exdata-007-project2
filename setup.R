## subject: Coursera exdata-007 project 2
## title: "setup.R"
## author: "Eric Bratt"
## date: "Thursday, October 9,2014"
## output: R script
################################################################################
## clear out the environment
rm(list=ls())

################################################################################
## function that checks to see if a package is installed and,if not,installs it
## portions of this code came from http://stackoverflow.com/questions/9341635/how-can-i-check-for-installed-r-packages-before-running-install-packages
load_package <- function(x) {
  if (x %in% rownames(installed.packages())) { print("package already installed...") }
  else { install.packages(x) }
}

################################################################################
# install necessary packages
load_package("lubridate") # for easy date-handling
load_package("Rcpp")      # dependency for lubridate
load_package("dplyr")     # for managing data

# load necessary libraries
library(lubridate)
library(dplyr)

################################################################################
## UTILITY FUNCTIONS
## function that concatenates strings (useful for directory paths)
concat <- function(x1,x2) {
  result <- paste(x1,x2,sep="")
  return(result)
}

################################################################################
## define some global variables
DATA_DIR            <- "./data"
DATA_FILE           <- concat(DATA_DIR,"/data.zip")
FILE_URL            <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
NEI_FILE            <- concat(DATA_DIR,"/summarySCC_PM25.rds")
SCC_FILE            <- concat(DATA_DIR,"/Source_Classification_Code.rds")

################################################################################
# ensure the local data directory exists
if (!file.exists(DATA_DIR)) { dir.create(DATA_DIR) }
# log the date the archive was downloaded
dateDownloaded <- now()
# download the archive file
if (!file.exists(NEI_FILE) | !file.exists(SCC_FILE)) {
  if (.Platform$OS.type == "unix") {
    download.file(FILE_URL,destfile = DATA_FILE,method="curl")
  } else {
    download.file(FILE_URL,destfile = DATA_FILE)
  }
  # unzip the archive file to the data directory
  unzip(DATA_FILE,exdir = DATA_DIR)
}

################################################################################
# read the file into data.                                                     #
################################################################################
NEI_file_size <- file.info(NEI_FILE)$size / 1048576 # file size in Mb's
SCC_file_size <- file.info(SCC_FILE)$size / 1048576 # file size in Mb's
NEI           <- readRDS(NEI_FILE)
SCC           <- readRDS(SCC_FILE)

################################################################################
# explore the data.                                                            #
################################################################################
# NEI Data
dim(NEI)
# [1] 6497651       6
head(NEI)
#     fips      SCC Pollutant Emissions  type year
# 4  09001 10100401  PM25-PRI    15.714 POINT 1999
# 8  09001 10100404  PM25-PRI   234.178 POINT 1999
# 12 09001 10100501  PM25-PRI     0.128 POINT 1999
# 16 09001 10200401  PM25-PRI     2.036 POINT 1999
# 20 09001 10200504  PM25-PRI     0.388 POINT 1999
# 24 09001 10200602  PM25-PRI     1.490 POINT 1999
str(NEI)
# 'data.frame':  6497651 obs. of  6 variables:
# $ fips     : chr  "09001" "09001" "09001" "09001" ...
# $ SCC      : chr  "10100401" "10100404" "10100501" "10200401" ...
# $ Pollutant: chr  "PM25-PRI" "PM25-PRI" "PM25-PRI" "PM25-PRI" ...
# $ Emissions: num  15.714 234.178 0.128 2.036 0.388 ...
# $ type     : chr  "POINT" "POINT" "POINT" "POINT" ...
# $ year     : int  1999 1999 1999 1999 1999 1999 1999 1999 1999 1999 ...
summary(NEI)
# fips               SCC             Pollutant           Emissions            type                year     
# Length:6497651     Length:6497651     Length:6497651     Min.   :     0.0   Length:6497651     Min.   :1999  
# Class :character   Class :character   Class :character   1st Qu.:     0.0   Class :character   1st Qu.:2002  
# Mode  :character   Mode  :character   Mode  :character   Median :     0.0   Mode  :character   Median :2005  
#                                                          Mean   :     3.4                      Mean   :2004  
#                                                          3rd Qu.:     0.1                      3rd Qu.:2008  
#                                                          Max.   :646952.0                      Max.   :2008
# looks like there is something strange with Emissions outliers
# the mean is 3.4 but the 3rd quad. is 0.1? 
with(NEI, table(year))
# year
#    1999    2002    2005    2008 
# 1108469 1698677 1713850 1976655 
with(NEI, prop.table(table(year)))
# year
#      1999      2002      2005      2008 
# 0.1705953 0.2614294 0.2637646 0.3042107

# SCC Data
dim(SCC)
# [1] 11717    15
head(SCC)
#        SCC Data.Category                                                                 Short.Name                              EI.Sector Option.Group Option.Set               SCC.Level.One       SCC.Level.Two
# 1 10100101         Point                   Ext Comb /Electric Gen /Anthracite Coal /Pulverized Coal Fuel Comb - Electric Generation - Coal                         External Combustion Boilers Electric Generation
# 2 10100102         Point Ext Comb /Electric Gen /Anthracite Coal /Traveling Grate (Overfeed) Stoker Fuel Comb - Electric Generation - Coal                         External Combustion Boilers Electric Generation
# 3 10100201         Point       Ext Comb /Electric Gen /Bituminous Coal /Pulverized Coal: Wet Bottom Fuel Comb - Electric Generation - Coal                         External Combustion Boilers Electric Generation
# 4 10100202         Point       Ext Comb /Electric Gen /Bituminous Coal /Pulverized Coal: Dry Bottom Fuel Comb - Electric Generation - Coal                         External Combustion Boilers Electric Generation
# 5 10100203         Point                   Ext Comb /Electric Gen /Bituminous Coal /Cyclone Furnace Fuel Comb - Electric Generation - Coal                         External Combustion Boilers Electric Generation
# 6 10100204         Point                   Ext Comb /Electric Gen /Bituminous Coal /Spreader Stoker Fuel Comb - Electric Generation - Coal                         External Combustion Boilers Electric Generation
#                 SCC.Level.Three                                SCC.Level.Four Map.To Last.Inventory.Year Created_Date Revised_Date Usage.Notes
# 1               Anthracite Coal                               Pulverized Coal     NA                  NA                                      
# 2               Anthracite Coal             Traveling Grate (Overfeed) Stoker     NA                  NA                                      
# 3 Bituminous/Subbituminous Coal Pulverized Coal: Wet Bottom (Bituminous Coal)     NA                  NA                                      
# 4 Bituminous/Subbituminous Coal Pulverized Coal: Dry Bottom (Bituminous Coal)     NA                  NA                                      
# 5 Bituminous/Subbituminous Coal             Cyclone Furnace (Bituminous Coal)     NA                  NA                                      
# 6 Bituminous/Subbituminous Coal             Spreader Stoker (Bituminous Coal)     NA                  NA
str(SCC)
# 'data.frame':  11717 obs. of  15 variables:
# $ SCC                : Factor w/ 11717 levels "10100101","10100102",..: 1 2 3 4 5 6 7 8 9 10 ...
# $ Data.Category      : Factor w/ 6 levels "Biogenic","Event",..: 6 6 6 6 6 6 6 6 6 6 ...
# $ Short.Name         : Factor w/ 11238 levels "","2,4-D Salts and Esters Prod /Process Vents, 2,4-D Recovery: Filtration",..: 3283 3284 3293 3291 3290 3294 3295 3296 3292 3289 ...
# $ EI.Sector          : Factor w/ 59 levels "Agriculture - Crops & Livestock Dust",..: 18 18 18 18 18 18 18 18 18 18 ...
# $ Option.Group       : Factor w/ 25 levels "","C/I Kerosene",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ Option.Set         : Factor w/ 18 levels "","A","B","B1A",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ SCC.Level.One      : Factor w/ 17 levels "Brick Kilns",..: 3 3 3 3 3 3 3 3 3 3 ...
# $ SCC.Level.Two      : Factor w/ 146 levels "","Agricultural Chemicals Production",..: 32 32 32 32 32 32 32 32 32 32 ...
# $ SCC.Level.Three    : Factor w/ 1061 levels "","100% Biosolids (e.g., sewage sludge, manure, mixtures of these matls)",..: 88 88 156 156 156 156 156 156 156 156 ...
# $ SCC.Level.Four     : Factor w/ 6084 levels "","(NH4)2 SO4 Acid Bath System and Evaporator",..: 4455 5583 4466 4458 1341 5246 5584 5983 4461 776 ...
# $ Map.To             : num  NA NA NA NA NA NA NA NA NA NA ...
# $ Last.Inventory.Year: int  NA NA NA NA NA NA NA NA NA NA ...
# $ Created_Date       : Factor w/ 57 levels "","1/27/2000 0:00:00",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ Revised_Date       : Factor w/ 44 levels "","1/27/2000 0:00:00",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ Usage.Notes        : Factor w/ 21 levels ""," ","includes bleaching towers, washer hoods, filtrate tanks, vacuum pump exhausts",..: 1 1 1 1 1 1 1 1 1 1 ...
with(SCC, table(Data.Category))
Data.Category
# Biogenic    Event Nonpoint  Nonroad   Onroad    Point 
#       82       71     2305      572     1137     7550 
# check for negative values
negative <- NEI$Emissions < 0
sum(negative, na.rm = T)
mean(negative, na.rm = T)
# no negative, so check on NAs