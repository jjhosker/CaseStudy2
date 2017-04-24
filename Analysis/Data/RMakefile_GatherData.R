###################################################
## RMakefile_GathereData.R is a R Makefile for
##     for the Rmd file:  CaseStudy2PDF.Rmd and 
##     RMakefile_CaseStudy2.Rmd
## Create by:    James Hosker and Samuel Coyne
## SMU Course:   MSDS6306-402
## Assignment:   For Case Study 2 
## Date Created: 22-Apr-2017
## Last Update:  22-Apr-2017
## Description:  Makefiel_GatherDate.txt inputs two
##               csv files for analysis in CaseStudy2. 
## Output:       We output the cleaned dataframes of
##               those input csv files. We also
##               save a R session into to 
##               Analysis/Data/sessionInfo.txt
###################################################

###################################################
## Libraries and packages that need to be Installed
###################################################
packageslibs = c("devtools","repmis","downloader",
                 "ggplot2","xtable","kableExtra",
                 "knitr","tseries","lubridate","dplyr","zoo")

######################################################
## Function packagelibrary.check
## Create by:    James Hosker and Samuel Coyne
## SMU Course:   MSDS6306-402
## Date Created: 22-Apr-2017
## Last Update:  22-Apr-2017
## Description:  We use this function to check if 
##               each package is on the local machine.
##               If the package(s) is installed, 
##               it will be loaded as a library. 
##               If the package(s) is not install, 
##               the missing package(s) will be 
##               installed and libray loaded.
######################################################
options(warn=-1) ## Turn off Warnings
packagelibrary.check <- lapply(packageslibs, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    if(x == "devtools"){
      install.packages("devtools")
      devtools::install_github("rstudio/rmarkdown")
    }
    else{
      install.packages(x, dependencies = TRUE)
      suppressPackageStartupMessages(library(x, character.only = TRUE))
    }
  }
  else{
    suppressPackageStartupMessages(library(x, character.only = TRUE))
  }
})
options(warn=0)  ## Turn on Warnings
## Verify they are loaded and list the loaded packages
## and libraries.
print(paste("Libraries sucessfully that are load are the following"))
print(search())


###################################################
## Save R Session Info for Reference
###################################################
## write R session info to file for reference
## sink("Analysis/Data/sessionInfo.txt")  ## put all messages from session_info into file
writeLines(capture.output(session_info()),"Analysis/Data/sessionInfo.txt")
## writeLines(capture.output(session_info()), "Analysis/Data/sessionInfo.txt")
## sink() ## remove file reference 

###################################################
## Load in 2 csv files below:
## 1. Read in Analysis/Data/CityTemp.csv
## 1. Read in Analysis/Data/TEMP.csv
###################################################
## URLs and files to download
CityTempraw <- data.frame()
TEMPraw <- data.frame()
## Read in raw data files, Analysis/Data/CityTemp.csv and headers
CityTempraw <- read.csv("Analysis/Data/CityTemp.csv", header=TRUE, sep=",")
## Read in raw data files, Analysis/Data/TEMP.csv and headers
TEMPraw <- read.csv("Analysis/Data/TEMP.csv", header=TRUE, sep=",")

options(warn=-1)  ## Turn off Warnings
attach(CityTempraw)
attach(TEMPraw)
options(warn=0)  ## Turn on Warnings

############ Clean TEMPraw to TEMPclean ##################
TEMPclean <- TEMPraw
## Cleanup all columns 
TEMPclean$Monthly.AverageTemp <- as.numeric(TEMPclean$Monthly.AverageTemp)
TEMPclean$Monthly.AverageTemp.Uncertainty <- as.numeric(TEMPclean$Monthly.AverageTemp.Uncertainty)
TEMPclean$Country <- as.character(TEMPclean$Country)
## Clean Date and create Date2
TEMPclean$Date2 <-as.Date("1/2/2017","%m/%d/%Y")
TEMPclean$Date <- as.character(TEMPclean$Date)
DatePt1 <- as.Date(TEMPclean$Date,format="%Y-%m-%d") ## produces NA if format not %Y-%m-%d
DatePt2 <- as.Date(TEMPclean$Date,format="%m/%d/%Y") ## produces NA if format not %m/%d/%Y
DatePt1[is.na(DatePt1)] <- DatePt2[!is.na(DatePt2)]  ## Combine both while keeping their ranks
##length(DatePt1[DatePt1=="NA"])
TEMPclean$Date2 <- DatePt1
## Remove Country NAs 
TEMPclean <- TEMPclean[!is.na(TEMPclean$Country),]
## Remove Country Monthly Average Temperature NAs 
TEMPclean <- TEMPclean[!is.na(TEMPclean$Monthly.AverageTemp),]
## Write CityTempclean data file to Analysis/Data directory
write.csv(TEMPclean, file = "Analysis/Data/TEMPclean.csv",row.names=FALSE)


############ Clean CityTempraw to CityTempclean ##################
CityTempclean <- CityTempraw
## Cleanup all columns 
CityTempclean$Monthly.AverageTemp <- as.numeric(CityTempclean$Monthly.AverageTemp)
CityTempclean$Monthly.AverageTemp.Uncertainty <- 
      as.numeric(CityTempclean$Monthly.AverageTemp.Uncertainty)
CityTempclean$Country <- as.character(CityTempclean$Country)
CityTempclean$City <- as.character(CityTempclean$City)
CityTempclean$Latitude <- as.numeric(CityTempclean$Latitude)
CityTempclean$Longitude <- as.numeric(CityTempclean$Longitude)

## Clean Date and create Date2
CityTempclean$Date2 <-as.Date("1/2/2017","%m/%d/%Y")
CityTempclean$Date <- as.character(CityTempclean$Date)
CDatePt1 <- as.Date(CityTempclean$Date,format="%Y-%m-%d") ## produces NA if format not %Y-%m-%d
CDatePt2 <- as.Date(CityTempclean$Date,format="%m/%d/%Y") ## produces NA if format not %m/%d/%Y
CDatePt1[is.na(CDatePt1)] <- CDatePt2[!is.na(CDatePt2)]   ## Combine both while keeping their ranks
##length(DatePt1[DatePt1=="NA"])
CityTempclean$Date2 <- CDatePt1
## Remove City NAs 
CityTempclean <- CityTempclean[!is.na(CityTempclean$City),]
## Remove Country Monthly Average Temperature NAs 
CityTempclean <- CityTempclean[!is.na(CityTempclean$Monthly.AverageTemp),]
## Write CityTempclean data file to Analysis/Data directory
write.csv(CityTempclean, file = "Analysis/Data/CityTempclean.csv",row.names=FALSE)

## head(CityTempclean)
## head(TEMPclean)

detach(CityTempraw)
detach(TEMPraw)

