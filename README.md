# CaseStudy2 for MSDS 6306-402
## Authors: Jim Hosker and Sam Coyne

# Introduction
### This repository for this case study accomplishes data cleaning, graphing, and analysis. Most of the analysis was conducted in R Studio (version 3.3.2) with the remainder in SAS on-demand (version 3.4) and Python (version 3.6.0). The purpose of this case study is to complete several analyses that touch on all aread covered in the course "Doing Data Science".   

# Running R Code
### 1. To run this R-code, download and clone this GitHub directory:  [\textcolor{blue}{https://github.com/jjhosker/CaseStudy2}](https://github.com/jjhosker/CaseStudy2.git).  Check to ensure the following files are downloaded and located in the directories indicated.  The two main files to execute are RMakefile_CaseStudy2.R for the analysis and CaseStudy2PDF.Rmd to generate the PDF file.
####     - RMakefile_CaseStudy2.R found in the Analysis sub-directory.
####     - Makefile_GatherData.R found in the Analysis/Data sub-directory.
####     - RFunctions_CaseStudy2.R found in the Analysis sub-directory.
####     - CaseStudy2PDF.Rmd found in the main GitHub repository.

### 2. The input/output files are also included in the repository  sub-directories and can be downloaded as a reference for the client.
####     - Raw Input:      TEMP.csv found in the Analysis/Data sub-directory.
####     - Raw Input:      CityTemp.csv found in the Analysis/Data sub-directory.
####     - PNG Input:      SAS_MatrixCode.PNG found in the Analysis/Data sub-directory
####     - PNG Input:      SAS_MatrixOutput.PNG found in the Analysis/Data sub-directory
####     - PNG Input:      Python_Matrix.PNG found in the Analysis/Data sub-directory
####     - Cleaned Output: TEMPclean.csv found in the Analysis/Data sub-directory.
####     - Cleaned Output: CityTempclean.csv found in the Analysis/Data sub-directory.
####     - PDF Report Output: CaseStudy2PDF.pfd found in the main GitHub repository.
####     - R version Output:  SessionInfo.txt (for reference of libraries and R-version).  Found in the Analysis/Data sub-directory.

### 3.  Execute RMakefile_CaseStudy2.R in R studio to run all the R code. RMakefile_CaseStudy2.R loads all the libraries, reads the raw data, creates the cleaned data and performs the analysis for this case study.  The csv files will be created in the Analysis/Data sub-director in the CaseStudy2 project directory that you have cloned on your computer.  In addition, we save the R session information that provided the version of R and all the library and packages being utilized in the Analysis/Data sub-directory.  
####     - The RMakefile_CaseStudy2.R executes the makefile Makefile_GatherData.R, which installs the packages and load the libraries.  A message indicating that the libraries loaded may appear after executing the R code.  If a failure occurs, please check both the function "packagelibrary.check" in the makefile Makefile_GatherData.R and SessionInfo.txt file in the GitHub directory referenced above.

### 4.  Execute CaseStudy2PDF.Rmd to generate the PDF file in this repository (installation of MikTex is required as detailed in point 6 in the Repository Description below).

# Repository Description
### Below is a listing of all the files in this repository: [\textcolor{blue}{https://github.com/jjhosker/CaseStudy2}](https://github.com/jjhosker/CaseStudy2.git).  It contains the following important files:

###   1.  README:  This file provides instructions on how to run the R code and provides information in detail of each of the files in the GitHub repository. 
  
###   2.  RMakefile_CaseStudy2.R:  This file can be found in the Analysis sub-directory under the CaseStudy2 GitHub repository.   It is the main R makefile with the functions specifically called by this.
  
###   3. Makefile_GatherData.R:  This file can be found in the Analysis/Data sub-directory under CaseStudy2 GitHub repository.  This R makefile installs all required R-libraries and then loads the R-libraries.  Second, this R makefile uploads the TEMP.csv and CityTemp.csv data files with monthly average temperature data by country and by city, respectively.  This makefile also creates the raw data, and creates the cleaned datasets.   In cleaning the data, we read in the csv files; we remove NAs and blanks in the data; and we reformat the date data.  This makefile then saves all these datasets to the Analysis/Data sub-directory.

###   4. RFunctions_CaseStudy2.R:  This file can be found in the Analysis sub-directory under the CaseStudy2 GitHub repository.   This file contains the functions that are loaded and used the main R makefile(RMakefile_CaseStudy2.R).

###   5. In addition to the two raw data sets, there are three data files created below and three PNG files which are loaded into the PDF file on creation.   In addition, we provide a text file (SessionsInfo.txt) containing all the R session information including the current R version and all the libraries and packages being utilized.  All the files below can be found in the Analysis/Data sub-directory under this GitHub repository. 
####     - Input Raw:  TEMP.csv
####     - Input Raw:  CityTemp.csv
####     - Input PNG:  SAS_MatrixCode.PNG
####     - Input PNG:  SAS_MatrixOutput.PNG
####     - Input PNG:  Python_Matrix.PNG
####     - Output Cleaned:  TEMPclean.csv
####     - Output Cleaned:  CityTempclean.csv
####     - Output R version:  SessionInfo.txt

###   6. Finally the CaseStudy2PDF.Rmd file runs the main RMakefile_CaseStudy2.R and extracts code to generates this "CaseStudy2PDF.pdf"" report. This report uses code that that is extracted from the main RMakefile_CaseStudy2.R and then run.
####      - To use this Rmd file to create a PDF, the user has to install MikTex (the non-basic version).  The URL with instructions to install MikTex is [\textcolor{blue}{MikText Install website}](https://miktex.org/howto/install-miktex).
