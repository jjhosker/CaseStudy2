#############################################################
## RFunction_Makefile used by RMakefile_CaseStudy2.R and 
##      CaseStudy2PDF.Rmd
## Create by:    James Hosker and Samuel Coyne
## SMU Course:   MSDS6306-402
## Date Created: 22-Apr-2017
## Last Update:  22-Mar-2017
## Description:  This R file is provides the functions that
##               used in RMakefile_CaseStudy2.R and 
##               CaseStudy2PDF.Rmd
##
##############################################################

###################################################
## Function Vol
##
## Create by:    James Hosker and Samuel Coyne
## SMU Course:   MSDS6306-402
## Date Created: 22-Apr-2017
## Last Update:  22-Apr-2017
## Description:  Function Vol, d is weight variable 
##               go back in timeThis function is
##               used in  RMakefile_CaseStudy2.R
##               and CaseStudy2PDF.Rmd.
###################################################
## Function Vol, d is weight variable go back in time
## get
Vol <- function(d,logrets){
  var = 0
  lam = 0
  varlist <- c()
  for (r in logrets){
    lam = lam *(1-1/d) + 1
    var = (1-1/lam)*var + (1/lam)*r^2
    varlist <- c(varlist,var)
  }
  sqrt(varlist)
}

