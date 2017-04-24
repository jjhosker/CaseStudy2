#############################################################
## Country Analysis by GDP and Income Group for 2012
## Create by:    James Hosker
## SMU Course:   MSDS6306-402
## Date Created: 22-Apr-2017
## Last Update:  22-Apr-2017
## Description:  This R file is the main analysis
##               file for Case Study 2. 
##
##############################################################

###################################################
## Execute Main RMakefile_GatherData.R Makefile
## 1. Installs and load libraries 
## 2. Reads in the data files 
## 2. Creates the clean data frames
###################################################
source("Analysis/Data/RMakefile_GatherData.R")

###################################################
## Execute Main RFunctions_CaseStudy2.R Makefile
## 1. Loads in all function used in our analysis
###################################################
source("Analysis/RFunctions_CaseStudy2.R")

####################################################
##  Question #1:  Create the X matrix and print 
##                it out in SAS, R and Python.  
####################################################
xlist <- c(4,5,1,2,1,0,3,5,2,1,8,2)
X <- matrix(xlist,nrow=3,ncol=4,byrow=TRUE)
## Below in CaseStudy2PDF.Rmd file
## print(X)

####################################################
##  Question #2:  Download ADP, Plot series
##                and create/plot volatility series  
####################################################
## libraries fpp and tseries are loaded in Makefile_GatherData.R

## Get ADP data and analyze volatility of price series
## a. Download the data.
ADPdata <- get.hist.quote("ADP", start = "2000-01-01", quote="Close",provider="yahoo")
## plot(ADPdata,xlab="Date",ylab="Price ($)", 
##     main= "Fig.3: Price of Automatic Data Processing (ADP)")
## length(ADPdata)
## Below in CaseStudy2PDF.Rmd file
## print(paste(" a.  We download the data for ADP from Jan 2000 to today."))

## b. Calculate log returns.
ADPlnret <- log(lag(ADPdata))-log(ADPdata)
ADPlnret <- ADPlnret[!is.na(ADPlnret)]
## plot(ADPlnret,xlab="Date",ylab="Price ($)", 
##     main= "Price of Automatic Data Processing (ADP)")

## Below in CaseStudy2PDF.Rmd file
## print(paste(" b.  We calculate log returns."))

## c. Calculate volatility measure.
## length(ADPlnret)
ADPvol <- sd(ADPlnret) *sqrt(250)*100
## Below in CaseStudy2PDF.Rmd file
## print(paste("c. ADP volatility of series from 01-Jan-2000 to 22-Apr-2017: ",round(ADPvol,2),"%"))

## d. Calculate volatility over entire length of series for 
##      various three different decay factors.
## Function Vol is located in Analysis/RFunction_Makefile.R
## ADPvol_10d <- ADPlnret
ddates <- as.Date(index(ADPdata))
ddates <- ddates[2:length(ddates)]
volest_10d <- Vol(10,ADPlnret)  
ADPvol_10d <- volest_10d

## ADPvol_30d <- ADPlnret
volest_30d <- Vol(30,ADPlnret)  
ADPvol_30d <- volest_30d

## ADPvol_100d <- ADPlnret
volest_100d <- Vol(100,ADPlnret)  
ADPvol_100d <- volest_100d

## length(volest_10d)
## volest_30d <- Vol(30,ADPlnret)
## length(volest_30d)
## volest_100d <- Vol(100,ADPlnret)
## length(volest_100d) */
## Below in CaseStudy2PDF.Rmd file
## print(paste("d.  We calculate the volatility of log returns for 10, 30 and 100 decay factor"))

## e. Plot the results, overlaying the volatility curves on the data, 
##      just as was done in the ADP example.

## Below in CaseStudy2PDF.Rmd file
## plot(x=ddates,y=ADPvol_10d,type="s",xlab="Date", ylab="Volatility (decayed)", 
##    main="Fig. 4: ADP Volatility using 10, 30 and 100 Day Decay", lty=2)
## lines(x=ddates,y=ADPvol_30d,type="s", col="red",lty=4)
## lines(x=ddates,y=ADPvol_100d,type="s", col="blue",lty=1)
## legend(as.Date("2011-01-02"), 0.08, legend=c("10d Decay", "30d Decay", "100d Decay"),
##        col=c("black","red","blue"), lty=c(2,4,1), cex=0.8)

## Below in CaseStudy2PDF.Rmd file
## print(paste(" e.  We plotted the volatilities with decay factors."))


####################################################
##  Question #3:  Anlayze dataset Orange part 
##                of fpp library
####################################################
#load the data
Orange

## calculate the mean of trunk circumference for different Tree sizes
## Below in CaseStudy2PDF.Rmd file
TrunkMeanCir <- tapply(Orange$circumference,Orange$Tree,mean)
## Below in CaseStudy2PDF.Rmd file
## print(paste("Mean Trunk Circumference"))
## print(TrunkMeanCir)

## calculate the median of trunk circumference for different Tree sizes
TrunkMedCir <- tapply(Orange$circumference,Orange$Tree,median)
## Below in CaseStudy2PDF.Rmd file
## print(paste("Median Trunk Circumference"))
## print(TrunkMedCir)

## make a scatterplot of age vs. circumference with different plotting symbols
## Below in CaseStudy2PDF.Rmd file
## ggplot(Orange, aes(x=circumference, y=age, shape=Tree, colour=Tree)) + 
## geom_point() + scale_colour_brewer(palette="Set1") +
## ggtitle("Fig. 5: Scatterplot of Tree Age vs. Circumference")

## make a comparative boxplot of tree vs. circumference, displayed 
## in increasing order of max circumference
## Below in CaseStudy2PDF.Rmd file
## ggplot(Orange, aes(x=Tree, y=circumference)) + geom_boxplot() +
## ggtitle("Fig. 6: BoxPlot of Tree vs. Circumference")

############################################
##  Question #4:  Analyze the data Temp.csv
############################################
##### Part (i) ####################
#  Create month column
TEMPclean$Month <- months(TEMPclean$Date2)
#  Get year column as numeric
TEMPclean$numYear <- as.numeric(format(TEMPclean$Date2,format="%Y"))
##Create TEMPclean2 to begin filtering data set
TEMPclean2 <- TEMPclean
## Remove years less than 1900
TEMPclean2 <- TEMPclean2[TEMPclean2$numYear>=1900,]
#make a summary table by Country using group_by in dplyr library
pivot_table2 <- group_by(TEMPclean2, Country)
#summarize pivot table by min temp, max temp and find the difference
pivot_summary2 <- summarise(pivot_table2, min_temp=min(Monthly.AverageTemp), 
                           max_temp=max(Monthly.AverageTemp), 
                           difference=max_temp-min_temp)
#sort pivot summary in descending order on the temp difference
pivot_summary_sorted2 <- pivot_summary2[order(-pivot_summary2$difference),]
#make a subset of top 20 cities based on temp difference
top_20_country <- pivot_summary_sorted2[1:20,]

TableNames <- c("Country","Temp. Min","Temp. Max","Diff (Max-Min)")
## R Code used in CaseStudy2PDF.Rmd for Summary Table output 
## kable(top_20_country, format="pandoc", align='c',row.names = FALSE,
##      col.names = TableNames, 
##      caption = "Top 20 Countries with Largest Monthly Avg Max - Min Temp. (1900 TO 2013)")

## R Code used in CaseStudy2PDF.Rmd 
#make a visual of top 20 countries and their temp difference
## visual1 <- ggplot(top_20_country, aes(x=difference, y=reorder(Country, difference))) + 
##            geom_point(size=3) + ylab("Country") + theme_bw() + 
##            theme(panel.grid.major.x=element_blank(), panel.grid.minor.x = element_blank(), 
##            panel.grid.major.y = element_line(colour="grey60", linetype="dashed")) +
##            ggtitle("Fig. 7: Top 20 Monthly Avg Max-Min Temp by Country (1900 to 2013)")

## R Code used in CaseStudy2PDF.Rmd 
#show the plot
##visual1


#####   Part (ii)  ####################

  #a##### Part (ii.a.) ####################
  ## Create new Column for monthly average land temperatures after 1990
UStemp <- TEMPclean[TEMPclean$Country=="United States",]
UStemp <- UStemp[UStemp$numYear>=1990,]
UStemp$MthAvgTemp.Fahrenheit <-  (UStemp$Monthly.AverageTemp * 9/5 + 32)
## R Code used in CaseStudy2PDF.Rmd 
## plot UStemp monthly avg temp for USA
## plot(UStemp$Date2, UStemp$MthAvgTemp.Fahrenheit,type="s",xlab="Date", 
##     ylab="Monthly Avg. Temp (degr-F)", 
##     main="Fig. 8: Monthly Average Temperature for USA (Fahrenheit)")

  #b##### Part (ii.b.) ####################
  ## Create new Column for yearly average land temperatures after 1990 for USA
CkYrs  <- unique(UStemp$numYear)
## create new data frame for question
Yclass <- c("Country","Year","YearlyAvg","Diff")
YrAvg <- data.frame()
YrAvg <- data.frame("Country", "Year"<-CkYrs, "YearlyAvg"<-c(0), "Diff"<-c(0))
names(YrAvg) <- Yclass
YrAvg$Country <- "United States"

  ## Create Table
cnt=1
for(k in CkYrs){
  ## print(k)
  tempdata <- UStemp[UStemp$numYear==k,]
  ## print(tempdata)
  YrAvg$YearlyAvg[cnt]  <- mean(tempdata$MthAvgTemp.Fahrenheit,na.rm=TRUE)
  YrAvg$Year[cnt]  <- k
  cnt = cnt + 1
}
TableFrame <- YrAvg[,c("Year","YearlyAvg")]
## R Code used in CaseStudy2PDF.Rmd 
## make a table of yearly temp in US 
## kable(TableFrame, format="pandoc", align='c',row.names = FALSE,
##      col.names = c("Year", "Yearly Temperature Avg. (Fahrenheit)"), 
##      caption = "Yearly Avg. Temperature for USA (1990-2013)")
## make a visual of yearly temp in US 
## visual3 <- ggplot(TableFrame, aes(x=Year, y=YearlyAvg)) + 
##  geom_point(size=3) + ylab("Temperatur (Fahrenheit)") + xlab("Year") + theme_bw() + 
##  theme(panel.grid.major.x=element_blank(), panel.grid.minor.x = element_blank(), 
##        panel.grid.major.y = element_line(colour="grey60", linetype="dashed")) +
##  ggtitle("Fig. 9: Yearly Avg. Temperature for USA (1990-2013)")
#show the plot
## visual3


  #c##### Part (ii.c.) ####################
  ## Calculate the one year difference of the average land temperature by year-end
BtwYrAvg <- YrAvg
options(warn=-1) ## Turn off Warnings
BtwYrAvg$Diff <- round((BtwYrAvg$YearlyAvg[-1]-BtwYrAvg$YearlyAvg),3)
BtwYrAvg$Diff[length(YrAvg$Diff)] <-"NA"
BtwYrAvg$YearDiff <- paste(BtwYrAvg$Year[-1],"-", BtwYrAvg$Year)
options(warn=0)  ## Turn on Warnings
BtwYrAvg <- BtwYrAvg[1:(nrow(BtwYrAvg)-1),]
## sort by the absolute value of the difference since we are
##      looking for the largest spread + or -
SortBtwYrAvg <-  BtwYrAvg[order(abs(as.numeric(BtwYrAvg$Diff)),
                         decreasing = TRUE, na.last=TRUE),]
TableFrame2 <- BtwYrAvg[,c("YearDiff","Diff")]
## R Code used in CaseStudy2PDF.Rmd 
## kable(TableFrame2, format="pandoc", align='c',row.names = FALSE,
##      col.names = c("Range of Years", "Yearly Avg. Temp. Diff of Year - Prior Year (Fahrenheit)"), 
##      caption = "Yearly Avg. Temperature Between Years for USA (1990-2013)")
## print(paste("Largest avg. yearly temp. difference (+/-) for USA is:",
##            SortBtwYrAvg$Diff[1]," Fahrenheit (",SortBtwYrAvg$YearDiff[1],")"))

##### Part (iii) ####################
#  Create month column
CityTempclean$Month <- months(CityTempclean$Date2)
#  Get year column as numeric
CityTempclean$numYear <- as.numeric(format(CityTempclean$Date2,format="%Y"))
##Create CityTempclean2 to begin filtering data set
CityTempclean2 <- CityTempclean
## Remove years less than 1900
CityTempclean2 <- CityTempclean2[CityTempclean2$numYear>=1900,]

#make a summary table by City using group_by in dplyr library
pivot_table <- group_by(CityTempclean2, City, Country)

#summarize pivot table by min temp, max temp, and find the difference
pivot_summary <- summarise(pivot_table, min_temp=min(Monthly.AverageTemp), 
                           max_temp=max(Monthly.AverageTemp), 
                           difference=max_temp-min_temp)

#sort pivot summary in descending order on the temp difference
pivot_summary_sorted <- pivot_summary[order(-pivot_summary$difference),]

#make a subset of top 20 cities based on temp difference
top_20_cities <- pivot_summary_sorted[1:20,]

TableNames3 <- c("City","Country","Temp. Min","Temp. Max","Diff (Max-Min)")
## R Code used in CaseStudy2PDF.Rmd 
## R Code for CaseStudy2PDF.Rmd for Summary Table output 
##kable(top_20_cities, format="pandoc", align='c',row.names = FALSE,
##      col.names = TableNames3, 
##      caption = "Top 20 Cities with Largest Monthly Avg Max - Min Temp. (1900 TO 2013)")
#make a visual of top 20 cities and their temp difference
## visual2 <- ggplot(top_20_cities, aes(x=difference, y=reorder(City, difference))) + 
##            geom_point(size=3) + ylab("City") + theme_bw() + 
##           theme(panel.grid.major.x=element_blank(), panel.grid.minor.x = element_blank(), 
##           panel.grid.major.y = element_line(colour="grey60", linetype="dashed")) +
##           ggtitle("Fig. 10: Top 20 Monthly Avg Max-Min Temp by City (1900 to 2013)")
#show the plot
## visual2

##########################################
##  Question #5:  Semester End Bonus Plot
##########################################
## Below in CaseStudy2PDF.Rmd file
## t<-seq(0,10,length=1000)
## x<-sqrt(t)*cos(2*pi*t)
## y<-sqrt(t)*sin(2*pi*t)
## plot(x,y,axes=F,type="l",lwd=3,xlab="x(t)",ylab="y(t)",col="red")

## axis(1,at=seq(-3,3,by=0.5),labels=seq(-3,3,by=0.5))
## axis(2)
## box()
## title(main=expression(
##        paste("Fig. 11: (x(t),y(t)) with polar coordinates",
##        (list(sqrt(t),2*pi*t))
##  )))

