## subject: Coursera exdata-007 project 2
## title: "plot1.R"
## author: "Eric Bratt"
## date: "Thursday, October 9,2014"
## output: boxplot
################################################################################
## setup the environment
source("setup.R")

################################################################################
# Question 1. Have total emissions from PM2.5 decreased in the United States   #
#             from 1999 to 2008? Using the base plotting system, make a plot   #
#             showing the total PM2.5 emission from all sources for each of    #
#             the years 1999, 2002, 2005, and 2008.                            #
################################################################################
# Boxplot of Emissions by Year 
png(filename="plot1.png", width=480, height=480)
boxplot(log10(Emissions)~year,data=NEI, main="Total US Emissions by Year from 1999 to 2008", 
        xlab=paste(length(NEI[, 1]), "total observations", sep=" "), ylab=expression("log base 10 of Emissions PM"[2.5]))
dev.off()
