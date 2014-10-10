## subject: Coursera exdata-007 project 2
## title: "plot2.R"
## author: "Eric Bratt"
## date: "Thursday, October 9,2014"
## output: boxplot
################################################################################
## setup the environment
source("setup.R")
################################################################################
# Question 2. Have total emissions from PM2.5 decreased in the Baltimore City, #
#             Maryland (fips == "24510") from 1999 to 2008? Use the base       #
#             plotting system to make a plot answering this question.          #
################################################################################
NEI <- subset(NEI, fips == "24510")
# Boxplot of Emissions by Year 
png(filename="plot2.png", width=480, height=480)
boxplot(log10(Emissions)~year,data=NEI, main="Baltimore Emissions by Year from 1999 to 2008", 
        xlab=paste(length(NEI[, 1]), "total observations", sep=" "), ylab=expression("log base 10 of Emissions PM"[2.5]))
dev.off()
