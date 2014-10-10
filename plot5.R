## subject: Coursera exdata-007 project 2
## title: "plot5.R"
## author: "Eric Bratt"
## date: "Thursday, October 9,2014"
## output: ggplot
################################################################################
## setup the environment
source("setup.R")

################################################################################
# Question 5. How have emissions from motor vehicle sources changed from       #
#             1999–2008 in Baltimore City?                                     #
################################################################################
# Only Baltimore
NEI <- subset(NEI, fips == "24510")
# find motor vehicle-related sources
vehicleCats <- unique(grep("[Vv]ehicle", SCC$EI.Sector, value=T))
vehicleCatIDs <- subset(SCC, EI.Sector %in% vehicleCats)[1]
# create a df containing only vehicle sources
NEI <- inner_join(NEI, vehicleCatIDs) # dplyr
# need this for linear regression lines
log_10 <- function(x) {
  ifelse(x <= 0, 0, base::log10(x))
}

# use ggplot2
load_package("ggplot2")     # for ggplot2 graphs
library(ggplot2)

png(filename="plot5.png", width=480, height=480)

ggplot(NEI, aes(x = factor(year), y = log_10(Emissions))) +
  xlab(paste(length(NEI[, 1]), "total observations", sep=" ")) +
  ylab(expression("Log10 of Emissions"[2.5])) +
  ggtitle("Baltimore Emissions by Year\nfrom 1999 to 2008\nfrom Vehicles") + 
  theme(plot.title = element_text(lineheight=.8, face="bold")) +
  geom_boxplot(outlier.colour = "green", outlier.size = 3) + 
  stat_smooth(method = "lm", se=F, color="red", aes(group=1))

dev.off()