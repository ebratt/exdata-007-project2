## subject: Coursera exdata-007 project 2
## title: "plot3.R"
## author: "Eric Bratt"
## date: "Thursday, October 9,2014"
## output: ggplot
################################################################################
## setup the environment
source("setup.R")

################################################################################
# Question 3. Of the four types of sources indicated by the type               #
#             (point, nonpoint, onroad, nonroad) variable, which of these four #
#             sources have seen decreases in emissions from 1999–2008 for      #
#             Baltimore City? Which have seen increases in emissions from      #
#             1999–2008? Use the ggplot2 plotting system to make a plot answer #
#             this question.                                                   #
################################################################################
table(NEI$type)
NEI <- subset(NEI, fips == "24510")
load_package("ggplot2")     # for ggplot2 graphs
library(ggplot2)

# need this for linear regression lines
log_10 <- function(x) {
  ifelse(x <= 0, 0, base::log10(x))
}

png(filename="plot3.png", width=960, height=960)

ggplot(NEI, aes(x = factor(year), y = log_10(Emissions))) +
  xlab(paste(length(NEI[, 1]), "total observations", sep=" ")) +
  ylab(expression("Log10 of Emissions"[2.5])) +
  ggtitle("Baltimore Emissions by Year by Type\nfrom 1999 to 2008") + 
  theme(plot.title = element_text(lineheight=.8, face="bold")) +
  facet_grid(.~type) + 
  geom_boxplot(outlier.colour = "green", outlier.size = 3) + 
  stat_smooth(method = "lm", se=F, color="red", aes(group=1))

dev.off()
