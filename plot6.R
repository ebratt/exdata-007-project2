## subject: Coursera exdata-007 project 2
## title: "plot6.R"
## author: "Eric Bratt"
## date: "Thursday, October 9,2014"
## output: ggplot
################################################################################
## setup the environment
source("setup.R")

################################################################################
# Question 6. Compare emissions from motor vehicle sources in Baltimore City   #
#             with emissions from motor vehicle sources in Los Angeles County, #
#             California (fips == "06037"). Which city has seen greater        #
#             changes over time in motor vehicle emissions?                    #
################################################################################
# Only Baltimore
balt <- subset(NEI, fips == "24510")
la   <- subset(NEI, fips == "06037")
# find motor vehicle-related sources
vehicleCats <- unique(grep("[Vv]ehicle", SCC$EI.Sector, value=T))
vehicleCatIDs <- subset(SCC, EI.Sector %in% vehicleCats)[1]
# create a df containing only vehicle sources
balt <- inner_join(balt, vehicleCatIDs) # dplyr
la   <- inner_join(la, vehicleCatIDs) # dplyr
# need this for linear regression lines
log_10 <- function(x) {
  ifelse(x <= 0, 0, base::log10(x))
}

# Multiple plot function
# borrowed from http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

# need to find ymin and ymax for multi-plotting
ymin <- min(log_10(balt$Emissions), log_10(la$Emissions))
ymax <- max(log_10(balt$Emissions), log_10(la$Emissions))

# use ggplot2
load_package("ggplot2")     # for ggplot2 graphs
library(ggplot2)

png(filename="plot6.png", width=960, height=480)

balt_plot <- ggplot(balt) +
  aes(x = factor(year), y = log_10(Emissions), ymin=ymin, ymax=ymax) + 
  xlab(paste(length(balt[, 1]), "total observations", sep=" ")) +
  ylab(expression("Log10 of Emissions"[2.5])) +
  ggtitle("Baltimore Emissions by Year\nfrom 1999 to 2008\nfrom Vehicles") + 
  theme(plot.title = element_text(lineheight=.8, face="bold")) +
  geom_boxplot(outlier.colour = "green", outlier.size = 3) + 
  stat_smooth(method = "lm", se=F, color="red", aes(group=1))

la_plot <- ggplot(la) +
  aes(x = factor(year), y = log_10(Emissions), ymin=ymin, ymax=ymax) + 
  xlab(paste(length(la[, 1]), "total observations", sep=" ")) +
  ylab(expression("Log10 of Emissions"[2.5])) +
  ggtitle("Los Angeles Emissions by Year\nfrom 1999 to 2008\nfrom Vehicles") + 
  theme(plot.title = element_text(lineheight=.8, face="bold")) +
  geom_boxplot(outlier.colour = "green", outlier.size = 3) + 
  stat_smooth(method = "lm", se=F, color="red", aes(group=1))

multiplot(balt_plot, la_plot, cols=2)

dev.off()
