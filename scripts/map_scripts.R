library(ggmap)
library(ggplot2)

setwd('D:\\Program File\\Git\\git_projects\\STATS 504\\ProjectProposal\\stat-504-airbnb\\data')

df.data <- read.csv('listings.csv',header=TRUE,sep=',')

AmsterdamMap <- qmap('amsterdam', zoom = 12, color = 'bw')
AmsterdamMap +
  geom_point(aes(x = longitude, y = latitude), 
            colour="#1E2B6A", data = df.data[,49:50], alpha=0.3)
