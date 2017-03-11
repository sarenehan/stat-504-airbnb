library(ggmap)
library(ggplot2)

setwd('D:\\Program File\\Git\\git_projects\\STATS 504\\ProjectProposal\\stat-504-airbnb\\data')

df.data <- read.csv('listings.csv',header=TRUE,sep=',')

AmsterdamMap <- qmap('amsterdam', zoom = 12, color = 'bw')
AmsterdamMap +
  geom_point(aes(x = longitude, y = latitude, color=price), 
            data = df.data, alpha=0.3)
