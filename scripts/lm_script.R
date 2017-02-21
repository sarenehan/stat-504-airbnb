library(ggmap)
library(ggplot2)
library(dplyr)

setwd('D:\\Program File\\Git\\git_projects\\STATS 504\\ProjectProposal\\stat-504-airbnb\\data')

df.data <- read.csv('listings.csv',header=TRUE,sep=',',stringsAsFactors=FALSE)

#Reducing to variables of interests
df.data <- select(df.data, id, price, host_response_rate, host_acceptance_rate, host_is_superhost, 
                  host_listings_count, neighbourhood_cleansed, property_type, room_type,
                  accommodates, bathrooms, bedrooms, beds, guests_included, 
                  availability_365, number_of_reviews, review_scores_accuracy, review_scores_checkin,
                  review_scores_cleanliness, review_scores_communication, review_scores_location,
                  review_scores_value, reviews_per_month)

#cleaning data
df.data$price <- as.numeric(substring(df.data$price,2))
df.data$host_response_rate <- as.numeric(ifelse(df.data$host_response_rate=="N/A",0,substr(df.data$host_response_rate, 1, nchar(df.data$host_response_rate)-1)))
df.data$host_acceptance_rate <- as.numeric(ifelse(df.data$host_acceptance_rate=="N/A",0,substr(df.data$host_acceptance_rate, 1, nchar(df.data$host_acceptance_rate)-1)))
df.data$host_is_superhost[df.data$host_is_superhost==""] <- "f"
df.data$host_is_superhost <- factor(df.data$host_is_superhost)
df.data$neighbourhood_cleansed <- factor(df.data$neighbourhood_cleansed)
df.data$property_type <- factor(df.data$property_type)
df.data$room_type <- factor(df.data$room_type)

lm.airbnb <- lm(price~., data=df.data[,-1])
summary(lm.airbnb)
