library(ggmap)
library(ggplot2)
library(dplyr)
library(faraway)

# setwd('D:\\Program File\\Git\\git_projects\\STATS 504\\ProjectProposal\\stat-504-airbnb\\data')
#setwd('/Users/stewart/projects/stat-504-airbnb/data/');
setwd("~/Documents/courses/504/stat-504-airbnb/data")

df.data <- read.csv('listings.csv',header=TRUE,sep=',',stringsAsFactors=FALSE)

#Reducing to variables of interests
df.data <- select(df.data, id, price, host_response_rate, host_acceptance_rate, host_is_superhost, 
                  host_listings_count, neighbourhood_cleansed, property_type, room_type,
                  accommodates, bathrooms, bedrooms, beds, guests_included, 
                  availability_365, number_of_reviews, review_scores_accuracy, review_scores_checkin,
                  review_scores_cleanliness, review_scores_communication, review_scores_location,
                  review_scores_value, reviews_per_month)

## cleaning data
# Coerce columns into proper types
df.data$price <- as.numeric(gsub(",", "", substring(df.data$price,2)))
nrow(df.data)  # 13849
df.data = df.data[!df.data$host_response_rate == "N/A",]
nrow(df.data)  # 11580
df.data$host_response_rate <- as.numeric(substr(df.data$host_response_rate, 1, nchar(df.data$host_response_rate)-1))
df.data = df.data[!df.data$host_acceptance_rate == "N/A",]
nrow(df.data)  # 11141 
df.data$host_acceptance_rate <- as.numeric(substr(df.data$host_acceptance_rate, 1, nchar(df.data$host_acceptance_rate)-1))
df.data = df.data[!df.data$host_is_superhost == "",]
nrow(df.data)  # 11105
df.data$host_is_superhost <- factor(df.data$host_is_superhost)
df.data = df.data[!df.data$neighbourhood_cleansed == "",]
nrow(df.data)  # 11105
df.data$neighbourhood_cleansed <- factor(df.data$neighbourhood_cleansed)
df.data = df.data[!df.data$property_type == "",]
nrow(df.data)  # 11105
df.data$property_type <- factor(df.data$property_type)
df.data = df.data[!df.data$room_type == "",]
nrow(df.data)  # 11105
df.data$room_type <- factor(df.data$room_type)


# Remove data with null values
for (var in names(df.data)) {
  df.data = df.data[!is.na(df.data[,var]),]
}
nrow(df.data)  # 9574

summary(df.data$property_type)
# These types have less than 5 counts
property_types_to_remove = c(
  "Bungalow", "Chalet", "Earth House", "Dorm", "Hut", "Tent", "Yurt"
)
df.data = df.data[!df.data$property_type %in% property_types_to_remove,]
nrow(df.data)  # 9562


# Build model
lm.airbnb <- lm(price~., data=df.data[,-1])
summary(lm.airbnb)
plot(lm.airbnb$fitted.values, lm.airbnb$residuals) # Clear violation of constant variance

# Remove outliers
model.influence = influence(lm.airbnb)
halfnorm(model.influence$hat,
         xlim=c(-0.1,4.8),
         ylab="Leverages")
title(main="Half-Normalquantileplotforleverages")
max(model.influence$hat) # 0.0997
# No obvious leverage points

# Cooks distances
cook = cooks.distance(lm.airbnb)
halfnorm(cook) # 422, 221
# 422 is removed because it only has 0.11 removed per month and costs $1307 per night.
# 221 is removed because it only has 1 review and it accomodates 16 but only costs $100.
bad_ids = c(11946090, 5587500)
df.data = df.data[!df.data$id %in% bad_ids,]
lm.airbnb <- lm(price~., data=df.data[,-1])
cook = cooks.distance(lm.airbnb)
halfnorm(cook) # 276, 218
# 276 accommodates 16 but only costs $60, something is wrong
# 218 sems normal
df.data = df.data[-276,]
lm.airbnb <- lm(price~., data=df.data[,-1])
cook = cooks.distance(lm.airbnb)
halfnorm(cook) # 317, 868
# 317 accommodates 16 and only costs 50, something is wrong
# This is recurring, something is strange
sum(df.data$accommodates == 16) # 21
# These are outliers with wrong data.
df.data = df.data[!df.data$accommodates == 16,]
nrow(df.data)  # 9538

lm.airbnb <- lm(price~., data=df.data[,-1])
cook = cooks.distance(lm.airbnb)
halfnorm(cook) # 354, 509
# These two are both > $400, but seem normal. I am stopping here.

plot(lm.airbnb, which=1)
plot(lm.airbnb$fitted.values,
     lm.airbnb$residuals,
     xlab="Fitted Values",
     ylab="Residuals",
     main="Residuals vs Fitted Values") # Clear violation of constant variance

##transformation 
library('MASS')
boxcox(lm.airbnb)
##we can choose lambda =0 and transform price to log(price)
lm.airbnb=lm(log(price)~.,data=df.data[,-1])
summary(lm.airbnb)
plot(lm.airbnb,which=1) 

##variable selection
step(lm.airbnb)
##from the output of step, we exclude beds, host_acceptance_rate,review_scores_checkin, review_scores_communication.

drop=c('beds','host_acceptance_rate','review_scores_checkin','review_scores_communication')
df.data.drop=df.data[,!names(df.data)%in%drop]
lm.airbnd=lm(log(price)~.,data=df.data.drop[,-1])
anova(lm.airbnd)
plot(lm.airbnb,which=1)
summary(lm.airbnb)

# Feature importances 
adjusted_r_squared_full = summary(lm.airbnd)$adj.r.squared
features.importances = c()
for (idx in seq(ncol(df.data.drop) - 2)) {
  model.reduced = lm(log(price)~., data=df.data.drop[,-c(1, idx+2)]);
  features.importances = c(features.importances,
                           adjusted_r_squared_full - summary(model.reduced)$adj.r.squared);
}
feature_names = c(
  "Response Rate",
  "Superhost",
  "Listing Count",
  "Neighborhood",
  "Property Type",
  "Room Type",
  "Accommodates",
  "Bathrooms",
  "Bedrooms",
  "Guests Incl.",
  "Availability",
  "No. Reviews",
  "Accuracy",
  "Cleanliness",
  "Location Score",
  "Value Score",
  "Reviews / Month"
)
barplot(features.importances, names.arg = feature_names,
        ylab = "Improvement to Adj R-Squared",
        main = "Feature Importance",las=3)







