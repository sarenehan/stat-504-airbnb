library(MASS)
library(ggmap)
library(ggplot2)
library(dplyr)
library(faraway)

# setwd('D:\\Program File\\Git\\git_projects\\STATS 504\\ProjectProposal\\stat-504-airbnb\\data')
setwd('/Users/stewart/projects/stat-504-airbnb/data/');
# setwd("~/Documents/courses/504/stat-504-airbnb/data")

df.data <- read.csv('listings.csv',header=TRUE,sep=',',stringsAsFactors=FALSE)

#Reducing to variables of interests
df.data <- dplyr::select(df.data, price, host_is_superhost, host_has_profile_pic, host_identity_verified, 
                         neighbourhood_cleansed, property_type, room_type, accommodates, bathrooms, bedrooms, beds, bed_type,
                         amenities, guests_included, minimum_nights, number_of_reviews)

## cleaning data
# Coerce columns into proper types
df.data$price <- as.numeric(gsub(",", "", substring(df.data$price,2)))
nrow(df.data)  # 13849

df.data <- df.data[!df.data$host_is_superhost == "",]
nrow(df.data)  # 13813

df.data$host_is_superhost <- factor(df.data$host_is_superhost)
df.data <- df.data[!df.data$host_has_profile_pic=="",]
nrow(df.data) #13813

df.data$host_has_profile_pic <- factor(df.data$host_has_profile_pic)
df.data <- df.data[!df.data$host_identity_verified=="",]
nrow(df.data) #13813

df.data$host_identity_verified <- factor(df.data$host_identity_verified)
df.data = df.data[!df.data$neighbourhood_cleansed == "",]
nrow(df.data)  # 13813

df.data$neighbourhood_cleansed <- factor(df.data$neighbourhood_cleansed)
df.data = df.data[!df.data$property_type == "",]
nrow(df.data)  # 13813

df.data$property_type <- factor(df.data$property_type)

df.data = df.data[!df.data$room_type == "",]
nrow(df.data)  # 13813
df.data$room_type <- factor(df.data$room_type)

df.data = df.data[!df.data$bed_type == "",]
nrow(df.data)  # 13813
df.data$bed_type <- factor(df.data$bed_type)

#Aemnities extraction
#Wireless Internet
df.data$wifi <- 'f'
df.data$wifi[grep("Wireless Internet", df.data$amenities, ignore.case = TRUE)] <- 't'
df.data$wifi <- factor(df.data$wifi)
#Free Parking on Premises
df.data$parking <- 'f'
df.data$parking[grep("Free Parking on Premises", df.data$amenities, ignore.case = TRUE)] <- 't'
df.data$parking <- factor(df.data$parking)
#Smoking Allowed
df.data$smoke <- 'f'
df.data$smoke[grep("Smoking Allowed", df.data$amenities, ignore.case = TRUE)] <- 't'
df.data$smoke <- factor(df.data$smoke)

#Drop amenities
df.data <- dplyr::select(df.data, -amenities)

# Remove data with null values
for (var in names(df.data)) {
  df.data = df.data[!is.na(df.data[,var]),]
}
nrow(df.data)  # 13767

sum(df.data$number_of_reviews == 0)  # 2451
# Listings with no reviews have few stays; their prices may not be indicitive of what they are worth.
df.data = df.data[!df.data$number_of_reviews == 0,]
nrow(df.data)  # 11316
#Drop number of reviews
df.data <- dplyr::select(df.data, -number_of_reviews)

summary(df.data$property_type)
# These types have less than 5 counts
property_types_to_remove = c(
  "Bungalow", "Chalet", "Earth House", "Dorm", "Hut", "Tent", "Yurt"
)
df.data = df.data[!df.data$property_type %in% property_types_to_remove,]
nrow(df.data)  # 11304

# Remove listings with accommodates outliers. 
df.data <- subset(df.data, !(accommodates > 10 & price < 150))
nrow(df.data)  # 11295


# Build model
lm.airbnb <- lm(price~., data=df.data)
summary(lm.airbnb)
plot(lm.airbnb$fitted.values, lm.airbnb$residuals) # Clear violation of constant variance

# Remove outliers
# Cooks distances
cook = cooks.distance(lm.airbnb)
halfnorm(cook) # 487, 5703
# 487 is removed because it only has 0.11 removed per month and costs $1307 per night.
# 5703 is removed because it has a minimum_nights value of 1000
bad_ids = c(487, 5703)
df.data = df.data[-bad_ids,]

# Remove points with minimum_nights > 15. These dont fall under the category of short term vacation
# rental. 
df.data = df.data[df.data$minimum_nights <= 15,]
nrow(df.data)  # 11223

lm.airbnb <- lm(price~., data=df.data)
cook = cooks.distance(lm.airbnb)
halfnorm(cook) # 100, 595
# 100 seems normal, but cheap.
# 595 sems normal but more expensive. Stop here

plot(lm.airbnb, which=1)
plot(lm.airbnb$fitted.values,
     lm.airbnb$residuals,
     xlab="Fitted Values",
     ylab="Residuals",
     main="Residuals vs Fitted Values") # Clear violation of constant variance

##transformation 
boxcox(lm.airbnb)
##we can choose lambda =0 and transform price to log(price)
lm.airbnb=lm(log(price)~.,data=df.data)
summary(lm.airbnb)
plot(lm.airbnb,which=1) 

##variable selection
step(lm.airbnb)
##from the output of step, we exclude beds, host_acceptance_rate,review_scores_checkin, review_scores_communication.

# exhaustive variable selection
library(leaps)
rs<-summary(regsubsets(price~.,data=df.data, really.big = T))
plot(2:10,rs$cp,xlab="Numberofparameters",ylab="MallowsC_p") # Monotonically decreasing;
# Dont throw out any variables (and look for more to add)

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
