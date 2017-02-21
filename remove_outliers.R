library(dplyr)
library(faraway)
library(knitr)
#Just set your working directory when you are using it.
setwd('/Users/stewart/projects/stat-504-airbnb/data/');
# setwd('D:\\Program File\\Git\\git_projects\\STATS 504\\ProjectProposal\\stat-504-airbnb\\data')
df.var <- read.csv('variables.csv',header=TRUE,sep=',')
df.data <- read.csv('listings.csv',header=TRUE,sep=',')

# Format data and fit initial model 

sorted_variables = sort(names(df.data))
variables = c(
  'price',
  'host_response_rate',
  'host_acceptance_rate',
  'host_is_superhost',
  'neighbourhood',
  'property_type',
  'room_type',
  'accommodates',
  'bedrooms',
  'bathrooms',
  'beds',
  'bed_type',
  'guests_included',
  'number_of_reviews',
  'review_scores_rating',
  'review_scores_accuracy',
  'review_scores_cleanliness',
  'review_scores_checkin',
  'review_scores_communication',
  'review_scores_location',
  'review_scores_value',
  'reviews_per_month',
  'availability_365'
);

data = df.data[variables];

variables.numeric = c(
  'price',
  'host_acceptance_rate',
  'host_response_rate'
)

for (var in variables.numeric) {
  data[,var] = as.numeric(data[,var])
}

# Remove the outliers
# Remove 2702 rows with null values
nrow(data)  # 13849
for (var in variables) {
  data = data[!is.na(data[,var]),]
}
nrow(data)  # 11147. 

model = lm(price~., data=data)
summary(model)

plot(model$fitted.values, model$residuals)

# Leverage points
model.influence = influence(model)
sum(model.influence$hat)  # 87
halfnorm(model.influence$hat,
         xlim=c(-0.1,3.8),
         ylab="Leverages")
title(main="Half-Normalquantileplotforleverages")
# 3779, 498 are outlier candidates

# Cooks distances
cook = cooks.distance(model)
halfnorm(cook)

