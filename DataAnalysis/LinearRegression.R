# import libraries
library(caret)
library(lars)
library(Metrics)

# load data
data("diabetes")
df <- diabetes[c("x", "y")]
set.seed(42)
partition <- createDataPartition(y = df$y, times = 1, p = 0.7, list = FALSE)
train <- df[partition,]
test <- df[-partition,]
# build the model
model <- lm(y ~ . , data = train)
# test on unseen data
test$predictions <- predict(model, test)
# calculate mse
mse_value <- mse(actual = test$y, predicted = test$predictions)
print(mse_value)

