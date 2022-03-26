library(cometr)
library(caret)
library(Metrics)

df <- read.csv('../source/breast-cancer.csv')
df$diagnosis <- as.factor(df$diagnosis)

set.seed(30)
n <- dim(df)[1]
for (method in c('knn', 'rpart')){
  exp <- create_experiment()
  
  for (i in seq(200, n+1, by=50)) {
    if(i > n)
      i = n
    dft <- df[c(1:i),]
    
    index <- createDataPartition(y = dft$diagnosis, times = 1, p = 0.8, list = FALSE)
    training_set <- dft[index,]
    test_set <- dft[-index,]
    trControl <- trainControl(method = "repeatedcv",number = 10,repeats = 10)
    model <- train(diagnosis ~ ., method=method, data = training_set, metric='Accuracy',
                   preProcess = c("center","scale"), trControl=trControl)
    
    test_set$pred <- predict(model, test_set)
    
    acc <- accuracy(test_set$diagnosis, test_set$pred)
    
    exp$log_metric("accuracy", acc, step=i)
    exp$add_tags(list(method))
    
  }
  
  exp$stop()
}