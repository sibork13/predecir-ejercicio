library(dplyr)
library(caret)
library(rattle)
train<-read.csv("pml-training.csv")
validation<-read.csv("pml-testing.csv")


in_train  <- createDataPartition(train$classe, p=0.75, list=FALSE)
training <- train[ in_train, ]
testing  <- train[-in_train, ]


# train<-train[complete.cases(train),]

nzv_var <- nearZeroVar(training)

training <- training[ , -nzv_var]
testing  <- testing [ , -nzv_var]




na_var <- sapply(training, function(x) mean(is.na(x))) > 0.95
training <- training[ , na_var == FALSE]
testing  <- testing [ , na_var == FALSE]

training <- training[ , -(1:5)]
testing  <- testing [ , -(1:5)]

mod1<-train(classe ~ ., data=training,method="rpart")
fancyRpartPlot(mod1$finalModel)

ctrl_RF <- trainControl(method = "repeatedcv", number = 5, repeats = 2)
fit_RF  <- train(classe ~ ., data = training, method = "rf",
                  trControl = ctrl_RF, verbose = FALSE)

#Checando accuracy
predict_RF <- predict(fit_RF, newdata = testing)
conf_matrix_RF <- confusionMatrix(predict_RF, factor(testing$classe))
conf_matrix_RF

pred<-predict(fit_RF,newdata=validation)
