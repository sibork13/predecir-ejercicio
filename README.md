# Course Project

In this case. we have a dataset from groupware
This data is about accelerometers metrics obtained.

With this data, we must predict the manner in which they did the exercise.

first, need to load library and data
```{r,echo=TRUE}
library(dplyr)
library(caret)
library(rattle)
train<-read.csv("pml-training.csv")
validation<-read.csv("pml-testing.csv")
```
only loaded train and validation data because actual train data
will be splited in two groups, trainig and testing
```{r,echo=TRUE}
in_train  <- createDataPartition(train$classe, p=0.75, list=FALSE)
training <- train[ in_train, ]
testing  <- train[-in_train, ]
```
We alredy have data, but to predict something with data, we need first know
what data we have.
Then, a brief exploratory data analisys
```{r,echo=TRUE}
# str(test)
nCols<-dim(test)[2]
```
I comented the line because the output it too long.
But, in the dat have `r ncols` columns and his class are
near zero values and NA
so we must remove that values
```{r, eco=TRUE}
nzv_var <- nearZeroVar(training)

training <- training[ , -nzv_var]
testing  <- testing [ , -nzv_var]
```
to NA's values, only will be removed the data how NA mean are above of 95%

```{r,echo=TRUE}
na_var <- sapply(training, function(x) mean(is.na(x))) > 0.95
training <- training[ , na_var == FALSE]
testing  <- testing [ , na_var == FALSE]
```

Now we have almost clean our data, now the first 5 columns will be removed 
because are only idex

```{r,echo=TRUE}
training <- training[ , -(1:5)]
testing  <- testing [ , -(1:5)]

```

Only train left.

first will use tree decition

```{r,echo=TRUE}
mod1<-train(classe ~ ., data=training,method="rpart")
```

this model, was relatively fat, so the accuracy will be checked

```{r,echo=TRUE}
predm1<- predict(mod1, newdata = testing)
matrix2 <- confusionMatrix(predm1, factor(testing$classe))
matrix2
```

The accuracy are so bad, and **"D"**" classe aren´t predicted.
This is easier to see in a graph

```{r,echo=TRUE}
fancyRpartPlot(mod1$finalModel)
```

Now is it is clearer that aren't good predictor.
We need to change it.

In this step, will be used random forest

```{r,echo=TRUE}
ctrl_RF <- trainControl(method = "repeatedcv", number = 5, repeats = 2)
fit_RF  <- train(classe ~ ., data = training, method = "rf",
                  trControl = ctrl_RF, verbose = FALSE)
```
Also, the accuracy will be checked

```{r,echo=TRUE}
predict_RF <- predict(fit_RF, newdata = testing)
conf_matrix_RF <- confusionMatrix(predict_RF, factor(testing$classe))
conf_matrix_RF
```

now the accuracy its to much better.
With this, we can predict our validation data-

```{r,echo=TRUE}
pred<-predict(fit_RF,newdata=validation)
pred
```

