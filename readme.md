---
title: "Classe prediction project"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

First we load in our data set.


```r
datatraining <- read_csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv")
datatesting <- read_csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv")
```


## Now we clean the data

To start, I sifted through the data and pulled out the columns that had no NAs nor any "DIV/0!". Perhaps not the most elegant solution but it ended up working!

```r
cleandatatraining<-datatraining[,c("pitch_arm", "pitch_forearm", "pitch_dumbbell", "pitch_belt", "roll_arm", "roll_forearm", "roll_dumbbell", "roll_belt", "yaw_arm", "yaw_forearm", "yaw_dumbbell", "yaw_belt", "classe")]
cleandatatesting<-datatesting[,c("pitch_arm", "pitch_forearm", "pitch_dumbbell", "pitch_belt", "roll_arm", "roll_forearm", "roll_dumbbell", "roll_belt", "yaw_arm", "yaw_forearm", "yaw_dumbbell", "yaw_belt", "problem_id")]
```


Now we create the training, testing, and validation sets:


```r
inBuild<-createDataPartition(y=cleandatatraining$classe ,p=0.7,list=FALSE)
validation<-cleandatatraining[-inBuild,]

builddata<-cleandatatraining[inBuild,]

inTrain<-createDataPartition(y=builddata$classe ,p=0.7,list=FALSE)
training<-builddata[inTrain,]
testing<-builddata[-inTrain,]
```

Now we make our models, I went with the random forest, gbm, lda, and rpart methods.

```r
mod1<-train(classe~., method="rf", data=training)
mod2<-train(classe~., method="gbm", data=training)
mod3<-train(classe~., method="lda", data=training)
mod4<-train(classe~., method="rpart", data=training)
```

I ran the predictions and made a data frame containing the outcome of the four predictions against the actual values in the testing$classe vector, and then again against the validation set.


```r
pred1<-predict(mod1,testing)
pred2<-predict(mod2,testing)
pred3<-predict(mod3,testing)
pred4<-predict(mod4,testing)


predDF<-data.frame(pred1,pred2,pred3,pred4,classe=testing$classe) 

pred1V<-predict(mod1,validation)
pred2V<-predict(mod2,validation)
pred3V<-predict(mod3,validation)
pred4V<-predict(mod4,validation)

predDFV<-data.frame(pred1=pred1V,pred2=pred2V,pred3=pred3V,pred4=pred4V, classe=validation$classe)
```

I quickly computed the accuracy against the testing set and against the validation set. The outcome probabilities are commented next to the code.


```r
(sum(predDF[,1]==predDF[,5])/4118) ##.983 
(sum(predDF[,2]==predDF[,5])/4118) ##.927
(sum(predDF[,3]==predDF[,5])/4118) ##.427
(sum(predDF[,4]==predDF[,5])/4118) ##.431

(sum(predDFV[,1]==predDFV[,5])/5885) ##.982
(sum(predDFV[,2]==predDFV[,5])/5885) ##.923
(sum(predDFV[,3]==predDFV[,5])/5885) ##.433
(sum(predDFV[,4]==predDFV[,5])/5885) ##.431
```


## Conclusions

Looking at the out of sample error for the validation set with respect to methods one and two (rf and gbm respectively), we see that both of them are less than 10%. To make my first round of predictions I then created a prediction model against the given test data of 20 cases


```r
finalpred1<-predict(mod1,cleandatatesting)
finalpred2<-predict(mod2,cleandatatesting)
finalpred3<-predict(mod3,cleandatatesting)
finalpred4<-predict(mod4,cleandatatesting)

finalpreddf<-data.frame(finalpred1,finalpred2,finalpred3,finalpred4)
```

I then compared the first two columns of the table, as those were the predictions with the highest accuracy, and if they matched, that would be my prediction for the classe. As it turns out, the first two columns always agreed! While my methods were admittedly a touch barbaric and uncouth, I decided to give the quiz a go with the above information, and all the predictions were correct~ 
