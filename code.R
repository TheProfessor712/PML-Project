library(caret)
library(readr)
library(randomForest)

##Loading in the date sets
datatraining <- read_csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv")
datatesting <- read_csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv")

##Isolating columns with no NA or DIV/0 strings
cleandatatraining<-datatraining[,c("pitch_arm", "pitch_forearm", "pitch_dumbbell", "pitch_belt", "roll_arm", "roll_forearm", "roll_dumbbell", "roll_belt", "yaw_arm", "yaw_forearm", "yaw_dumbbell", "yaw_belt", "classe")]
cleandatatesting<-datatesting[,c("pitch_arm", "pitch_forearm", "pitch_dumbbell", "pitch_belt", "roll_arm", "roll_forearm", "roll_dumbbell", "roll_belt", "yaw_arm", "yaw_forearm", "yaw_dumbbell", "yaw_belt", "problem_id")]

##Make training, testing, validation sets
inBuild<-createDataPartition(y=cleandatatraining$classe ,p=0.7,list=FALSE)
validation<-cleandatatraining[-inBuild,]

builddata<-cleandatatraining[inBuild,]

inTrain<-createDataPartition(y=builddata$classe ,p=0.7,list=FALSE)
training<-builddata[inTrain,]
testing<-builddata[-inTrain,]

##Makding the models
mod1<-train(classe~., method="rf", data=training)
mod2<-train(classe~., method="gbm", data=training)
mod3<-train(classe~., method="lda", data=training)
mod4<-train(classe~., method="rpart", data=training)
#mod4$finalmodel

##stand alone predictions on the testing set
pred1<-predict(mod1,testing)
pred2<-predict(mod2,testing)
pred3<-predict(mod3,testing)
pred4<-predict(mod4,testing)

##Combined prediction model
predDF<-data.frame(pred1,pred2,pred3,pred4,classe=testing$classe) 
#Use this to get accuracey
combModFit<-train(classe~.,method="rf", data=predDF)
combPred<-predict(combModFit,predDF)

##Stand alone predictions on the 20 test cases
finalpred1<-predict(mod1,cleandatatesting)
finalpred2<-predict(mod2,cleandatatesting)
finalpred3<-predict(mod3,cleandatatesting)
finalpred4<-predict(mod4,cleandatatesting)

finalpreddf<-data.frame(finalpred1,finalpred2,finalpred3,finalpred4)

##To get the accuracy
(sum(predDF[,1]==predDF[,5])/4118) ##.983 ##likely to change in run again!!
(sum(predDF[,2]==predDF[,5])/4118) ##.927
(sum(predDF[,3]==predDF[,5])/4118) ##.427
(sum(predDF[,4]==predDF[,5])/4118) ##.431

##Making the Validation set predictions to get the Out of Sample Error
pred1V<-predict(mod1,validation)
pred2V<-predict(mod2,validation)
pred3V<-predict(mod3,validation)
pred4V<-predict(mod4,validation)

predDFV<-data.frame(pred1=pred1V,pred2=pred2V,pred3=pred3V,pred4=pred4V, classe=validation$classe)

(sum(predDFV[,1]==predDFV[,5])/5885) ##.982 ##likely to change in run again!!
(sum(predDFV[,2]==predDFV[,5])/5885) ##.923
(sum(predDFV[,3]==predDFV[,5])/5885) ##.433
(sum(predDFV[,4]==predDFV[,5])/5885) ##.431