#Read in data
trainingdat <- read.csv("pml-training.csv", na.strings = c("#DIV/0!","","NA"))
testingdat <- read.csv("pml-testing.csv", na.strings = c("#DIV/0!","","NA"))

#Split data into training and testing data sets
inTrain <- createDataPartition(y=trainingdat$classe, p=0.1, list=FALSE)
trtesting <- trainingdat[-inTrain,]
trtraining <- trainingdat[inTrain,]

#Clean (remove) first 7 columns of data of both testing and training sets
trtraining <-trtraining[,-c(1:7)]
trtesting<-trtesting[,-c(1:7)]

#Remove columns with >60% NA values
trtraining <- trtraining[, colSums(is.na(trtraining)) < .4*nrow(trtraining)]
trtesting <- trtesting[, colSums(is.na(trtesting)) < .4*nrow(trtesting)]

#Using decision Tree
treeFit <- train(classe ~., data=trtraining, method = "rpart")
treeFit
treepredictions <- predict(treeFit, newdata=trtesting)
confusionMatrix(treepredictions, trtesting$classe)


#Using random forest model
rfFit <- train(classe ~., data=trtraining, method = "rf")
rfFit
rfpredictions <- predict(rfFit, newdata=trtesting)
confusionMatrix(rfpredictions, trtesting$classe)

##Use random forest model on testing Data
#Since, the accuracy of the random forest model was higher than the accuracy of the decision tree model, the random forest model will be used in predictions with the test set
model<- randomForest(classe ~. , data=trtraining, method="class")
pred <- predict(model ,testingdat)

#Create text files for submission
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}
pml_write_files(pred)