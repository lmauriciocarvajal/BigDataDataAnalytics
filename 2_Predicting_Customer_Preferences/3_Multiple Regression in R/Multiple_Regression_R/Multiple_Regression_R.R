install.packages("corrplot")

library(corrplot)

library(caret)
#Read the data
existing_products <- read.csv("Datasets\\existingproductattributes2017.csv")
# dummify the data
newDataFrame <- dummyVars(" ~ .", data = existing_products)

readyData <- data.frame(predict(newDataFrame, newdata = existing_products))

#To check the correlation between the variables in the data, all variables must not contain nominal data types.
str(readyData)

#Searching missing Data
summary(readyData)
#For now deleting the attribute with missing information
readyData$BestSellersRank<-NULL
#builing the correlation matrix
corrData<-cor(readyData)
corrData

corrplot(corrData)


#Creating a linear model
set.seed(123)

trainSize<-round(nrow(readyData)*0.75)
testSize<-nrow(readyData)-trainSize
training_indices<-sample(seq_len(nrow(readyData)),size =trainSize)
trainSet<-readyData[training_indices,]
testSet<-readyData[-training_indices,]


names(readyData)
LinearModel<-lm(Volume~ ProductType.PC+ ProductType.Laptop+ ProductType.Netbook+ProductType.Smartphone, trainSet)

#Summary of the model
summary(LinearModel)
plot(LinearModel)

#Creating the datas sets for the training and testing.
#Our dependen variable is $volume 
#It's taking 75% of the data for training and 25% for testing or verification. 
inTrain <- createDataPartition(y = readyData$Volume,
                               p = .75, # As requested, will split the data in 75%
                               list = FALSE)

trainSet <- readyData[ inTrain,]
testSet <- readyData[-inTrain,]

#SVM

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

svm_LinearTime<-system.time(svm_Linear <- train(Volume ~., 
                    data = trainSet, 
                    method = "svmLinear",
                    trControl=trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10))

svm_Linear

#######################################################################################
#Creating RF Model with custon grid
#######################################################################################
#This is not needed, we can reused, but I just leaved due to academic purposes. 
rfmodelControl <- trainControl(method = "repeatedcv", number = 10,repeats = 3)
#dataframe for manual tuning of mtry
rfGrid <- expand.grid(mtry=c(4,5,6,8,10))

#traning the model with random forest
rfmodel1Time<-system.time(rfmodel <- train(Volume~., 
                                           data = trainSet, 
                                           method = "rf", 
                                           trControl=rfmodelControl, 
                                           #tuneLength = 1,
                                           tuneGrid=rfGrid, 
                                           preProc = c("center", "scale")))
