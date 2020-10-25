#######################################################################################
#Mauricio Carvajal
#######################################################################################
install.packages("mime")
install.packages("fastmap")
install.packages("corrplot")
install.packages("rmarkdown")
install.packages("farver")

#######################################################################################
#Libraries needed for the script.
#######################################################################################
library(doParallel)
library(plotly)
library(caret) #caret-->clasificacion and regresion testing
library(C50)
library(plotrix)
library(ggplot2)
library(corrplot)
library(varImp)
library(e1071)  # For SVN
library(kknn)   # For KNN
library(dplyr)
library (ggplot2)

#######################################################################################
#Set up Parallel Processing
#######################################################################################
# Required library(doParallel)

# Find how many cores are on your machine
detectCores() # Result = Typically 4 to 6, I have 8 core so I will use 6
# Create Cluster with desired number of cores. Don't use them all! Your computer is running other processes. 
cl <- makeCluster(4)

# Register Cluster
registerDoParallel(cl)

# Confirm how many cores are now "assigned" to R and RStudio
getDoParWorkers() # Result 6, due to I have 8 core

# Stop Cluster. After performing your tasks, stop your cluster.
#stopCluster(cl)


#######################################################################################
#Getting the data
#######################################################################################
iphoneSmallMatrix <- read.csv("Data\\iphone_smallmatrix_labeled_8d.csv")
str(iphoneSmallMatrix)
iphoneDF<-iphoneSmallMatrix

#######################################################################################
#Getting the data
#######################################################################################
#Structure
str(iphoneSmallMatrix)

#Checking the summary
summary(iphoneSmallMatrix)

#Plotting iphone sentiment
plot_ly(iphoneSmallMatrix, x= ~iphoneSmallMatrix$iphonesentiment, type='histogram')

#Checking Missing Data
is.na(iphoneSmallMatrix)


#######################################################################################
#######################################################################################
# Preprocessing & Feature Selection
#######################################################################################
#######################################################################################


#######################################################################################
# Correlation
#######################################################################################
#Creating the matrix of correlation
corrData <- cor(iphoneSmallMatrix)
#Printing the results
corrData
#Ploting the matrix of correlation
corrplot(corrData)

#Using the function findcorrelation to see the colleration between the variables
findCorrelation(corrData, cutoff = 0.9, 
                verbose = FALSE, 
                names = TRUE,
                exact = ncol(corrData) < 100)
#From the analsysis, we are going to delete the independent variables that has a correlation 
#up of 90 in order to avoid collinearity

# create a new data set and remove features highly correlated with the dependant

iphoneCOR <- iphoneSmallMatrix
iphoneCOR$iosperneg <- NULL
iphoneCOR$samsungdisneg <- NULL
iphoneCOR$samsungdispos <- NULL
iphoneCOR$googleperneg <- NULL
iphoneCOR$samsungdisunc <- NULL
iphoneCOR$nokiacamunc <- NULL
iphoneCOR$nokiadisneg <- NULL
iphoneCOR$nokiaperunc <- NULL
iphoneCOR$nokiaperneg <- NULL
iphoneCOR$nokiacamneg <- NULL
iphoneCOR$ios <- NULL
iphoneCOR$htcphone <- NULL
iphoneCOR$iosperunc <- NULL


#######################################################################################
# Examine Feature Variance
#######################################################################################

#Using nearZeroVar() from the caret package.  
#Features with no variance can be said to hold little to no information. Features that have very 
#little, or "near zero variance", may or may not have useful information. 

#nearZeroVar() with saveMetrics = TRUE returns an object containing a table including: frequency ratio, 
#percentage unique, zero variance and near zero variance 
nzvMetrics <- nearZeroVar(iphoneSmallMatrix, saveMetrics = TRUE)
nzvMetrics

# nearZeroVar() with saveMetrics = FALSE returns an vector 
nzv <- nearZeroVar(iphoneSmallMatrix, saveMetrics = FALSE) 
nzv


# create a new data set and remove near zero variance features
iphoneNZV <- iphoneSmallMatrix[,-nzv]
str(iphoneNZV)


#######################################################################################
# Recursive Feature Elimination 
#######################################################################################
#Converting the y variable to char
# Let's sample the data before using RFE
set.seed(123)


iphoneSample <- iphoneSmallMatrix[sample(1:nrow(iphoneSmallMatrix), 1000, replace=FALSE),]

# Set up rfeControl with randomforest, repeated cross validation and no updates
ctrl <- rfeControl(functions = rfFuncs, 
                   method = "repeatedcv",
                   repeats = 5,
                   verbose = FALSE)

# Use rfe and omit the response variable (attribute 59 iphonesentiment) 
rfeResults <- rfe(iphoneSample[,1:58], 
                  iphoneSample$iphonesentiment, 
                  sizes=(1:58), 
                  rfeControl=ctrl)

# Get results
rfeResults

# Plot results
plot(rfeResults, type=c("g", "o"))

# create new data set with rfe recommended features
iphoneRFE <- iphoneSmallMatrix[,predictors(rfeResults)]

# add the dependent variable to iphoneRFE
iphoneRFE$iphonesentiment <- iphoneDF$iphonesentiment

# review outcome
str(iphoneRFE)


#######################################################################################
# DataSets available
#######################################################################################
iphoneDF 
iphoneCOR
iphoneNZV
iphoneRFE

#######################################################################################
#Converting the numeric Values into categorical.
#######################################################################################
str(iphoneSmallMatrix$iphonesentiment)
iphoneSmallMatrix$iphonesentiment<-as.factor(iphoneSmallMatrix$iphonesentiment)
str(iphoneSmallMatrix$iphonesentiment)
iphoneDF$iphonesentiment<-as.factor(iphoneDF$iphonesentiment)
iphoneCOR$iphonesentiment<-as.factor(iphoneCOR$iphonesentiment)
iphoneNZV$iphonesentiment<-as.factor(iphoneNZV$iphonesentiment)
iphoneRFE$iphonesentiment<-as.factor(iphoneRFE$iphonesentiment)
str(iphoneRFE)
#######################################################################################
#Creating the Data Set
#######################################################################################

#Seed needed for repeability 
set.seed(123)
#Creating the datas sets for the training and testing.
#Our dependen variable is $brand.
#It's taking 75% of the data for training and 25% for testing or verification. 
inTrain <- createDataPartition(y = iphoneRFE$iphonesentiment,
                               p = .75, # As requested, will split the data in 75%
                               list = FALSE)
#Creating the training and testing datasets.
trainingIphoneRFE <- iphoneRFE[ inTrain,]
testingIphoneRFE <- iphoneRFE[-inTrain,]

#######################################################################################
#Creating the model c50
#######################################################################################

#Starting the training
#10 fold cross validation
c50modelControlIphoneRPE <- trainControl(
                                      method = "repeatedcv", 
                                      number = 10,
                                      repeats = 1)

c50modelTimeIphoneRPE<-system.time(c50modelIphoneRPE <- train(iphonesentiment~., 
                                            data = trainingIphoneRFE, 
                                            method = "C5.0", 
                                            trControl=c50modelControlIphoneRPE, 
                                            #tuneLength = 1,
                                            preProc = c("center", "scale"),
                                            verbose = TRUE))

#Printing the model already trained
c50modelIphoneRPE
#Testime
c50modelTimeIphoneRPE
#varImp(c50model, scale =FALSE)
#Here we review how the model prioritized each feature in the training.
#varImp(c50modelIphoneRPE)

#######################################################################################
#Creating RF Model 
#######################################################################################
set.seed(123)
#This is not needed, we can reused, but I just leaved due to academic purposes. 
rfmodelControlIphoneRPE <- trainControl(method = "repeatedcv", number = 10,repeats = 1)


#traning the model with random forest
rfmodelTimeIphoneRPE<-system.time(rfmodelIphoneRPE <- train(iphonesentiment~., 
                                           data = trainingIphoneRFE, 
                                           method = "rf", 
                                           trControl=rfmodelControlIphoneRPE, 
                                           #tuneLength = 1,
                                           preProc = c("center", "scale"),
                                           verbose = TRUE))

#training results
rfmodelIphoneRPE
#Testtime
rfmodelTimeIphoneRPE


#######################################################################################
#Creating KNN Model 
#######################################################################################
set.seed(123)
#This is not needed, we can reused, but I just leaved due to academic purposes. 
#knnmodelControlIphoneRPE <- trainControlIphoneRPE(method = "repeatedcv", number = 10,repeats = 3)

#traning the model with random forest
#knnmodelTimeIphoneRPE<-system.time(knnmodelIphoneRPE <- train(iphonesentiment~., 
#                                                            data = trainingIphoneRFE, 
#                                                            method = "knn", 
#                                                            trControl=knnmodelControlIphoneRPE, 
#                                                            #tuneLength = 1,
#                                                            preProc = c("center", "scale"),
#                                                            verbose = TRUE))


knnmodelTimeIphoneRPE<-system.time(knnmodelIphoneRPE <- train.kknn(iphonesentiment ~ ., 
                                                                   data = trainingIphoneRFE)) #, 
                                                                   #kmax = 9))


#training results
knnmodelIphoneRPE
#Testtime
knnmodelTimeIphoneRPE

#######################################################################################
#Creating svn Model 
#######################################################################################
set.seed(123)
#using library("e1071")
svm_modelTimeIphoneRPE<-system.time(svm_modelIphoneRPE <- svm(iphonesentiment ~ ., data=trainingIphoneRFE))

summary(svm_modelIphoneRPE)
#training results
svm_modelIphoneRPE
#Testtime
svm_modelTimeIphoneRPE


#snvmodelControlIphoneRPE <- trainControlIphoneRPE(method = "repeatedcv", number = 10,repeats = 3)
#traning the model with random forest
#snvmodelTimeIphoneRPE2<-system.time(svnmodelIphoneRPE <- train(iphonesentiment~., 
#                                                            data = trainingIphoneRFE, 
#                                                            method = "svn", 
#                                                            trControl=snvmodelControlIphoneRPE, 
#                                                            #tuneLength = 1,
#                                                            preProc = c("center", "scale"),
#                                                            verbose = TRUE))


#######################################################################################
#######################################################################################
#
#Comparing Results
#
#######################################################################################
#######################################################################################

#Printing the model already trained
c50modelIphoneRPE
#Testime
c50modelTimeIphoneRPE

#training results
rfmodelIphoneRPE
#Testtime
rfmodelTimeIphoneRPE

#training results
svm_modelIphoneRPE
#Testtime
svm_modelTimeIphoneRPE
#Need addional step do evaluate the accuracy and the Kappa, using the training data set, an Post Resamp
svm_modelIphoneRPEModel <- predict(svm_modelIphoneRPE, newdata = trainingIphoneRFE)
postResampleDataSVNIphoneRPE<-postResample(pred=svm_modelIphoneRPEModel,obs=trainingIphoneRFE$iphonesentiment)
#This is the value of the model, due to I use training in the Dataset
postResampleDataSVNIphoneRPE

#training results
knnmodelIphoneRPE
#Testtime
knnmodelTimeIphoneRPE
#Need addional step do evaluate the accuracy and the Kappa, using the training data set, an Post Resamp
knnmodelIphoneRPEModel <- predict(c50modelIphoneRPE, newdata = trainingIphoneRFE)
postResampleDatakknnIphoneRPE<-postResample(pred=knnmodelIphoneRPEModel,obs=trainingIphoneRFE$iphonesentiment)
#This is the value of the model, due to I use training in the Dataset
postResampleDatakknnIphoneRPE



#######################################################################################
#Comparing the models in tearms of resampling
#######################################################################################
resamps <- resamples(list(C5.0 = c50modelIphoneRPE, rf = rfmodelIphoneRPE))#, svm=svm_modelIphoneRPE))#, kknn=knnmodelIphoneRPE, svm=svm_modelIphoneRPE)) 
summary(resamps)

# boxplots of results
bwplot(resamps)
# dot plots of results
dotplot(resamps)

#Visualizing resamps
xyplot(resamps, what = "BlandAltman")

diffs <- diff(resamps) 
summary(diffs)
#Comparing Testing 
c50model2Time
rfmodelTestTime

#######################################################################################
#
#                                  Prediction part
#
#######################################################################################


#######################################################################################
#  c50
#######################################################################################
c50modelIphoneRPEPredition <- predict(c50modelIphoneRPE, newdata = testingIphoneRFE) 
str(c50modelIphoneRPEPredition)
summary(c50modelIphoneRPEPredition)
c50modelIphoneRPEPredition

#Confusion Matrix
ConfusionMatrixC50IphoneRPE<-confusionMatrix(data = c50modelIphoneRPEPredition, testingIphoneRFE$iphonesentiment)

#Using the test set use postResample() to assess the metrics of the new predictions 
#compared to the Ground Truth (see the resources for more information)
postResampleDataC50IphoneRPE<-postResample(pred=c50modelIphoneRPEPredition,obs=testingIphoneRFE$iphonesentiment)
postResampleDataC50IphoneRPE
plot(postResampleDataC50IphoneRPE)


#######################################################################################
#  RF
#######################################################################################
rfmodelIphoneRPEPredition <- predict(rfmodelIphoneRPE, newdata = testingIphoneRFE) 
str(rfmodelIphoneRPEPredition)
summary(rfmodelIphoneRPEPredition)

#Confusion Matrix
ConfusionMatrixRFIphoneRPE<-confusionMatrix(data = rfmodelIphoneRPEPredition, testingIphoneRFE$iphonesentiment)

#Using the test set use postResample() to assess the metrics of the new predictions 
#compared to the Ground Truth (see the resources for more information)
# CompleteResponses and SurveyIncomplete
postResampleDataRFIphoneRPE<-postResample(pred=rfmodelIphoneRPEPredition,obs=testingIphoneRFE$iphonesentiment)
postResampleDataRFIphoneRPE
plot(postResampleDataRFIphoneRPE)

#######################################################################################
# SVM
#######################################################################################
svm_modelIphoneRPEPredition <- predict(svm_modelIphoneRPE, newdata = testingIphoneRFE) 
str(svm_modelIphoneRPEPredition)
summary(svm_modelIphoneRPEPredition)

#Confusion Matrix
ConfusionMatrixSVNIphoneRPE<-confusionMatrix(data = svm_modelIphoneRPEPredition, testingIphoneRFE$iphonesentiment)

#Using the test set use postResample() to assess the metrics of the new predictions 
#compared to the Ground Truth (see the resources for more information)
# CompleteResponses and SurveyIncomplete
postResampleDataSVNIphoneRPE<-postResample(pred=svm_modelIphoneRPEPredition,obs=testingIphoneRFE$iphonesentiment)
postResampleDataSVNIphoneRPE
plot(postResampleDataSVNIphoneRPE)

#######################################################################################
# Kknn
#######################################################################################
knnmodelIphoneRPEPredition <- predict(c50modelIphoneRPE, newdata = testingIphoneRFE) 
str(knnmodelIphoneRPEPredition)
summary(knnmodelIphoneRPEPredition)

#Confusion Matrix
ConfusionMatrixkknnIphoneRPE<-confusionMatrix(data = knnmodelIphoneRPEPredition, testingIphoneRFE$iphonesentiment)
ConfusionMatrixkknnIphoneRPE

#Using the test set use postResample() to assess the metrics of the new predictions 
#compared to the Ground Truth (see the resources for more information)
# CompleteResponses and SurveyIncomplete
postResampleDatakknnIphoneRPE<-postResample(pred=knnmodelIphoneRPEPredition,obs=testingIphoneRFE$iphonesentiment)
postResampleDatakknnIphoneRPE
plot(postResampleDatakknnIphoneRPE)

#######################################################################################
# Comparing PostResamp
#######################################################################################
#Predition of the model c50
postResampleDataC50IphoneRPE
#Predition of the model RF
postResampleDataRFIphoneRPE
#Predition of the model SVN
postResampleDataSVNIphoneRPE
#Predition of the model Knn
postResampleDatakknnIphoneRPE





#######################################################################################
#
#    Apling the seleted classifier to the different data sets that has been preproced
#
#######################################################################################

#######################################################################################
#Creating the model c50 IphoneRPE repeats 3
#######################################################################################

#Creating the Data Set

#Seed needed for repeability 
set.seed(123)
#Creating the datas sets for the training and testing.
#It's taking 75% of the data for training and 25% for testing or verification. 
inTrain <- createDataPartition(y = iphoneRFE$iphonesentiment,
                               p = .70, # As requested, will split the data in 70%
                               list = FALSE)
#Creating the training and testing datasets.
trainingIphoneRFE <- iphoneRFE[ inTrain,]
testingIphoneRFE <- iphoneRFE[-inTrain,]


#Starting the training
#10 fold cross validation
c50modelControlIphoneRPE <- trainControl(
  method = "repeatedcv", 
  number = 10,
  repeats = 3)

c50modelTimeIphoneRPE<-system.time(c50modelIphoneRPE <- train(iphonesentiment~., 
                                                              data = trainingIphoneRFE, 
                                                              method = "C5.0", 
                                                              trControl=c50modelControlIphoneRPE, 
                                                              #tuneLength = 1,
                                                              preProc = c("center", "scale"),
                                                              verbose = TRUE))

#Printing the model already trained
c50modelIphoneRPE
#Testime
c50modelTimeIphoneRPE

#######################################################################################
#Creating the model c50 iphoneCOR repeats 3
#######################################################################################

#Creating the Data Set

#Seed needed for repeability 
set.seed(123)
#Creating the datas sets for the training and testing.
#It's taking 75% of the data for training and 25% for testing or verification. 
inTrain <- createDataPartition(y = iphoneCOR$iphonesentiment,
                               p = .70, # As requested, will split the data in 70%
                               list = FALSE)
#Creating the training and testing datasets.
trainingiphoneCOR <- iphoneCOR[ inTrain,]
testingiphoneCOR <- iphoneCOR[-inTrain,]


#Starting the training
#10 fold cross validation
c50modelControliphoneCOR <- trainControl(
  method = "repeatedcv", 
  number = 10,
  repeats = 3)

c50modelTimeiphoneCOR<-system.time(c50modeliphoneCOR <- train(iphonesentiment~., 
                                                              data = trainingiphoneCOR, 
                                                              method = "C5.0", 
                                                              trControl=c50modelControliphoneCOR, 
                                                              #tuneLength = 1,
                                                              preProc = c("center", "scale"),
                                                              verbose = TRUE))

#Printing the model already trained
c50modeliphoneCOR
#Testime
c50modelTimeiphoneCOR

#######################################################################################
#Creating the model c50 iphoneNZV repeats 3
#######################################################################################

#Creating the Data Set

#Seed needed for repeability 
set.seed(123)
#Creating the datas sets for the training and testing.
#It's taking 75% of the data for training and 25% for testing or verification. 
inTrain <- createDataPartition(y = iphoneNZV$iphonesentiment,
                               p = .70, # As requested, will split the data in 70%
                               list = FALSE)
#Creating the training and testing datasets.
trainingiphoneNZV <- iphoneNZV[ inTrain,]
testingiphoneNZV <- iphoneNZV[-inTrain,]


#Starting the training
#10 fold cross validation
c50modelControliphoneNZV <- trainControl(
  method = "repeatedcv", 
  number = 10,
  repeats = 3)

c50modelTimeiphoneNZV<-system.time(c50modeliphoneNZV <- train(iphonesentiment~., 
                                                              data = trainingiphoneNZV, 
                                                              method = "C5.0", 
                                                              trControl=c50modelControliphoneNZV, 
                                                              #tuneLength = 1,
                                                              preProc = c("center", "scale"),
                                                              verbose = TRUE))

#Printing the model already trained
c50modeliphoneNZV
#Testime
c50modelTimeiphoneNZV

#######################################################################################
#Creating the model c50 iphoneRF repeats 3
#######################################################################################

#Creating the Data Set

#Seed needed for repeability 
set.seed(123)
#Creating the datas sets for the training and testing.
#It's taking 75% of the data for training and 25% for testing or verification. 
inTrain <- createDataPartition(y = iphoneDF$iphonesentiment,
                               p = .70, # As requested, will split the data in 70%
                               list = FALSE)
#Creating the training and testing datasets.
trainingiphoneDF <- iphoneDF[ inTrain,]
testingiphoneDF <- iphoneDF[-inTrain,]


#Starting the training
#10 fold cross validation
c50modelControliphoneDF <- trainControl(
  method = "repeatedcv", 
  number = 10,
  repeats = 3)

c50modelTimeiphoneDF<-system.time(c50modeliphoneDF <- train(iphonesentiment~., 
                                                              data = trainingiphoneDF, 
                                                              method = "C5.0", 
                                                              trControl=c50modelControliphoneDF, 
                                                              #tuneLength = 1,
                                                              preProc = c("center", "scale"),
                                                              verbose = TRUE))

#Printing the model already trained
c50modeliphoneDF
#Testime
c50modelTimeiphoneDF

#######################################################################################
#Comparing the resutls
#######################################################################################
#using the RPE DataSet
c50modelTimeIphoneRPE
c50modelIphoneRPE
#using the COR DataSet
c50modelTimeiphoneCOR
c50modeliphoneCOR
#using the NZV DataSet
c50modelTimeiphoneNZV
c50modeliphoneNZV #Best Model
#Using the full dataSet
c50modelTimeiphoneDF
c50modeliphoneDF

#######################################################################################
#######################################################################################
# Optional PART FEATURE ENGINEERING
#######################################################################################
#######################################################################################

#######################################################################################
#Engineering the Dependant variable
#######################################################################################

# create a new dataset that will be used for recoding sentiment
iphoneRC <- iphoneDF
# recode sentiment to combine factor levels 0 & 1 and 4 & 5
iphoneRC$iphonesentiment <- recode(iphoneRC$iphonesentiment, '0' = 1, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 4) 
# inspect results
summary(iphoneRC)
str(iphoneRC)
# make iphonesentiment a factor
iphoneRC$iphonesentiment <- as.factor(iphoneRC$iphonesentiment)



#Creating the Data Set

#Seed needed for repeability 
set.seed(123)
#Creating the datas sets for the training and testing.
#It's taking 75% of the data for training and 25% for testing or verification. 
inTrain <- createDataPartition(y = iphoneRC$iphonesentiment,
                               p = .70, # As requested, will split the data in 70%
                               list = FALSE)
#Creating the training and testing datasets.
trainingiphoneRC <- iphoneRC[ inTrain,]
testingiphoneRC <- iphoneRC[-inTrain,]


#Starting the training
#10 fold cross validation
c50modelControliphoneRC <- trainControl(
  method = "repeatedcv", 
  number = 10,
  repeats = 3)

c50modelTimeiphoneRC<-system.time(c50modeliphoneRC <- train(iphonesentiment~., 
                                                              data = trainingiphoneRC, 
                                                              method = "C5.0", 
                                                              trControl=c50modelControliphoneRC, 
                                                              #tuneLength = 1,
                                                              preProc = c("center", "scale"),
                                                              verbose = TRUE))

#Printing the model already trained
c50modeliphoneRC
#Testime
c50modelTimeiphoneRC

#######################################################################################
# Principal Component Analysis 
#######################################################################################
iphoneRC <- iphoneDF
#data = training and testing from iphoneDF (no feature selection) 
# create object containing centered, scaled PCA components from training set
# excluded the dependent variable and set threshold to .95
#preprocessParams <- preProcess(training[,-59], method=c("center", "scale", "pca"), thresh = 0.95)
#print(preprocessParams)

# use predict to apply pca parameters, create training, exclude dependant
#train.pca <- predict(preprocessParams, training[,-59])

# add the dependent to training
#train.pca$iphonesentiment <- training$iphonesentiment

# use predict to apply pca parameters, create testing, exclude dependant
#test.pca <- predict(preprocessParams, testing[,-59])

# add the dependent to training
#test.pca$iphonesentiment <- testing$iphonesentiment

# inspect results
#str(train.pca)
#str(test.pca)


#######################################################################################
#Getting the data
#######################################################################################
iphoneLargeMatrix <- read.csv("Data\\LargeMatrix.csv")

#######################################################################################
# Examine Feature Variance For La
#######################################################################################
# create a new data set and remove near zero variance features
IphoneLargeMatrixPrediction<- iphoneLargeMatrix[,-nzv]
IphoneLargeMatrixPrediction<-iphoneLargeMatrix
str(IphoneLargeMatrixPrediction)


#######################################################################################
# Predicting
#######################################################################################

iphoneSentimentPredition <- predict(c50modeliphoneNZV, newdata = IphoneLargeMatrixPrediction) 
str(iphoneSentimentPredition)
summary(iphoneSentimentPredition)


#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################
#GALAXY
#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################

#######################################################################################
#Getting the data
#######################################################################################
galaxySmallMatrix <- read.csv("Data\\galaxy_smallmatrix_labeled_9d.csv")
galaxyDF<-galaxySmallMatrix

#######################################################################################
#Getting the data
#######################################################################################
#Structure
str(galaxySmallMatrix)

#Checking the summary
summary(galaxySmallMatrix)

#Plotting galaxy sentiment
plot_ly(galaxySmallMatrix, x= ~galaxySmallMatrix$galaxysentiment, type='histogram')

#Checking Missing Data
is.na(galaxySmallMatrix)


#######################################################################################
#######################################################################################
# Preprocessing & Feature Selection
#######################################################################################
#######################################################################################


#######################################################################################
# Correlation
#######################################################################################
#Creating the matrix of correlation
corrDataGalaxy <- cor(galaxySmallMatrix)
#Printing the results
corrDataGalaxy
#Ploting the matrix of correlation
corrplot(corrDataGalaxy)

#Using the function findcorrelation to see the colleration between the variables
findCorrelation(corrDataGalaxy, cutoff = 0.9, 
                verbose = FALSE, 
                names = TRUE,
                exact = ncol(corrDataGalaxy) < 100)
#From the analsysis, we are going to delete the independent variables that has a correlation 
#up of 90 in order to avoid collinearity

# create a new data set and remove features highly correlated with the dependant

galaxyCOR <- galaxySmallMatrix
galaxyCOR$samsungdisneg<- NULL
galaxyCOR$samsungdispos <- NULL
galaxyCOR$googleperneg <- NULL 
galaxyCOR$samsungdisunc<- NULL 
galaxyCOR$nokiacamunc<- NULL   
galaxyCOR$nokiadisneg<- NULL  
galaxyCOR$nokiaperunc<- NULL  
galaxyCOR$nokiaperneg<- NULL   
galaxyCOR$nokiacamneg<- NULL   
galaxyCOR$iosperunc<- NULL     
galaxyCOR$iosperneg<- NULL     
galaxyCOR$sonydisneg<- NULL    
galaxyCOR$ios<- NULL
galaxyCOR$tcphone<- NULL


#######################################################################################
# Examine Feature Variance
#######################################################################################

#Using nearZeroVar() from the caret package.  
#Features with no variance can be said to hold little to no information. Features that have very 
#little, or "near zero variance", may or may not have useful information. 

#nearZeroVar() with saveMetrics = TRUE returns an object containing a table including: frequency ratio,
str(galaxySmallMatrix)
#percentage unique, zero variance and near zero variance 
nzvMetricsGalaxy <- nearZeroVar(galaxySmallMatrix, saveMetrics = TRUE)
nzvMetricsGalaxy

# nearZeroVar() with saveMetrics = FALSE returns an vector 
nzvGalaxy <- nearZeroVar(galaxySmallMatrix, saveMetrics = FALSE) 
nzvGalaxy


# create a new data set and remove near zero variance features
galaxyNZV <- galaxySmallMatrix[,-nzvGalaxy]
str(galaxyNZV)


#######################################################################################
# Recursive Feature Elimination 
#######################################################################################
#Converting the y variable to char
# Let's sample the data before using RFE
set.seed(123)


galaxySample <- galaxySmallMatrix[sample(1:nrow(galaxySmallMatrix), 1000, replace=FALSE),]

# Set up rfeControl with randomforest, repeated cross validation and no updates
ctrl <- rfeControl(functions = rfFuncs, 
                   method = "repeatedcv",
                   repeats = 5,
                   verbose = FALSE)

# Use rfe and omit the response variable (attribute 59 galaxysentiment) 
rfeResultsGalaxy <- rfe(galaxySample[,1:58], 
                  galaxySample$galaxysentiment, 
                  sizes=(1:58), 
                  rfeControl=ctrl)

# Get results
str(rfeResultsGalaxy)

# Plot results
plot(rfeResultsGalaxy, type=c("g", "o"))

# create new data set with rfe recommended features
galaxyRFE <- galaxySmallMatrix[,predictors(rfeResultsGalaxy)]

# add the dependent variable to galaxyRFE
galaxyRFE$galaxysentiment <- galaxyDF$galaxysentiment

# review outcome
str(galaxyRFE)


#######################################################################################
# DataSets available
#######################################################################################
galaxyDF 
galaxyCOR
galaxyNZV
galaxyRFE

#######################################################################################
#Converting the numeric Values into categorical.
#######################################################################################
str(galaxySmallMatrix$galaxysentiment)
galaxySmallMatrix$galaxysentiment<-as.factor(galaxySmallMatrix$galaxysentiment)
str(galaxySmallMatrix$galaxysentiment)
galaxyDF$galaxysentiment<-as.factor(galaxyDF$galaxysentiment)
galaxyCOR$galaxysentiment<-as.factor(galaxyCOR$galaxysentiment)
galaxyNZV$galaxysentiment<-as.factor(galaxyNZV$galaxysentiment)
galaxyRFE$galaxysentiment<-as.factor(galaxyRFE$galaxysentiment)
str(galaxyRFE)
#######################################################################################
#Creating the Data Set
#######################################################################################

#Seed needed for repeability 
set.seed(123)
#Creating the datas sets for the training and testing.
#Our dependen variable is $brand.
#It's taking 75% of the data for training and 25% for testing or verification. 
inTrain <- createDataPartition(y = galaxyRFE$galaxysentiment,
                               p = .75, # As requested, will split the data in 75%
                               list = FALSE)
#Creating the training and testing datasets.
traininggalaxyRFE <- galaxyRFE[ inTrain,]
testinggalaxyRFE <- galaxyRFE[-inTrain,]

#######################################################################################
#Creating the model c50
#######################################################################################

#Starting the training
#10 fold cross validation
c50modelControlgalaxyRPE <- trainControl(
  method = "repeatedcv", 
  number = 10,
  repeats = 1)

c50modelTimegalaxyRPE<-system.time(c50modelgalaxyRPE <- train(galaxysentiment~., 
                                                              data = traininggalaxyRFE, 
                                                              method = "C5.0", 
                                                              trControl=c50modelControlgalaxyRPE, 
                                                              #tuneLength = 1,
                                                              preProc = c("center", "scale"),
                                                              verbose = TRUE))

#Printing the model already trained
c50modelgalaxyRPE
#Testime
c50modelTimegalaxyRPE
#varImp(c50model, scale =FALSE)
#Here we review how the model prioritized each feature in the training.
#varImp(c50modelgalaxyRPE)

#######################################################################################
#Creating RF Model 
#######################################################################################
set.seed(123)
#This is not needed, we can reused, but I just leaved due to academic purposes. 
rfmodelControlgalaxyRPE <- trainControl(method = "repeatedcv", number = 10,repeats = 1)


#traning the model with random forest
rfmodelTimegalaxyRPE<-system.time(rfmodelgalaxyRPE <- train(galaxysentiment~., 
                                                            data = traininggalaxyRFE, 
                                                            method = "rf", 
                                                            trControl=rfmodelControlgalaxyRPE, 
                                                            #tuneLength = 1,
                                                            preProc = c("center", "scale"),
                                                            verbose = TRUE))

#training results
rfmodelgalaxyRPE
#Testtime
rfmodelTimegalaxyRPE


#######################################################################################
#Creating KNN Model 
#######################################################################################
set.seed(123)
#This is not needed, we can reused, but I just leaved due to academic purposes. 
#knnmodelControlgalaxyRPE <- trainControl(method = "repeatedcv", number = 10,repeats = 3)

#traning the model with random forest
#knnmodelTimegalaxyRPE2<-system.time(knnmodelgalaxyRPE2 <- train(galaxysentiment~., 
#                                                            data = traininggalaxyRFE, 
#                                                            method = "knn", 
#                                                            trControl=knnmodelControlgalaxyRPE, 
#                                                            #tuneLength = 1,
#                                                            preProc = c("center", "scale"),
#                                                            verbose = TRUE))


knnmodelTimegalaxyRPE<-system.time(knnmodelgalaxyRPE <- train.kknn(galaxysentiment ~ ., 
                                                                   data = traininggalaxyRFE)) #, 
#kmax = 9))


#training results
knnmodelgalaxyRPE
#Testtime
knnmodelTimegalaxyRPE

#######################################################################################
#Creating svn Model 
#######################################################################################
set.seed(123)
#using library("e1071")
svm_modelTimegalaxyRPE<-system.time(svm_modelgalaxyRPE <- svm(galaxysentiment ~ ., data=traininggalaxyRFE))

summary(svm_modelgalaxyRPE)
#training results
svm_modelgalaxyRPE
#Testtime
svm_modelTimegalaxyRPE


#snvmodelControlgalaxyRPE <- trainControl(method = "repeatedcv", number = 10,repeats = 3)
#traning the model with random forest
#snvmodelTimegalaxyRPE2<-system.time(svnmodelgalaxyRPE <- train(galaxysentiment~., 
#                                                            data = traininggalaxyRFE, 
#                                                            method = "svn", 
#                                                            trControl=snvmodelControlgalaxyRPE, 
#                                                            #tuneLength = 1,
#                                                            preProc = c("center", "scale"),
#                                                            verbose = TRUE))


#######################################################################################
#######################################################################################
#
#Comparing Results
#
#######################################################################################
#######################################################################################

#Printing the model already trained
c50modelgalaxyRPE
#Testime
c50modelTimegalaxyRPE

#training results
rfmodelgalaxyRPE
#Testtime
rfmodelTimegalaxyRPE

#training results
svm_modelgalaxyRPE
#Testtime
svm_modelTimegalaxyRPE
#Need addional step do evaluate the accuracy and the Kappa, using the training data set, an Post Resamp
svm_modelgalaxyRPEModel <- predict(svm_modelgalaxyRPE, newdata = traininggalaxyRFE)
postResampleDataSVNgalaxyRPE<-postResample(pred=svm_modelgalaxyRPEModel,obs=traininggalaxyRFE$galaxysentiment)
#This is the value of the model, due to I use training in the Dataset
postResampleDataSVNgalaxyRPE

#training results
knnmodelgalaxyRPE
#Testtime
knnmodelTimegalaxyRPE
#Need addional step do evaluate the accuracy and the Kappa, using the training data set, an Post Resamp
knnmodelgalaxyRPEModel <- predict(c50modelgalaxyRPE, newdata = traininggalaxyRFE)
postResampleDatakknngalaxyRPE<-postResample(pred=knnmodelgalaxyRPEModel,obs=traininggalaxyRFE$galaxysentiment)
#This is the value of the model, due to I use training in the Dataset
postResampleDatakknngalaxyRPE





#######################################################################################
#Comparing the models in tearms of resampling
#######################################################################################
resamps <- resamples(list(C5.0 = c50modelgalaxyRPE, rf = rfmodelgalaxyRPE))#, svm=svm_modelgalaxyRPE))#, kknn=knnmodelgalaxyRPE, svm=svm_modelgalaxyRPE)) 
summary(resamps)

# boxplots of results
bwplot(resamps)
# dot plots of results
dotplot(resamps)

#Visualizing resamps
xyplot(resamps, what = "BlandAltman")

diffs <- diff(resamps) 
summary(diffs)
#Comparing Testing 
c50model2Time
rfmodelTestTime





#######################################################################################
#
#                                  Prediction part
#
#######################################################################################


#######################################################################################
#  c50
#######################################################################################
c50modelgalaxyRPEPredition <- predict(c50modelgalaxyRPE, newdata = testinggalaxyRFE) 
str(c50modelgalaxyRPEPredition)
summary(c50modelgalaxyRPEPredition)

#Confusion Matrix
ConfusionMatrixC50galaxyRPE<-confusionMatrix(data = c50modelgalaxyRPEPredition, testinggalaxyRFE$galaxysentiment)
ConfusionMatrixC50galaxyRPE
#Using the test set use postResample() to assess the metrics of the new predictions 
#compared to the Ground Truth (see the resources for more information)
postResampleDataC50galaxyRPE<-postResample(pred=c50modelgalaxyRPEPredition,obs=testinggalaxyRFE$galaxysentiment)
postResampleDataC50galaxyRPE
plot(postResampleDataC50galaxyRPE)


#######################################################################################
#  RF
#######################################################################################
rfmodelgalaxyRPEPredition <- predict(rfmodelgalaxyRPE, newdata = testinggalaxyRFE) 
str(rfmodelgalaxyRPEPredition)
summary(rfmodelgalaxyRPEPredition)

#Confusion Matrix
ConfusionMatrixRFgalaxyRPE<-confusionMatrix(data = rfmodelgalaxyRPEPredition, testinggalaxyRFE$galaxysentiment)

#Using the test set use postResample() to assess the metrics of the new predictions 
#compared to the Ground Truth (see the resources for more information)
# CompleteResponses and SurveyIncomplete
postResampleDataRFgalaxyRPE<-postResample(pred=rfmodelgalaxyRPEPredition,obs=testinggalaxyRFE$galaxysentiment)
postResampleDataRFgalaxyRPE
plot(postResampleDataRFgalaxyRPE)

#######################################################################################
# SVM
#######################################################################################
svm_modelgalaxyRPEPredition <- predict(svm_modelgalaxyRPE, newdata = testinggalaxyRFE) 
str(svm_modelgalaxyRPEPredition)
summary(svm_modelgalaxyRPEPredition)

#Confusion Matrix
ConfusionMatrixSVNgalaxyRPE<-confusionMatrix(data = svm_modelgalaxyRPEPredition, testinggalaxyRFE$galaxysentiment)

#Using the test set use postResample() to assess the metrics of the new predictions 
#compared to the Ground Truth (see the resources for more information)
# CompleteResponses and SurveyIncomplete
postResampleDataSVNgalaxyRPE<-postResample(pred=svm_modelgalaxyRPEPredition,obs=testinggalaxyRFE$galaxysentiment)
postResampleDataSVNgalaxyRPE
plot(postResampleDataSVNgalaxyRPE)

#######################################################################################
# Kknn
#######################################################################################
knnmodelgalaxyRPEPredition <- predict(c50modelgalaxyRPE, newdata = testinggalaxyRFE) 
str(knnmodelgalaxyRPEPredition)
summary(knnmodelgalaxyRPEPredition)

#Confusion Matrix
ConfusionMatrixkknngalaxyRPE<-confusionMatrix(data = knnmodelgalaxyRPEPredition, testinggalaxyRFE$galaxysentiment)
ConfusionMatrixkknngalaxyRPE

#Using the test set use postResample() to assess the metrics of the new predictions 
#compared to the Ground Truth (see the resources for more information)
# CompleteResponses and SurveyIncomplete
postResampleDatakknngalaxyRPE<-postResample(pred=knnmodelgalaxyRPEPredition,obs=testinggalaxyRFE$galaxysentiment)
postResampleDatakknngalaxyRPE
plot(postResampleDatakknngalaxyRPE)

#######################################################################################
# Comparing PostResamp
#######################################################################################
#C50 Model PostResamp
postResampleDataC50galaxyRPE
#RF Model PostResamp
postResampleDataRFgalaxyRPE
#SVN Model PostResamp
postResampleDataSVNgalaxyRPE
#KNN Model PostResamp
postResampleDatakknngalaxyRPE


c50modelTimegalaxyRPE
rfmodelTimegalaxyRPE
svm_modelTimegalaxyRPE
knnmodelTimegalaxyRPE


ConfusionMatrixC50galaxyRPE
ConfusionMatrixRFgalaxyRPE
ConfusionMatrixSVNgalaxyRPE
ConfusionMatrixkknngalaxyRPE





#######################################################################################
#
#                                  Prediction part
#
#######################################################################################

#######################################################################################
#Creating the model c50 galaxyRPE repeats 3
#######################################################################################

#Creating the Data Set

#Seed needed for repeability 
set.seed(123)
#Creating the datas sets for the training and testing.
#It's taking 75% of the data for training and 25% for testing or verification. 
inTrain <- createDataPartition(y = galaxyRFE$galaxysentiment,
                               p = .70, # As requested, will split the data in 70%
                               list = FALSE)
#Creating the training and testing datasets.
traininggalaxyRFE <- galaxyRFE[ inTrain,]
testinggalaxyRFE <- galaxyRFE[-inTrain,]


#Starting the training
#10 fold cross validation
c50modelControlgalaxyRPE <- trainControl(
  method = "repeatedcv", 
  number = 10,
  repeats = 3)

c50modelTimegalaxyRPE<-system.time(c50modelgalaxyRPE <- train(galaxysentiment~., 
                                                              data = traininggalaxyRFE, 
                                                              method = "C5.0", 
                                                              trControl=c50modelControlgalaxyRPE, 
                                                              #tuneLength = 1,
                                                              preProc = c("center", "scale"),
                                                              verbose = TRUE))

#Printing the model already trained
c50modelgalaxyRPE
#Testime
c50modelTimegalaxyRPE

#######################################################################################
#Creating the model c50 galaxyCOR repeats 3
#######################################################################################

#Creating the Data Set

#Seed needed for repeability 
set.seed(123)
#Creating the datas sets for the training and testing.
#It's taking 75% of the data for training and 25% for testing or verification. 
inTrain <- createDataPartition(y = galaxyCOR$galaxysentiment,
                               p = .70, # As requested, will split the data in 70%
                               list = FALSE)
#Creating the training and testing datasets.
traininggalaxyCOR <- galaxyCOR[ inTrain,]
testinggalaxyCOR <- galaxyCOR[-inTrain,]


#Starting the training
#10 fold cross validation
c50modelControlgalaxyCOR <- trainControl(
  method = "repeatedcv", 
  number = 10,
  repeats = 3)

c50modelTimegalaxyCOR<-system.time(c50modelgalaxyCOR <- train(galaxysentiment~., 
                                                              data = traininggalaxyCOR, 
                                                              method = "C5.0", 
                                                              trControl=c50modelControlgalaxyCOR, 
                                                              #tuneLength = 1,
                                                              preProc = c("center", "scale"),
                                                              verbose = TRUE))

#Printing the model already trained
c50modelgalaxyCOR
#Testime
c50modelTimegalaxyCOR

#######################################################################################
#Creating the model c50 galaxyNZV repeats 3
#######################################################################################

#Creating the Data Set

#Seed needed for repeability 
set.seed(123)
#Creating the datas sets for the training and testing.
#It's taking 75% of the data for training and 25% for testing or verification. 
inTrain <- createDataPartition(y = galaxyNZV$galaxysentiment,
                               p = .70, # As requested, will split the data in 70%
                               list = FALSE)
#Creating the training and testing datasets.
traininggalaxyNZV <- galaxyNZV[ inTrain,]
testinggalaxyNZV <- galaxyNZV[-inTrain,]


#Starting the training
#10 fold cross validation
c50modelControlgalaxyNZV <- trainControl(
  method = "repeatedcv", 
  number = 10,
  repeats = 3)

c50modelTimegalaxyNZV<-system.time(c50modelgalaxyNZV <- train(galaxysentiment~., 
                                                              data = traininggalaxyNZV, 
                                                              method = "C5.0", 
                                                              trControl=c50modelControlgalaxyNZV, 
                                                              #tuneLength = 1,
                                                              preProc = c("center", "scale"),
                                                              verbose = TRUE))

#Printing the model already trained
c50modelgalaxyNZV
#Testime
c50modelTimegalaxyNZV

#######################################################################################
#Creating the model c50 galaxyRF repeats 3
#######################################################################################

#Creating the Data Set

#Seed needed for repeability 
set.seed(123)
#Creating the datas sets for the training and testing.
#It's taking 75% of the data for training and 25% for testing or verification. 
inTrain <- createDataPartition(y = galaxyDF$galaxysentiment,
                               p = .70, # As requested, will split the data in 70%
                               list = FALSE)
#Creating the training and testing datasets.
traininggalaxyDF <- galaxyDF[ inTrain,]
testinggalaxyDF <- galaxyDF[-inTrain,]


#Starting the training
#10 fold cross validation
c50modelControlgalaxyDF <- trainControl(
  method = "repeatedcv", 
  number = 10,
  repeats = 3)

c50modelTimegalaxyDF<-system.time(c50modelgalaxyDF <- train(galaxysentiment~., 
                                                            data = traininggalaxyDF, 
                                                            method = "C5.0", 
                                                            trControl=c50modelControlgalaxyDF, 
                                                            #tuneLength = 1,
                                                            preProc = c("center", "scale"),
                                                            verbose = TRUE))

#Printing the model already trained
c50modelgalaxyDF
#Testime
c50modelTimegalaxyDF

#######################################################################################
#Comparing the resutls
#######################################################################################
c50modelTimegalaxyRPE
c50modelgalaxyRPE
c50modelTimegalaxyCOR
c50modelgalaxyCOR
c50modelTimegalaxyNZV
c50modelgalaxyNZV #Best Model
c50modelTimegalaxyDF
c50modelgalaxyDF

#######################################################################################
#######################################################################################
# Optional PART FEATURE ENGINEERING
#######################################################################################
#######################################################################################

#######################################################################################
#Engineering the Dependant variable
#######################################################################################

# create a new dataset that will be used for recoding sentiment
galaxyRC <- galaxyDF
# recode sentiment to combine factor levels 0 & 1 and 4 & 5
galaxyRC$galaxysentiment <- recode(galaxyRC$galaxysentiment, '0' = 1, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 4) 
# inspect results
summary(galaxyRC)
str(galaxyRC)
# make galaxysentiment a factor
galaxyRC$galaxysentiment <- as.factor(galaxyRC$galaxysentiment)

#Creating the Data Set

#Seed needed for repeability 
set.seed(123)
#Creating the datas sets for the training and testing.
#It's taking 75% of the data for training and 25% for testing or verification. 
inTrain <- createDataPartition(y = galaxyRC$galaxysentiment,
                               p = .70, # As requested, will split the data in 70%
                               list = FALSE)
#Creating the training and testing datasets.
traininggalaxyRC <- galaxyRC[ inTrain,]
testinggalaxyRC <- galaxyRC[-inTrain,]


#Starting the training
#10 fold cross validation
c50modelControlgalaxyRC <- trainControl(
  method = "repeatedcv", 
  number = 10,
  repeats = 3)

c50modelTimegalaxyRC<-system.time(c50modelgalaxyRC <- train(galaxysentiment~., 
                                                            data = traininggalaxyRC, 
                                                            method = "C5.0", 
                                                            trControl=c50modelControlgalaxyRC, 
                                                            #tuneLength = 1,
                                                            preProc = c("center", "scale"),
                                                            verbose = TRUE))

#Printing the model already trained
c50modelgalaxyRC
#Testime
c50modelTimegalaxyRC
c50modelgalaxyRC



#######################################################################################
#Getting the data
#######################################################################################
galaxyLargeMatrix <- read.csv("Data\\LargeMatrix.csv")

#######################################################################################
# Examine Feature Variance For La
#######################################################################################
# create a new data set and remove near zero variance features
galaxyLargeMatrixPrediction<- galaxyLargeMatrix[,-nzvGalaxy]
#galaxyLargeMatrixPrediction<-galaxyLargeMatrix
str(galaxyLargeMatrixPrediction)


#######################################################################################
# Predicting
#######################################################################################

galaxySentimentPredition <- predict(c50modelgalaxyNZV, newdata = galaxyLargeMatrixPrediction) 
str(galaxySentimentPredition)
summary(galaxySentimentPredition)
galaxySentimentPredition
plot(galaxySentimentPredition)
plot_ly(galaxySentimentPredition, x= ~galaxySentimentPredition$galaxysentiment, type='histogram')
plot_ly(galaxyLargeMatrix, x= ~galaxySentimentPredition$galaxysentiment, type='histogram')

#######################################################################################
# Resutls
#######################################################################################

plot(iphoneSentimentPredition)
plot(galaxySentimentPredition)
galaxySentimentPredition
summary(iphoneSentimentPredition)
summary(galaxySentimentPredition)


library(plotly)
x<-c("Very negative","Negative","Somewhat Negative","Somewhat Positive","Positive","Very Positive")
iphoneResultPrediction <- c(17078 ,0,0,1370,783,14943)
galaxyResultPrediction <- c(17215,0,0,1275,279,15405)

data <- data.frame(x, iphoneResultPrediction, galaxyResultPrediction)

#The default order will be alphabetized unless specified as below:
data$x <- factor(data$x, levels = data[["x"]])

fig <- plot_ly(data, x = ~x, y = ~iphoneResultPrediction, type = 'bar', name = 'GalaxyPrediction', marker = list(color = 'rgb(49,130,189)'))
fig <- fig %>% add_trace(y = ~galaxyResultPrediction, name = ' Iphone Prediction', marker = list(color = 'rgb(204,204,204)'))
fig <- fig %>% layout(xaxis = list(title = "", tickangle = -45),
                      yaxis = list(title = ""),
                      margin = list(b = 100),
                      barmode = 'group')

fig

#######################################################################################
# Using 
#######################################################################################
LargeMatrixModify <- read.csv("Data\\LargeMatrixModify.csv")
typeof(LargeMatrixModify)

#From Excel
Summary_of_1 <- 55054
Summary_of_2<- 14206
Summary_of_3 <- 7936
Summary_of_4 <- 4144
Summary_of_5 <- 2354


ScaleMeasurement<-c("Very negative","Negative","Somewhat Negative","Somewhat Positive","Positive","Very Positive")
Summary_of_average <- c(55054,14206,7936,1370,4144,2354)




df = data.frame(ScaleMeasurement, Summary_of_average)
df$ScaleMeasurement <- factor(df$ScaleMeasurement ,levels = c("Very negative","Negative","Somewhat Negative","Somewhat Positive","Positive","Very Positive"))

ggplot(data=df, aes(x=ScaleMeasurement, y=Summary_of_average)) + 
  geom_bar(stat="identity", position="stack") # position=position.stack se puede abreviar con position="stack".



#######################################################################################
# Final check
#######################################################################################

c50modelgalaxyNZVPredition <- predict(c50modelgalaxyNZV, newdata = testinggalaxyNZV) 

#Confusion Matrix
ConfusionMatrixRFgalaxyRPE<-confusionMatrix(data = c50modelgalaxyNZVPredition, testinggalaxyNZV$galaxysentiment)

#Using the test set use postResample() to assess the metrics of the new predictions 
#compared to the Ground Truth (see the resources for more information)
# CompleteResponses and SurveyIncomplete
postResampleDatac50galaxyNVZ<-postResample(pred=c50modelgalaxyNZVPredition,obs=testinggalaxyNZV$galaxysentiment)
postResampleDatac50galaxyNVZ

#########
#####
#######
c50modeliphnoeNZVPredition <- predict(c50modeliphoneNZV, newdata = testingiphoneNZV) 

#Confusion Matrix
ConfusionMatrixRFgalaxyRPE<-confusionMatrix(data = c50modeliphnoeNZVPredition, testingiphoneNZV$iphonesentiment)

#Using the test set use postResample() to assess the metrics of the new predictions 
#compared to the Ground Truth (see the resources for more information)
# CompleteResponses and SurveyIncomplete
postResampleDatac50galaxyNVZ<-postResample(pred=c50modeliphnoeNZVPredition,obs=testingiphoneNZV$iphonesentiment)
postResampleDatac50galaxyNVZ

resamps <- resamples(list(C5.0 = c50modeliphoneNZV, C5.0 =c50modelgalaxyNZV)) 
summary(resamps)







