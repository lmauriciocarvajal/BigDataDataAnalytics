######################################################################################
#Mauricio Carvajal
######################################################################################

######################################################################################
#Installing packages
######################################################################################
#install.packages("corrplot")
#install.packages("PerformanceAnalytics")

######################################################################################
#Libraries that are needed
######################################################################################
library(corrplot)
#library(PerformanceAnalytics)
library(caret)

#######################################################################################
#Loading the Data
#######################################################################################
#Read the data
existing_products <- read.csv("Datasets\\existingproductattributes2017.csv")

#######################################################################################
#Preprocess the Data
#######################################################################################
# dummify the data
newDataFrame <- dummyVars(" ~ .", data = existing_products)
readyData <- data.frame(predict(newDataFrame, newdata = existing_products))
#Categorical variables may be used directly as predictor or predicted variables in a multiple regression model 
#as long as they've been converted to binary values. In order to pre-process the sales data as needed we first 
#need to convert all factor or 'chr' classes to binary features that contain ‘0’ and ‘1’ classes. Fortunately,
#caret has a method for creating these 'Dummy Variables'.

#All variables must not contain nominal data types, so checking the structure of the data
str(readyData)

#Printing the summary to check if there is any missing data
summary(readyData)

#For now deleting the attribute with missing information
readyData$BestSellersRank<-NULL


#######################################################################################
# Creating the correlation matrix
######################################################################################
#builing the correlation matrix
#?cor
#creating the matrix of correlation
corrData<-cor(readyData)
#Printing the correlation matrix
corrData
#Plot a correlation matrix
corrplot(corrData)

#Using the function findcorrelation to see the colleration between the variables
findCorrelation(corrData, cutoff = 0.9, verbose = TRUE, names = TRUE,
                exact = ncol(corrData) < 100)
#From the analsysis, we are going to delete the independent variables that has a correlation 
#up of 90 in order to avoid collinearity
readyData$x1StarReviews<-NULL
readyData$x4StarReviews<-NULL


#######################################################################################
#Creating linear model
#######################################################################################
#Creating the seed for 
set.seed(123)
#Taking 0.75 of the data for training the model
trainSize<-round(nrow(readyData)*0.75)
#The 25% will be for testing
testSize<-nrow(readyData)-trainSize
#Taking a sample of the data of 75%
training_indices<-sample(seq_len(nrow(readyData)),size =trainSize)
#setting the 75% of the data for training
trainSet<-readyData[training_indices,]
#setting the 25% of the data for training
testSet<-readyData[-training_indices,]
#Printing the names to review the independent varialbes of interest (PC,Laptop,Netbook,Smartphone)
names(readyData)
LinearModel<-lm(Volume~ 
                  ProductType.PC
                + ProductType.Laptop
                + ProductType.Netbook
                +ProductType.Smartphone, 
                trainSet)
LinearModel_PC<-lm(Volume~ 
                  ProductType.PC, 
                trainSet)

#Summary of the model
summary(LinearModel)

#Plotting the result model
plot(LinearModel)



#######################################################################################
#checking the data Near-zero or zero variance predictors
#######################################################################################
nearZeroVarReadyDAta = nearZeroVar(readyData, saveMetrics = TRUE)
str(nearZeroVarReadyDAta, vec.len=2)
nearZeroVarReadyDAta[nearZeroVarReadyDAta[,"zeroVar"] > 0, ] 
nearZeroVarReadyDAta[nearZeroVarReadyDAta[,"zeroVar"] + nearZeroVarReadyDAta[,"nzv"] > 0, ]

#######################################################################################
#Creating the data sets that will be use by the differents models  
#using the adventages of caret
#######################################################################################
#Creating the datas sets for the training and testing.
#Our dependen variable is $volume 
#It's taking 75% of the data for training and 25% for testing or verification.
set.seed(123)
inTrain <- createDataPartition(y = readyData$Volume,
                               p = .75, # As requested, will split the data in 75%
                               list = FALSE)

#Having the 75% of the data for training the model
trainSet <- readyData[ inTrain,]
#Having the 25% of the data for testing the model
testSet <- readyData[-inTrain,]

#######################################################################################
#Creating SVM model
#######################################################################################
summary(readyData)
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
svm_LinearTime<-system.time(
              svm_Linear <- train(
                    Volume ~., 
                    data = trainSet, 
                    method = "svmLinear",
                    trControl=trctrl,
                   # preProcess = c("center", "scale"),
                    tuneLength =2 
                    )
              )

svm_Linear
SVM_Predictions<-predict(svm_Linear, newdata=testSet)
SVM_Predictions
plot(SVM_Predictions)

#######################################################################################
#Creating RF Model with custon grid
#######################################################################################
#This is not needed, we can reused, but I just leaved due to academic purposes. 
rfmodelControl <- trainControl(method = "repeatedcv", number = 10,repeats = 3)
#dataframe for manual tuning of mtry
rfGrid <- expand.grid(mtry=c(6,7,8,9,10))

#traning the model with random forest
rfmodel1Time<-system.time(rfmodel <- train(Volume~., 
                                           data = trainSet, 
                                           method = "rf", 
                                           trControl=rfmodelControl, 
                                           #tuneLength = 1,
                                           #preProc = c("center", "scale"),
                                           tuneGrid=rfGrid))
rfmodel
summary(rfmodel)
RF_Predictions<-predict(rfmodel, newdata=testSet)
RF_Predictions
plot(RF_Predictions)

#######################################################################################
#Creating Gradient Boosting Model
#######################################################################################
fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 3)

gbmGrid <-  expand.grid(interaction.depth = c(1, 2), 
                        n.trees = (1:30)*50, 
                        shrinkage = 0.1,
                        n.minobsinnode = 5)

gbmFit1 <- train(Volume ~ ., 
                 data = trainSet, 
                 method = "gbm", 
                 trControl = fitControl,
                 ## This last option is actually one
                 ## for gbm() that passes through
                 #tuneGrid = gbmGrid
                 )
gbmFit1
GF_Predictions<-predict(gbmFit1, newdata=testSet)
GF_Predictions
plot(GF_Predictions)

#######################################################################################
#Comparing the models in tearms of resampling
#######################################################################################
resamps <- resamples(list(svmLinear=svm_Linear,rf = rfmodel, gbm = gbmFit1))
summary(resamps)
# boxplots of results
bwplot(resamps)
# dot plots of results
dotplot(resamps)

#Visualizing resamps
xyplot(resamps, what = "BlandAltman")

diffs <- diff(resamps) 
summary(diffs)

#######################################################################################
#Loading the Data
#######################################################################################
#Read the data
news_products <- read.csv("Datasets\\newproductattributes2017.csv")

#######################################################################################
#Preprocess the Data
#######################################################################################
# dummify the data
newProductsDataFrame <- dummyVars(" ~ .", data = news_products)
readyNewProductData <- data.frame(predict(newProductsDataFrame, newdata = news_products))

#All variables must not contain nominal data types, so checking the structure of the data
str(readyNewProductData)

#Printing the summary to check if there is any missing data
summary(readyNewProductData)

#For now deleting the attribute with missing information
readyNewProductData$BestSellersRank<-NULL

#From the analsysis, we are going to delete the independent variables that has a correlation 
#up of 90 in order to avoid collinearity
readyNewProductData$x1StarReviews<-NULL
readyNewProductData$x4StarReviews<-NULL

#######################################################################################
#Final Prediction
#######################################################################################

finalPred<-predict(svm_Linear, newdata=readyNewProductData)
finalPred
plot(finalPred)

#######################################################################################
#Add predictions to the new products data and then create a csv file
#######################################################################################
#Add predictions to the new products data set
output <- readyNewProductData
output$predictions <- finalPred
#Create a csv file and write it to your hard drive. Note: You may need to use your computer’s 
#search function to locate your output file.
write.csv(output, file="C2.T3output.csv", row.names = TRUE)

#######################################################################################
#Add predictions to the new products data and then create a csv file
#######################################################################################





