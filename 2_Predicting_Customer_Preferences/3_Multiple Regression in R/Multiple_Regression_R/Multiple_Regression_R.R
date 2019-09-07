######################################################################################
#Mauricio Carvajal
######################################################################################

######################################################################################
#Installing packages
######################################################################################
install.packages("corrplot")
install.packages("PerformanceAnalytics")
######################################################################################
#Libraries that are needed
######################################################################################
library(corrplot)
library(PerformanceAnalytics)
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
#caret has a method for creating these 'Dummy Variables' as follows:

#Checking the correlation between the variables in the data
#all variables must not contain nominal data types.
str(readyData)

#Printing the summary to check if there is any missing data
summary(readyData)

#For now deleting the attribute with missing information
readyData$BestSellersRank<-NULL


#######################################################################################
# Creating the correlation matrix
######################################################################################
#builing the correlation matrix
?cor
#creating the matrix of correlation
corrData<-cor(readyData)
#Printing the correlation matrix
corrData
#Plot a correlation matrix
corrplot(corrData)
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
x = nearZeroVar(readyData, saveMetrics = TRUE)
str(x, vec.len=2)
x[x[,"zeroVar"] > 0, ] 
x[x[,"zeroVar"] + x[,"nzv"] > 0, ]


#######################################################################################
#Creating the data sets taht will be use by the differents models  
#using the adventages of caret
#######################################################################################
#Creating the datas sets for the training and testing.
#Our dependen variable is $volume 
#It's taking 75% of the data for training and 25% for testing or verification. 

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
                    #preProcess = c("center", "scale"),
                    #tuneLength = 10
                    )
              )

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

#######################################################################################
#Creating RF Model with custon grid
#######################################################################################

