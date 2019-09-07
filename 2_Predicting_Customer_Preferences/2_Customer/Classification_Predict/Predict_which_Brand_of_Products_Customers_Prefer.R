#######################################################################################
#Mauricio Carvajal
#######################################################################################


#######################################################################################
#Intalling Packeges
#######################################################################################
#install Packges for c50
#installed.packages("C50", dependencies = c("Depends", "Suggests"))
install.packages("varImp", dependencies = c("Depends", "Suggests"))
install.packages("matrixStats")
install.packages("party")
install.packages("zoo")
install.packages("sandwich")
install.packages("plotrix")
#######################################################################################
#Libraries needed for the script.
#######################################################################################

library(caret) #caret-->clasificacion and regresion testing
library(C50)
library(plotrix)
library(ggplot2)
#library(varImp)
#######################################################################################
#Reading the Responses of the survey.
#######################################################################################
CompleteResponses <- read.csv("Data\\CompleteResponses.csv")

#######################################################################################
#Starting getting knowleage of the data
#######################################################################################
summary(CompleteResponses)
str(CompleteResponses)
names(CompleteResponses)
#attributes(CompleteResponses)
#hist(CompleteResponses$age)
#plot(CompleteResponses$salary)
#qnorm(CompleteResponses)

#Tips to remember
#To check is.xxx -->True/Fase, to convert is as.xxx
#is.numeric(CompleteResponses$salary)

#######################################################################################
#Converting the numeric Values into categorical, after analyce the survey's keys.
#######################################################################################
CompleteResponses$elevel<-as.factor(CompleteResponses$elevel)
CompleteResponses$car<-as.factor(CompleteResponses$car)
CompleteResponses$zipcode<-as.factor(CompleteResponses$zipcode)
CompleteResponses$brand<-as.factor(CompleteResponses$brand)
#Checking the convertion
str(CompleteResponses)

#######################################################################################
#Recursive Feature Elimination or RFE.
#######################################################################################

control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
results <- rfe(
    CompleteResponses[,1:6], 
    CompleteResponses[,7], 
    sizes=c(1:6), 
    rfeControl=control)
# summarize the results
print(results)

#######################################################################################
#Creating the model c50
#######################################################################################

#Seed needed for repeability 
set.seed(123)
#Creating the datas sets for the training and testing.
#Our dependen variable is $brand.
#It's taking 75% of the data for training and 25% for testing or verification. 
inTrain <- createDataPartition(y = CompleteResponses$brand,
                               p = .75, # As requested, will split the data in 75%
                               list = FALSE)
#Creating the training and testing datasets.
training <- CompleteResponses[ inTrain,]
testing <- CompleteResponses[-inTrain,]

#Starting the training
#10 fold cross validation
c50modelControl <- trainControl(method = "repeatedcv", number = 10,repeats = 3)

c50modelTime<-system.time(c50model <- train(brand~., 
                data = training, 
                method = "C5.0", 
                trControl=c50modelControl, 
                tuneLength = 1,
                preProc = c("center", "scale"),
                verbose = TRUE))

#Printing the model already trained
c50model
#varImp(c50model, scale =FALSE)
#Here we review how the model prioritized each feature in the training.
varImp(c50model)
#######################################################################################
#Creating second model c50 changing tunelength 2
#######################################################################################
#train Linear Regression model with a tuneLenght = 2 (trains with 2 mtry values for RandomForest)
c50model2Time<-system.time(c50model2 <- train(brand~., 
                  data = training, 
                  method = "C5.0", 
                  trControl=c50modelControl, 
                  tuneLength = 2,
                  preProc = c("center", "scale"),
                  verbose = TRUE))
#Printing the model already trained
c50model2
#Printing the time that took the model
print(c50model2Time)

#Here we review how the model prioritized each feature in the training.
varImp(c50model2)
#######################################################################################
#Creating second model c50 changing tunelength 3
#######################################################################################
#train Linear Regression model with a tuneLenght = 3 (trains with 3 mtry values for RandomForest)
c50model3Time<-system.time(c50model3 <- train(brand~., 
                                              data = training, 
                                              method = "C5.0", 
                                              trControl=c50modelControl, 
                                              tuneLength = 3,
                                              preProc = c("center", "scale"),
                                              verbose = TRUE))
#Printing the model already trained
c50model3
#Here we review how the model prioritized each feature in the training.
varImp(c50model3)

#######################################################################################
#Creating RF Model with custon grid
#######################################################################################
#This is not needed, we can reused, but I just leaved due to academic purposes. 
rfmodelControl <- trainControl(method = "repeatedcv", number = 10,repeats = 3)
#dataframe for manual tuning of mtry
rfGrid <- expand.grid(mtry=c(4,5,6,8,10))

#traning the model with random forest
rfmodel1Time<-system.time(rfmodel <- train(brand~., 
                  data = training, 
                  method = "rf", 
                  trControl=rfmodelControl, 
                  #tuneLength = 1,
                  tuneGrid=rfGrid, 
                  preProc = c("center", "scale"),
                  verbose = TRUE))

rfmodelControlTest <- trainControl(method = "repeatedcv", number = 10,repeats = 3)
rfGridTest <- expand.grid(mtry=c(12))
rfmodelTestTime<-system.time(rfmodelTest <- train(brand~., 
                 data = training, 
                 method = "rf", 
                 trControl=rfmodelControlTest, 
                 #tuneLength = 1,
                 tuneGrid=rfGridTest, 
                 preProc = c("center", "scale"),
                 verbose = TRUE))
#training results
rfmodel
varImp(rfmodel)
#######################################################################################
#Comparing the models in tearms of resampling
#######################################################################################
resamps <- resamples(list(C5.0 = c50model2, rf = rfmodel)) 
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
#
#                                  Prediction part
#
#######################################################################################

#######################################################################################
#Importing surveyIncomplete data
#######################################################################################
SurveyIncomplete <- read.csv("Data\\SurveyIncomplete.csv")

#######################################################################################
#Preprocessing the data
#######################################################################################
str(SurveyIncomplete)
names(SurveyIncomplete)
#hist(CompleteResponses$age)
#plot(CompleteResponses$salary)
#qnorm(CompleteResponses)

#Converting the numeric Values into categorical.
SurveyIncomplete$elevel<-as.factor(SurveyIncomplete$elevel)
SurveyIncomplete$car<-as.factor(SurveyIncomplete$car)
SurveyIncomplete$zipcode<-as.factor(SurveyIncomplete$zipcode)
SurveyIncomplete$brand<-as.factor(SurveyIncomplete$brand)
#Checking the convertion
str(SurveyIncomplete)

#######################################################################################
# The chosen model is c50
#######################################################################################
c50model2Predition <- predict(c50model2, newdata = SurveyIncomplete) 
str(c50model2Predition)
summary(c50model2Predition)

c50model2Probs <- predict(c50model2, newdata = SurveyIncomplete, type = "prob")
head(c50model2Probs)

#Confusion Matrix
confusionMatrix(data = c50model2Predition, SurveyIncomplete$brand)


#Using the test set use postResample() to assess the metrics of the new predictions 
#compared to the Ground Truth (see the resources for more information)
# CompleteResponses and SurveyIncomplete
postResampleData<-postResample(pred=c50model2Predition,obs=CompleteResponses$brand)
postResampleData
plot(postResampleData)


#For something
summary(c50model2Predition)
plot(c50model2Predition)
c50model2Predition
#######################################################################################
# Plotting the predictions
#######################################################################################
dataSummaryPrediction<-summary(c50model2Predition)
dataSummaryPrediction
#Plotting the predition of the brand
#slices <- c(c50model2Predition["0"], c50model2Predition["1"])
slices <- c(1844, 3156)
lbls <- c("Acer", "Sony")
pie3D(slices,labels=lbls,explode=0.1,
      main="Pie Chart of predictions of the brand's preference ")


#######################################################################################
# Plotting the all 15000 brand preferences
#######################################################################################
#Printing CompleteResponses
summary(CompleteResponses$brand)
#Plotting the predition of the brand
slices <- c(1844+3744, 3156+6154)
lbls <- c("Acer", "Sony")
pie + scale_fill_manual(values=c("#999999", "#E69F00"))
pie3D(slices,labels=lbls,explode=0.1,
      main="Pie Chart of predictions of the brand's preference ")

