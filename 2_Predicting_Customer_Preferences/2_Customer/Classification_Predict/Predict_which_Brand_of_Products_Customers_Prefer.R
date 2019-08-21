#Mauricio Carvajal
#install Packges for c50
#installed.packages("C50", dependencies = c("Depends", "Suggests"))

#Libraries needed for the script.

library(caret) #caret-->clasificacion and regresion testing
library(C50)
CompleteResponses <- read.csv("Data\\CompleteResponses.csv")


#Starting gettng knowleage of the data
#attributes(CompleteResponses)
summary(CompleteResponses)
str(CompleteResponses)
names(CompleteResponses)
#hist(CompleteResponses$age)
#plot(CompleteResponses$salary)
#qnorm(CompleteResponses)

#To check is.xxx -->True/Fase, to convert is as.xxx
#is.numeric(CompleteResponses$salary)

#Converting the numeric Values into categorical.
CompleteResponses$elevel<-as.factor(CompleteResponses$elevel)
CompleteResponses$car<-as.factor(CompleteResponses$car)
CompleteResponses$zipcode<-as.factor(CompleteResponses$zipcode)
CompleteResponses$brand<-as.factor(CompleteResponses$brand)
#Checking the convertion
str(CompleteResponses)

#Seed needed for repeability 
set.seed(123)
inTrain <- createDataPartition(y = CompleteResponses$brand,
                               p = .75, # As requested, will split the data in 75%
                               list = FALSE)

#str(inTrain)
#Creating the training and testing datasets.
training <- CompleteResponses[ inTrain,]
testing <- CompleteResponses[-inTrain,]

#10 fold cross validation
c50modelControl <- trainControl(method = "repeatedcv", number = 10,repeats = 3)

c50model <- train(brand~., 
                data = training, 
                method = "C5.0", 
                trControl=c50modelControl, 
                tuneLength = 1,
                preProc = c("center", "scale"),
                verbose = TRUE)

#training results
c50model
varImp(c50model, scale =FALSE)
varImp(c50model)
#train Linear Regression model with a tuneLenght = 2 (trains with 2 mtry values for RandomForest)
c50model2 <- train(brand~., 
                  data = training, 
                  method = "C5.0", 
                  trControl=c50modelControl, 
                  tuneLength = 2,
                  preProc = c("center", "scale"),
                  verbose = TRUE)
#training results
c50model2
varImp(c50model2, scale =FALSE)
varImp(c50model2)

rfmodelControl <- trainControl(method = "repeatedcv", number = 10,repeats = 3)
rfGrid <- expand.grid(mtry=c(1,2,3,4,5))

rfmodel <- train(brand~., 
                  data = training, 
                  method = "rf", 
                  trControl=rfmodelControl, 
                  #tuneLength = 1,
                  tuneGrid=rfGrid, 
                  preProc = c("center", "scale"),
                  verbose = TRUE)

#training results
rfmodel
varImp(rfmodel, scale =FALSE)
varImp(rfmodel)
################################################################
#
#                     Prediction part
#
################################################################
SurveyIncomplete <- read.csv("Data\\CompleteResponses.csv")
str(SurveyIncomplete)
names(SurveyIncomplete)
#hist(CompleteResponses$age)
#plot(CompleteResponses$salary)
#qnorm(CompleteResponses)

#To check is.xxx -->True/Fase, to convert is as.xxx
#is.numeric(CompleteResponses$salary)

#Converting the numeric Values into categorical.
SurveyIncomplete$elevel<-as.factor(CompleteResponses$elevel)
SurveyIncomplete$car<-as.factor(CompleteResponses$car)
SurveyIncomplete$zipcode<-as.factor(CompleteResponses$zipcode)
SurveyIncomplete$brand<-as.factor(CompleteResponses$brand)
#Checking the convertion
str(SurveyIncomplete)


