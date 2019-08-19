#Mauricio Carvajal
CompleteResponses <- read.csv("C:\\Users\\lmaur\\Google Drive\\U\\DataAnalyticsBigData\\Data\\2_Predicting_Customer_Preferences\\2_Classification_Predict_which_Brand_of_Products_Customers_Prefer\\Data\\CompleteResponses.csv")
#Libraries needed for the script.
#install.packages("C50")
library(caret) #caret-->clasificacion and regresion testing
library(C50)
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
c50modelControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)

c50model <- train(brand~., 
                data = training, 
                method = "C5.0", 
                trControl=fitControl, 
                tuneLength = 1,
                preProc = c("center", "scale"))

#training results
c50model
#train Linear Regression model with a tuneLenght = 2 (trains with 2 mtry values for RandomForest)
c50model2 <- train(brand~., 
                  data = training, 
                  method = "C5.0", 
                  trControl=fitControl, 
                  tuneLength = 2,
                  preProc = c("center", "scale"))
#training results
c50model2
varImp(c50model, scale =FALSE)
varImp(c50model2)

#10 fold cross validation
rfModelControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)

#train Random Forest Regression model with a tuneLenght = 1 (trains with 1 mtry value for RandomForest)
rfModel <- train(brand~., 
                data = training, 
                method = "rf", 
                trControl=fitControl, 
                tuneLength = 1,
                preProc = c("center", "scale"))

#training results
rfModel
varImp(rfModel)
#train Linear Regression model with a tuneLenght = 11 (trains with 11 mtry values for RandomForest)
rfModel11 <- train(brand~., 
                 data = training, 
                 method = "rf", 
                 trControl=fitControl, 
                 tuneLength = 11,
                 preProc = c("center", "scale"))
