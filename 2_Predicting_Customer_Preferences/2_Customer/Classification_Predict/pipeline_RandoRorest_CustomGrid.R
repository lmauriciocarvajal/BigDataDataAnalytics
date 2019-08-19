#caret model - Manual Tuning Grid
#http://topepo.github.io/caret/bytag.html
#model training: http://topepo.github.io/caret/training.html
#model measurement: http://topepo.github.io/caret/other.html
#dataframe = WholeYear
#Y Value = SolarRad
WholeYear <- read.csv("C:\\Users\\lmaur\\Google Drive\\U\\DataAnalyticsBigData\\Data\\2_Predicting_Customer_Preferences\\2_Classification_Predict_which_Brand_of_Products_Customers_Prefer\\Data\\WholeYear.csv")

#load library and set seed
library(caret)
set.seed(998)

#create a 20% sample of the data
WholeYear <- WholeYear[sample(1:nrow(WholeYear), 7000,replace=FALSE),]

# define an 75%/25% train/test split of the dataset
inTraining <- createDataPartition(WholeYear$SolarRad, p = .75, list = FALSE)
training <- WholeYear[inTraining,]
testing <- WholeYear[-inTraining,]

#10 fold cross validation
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)

#dataframe for manual tuning of mtry
rfGrid <- expand.grid(mtry=c(1,2,3))

#train Random Forest Regression model
#note the system time wrapper. system.time()
#this is used to measure process execution time 
system.time(rfFitm1 <- train(SolarRad~., data = training, method = "rf", trControl=fitControl, tuneGrid=rfGrid))

#training results
rfFitm1
