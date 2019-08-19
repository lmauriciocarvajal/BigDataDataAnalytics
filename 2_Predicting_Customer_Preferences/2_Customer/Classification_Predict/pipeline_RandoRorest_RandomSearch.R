#caret model - Random Tuning Search
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

#define an 75%/25% train/test split of the dataset
inTraining <- createDataPartition(WholeYear$SolarRad, p = .75, list = FALSE)
training <- WholeYear[inTraining,]
testing <- WholeYear[-inTraining,]

#10 fold cross validation
rfitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1, search = 'random')

#train Random Forest Regression model
rfFitr2 <- train(SolarRad~., data = training, method = "rf", trControl=rfitControl)

#training results
rfFit2