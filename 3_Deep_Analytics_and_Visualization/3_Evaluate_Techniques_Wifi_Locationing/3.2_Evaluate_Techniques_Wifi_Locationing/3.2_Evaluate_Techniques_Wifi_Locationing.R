#############################################################################################
# Mauricio Carvajal
#############################################################################################


#############################################################################################
# Install packages
#############################################################################################

#############################################################################################
# Libraries
library(caret)

#############################################################################################
# Reading the data
#############################################################################################
wifiTrainingData <- read.csv("C:\\Users\\lmcarva\\Documents\\Trabajo\\MVP\\Array\\Github_Array_MBIST\\BigDataDataAnalytics\\3_Deep_Analytics_and_Visualization\\3_Evaluate_Techniques_Wifi_Locationing\\UJIndoorLoc\\trainingData.csv")
wifiValidationData <- read.csv("C:\\Users\\lmcarva\\Documents\\Trabajo\\MVP\\Array\\Github_Array_MBIST\\BigDataDataAnalytics\\3_Deep_Analytics_and_Visualization\\3_Evaluate_Techniques_Wifi_Locationing\\UJIndoorLoc\\validationData.csv")

#############################################################################################
# Knowing the data
#############################################################################################
str(wifiTrainingData)
summary(wifiTrainingData)
head(wifiTrainingData)