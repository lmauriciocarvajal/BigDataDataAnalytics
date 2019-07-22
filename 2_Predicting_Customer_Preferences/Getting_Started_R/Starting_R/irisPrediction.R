#Mauricio Carvajal
#install.packages(readr)
#First call the library read, deleting ""
library(readr)
#Reading the csv changing the dirpath
IrisDataset <-read.csv("C:\\Users\\lmaur\\Google Drive\\U\\Proyecto_Graduacion\\BigDataDataAnalytics\\2_Predicting_Customer_Preferences\\Getting_Started_R\\R_Tutorial_Data_Sets\\iris.csv")
#Printing the attributes
attributes(IrisDataset)
#Printing the summary, modifyng the dataset adding "I"
summary(IrisDataset) 
#Printing the str, modifyng the dataset deleting the extra "s"
str(IrisDataset)
#the names of the Dataset
names(IrisDataset)
# Converting into numeric, seems that the graphic has 50 of each of the 3 distribution
species<- as.numeric(IrisDataset$Species)
#Pending to discuss
hist(species)
#plot(IrisDataset$X,IrisDataset$Species)
#Changing the plot,The analysis goal is to predict a petal's length using the petal’s width.  
plot(IrisDataset$Petal.Width,IrisDataset$Petal.Length)

#Pending to review what this means
#a way to see if your data is normally distribute
qqnorm(IrisDataset$Petal.Length)
#Checking any NA value
summary(IrisDataset)
is.na(IrisDataset)

#why I want to change to numeric?
#IrisDataset$Species<- as.numeric(IrisDataset$Species) 

#This is to set the random algorithm, so everytime I ran it, the random values are the same.      
set.seed(123)
#Changing th train size to 0.7
trainSize <- round(nrow(IrisDataset) * 0.7)
#The testsize is 1- traisize, =0.3 in this case, also changing TrainSet to trainSize
testSize <- nrow(IrisDataset) - trainSize
#Priting the trainsize, also deleting the extra "s"
trainSize
#Printing the testSize
testSize
#Creating the training datasets
Training_indices<-sample(seq_len(nrow(IrisDataset)),size =trainSize)

trainSet <- IrisDataset[Training_indices, ]
     testSet <- IrisDataset[-Training_indices, ]
     set.seed(405)
     trainSet <- IrisDataset[training_indices, ]
     testSet <- IrisDataset[-training_indices, ]
     LinearModel<- lm(trainSet$Petal.Width ~ testingSet$Petal.Length)
     summary(LinearModel)
  prediction<-predict(LinearModeltestSet)
predictions
     
     