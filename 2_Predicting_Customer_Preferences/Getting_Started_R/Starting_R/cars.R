#Mauricio Carvajal
#Learning R
library(readr)
DatasetName<- read.csv("C:\\Users\\lmaur\\Google Drive\\U\\Proyecto_Graduacion\\BigDataDataAnalytics\\2_Predicting_Customer_Preferences\\Getting_Started_R\\R_Tutorial_Data_Sets\\cars.csv")

attributes(DatasetName)#List your attributes within your data set.

summary(DatasetName) #Prints the min, max, mean, median, and quartiles of each attribute.

str(DatasetName) #Displays the structure of your data set.

names(DatasetName) #Names your attributes within your data set.

DatasetName$speed #Will print out the instances within that particular column in your data set.

#Histogram Plot
hist(DatasetName$distance)

#Scatter (Box) Plot
plot(DatasetName$speed,DatasetName$distance)

#Normal Quantile Plot- is a way to see if your data is normally distributed.
qqnorm(DatasetName$distance)
#Need to understand the change between Factor data vs character data
DatasetName$name<-as.character(DatasetName$name)
DatasetName$name<-as.factor(DatasetName$name)

#renaming the attributes/columns in the dataset
names(DatasetName)<-c("name","speed","distance") 
#Do any of your variables have missing values? 
summary(DatasetName) #Will count how many NA’s you have.
is.na(DatasetName) #Will show your NA’s through logical data. (TRUE if it’s missing, FALSE if it’s not.)

#na.omit(DatasetName$ColumnName#Drops any rows with missing values and omits them forever.
#na.exclude(DatasetName$ColumnName)#Drops any rows with missing values, but keeps track of where they were.
#DatasetName$ColumnName[is.na(DatasetName$ColumnName)]<-mean(DatasetName$ColumnName,na.rm = TRUE)

#Creating Testing and Training Sets
#How to begin? In order to create your training and testing sets, you need to use the set.seed() function. The seed is a number that you choose for a starting point used to create a sequence of random numbers. It is also helpful for others who want to recreate your same results. Here is the function:
#TIP: A common set.seed number is 123. To use the same set of random numbers, you’ll want to use the same seed number throughout your modeling process.
set.seed(123)
#How do you split the data into training and test sets? You’ll now want to split your data into two sets for modeling. One is the training set and the other one being the test set. A common split is 70/30, which means that 70% of the data will be the training set’s size and 30% of the data will be the test set’s size. You will be using the 70/30 split, but another common split is 80/20.
trainSize<-round(nrow(DatasetName)*0.7) 
testSize<-nrow(DatasetName)-trainSize
#How do you create the training and test sets? It’s now time for you to create the training and test sets. We also want these sets to be in a randomized order, which will create the most optimal model.
#To perform this, you need to run these three lines of code. Type in this code into R Script or Console:
training_indices<-sample(seq_len(nrow(DatasetName)),size =trainSize)
trainSet<-DatasetName[training_indices,]
testSet<-DatasetName[-training_indices,] 
LinearCarsModel<-lm(distance~ speed, trainSet)
summary(LinearCarsModel)
PredictionCarDistances<-predict(LinearCarsModel,testSet)
PredictionCarDistances

plot(PredictionCarDistances)

