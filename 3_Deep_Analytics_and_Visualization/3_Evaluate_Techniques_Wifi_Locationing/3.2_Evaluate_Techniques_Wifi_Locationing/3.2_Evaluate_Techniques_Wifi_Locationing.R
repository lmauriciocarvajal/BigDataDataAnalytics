#############################################################################################
# Mauricio Carvajal
#############################################################################################


#############################################################################################
# Install packages
#############################################################################################
#I running in new PC, so I have to do the installation of all the packages.
install.packages("caret", dependencies = c("Depends", "Suggests"))
install.packages("dplyr", dependencies = c("Depends", "Suggests"))
install.packages("Rtools", dependencies = c("Depends", "Suggests"))
install.packages("lifecycle", dependencies = c("Depends", "Suggests"))
install.packages("lazyeval", dependencies = c("Depends", "Suggests"))
install.packages("gower", dependencies = c("Depends", "Suggests"))
install.packages("C50", dependencies = c("Depends", "Suggests"))
install.packages("inum", dependencies = c("Depends", "Suggests"))
install.packages("lazyeval", dependencies = c("Depends", "Suggests"))
install.packages("lazyeval", dependencies = c("Depends", "Suggests"))

#############################################################################################
# Libraries
library(caret)
library(dplyr)
library(C50)

#############################################################################################
# Reading the data
#############################################################################################
wifiTrainingData <- read.csv("C:\\Users\\lmcarva\\Documents\\3.2 MachineLearning\\UJIndoorLoc\\trainingData.csv")
wifiValidationData <- read.csv("C:\\Users\\lmcarva\\Documents\\3.2 MachineLearning\\UJIndoorLoc\\validationData.csv")

#############################################################################################
# Knowing the data
#############################################################################################
str(wifiTrainingData)
summary(wifiTrainingData)
head(wifiTrainingData)

#############################################################################################
#MERGING the data sets
#############################################################################################
mergeWifiDatasetsUniqued=rbind(wifiTrainingData,wifiValidationData)


#############################################################################################
# Deleting the less variance columns
#############################################################################################
#The idea is to delete the coluns that has close or near close variance to zero, in order to 
#simplyfy the analysis
#Setting the variable in which is identify zero variance.
zeroVar <- logical()

for (i in c(1:ncol(mergeWifiDatasetsUniqued))) {
  zeroVar[i] <- var(mergeWifiDatasetsUniqued[,i]) != 0
}
#Printing
zeroVar
mergeWifiDatasetsUniqued <- mergeWifiDatasetsUniqued[, zeroVar]



#############################################################################################
# PCA
#############################################################################################
# This has been used from -->https://towardsdatascience.com/principal-component-analysis-pca-101-using-r-361f4c53a9ff

#First select only the WAP values
#head(mergeWifiDatasets[466])
wifiDataSetPCA <- prcomp(mergeWifiDatasetsUniqued[c(0:520)], 
                         center = TRUE, 
                         scale = TRUE)
summary(wifiDataSetPCA)

#los datos están ahora en otra escala totalmente diferente, y almacenados en el campo $x
# the results are locate in $x and with different scale and center
#View(head(wifiDataSetPCA$x))

#set aside the standard deviation of each component
std_dev <- wifiDataSetPCA$sdev

#variance of each component
pr_var <- std_dev^2

#The proportion of variance explained by each component
prop_varex <- pr_var/sum(pr_var)

#Plotting the proportion of variance explained
plot(prop_varex, 
     xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     type = "b")

abline(v = 85, col="blue", lty=5)
legend("topleft", legend=c("Cut-off @ PC85"),
       col=c("blue"), lty=5, cex=0.6)


Num_of_values<-100
#Checking the proportion
sum(prop_varex[1:Num_of_values]) #

#Spliting the datasets 100
UnifyWifiDataSetPCA<- data.frame(wifiDataSetPCA$x[,1:Num_of_values])



#Adding the rest of the variables
#UnifyWifiDataSetPCA <- cbind(UnifyWifiDataSetPCA,mergeWifiDatasetsUniqued[, 520:529])

#Unify the columns
UnifyWifiDataSetPCA$BUILDINGID_FLOOR <- paste(mergeWifiDatasetsUniqued$BUILDINGID,mergeWifiDatasetsUniqued$FLOOR)
UnifyWifiDataSetPCA$BUILDINGID_FLOOR <-as.factor(UnifyWifiDataSetPCA$BUILDINGID_FLOOR)

UnifyWifiDataSetPCA$BUILDINGID_FLOOR
str(UnifyWifiDataSetPCA$BUILDINGID_FLOOR)
head(UnifyWifiDataSetPCA$BUILDINGID_FLOOR)


#############################################################################################
# Plotting 
#############################################################################################

screeplot(wifiDataSetPCA, type = "l", npcs = Num_of_values, main = "Screeplot of the WAPs")
abline(h = 1, col="red", lty=5)
legend("topright", legend=c("Eigenvalue = 1"),
       col=c("red"), lty=5, cex=0.6)


cumpro <- cumsum(wifiDataSetPCA$sdev^2 / sum(wifiDataSetPCA$sdev^2))
plot(cumpro[0:Num_of_values], xlab = "PC #", ylab = "Amount of explained variance", main = "Cumulative variance plot")
abline(v = 6, col="blue", lty=5)
abline(h = 0.88759, col="blue", lty=5)
legend("topleft", legend=c("Cut-off @ PC6"),
       col=c("blue"), lty=5, cex=0.6)


plot(wifiDataSetPCA$x[,1],wifiDataSetPCA$x[,2], xlab="PC1 (44.3%)", ylab = "PC2 (19%)", main = "PC1 / PC2 - plot")

wifiTrainingData$BUILDINGID

fviz_pca_ind(wifiDataSetPCA, geom.ind = "point", pointshape = 21, 
             pointsize = 3, 
             fill.ind = wifiTrainingData$BUILDINGID, 
             col.ind = "black", 
             palette = "jco", 
             addEllipses = TRUE,
             label = "var",
             col.var = "black",
             repel = TRUE,
             legend.title = "Diagnosis") +
  ggtitle("2D PCA-plot from 30 feature dataset") +
  theme(plot.title = element_text(hjust = 0.5))


#######################################################################################
#Recursive Feature Elimination or RFE.
#######################################################################################

#control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
#results <- rfe(
#  CompleteResponses[,1:6], 
#  CompleteResponses[,7], 
#  sizes=c(1:6), 
#  rfeControl=control)
# summarize the results
#print(results)



#######################################################################################
#Creating KNN Model with custon grid
#######################################################################################
UnifyWifiDataSetPCA$BUILDINGID_FLOOR

#Seed needed for repeability 
set.seed(123)
#Creating the datas sets for the training and testing.
#Our dependen variable is $brand.
#It's taking 75% of the data for training and 25% for testing or verification. 

#Converting everything as factor
#for (i in 1:ncol(trainingWifiDataSetPCAKNN)){
#  trainingWifiDataSetPCAKNN[,i]=factor(trainingWifiDataSetPCAKNN[,i])
#}

inTrain <- createDataPartition(y = UnifyWifiDataSetPCA$BUILDINGID_FLOOR,
                               p = .75, # As requested, will split the data in 75%
                               list = FALSE)
#Creating the training and testing datasets.
training <- UnifyWifiDataSetPCA[ inTrain,]
testing <- UnifyWifiDataSetPCA[-inTrain,]


#Preprocessing
#kNN requires variables to be normalized or scaled. caret provides facility to preprocess data. I am going to choose centring and scaling


ctrlKNN <- trainControl(method="repeatedcv",repeats = 3 )#,classProbs=TRUE,summaryFunction = twoClassSummary)

KNNmodelTime<-system.time(knnFit <- train(BUILDINGID_FLOOR ~ .,  
                                          data = training, 
                                          method = "knn", 
                                          trControl = ctrlKNN, 
                                          preProcess = c("center","scale"), 
                                          tuneLength = 10)
)

#Output of kNN fit
knnFit

#Review the importance of the model
varImp(knnFit)

#Ploting the model

plot(knnFit)

#######################################################################################
#Creating RF Model with custon grid
#######################################################################################
set.seed(123)
#This is not needed, we can reused, but I just leaved due to academic purposes. 
rfmodelControl <- trainControl(method = "repeatedcv", number = 10,repeats = 3)
#dataframe for manual tuning of mtry
rfGrid <- expand.grid(mtry=c(4,5,6,8,10))

#traning the model with random forest
rfmodel1Time<-system.time(rfmodel <- train(BUILDINGID_FLOOR~., 
                                           data = training, 
                                           method = "rf", 
                                           trControl=rfmodelControl, 
                                           #tuneLength = 1,
                                           tuneGrid=rfGrid, 
                                           preProc = c("center", "scale"),
                                           verbose = TRUE))

#Testtime
rfmodel1Time
KNNmodelTime
varImp(rfmodel)

#######################################################################################
#Creating the model svm model
#######################################################################################


svmmodelControl <- trainControl(method = "cv", number = 10 )

svmmodelTime<-system.time(svmmodel <- train(BUILDINGID_FLOOR ~., 
                                            data = training, 
                                            method = "svmRadial",
                                            trControl = svmmodelControl,
                                            preProcess = c("center","scale"),
                                            tuneLength = 10))

svmmodelTime
rfmodel1Time
KNNmodelTime


#######################################################################################
#Creating the model c50
#######################################################################################

#Seed needed for repeability 
set.seed(123)
#Creating the datas sets for the training and testing.
#Our dependen variable is $brand.
#It's taking 75% of the data for training and 25% for testing or verification. 

#Converting everything as factor
#for (i in 1:ncol(trainingWifiDataSetPCAc50)){
#  trainingWifiDataSetPCAc50[,i]=factor(trainingWifiDataSetPCAc50[,i])
#}

#Starting the training
#10 fold cross validation

c50modelControl <- trainControl(method = "repeatedcv", number = 10,repeats = 3)

c50modelTime<-system.time(c50model <- train(BUILDINGID_FLOOR~., 
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
# Reviewing the Models TestTime
#######################################################################################

svmmodel
rfmodel
knnFit
c50model

#######################################################################################
# Reviewing the Models TestTime
#######################################################################################

svmmodelTime
rfmodel1Time
KNNmodelTime
c50modelTime

#######################################################################################
#Comparing the models in tearms of resampling
#######################################################################################
resamps <- resamples(list(C5.0 = c50model, rf = rfmodel, knn=knnFit))#, svmRadial=svmmodel))
resamps <- resamples(list(svmRadial=svmmodel,C5.0 = c50model)) 
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
# The chosen model is SVM
#######################################################################################
knnModelPredition <- predict(knnFit, newdata = testing) 

str(knnModelPredition)
summary(knnModelPredition)

knnModel2Probs <- predict(knnFit, newdata = testing, type = "prob")
head(knnModel2Probs)

#Confusion Matrix
confusionMatrix(data = knnModelPredition, testing$BUILDINGID_FLOOR)

#Using the test set use postResample() to assess the metrics of the new predictions 
#compared to the Ground Truth (see the resources for more information)
# CompleteResponses and SurveyIncomplete
postResampleData<-postResample(pred=knnModelPredition,obs=testing$BUILDINGID_FLOOR)
postResampleData
plot(postResampleData)










