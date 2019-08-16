library(caret) #caret-->clasificacion and regresion testing

library(mlbench)
data(Sonar)
set.seed(107)
inTrain <- createDataPartition(y = Sonar$Class,
                               ## the outcome data are needed 
                               p = .75,
                               ## The percentage of data in the
                               ## training set
                               list = FALSE) #Ask why list should be false?
                                ## The format of the results

## The output is a set of integers for the rows of Sonar
## that belong in the training set. 
str(inTrain)
training <- Sonar[ inTrain,]
testing <- Sonar[-inTrain,]
nrow(training)
nrow(testing)

ctrl <- trainControl(method = "repeatedcv", 
                       repeats = 3,
                       classProbs = TRUE, 
                       summaryFunction = twoClassSummary) #defaultSummary 
                       #summaryFunction = defaultSummary) 

plsFit <- train(Class ~ ., #con todas las variables
                data = training, 
                method = "pls",
                tuneLength = 15,
                trControl = ctrl,
                metric = "ROC",
                preProc = c("center", "scale"))
plsFit
plot(plsFit) 

plsClasses <- predict(plsFit, newdata = testing) 
str(plsClasses)
plsProbs <- predict(plsFit, newdata = testing, type = "prob")
head(plsProbs)
confusionMatrix(data = plsClasses, testing$Class)                 


## To illustrate, a custom grid is used
rdaGrid = data.frame(gamma = (0:4)/4, lambda = 3/4)
set.seed(123)
rdaFit <- train(Class ~ ., 
                data = training, 
                method = "rda",
                tuneGrid = rdaGrid, 
                trControl = ctrl, 
                metric = "ROC")
rdaFit
rdaClasses <- predict(rdaFit, newdata = testing) 
confusionMatrix(rdaClasses, testing$Class)
resamps <- resamples(list(pls = plsFit, rda = rdaFit))
summary(resamps)
diffs <- diff(resamps)
summary(diffs)
xyplot(resamps, what = "BlandAltman") #Testing
                                  