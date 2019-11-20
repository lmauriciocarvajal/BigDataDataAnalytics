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

#############################################################################################
# PreProcesing 
#############################################################################################
#Converting into categorical values
#CompleteResponses$elevel<-as.factor(CompleteResponses$elevel) 
#Moving the 



#############################################################################################
# Deleting the less variance columns
#############################################################################################
#The idea is to delete the coluns that has close or near close variance to zero, in order to 
#simplyfy the analysis
#Setting the variable in which is identify zero variance.
zeroVar <- logical()

for (i in c(1:ncol(wifiTrainingData))) {
  zeroVar[i] <- var(wifiTrainingData[,i]) != 0
}
#Printing
zeroVar
wifiTrainingData <- wifiTrainingData[, zeroVar]
wifiValidationData <- wifiValidationData[, zeroVar]


#############################################################################################
# PCA
#############################################################################################
# This has been used from -->https://towardsdatascience.com/principal-component-analysis-pca-101-using-r-361f4c53a9ff

#First select only the WAP values
#head(wifiTrainingData[466])
wifiDataSetPCA <- prcomp(wifiTrainingData[c(0:465)], 
                         center = TRUE, 
                         scale = TRUE)
summary(wifiDataSetPCA)

screeplot(wifiDataSetPCA, type = "l", npcs = 80, main = "Screeplot of the WAPs")
abline(h = 1, col="red", lty=5)
legend("topright", legend=c("Eigenvalue = 1"),
       col=c("red"), lty=5, cex=0.6)


cumpro <- cumsum(wifiDataSetPCA$sdev^2 / sum(wifiDataSetPCA$sdev^2))
plot(cumpro[0:80], xlab = "PC #", ylab = "Amount of explained variance", main = "Cumulative variance plot")
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


#los datos están ahora en otra escala totalmente diferente, y almacenados en el campo $x
View(head(wifiDataSetPCA$x))

#apartar la desviación estándar de cada componente
std_dev <- wifiDataSetPCA$sdev

#calcular la varianza de cada componente
pr_var <- std_dev^2

#calcular la proporción de varianza explicada por cada componente
prop_varex <- pr_var/sum(pr_var)

#graficar la proporción de varianza explicada
plot(prop_varex, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     type = "b")

#deberían ver un gráfico como el de abajo. La idea  es que uds  noten dónde está "el codo" del gráfico, porque a partir de ese punto

#es donde cada componente nuevo explica menos variabilidad, y por lo tanto es menos importante.

#yo voy a tomar el componente 50 como "el codo".


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



