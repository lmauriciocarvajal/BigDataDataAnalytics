#############################################################################################
# Mauricio Carvajal
#############################################################################################
#https://towardsdatascience.com/principal-component-analysis-pca-101-using-r-361f4c53a9ff

#############################################################################################
# Install packages
#############################################################################################
install.packages("factoextra")
install.packages("ggsignif")
install.packages("ggsci", dependencies = c("Depends", "Suggests"))

#############################################################################################
# Libraries
library(caret)
library("factoextra")
library("ggsignif")

#############################################################################################
# Reading the data
#############################################################################################
wdbc  <- read.csv("C:\\Users\\lmcarva\\Documents\\Trabajo\\MVP\\Array\\Github_Array_MBIST\\BigDataDataAnalytics\\3_Deep_Analytics_and_Visualization\\3_Evaluate_Techniques_Wifi_Locationing\\UJIndoorLoc\\wdbc.csv")
#creating the names of the table
features <- c("radius", "texture", "perimeter", "area", "smoothness", "compactness", "concavity", "concave_points", "symmetry", "fractal_dimension")
#adding the names to the dataset
names(wdbc) <- c("id", "diagnosis", paste0(features,"_mean"), paste0(features,"_se"), paste0(features,"_worst"))
#Calling "prcomp" function run PCA on the data of interest (independet variables) and scaling and centered
wdbc.pr <- prcomp(wdbc[c(3:32)], center = TRUE, scale = TRUE)
#Printing the summary,
#Standard deviation: This is simply the eigenvalues in our case since the data has been centered and scaled (standardized)
#Proportion of Variance: This is the amount of variance the component accounts for in the data, ie. PC1 accounts for >44% of total variance in the data alone!
#Cumulative Proportion: This is simply the accumulated amount of explained variance, ie. if we used the first 10 components we would be able to account for >95% of total variance in the data.
summary(wdbc.pr)


#
screeplot(wdbc.pr, type = "l", npcs = 15, main = "Screeplot of the first 10 PCs")
abline(h = 1, col="red", lty=5)
legend("topright", legend=c("Eigenvalue = 1"),
       col=c("red"), lty=5, cex=0.6)
#
cumpro <- cumsum(wdbc.pr$sdev^2 / sum(wdbc.pr$sdev^2))
plot(cumpro[0:15], xlab = "PC #", ylab = "Amount of explained variance", main = "Cumulative variance plot")
abline(v = 6, col="blue", lty=5)
abline(h = 0.88759, col="blue", lty=5)

legend("topleft", legend=c("Cut-off @ PC6"),
       col=c("blue"), lty=5, cex=0.6)



#
plot(wdbc.pr$x[,1],wdbc.pr$x[,2], xlab="PC1 (44.3%)", ylab = "PC2 (19%)", main = "PC1 / PC2 - plot")
wdbc$diagnosis

fviz_pca_ind(wdbc.pr, geom.ind = "point", pointshape = 21, 
             pointsize = 2, 
             fill.ind = wdbc$diagnosis, 
             col.ind = "black", 
             palette = "jco", 
             addEllipses = TRUE,
             label = "var",
             col.var = "black",
             repel = TRUE,
             legend.title = "Diagnosis") +
  ggtitle("2D PCA-plot from 30 feature dataset") +
  theme(plot.title = element_text(hjust = 0.5))
