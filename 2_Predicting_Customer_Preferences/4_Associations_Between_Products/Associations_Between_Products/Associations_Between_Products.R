###########################################################################
#Mauricio Carvajal Delgado
###########################################################################


###########################################################################
# Installing libraries
###########################################################################
install.packages("arules")
install.packages("arulesViz")
###########################################################################
# Libraries
###########################################################################
library(arules)
library(arulesViz)

#Understanding what does the libraries 
#?arules
#?arulesViz
###########################################################################
# Reading the data
###########################################################################
#The read.transactions() function changes the dataset into a sparse matrix.
#It makes each row represent a transaction and creates columns for each item that a customer might purchas
DatasetBasket<-read.transactions("data\\ElectronidexTransactions2017.csv") 


###########################################################################
# Getting knowleadge of the transactionl data
###########################################################################
#What are the most frequent items? 
#How many items do customers purchase the most? Least? What's the average?
inspect (DatasetBasket) # You can view the transactions. Is there a way to see a certain # of transactions?
length (DatasetBasket) # Number of transactions.
size (DatasetBasket) # Number of items per transaction
LIST(DatasetBasket) # Lists the transactions by conversion (LIST must be capitalized)
itemLabels(DatasetBasket)# To see the item labels

###########################################################################
# Visualize your Dataset
###########################################################################
itemFrequencyPlot(DatasetBasket)
image(DatasetBasket)
image(sample(DatasetBasket, 3))
             

