###########################################################################
#Mauricio Carvajal Delgado
###########################################################################


###########################################################################
# Installing libraries
###########################################################################
install.packages("arules")
install.packages("arulesViz")
install.packages("caTools")
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
DatasetBasket<-read.transactions("data\\ElectronidexTransactions2017.csv",
                                 format = "basket", 
                                 sep=",", 
                                 rm.duplicates=TRUE) ## We need to elminate duplicates from 1 transaction.


###########################################################################
# Getting knowleadge of the transactionl data
###########################################################################
#What are the most frequent items? 
#How many items do customers purchase the most? Least? What's the average?
#Summary give us idea of the most frequent items
summary(DatasetBasket)
inspectData<-inspect (DatasetBasket[1:10]) # You can view the transactions. Is there a way to see a certain # of transactions?
summary(inspectData)
# Number of transactions.
length (DatasetBasket) 
# Number of items per transaction
size (DatasetBasket) 
#Lists the transactions by conversion (LIST must be capitalized)
LIST(DatasetBasket) 
# To see the item labels
itemLabels(DatasetBasket)

###########################################################################
# Visualize your Dataset
###########################################################################
#checking all the data
itemFrequencyPlot(DatasetBasket)
#Checking the top10
itemFrequencyPlot(DatasetBasket, topN = 10)

itemFrequencyPlot(DatasetBasket, topN = 10,type = "relative")
itemFrequencyPlot(DatasetBasket, topN = 10,type = "absolute")
#checking by support, in this case the 8%
itemFrequencyPlot(DatasetBasket, support = 0.08)

#Using image
image(DatasetBasket)
image(DatasetBasket[1:150])

image(sample(DatasetBasket, 150))

###########################################################################
# Apply the Apriori Algorithm
###########################################################################
#These parameters are requesting that the rules cover 10% of the transactions and are 80% correct.
RulesName<- apriori (DatasetBasket, parameter = list(supp = 0.0045, conf = 0.68, minlen=2))
#To view your rules
inspect(RulesName)

###########################################################################
# Evaluate Your Model
###########################################################################
summary(RulesName)
#Print  only top 5 rules
inspect(sort(RulesName, by = "confidence")[1:5])


###########################################################################
# Improving the model
###########################################################################
#sorting your rule's by their measurements
inspect(sort(RulesName, by = "confidence"))
#Print  only top 5 that are sorted
inspect(sort(RulesName, by = "confidence")[1:5])
#Print  only top 5 that are sorted descending
inspect(sort(RulesName, by = "confidence",decreasing = TRUE)[1:5])

#seeing a specific item's rules be helpful to your analysis
ItemRules <- subset(RulesName, items %in% "HP Laptop")
inspect(ItemRules)

#Redundant rules
is.redundant(RulesName)

#Visualize Your Results
plot(RulesName)

plot(RulesName[1:5], method="graph", control=list(type="items"))
plot(RulesName[1:5], method="paracoord", control=list(type="items"))


plot(RulesName, method="graph", control=list(type="items"))
plot(RulesName, method="paracoord", control=list(type="items"))

oneRule <- sample(RulesName, 1)
inspect(oneRule)

plot(oneRule, method = "doubledecker", data = RulesName)
