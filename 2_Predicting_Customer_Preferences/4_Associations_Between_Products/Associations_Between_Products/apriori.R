
#From Here-->https://www.r-bloggers.com/association-rule-learning-and-the-apriori-algorithm/

###########################################################################
# Libs
###########################################################################
library("arules");
library("arulesViz");

###########################################################################
# Generate ramdon data
###########################################################################
patterns = random.patterns(nItems = 1000);
summary(patterns);

trans = random.transactions(nItems = 1000, nTrans = 1000, method = "agrawal",  patterns = patterns);

image(trans);

###########################################################################
# Generate ramdon data
###########################################################################
data("AdultUCI")

str(AdultUCI)
AdultUCI$`capital-gain`<-NULL
AdultUCI$`capital-loss`<-NULL
Adult = as(AdultUCI, "transactions")

rules = apriori(Adult, parameter=list(support=0.01, confidence=0.5))

rules;