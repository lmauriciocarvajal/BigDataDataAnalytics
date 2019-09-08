######################################################################################
#Libraries that are needed
######################################################################################
library(caret)
#?dummyVars

customers <- data.frame(
  id=c(10,20,30,40,50),
  gender=c('male','female','female','male','female'),
  mood=c('happy','sad','happy','sad','happy'),
  outcome=c(1,1,0,0,0))

# dummify the data
dmy <- dummyVars(" ~ .", data = customers)
trsf <- data.frame(predict(dmy, newdata = customers))
print(trsf)


require(caret)
data(GermanCredit)

require(MASS)
