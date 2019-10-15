#############################################################################################
# Mauricio Carvajal
#############################################################################################


#############################################################################################
# Install packages
#############################################################################################
install.packages("RMySQL")
install.packages("lubridate")
install.packages("forcats")

#############################################################################################
# Libraries
#############################################################################################
library(RMySQL)
library(dplyr)
library(lubridate)
library(ggplot2)
library(forcats)
library(RColorBrewer)

#############################################################################################
# SQL
#############################################################################################

## Create a database connection 
con = dbConnect(MySQL(), 
                user='deepAnalytics', 
                password='Sqltask1234!', 
                dbname='dataanalytics2018', 
                host='data-analytics-2018.cbrosir2cswx.us-east-1.rds.amazonaws.com')
## List the tables contained in the database 
dbListTables(con)

#############################################################################################
# Checking IRIS
#############################################################################################
## Lists attributes contained in a table
dbListFields(con,'iris')
## Use asterisk to specify all attributes for download
irisALL <- dbGetQuery(con, "SELECT * FROM iris")
## Use attribute names to specify specific attributes for download
irisSELECT <- dbGetQuery(con, "SELECT SepalLengthCm, SepalWidthCm FROM iris")

#############################################################################################
# Selecitng years from 2006 through 2010 with Date, Time and the 3 sub-meter attributes
#############################################################################################
#checking the attributes available
dbListFields(con,'yr_2006')
yr_2006<- dbGetQuery(con, "SELECT Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2006")
yr_2007<- dbGetQuery(con, "SELECT Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2007")
yr_2008<- dbGetQuery(con, "SELECT Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2008")
yr_2009<- dbGetQuery(con, "SELECT Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2009")
yr_2010<- dbGetQuery(con, "SELECT Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2010")

#############################################################################################
# Selecitng years from 2006 through 2010 for all the atributtes
#############################################################################################
#checking the attributes available
dbListFields(con,'yr_2006')
yr_2006_all<- dbGetQuery(con, "SELECT * FROM yr_2006")
yr_2007_all<- dbGetQuery(con, "SELECT * FROM yr_2007")
yr_2008_all<- dbGetQuery(con, "SELECT * FROM yr_2008")
yr_2009_all<- dbGetQuery(con, "SELECT * FROM yr_2009")
yr_2010_all<- dbGetQuery(con, "SELECT * FROM yr_2010")

#############################################################################################
# Looking for the new data frame
#############################################################################################
#checking the structure
str(yr_2006)
#summary
summary(yr_2006)
#head
head(yr_2006)
#tail
tail(yr_2006)
#2007
str(yr_2007)
#summary
summary(yr_2007)
#head
head(yr_2007)
#tail
tail(yr_2007)
#2008
str(yr_2008)
#summary
summary(yr_2008)
#head
head(yr_2008)
#tail
tail(yr_2008)

#2009
str(yr_2009)
#summary
summary(yr_2009)
#head
head(yr_2009)
#tail
tail(yr_2009)


#############################################################################################
# Creating a primarly data frame
#############################################################################################
#create a Multi-Year data frame that will serve as the primary data frame for the project
## Combine tables into one dataframe using dplyr
unifyDataFrame <- bind_rows(yr_2006_all, yr_2007_all, yr_2008_all,yr_2009_all,yr_2010_all)
#checking the structure
str(unifyDataFrame)
#summary
summary(unifyDataFrame)
#head
head(unifyDataFrame)
#tail
tail(unifyDataFrame)

#############################################################################################
# Combine data and time attributes
#############################################################################################

## Combine Date and Time attribute values in a new attribute column
unifyDataFrame <-cbind(unifyDataFrame,paste(unifyDataFrame$Date,unifyDataFrame$Time), stringsAsFactors=FALSE)

#unifyDataFrame<-"0"
## Give the new attribute in the 6th column a header name 
## NOTE: if you downloaded more than 5 attributes you will need to change the column number)
colnames(unifyDataFrame)[11] <-"DateTime"

## Move the DateTime attribute within the dataset
unifyDataFrame <- unifyDataFrame[,c(ncol(unifyDataFrame), 1:(ncol(unifyDataFrame)-1))]
head(unifyDataFrame)

## Convert DateTime from character to POSIXct
unifyDataFrame$DateTime <- as.POSIXct(unifyDataFrame$DateTime, "%Y/%m/%d %H:%M:%S")
## Add the time zone
attr(unifyDataFrame$DateTime, "tzone") <- "Europe/Paris"
## Inspect the data types
str(unifyDataFrame)

## Create "year" attribute with lubridate

unifyDataFrame$year <- year(unifyDataFrame$DateTime)
unifyDataFrame$quarter <- quarter(unifyDataFrame$DateTime)
unifyDataFrame$month <- month(unifyDataFrame$DateTime)
unifyDataFrame$week <- week(unifyDataFrame$DateTime)
unifyDataFrame$weekdays <- weekdays(unifyDataFrame$DateTime)
unifyDataFrame$day <- day(unifyDataFrame$DateTime)
unifyDataFrame$hour  <- hour(unifyDataFrame$DateTime)
unifyDataFrame$minute <- minute(unifyDataFrame$DateTime)

#############################################################################################
# Creating a plot of power comsuption per quarter/year.
#############################################################################################

#Getting the average value per quarter, also mutipling 1000/60 as the documentation suggest.
#Notes:
#1.(global_active_power*1000/60 - sub_metering_1 - sub_metering_2 - sub_metering_3) represents the active energy consumed every minute (in watt hour) in the household by electrical equipment not measured in sub-meterings 1, 2 and 3.
dataset2006q1  <-0*(1000/60)*(as.numeric(summarise(filter(unifyDataFrame, year==2006 & quarter ==1), delay = mean(Global_active_power, na.rm = TRUE))))
dataset2006q2  <-0*(1000/60)*(as.numeric(summarise(filter(unifyDataFrame, year==2006 & quarter ==2), delay = mean(Global_active_power, na.rm = TRUE))))
dataset2006q3  <-0*(1000/60)*(as.numeric(summarise(filter(unifyDataFrame, year==2006 & quarter ==3), delay = mean(Global_active_power, na.rm = TRUE))))
dataset2006q4  <-(1000/60)*(as.numeric(summarise(filter(unifyDataFrame, year==2006 & quarter ==4), delay = mean(Global_active_power, na.rm = TRUE))))
dataset2007q1  <-(1000/60)*(as.numeric(summarise(filter(unifyDataFrame, year==2007 & quarter ==1), delay = mean(Global_active_power, na.rm = TRUE))))
dataset2007q2  <-(1000/60)*(as.numeric(summarise(filter(unifyDataFrame, year==2007 & quarter ==2), delay = mean(Global_active_power, na.rm = TRUE))))
dataset2007q3  <-(1000/60)*(as.numeric(summarise(filter(unifyDataFrame, year==2007 & quarter ==3), delay = mean(Global_active_power, na.rm = TRUE))))
dataset2007q4  <-(1000/60)*(as.numeric(summarise(filter(unifyDataFrame, year==2007 & quarter ==4), delay = mean(Global_active_power, na.rm = TRUE))))
dataset2008q1  <-(1000/60)*(as.numeric(summarise(filter(unifyDataFrame, year==2008 & quarter ==1), delay = mean(Global_active_power, na.rm = TRUE))))
dataset2008q2  <-(1000/60)*(as.numeric(summarise(filter(unifyDataFrame, year==2008 & quarter ==2), delay = mean(Global_active_power, na.rm = TRUE))))
dataset2008q3  <-(1000/60)*(as.numeric(summarise(filter(unifyDataFrame, year==2008 & quarter ==3), delay = mean(Global_active_power, na.rm = TRUE))))
dataset2008q4  <-(1000/60)*(as.numeric(summarise(filter(unifyDataFrame, year==2008 & quarter ==4), delay = mean(Global_active_power, na.rm = TRUE))))
dataset2009q1  <-(1000/60)*(as.numeric(summarise(filter(unifyDataFrame, year==2009 & quarter ==1), delay = mean(Global_active_power, na.rm = TRUE))))
dataset2009q2  <-(1000/60)*(as.numeric(summarise(filter(unifyDataFrame, year==2009 & quarter ==2), delay = mean(Global_active_power, na.rm = TRUE))))
dataset2009q3  <-(1000/60)*(as.numeric(summarise(filter(unifyDataFrame, year==2009 & quarter ==3), delay = mean(Global_active_power, na.rm = TRUE))))
dataset2009q4  <-(1000/60)*(as.numeric(summarise(filter(unifyDataFrame, year==2009 & quarter ==4), delay = mean(Global_active_power, na.rm = TRUE))))
dataset2010q1  <-(1000/60)*(as.numeric(summarise(filter(unifyDataFrame, year==2010 & quarter ==1), delay = mean(Global_active_power, na.rm = TRUE))))
dataset2010q2  <-(1000/60)*(as.numeric(summarise(filter(unifyDataFrame, year==2010 & quarter ==2), delay = mean(Global_active_power, na.rm = TRUE))))
dataset2010q3  <-(1000/60)*(as.numeric(summarise(filter(unifyDataFrame, year==2010 & quarter ==3), delay = mean(Global_active_power, na.rm = TRUE))))
dataset2010q4  <-(1000/60)*(as.numeric(summarise(filter(unifyDataFrame, year==2010 & quarter ==4), delay = mean(Global_active_power, na.rm = TRUE))))

#Creating the matrix that will be used for plotting, 2006 will be deleted due to missing data, but will be analized in next graphics 
yeardataall<-cbind(
                   #c(dataset2006q1,dataset2006q2,dataset2006q3,dataset2006q4),
                   c(dataset2007q1,dataset2007q2,dataset2007q3,dataset2007q4),
                   c(dataset2008q1, dataset2008q2, dataset2008q3, dataset2008q4),
                   c(dataset2009q1,dataset2009q2,dataset2009q3,dataset2009q4),
                   c(dataset2010q1,dataset2010q2,dataset2010q3,dataset2010q4))

#Renamming the cols and rows
colnames(yeardataall) <- c("2007","2008","2009","2010")
rownames(yeardataall) <- c("Q1","Q2","Q3","Q4")


# Creating the plot
coul <- brewer.pal(4, "Paired")
barplot(yeardataall, 
        col=coul ,
        border="white", 
        space=0.04, 
        font.axis=2,
        legend=rownames(yeardataall), 
        xlab="Distribution of totalconsumption of power thru years",
        ylab="Average power in Watts")


#############################################################################################
# Distribution of Total power thru the week days
#############################################################################################
#Getting the average power from 2006-2010 per weekdays
datasetMonday   <-(1000/60)*(as.numeric(summarise(filter(unifyDataFrame, weekdays == "Monday"  ), delay = mean(Global_active_power, na.rm = TRUE))))
datasetTuesday  <-(1000/60)*(as.numeric(summarise(filter(unifyDataFrame, weekdays =="Tuesday"  ), delay = mean(Global_active_power, na.rm = TRUE))))  
datasetWednesday<-(1000/60)*(as.numeric(summarise(filter(unifyDataFrame, weekdays =="Wednesday"), delay = mean(Global_active_power, na.rm = TRUE))))  
datasetThursday <-(1000/60)*(as.numeric(summarise(filter(unifyDataFrame, weekdays =="Thursday" ), delay = mean(Global_active_power, na.rm = TRUE))))  
datasetFriday   <-(1000/60)*(as.numeric(summarise(filter(unifyDataFrame, weekdays =="Friday"   ), delay = mean(Global_active_power, na.rm = TRUE))))  
datasetSaturday <-(1000/60)*(as.numeric(summarise(filter(unifyDataFrame, weekdays =="Saturday" ), delay = mean(Global_active_power, na.rm = TRUE))))  
datasetSunday   <-(1000/60)*(as.numeric(summarise(filter(unifyDataFrame, weekdays =="Sunday"   ), delay = mean(Global_active_power, na.rm = TRUE))))
# Create data

Global_Active_Power_Bar_per_day <- data.frame(
  name=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday") ,  
  val=c(datasetMonday,datasetTuesday,datasetWednesday,datasetThursday,datasetFriday,datasetSaturday,datasetSunday)
)

# Reorder following the value of another column:
Global_Active_Power_Bar_per_day %>%
  arrange(val) %>%
  mutate(name = factor(name, levels=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))) %>%
  ggplot( aes(x=name, y=val)) +
  geom_segment( aes(xend=name, yend=0)) +
  geom_point( size=6, color="dark green") +
  theme_bw() +
  xlab("Distribution of totalconsumption of power per days weeks")+
  ylab("Average power in Watts")



#############################################################################################
# Summary of Distribution of Total power thru the week days per sub metering
#############################################################################################
#Getting the values of sub1
sub1_datasetMonday   <-as.numeric(summarise(filter(unifyDataFrame, weekdays == "Monday"  ), delay = mean(Sub_metering_1, na.rm = TRUE)))
sub1_datasetTuesday  <-as.numeric(summarise(filter(unifyDataFrame, weekdays =="Tuesday"  ), delay = mean(Sub_metering_1, na.rm = TRUE)))  
sub1_datasetWednesday<-as.numeric(summarise(filter(unifyDataFrame, weekdays =="Wednesday"), delay = mean(Sub_metering_1, na.rm = TRUE)))  
sub1_datasetThursday <-as.numeric(summarise(filter(unifyDataFrame, weekdays =="Thursday" ), delay = mean(Sub_metering_1, na.rm = TRUE)))  
sub1_datasetFriday   <-as.numeric(summarise(filter(unifyDataFrame, weekdays =="Friday"   ), delay = mean(Sub_metering_1, na.rm = TRUE)))  
sub1_datasetSaturday <-as.numeric(summarise(filter(unifyDataFrame, weekdays =="Saturday" ), delay = mean(Sub_metering_1, na.rm = TRUE)))  
sub1_datasetSunday   <-as.numeric(summarise(filter(unifyDataFrame, weekdays =="Sunday"   ), delay = mean(Sub_metering_1, na.rm = TRUE)))
#Getting the values of sub2
sub2_datasetMonday   <-as.numeric(summarise(filter(unifyDataFrame, weekdays == "Monday"  ), delay = mean(Sub_metering_2, na.rm = TRUE)))
sub2_datasetTuesday  <-as.numeric(summarise(filter(unifyDataFrame, weekdays =="Tuesday"  ), delay = mean(Sub_metering_2, na.rm = TRUE)))  
sub2_datasetWednesday<-as.numeric(summarise(filter(unifyDataFrame, weekdays =="Wednesday"), delay = mean(Sub_metering_2, na.rm = TRUE)))  
sub2_datasetThursday <-as.numeric(summarise(filter(unifyDataFrame, weekdays =="Thursday" ), delay = mean(Sub_metering_2, na.rm = TRUE)))  
sub2_datasetFriday   <-as.numeric(summarise(filter(unifyDataFrame, weekdays =="Friday"   ), delay = mean(Sub_metering_2, na.rm = TRUE)))  
sub2_datasetSaturday <-as.numeric(summarise(filter(unifyDataFrame, weekdays =="Saturday" ), delay = mean(Sub_metering_2, na.rm = TRUE)))  
sub2_datasetSunday   <-as.numeric(summarise(filter(unifyDataFrame, weekdays =="Sunday"   ), delay = mean(Sub_metering_2, na.rm = TRUE)))
#Getting the values of sub3
sub3_datasetMonday   <-as.numeric(summarise(filter(unifyDataFrame, weekdays == "Monday"  ), delay = mean(Sub_metering_3, na.rm = TRUE)))
sub3_datasetTuesday  <-as.numeric(summarise(filter(unifyDataFrame, weekdays =="Tuesday"  ), delay = mean(Sub_metering_3, na.rm = TRUE)))  
sub3_datasetWednesday<-as.numeric(summarise(filter(unifyDataFrame, weekdays =="Wednesday"), delay = mean(Sub_metering_3, na.rm = TRUE)))  
sub3_datasetThursday <-as.numeric(summarise(filter(unifyDataFrame, weekdays =="Thursday" ), delay = mean(Sub_metering_3, na.rm = TRUE)))  
sub3_datasetFriday   <-as.numeric(summarise(filter(unifyDataFrame, weekdays =="Friday"   ), delay = mean(Sub_metering_3, na.rm = TRUE)))  
sub3_datasetSaturday <-as.numeric(summarise(filter(unifyDataFrame, weekdays =="Saturday" ), delay = mean(Sub_metering_3, na.rm = TRUE)))  
sub3_datasetSunday   <-as.numeric(summarise(filter(unifyDataFrame, weekdays =="Sunday"   ), delay = mean(Sub_metering_3, na.rm = TRUE)))
#Getting the values of the rest of the house, using the especifications:
#Notes:
#1.(global_active_power*1000/60 - sub_metering_1 - sub_metering_2 - sub_metering_3) represents the active energy consumed every minute (in watt hour) in the household by electrical equipment not measured in sub-meterings 1, 2 and 3.
notMeasureDatasetMonday   <-datasetMonday   -sub1_datasetMonday   -sub2_datasetMonday   -sub3_datasetMonday   
notMeasureDatasetTuesday  <-datasetTuesday  -sub1_datasetTuesday  -sub2_datasetTuesday  -sub3_datasetTuesday  
notMeasureDatasetWednesday<-datasetWednesday-sub1_datasetWednesday-sub2_datasetWednesday-sub3_datasetWednesday
notMeasureDatasetThursday <-datasetThursday -sub1_datasetThursday -sub2_datasetThursday -sub3_datasetThursday 
notMeasureDatasetFriday   <-datasetFriday   -sub1_datasetFriday   -sub2_datasetFriday   -sub3_datasetFriday   
notMeasureDatasetSaturday <-datasetSaturday -sub1_datasetSaturday -sub2_datasetSaturday -sub3_datasetSaturday 
notMeasureDatasetSunday   <-datasetSunday   -sub1_datasetSunday   -sub2_datasetSunday   -sub3_datasetSunday  
#Creating the matrix
submeterdataall<-rbind(c(sub1_datasetMonday  ,sub1_datasetTuesday  ,sub1_datasetWednesday,sub1_datasetThursday ,sub1_datasetFriday   ,sub1_datasetSaturday ,sub1_datasetSunday),
                       c(sub2_datasetMonday  ,sub2_datasetTuesday  ,sub2_datasetWednesday,sub2_datasetThursday ,sub2_datasetFriday   ,sub2_datasetSaturday ,sub2_datasetSunday),
                       c(sub3_datasetMonday  ,sub3_datasetTuesday  ,sub3_datasetWednesday,sub3_datasetThursday ,sub3_datasetFriday   ,sub3_datasetSaturday ,sub3_datasetSunday),
                       c(notMeasureDatasetMonday,notMeasureDatasetTuesday,notMeasureDatasetWednesday,notMeasureDatasetThursday,notMeasureDatasetFriday,notMeasureDatasetSaturday,notMeasureDatasetSunday))
#Renamming the cols and rows
colnames(submeterdataall) <- c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
rownames(submeterdataall) <- c("SubMetering1","SubMetering2","SubMetering3","NoMeasurebySub")
coul <- brewer.pal(4, "Paired")
# Creating the plot
barplot(submeterdataall, 
        col=coul ,
        #col=colors()[c(23,89,12,60)] , 
        border="white", 
        font.axis=2, 
        beside=T, 
        legend=rownames(submeterdataall), 
        xlab="Distribution of totalconsumption of power per days weeks",
        ylab="Average power in Watts",
        font.lab=2)

#############################################################################################
# Porcentage of Distribution of Total power thru the week days per sub metering
#############################################################################################
# Transform the data in Porcentage
data_percentage <- apply(submeterdataall, 2, function(x){x*100/sum(x,na.rm=T)})
# Make a stacked barplot
barplot(data_percentage, 
        col=coul ,
        legend=rownames(submeterdataall), 
        border="white", 
        xlab="Distribution of totalconsumption of power per days weeks",
        ylab="Porcentage %")

#############################################################################################
# Creating the individual plots per sub metering 
#############################################################################################
#SubMeter1
SubMetering1<-rbind(c(sub1_datasetMonday  ,sub1_datasetTuesday  ,sub1_datasetWednesday,sub1_datasetThursday ,sub1_datasetFriday   ,sub1_datasetSaturday ,sub1_datasetSunday))
colnames(SubMetering1) <- c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
rownames(SubMetering1) <- c("SubMetering1")
barplot(SubMetering1, 
        border="white",
        col="dark green" ,
        font.axis=2, 
        beside=T, 
        #legend=rownames(submeterdataall), 
        xlab="Distribution of totalconsumption of power per days weeks",
        ylab="Average power in Watts",
        main="Sub-Meter 1",
        font.lab=2)
#############################################################################################
# Creating the individual plots per sub metering 
#############################################################################################
#SubMeter 2
SubMetering2<-rbind(c(sub2_datasetMonday  ,sub2_datasetTuesday  ,sub2_datasetWednesday,sub2_datasetThursday ,sub2_datasetFriday   ,sub2_datasetSaturday ,sub2_datasetSunday))
colnames(SubMetering2) <- c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
rownames(SubMetering2) <- c("SubMetering2")
barplot(SubMetering2, 
        border="white",
        col="dark green" ,
        font.axis=2, 
        beside=T, 
        #legend=rownames(submeterdataall), 
        xlab="Distribution of totalconsumption of power per days weeks",
        ylab="Average power in Watts",
        main="Sub-Meter 2",
        font.lab=2)
#############################################################################################
# Creating the individual plots per sub metering 
#############################################################################################
#SubMeter 3
SubMetering3<-rbind(c(sub3_datasetMonday  ,sub3_datasetTuesday  ,sub3_datasetWednesday,sub3_datasetThursday ,sub3_datasetFriday   ,sub3_datasetSaturday ,sub3_datasetSunday))
colnames(SubMetering3) <- c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
rownames(SubMetering3) <- c("SubMetering3")
barplot(SubMetering3, 
        border="white", 
        col="dark green" ,
        font.axis=2, 
        beside=T, 
        #legend=rownames(submeterdataall), 
        xlab="Distribution of totalconsumption of power per days weeks",
        ylab="Average power in Watts",
        main="Sub-Meter 3",
        font.lab=2)

#############################################################################################
# Creating the individual plots per sub metering 
#############################################################################################
#No Measure by sub meter
NoMeasurebySub<-rbind(c(notMeasureDatasetMonday,notMeasureDatasetTuesday,notMeasureDatasetWednesday,notMeasureDatasetThursday,notMeasureDatasetFriday,notMeasureDatasetSaturday,notMeasureDatasetSunday))
colnames(NoMeasurebySub) <- c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
rownames(NoMeasurebySub) <- c("NoMeasurebySub")
barplot(NoMeasurebySub, 
        col="dark green",
        border="white", 
        font.axis=2, 
        beside=T, 
        #legend=rownames(NoMeasurebySub), 
        xlab="Distribution of totalconsumption of power per days weeks",
        ylab="Average power in Watts",
        main="No Measure by any sub meter",
        font.lab=2)








