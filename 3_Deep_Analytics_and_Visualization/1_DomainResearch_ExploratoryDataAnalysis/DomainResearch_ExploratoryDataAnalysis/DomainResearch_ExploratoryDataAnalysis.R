#############################################################################################
# Mauricio Carvajal
#############################################################################################


#############################################################################################
# Install packages
#############################################################################################
install.packages("RMySQL")
install.packages("lubridate")

#############################################################################################
# Libraries
#############################################################################################
library(RMySQL)
library(dplyr)
library(lubridate)

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
unifyDataFrame$weekday <- weekday(unifyDataFrame$DateTime)
unifyDataFrame$day <- day(unifyDataFrame$DateTime)
unifyDataFrame$hour  <- hour(unifyDataFrame$DateTime)
unifyDataFrame$minute <- minute(unifyDataFrame$DateTime)

#############################################################################################
# Initial Exploration of the Data
#############################################################################################
summary(unifyDataFrame)
str(unifyDataFrame)

#Creating new structure
globalHeatMap <- unifyDataFrame[,c(5,12,14,15,16)]
str(globalHeatMap)

######## Plotting starts here#####################
p <-ggplot(globalHeatMap,aes(day,hour,fill=Global_active_power))+
  geom_tile(color= "white",size=0.1) + 
  scale_fill_viridis(name="Hrly Temps C",option ="C")
p <-p + facet_grid(year~month)
p <-p + scale_y_continuous(trans = "reverse", breaks = unique(globalHeatMap$hour))
p <-p + scale_x_continuous(breaks =c(1,10,20,31))
p <-p + theme_minimal(base_size = 8)
p <-p + labs(title= paste("Hourly Temps - Station"), x="Day", y="Hour Commencing")
p <-p + theme(legend.position = "bottom")+
  theme(plot.title=element_text(size = 14))+
  theme(axis.text.y=element_text(size=6)) +
  theme(strip.background = element_rect(colour="white"))+
  theme(plot.title=element_text(hjust=0))+
  theme(axis.ticks=element_blank())+
  theme(axis.text=element_text(size=7))+
  theme(legend.title=element_text(size=8))+
  theme(legend.text=element_text(size=6))+
  removeGrid()#ggExtra

# you will want to expand your plot screen before this bit!
p #awesomeness


