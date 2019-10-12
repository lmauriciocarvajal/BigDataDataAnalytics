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
unifyDataFrame <- bind_rows(yr_2006, yr_2007, yr_2008,yr_2009,yr_2010)
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

## Give the new attribute in the 6th column a header name 
## NOTE: if you downloaded more than 5 attributes you will need to change the column number)
colnames(unifyDataFrame)[6] <-"DateTime"

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
#unifyDataFrame$weekday <- weekday(unifyDataFrame$DateTime)
unifyDataFrame$day <- day(unifyDataFrame$DateTime)
unifyDataFrame$hour  <- hour(unifyDataFrame$DateTime)
unifyDataFrame$minute <- minute(unifyDataFrame$DateTime)

#############################################################################################
# Initial Exploration of the Data
#############################################################################################
summary(unifyDataFrame)





