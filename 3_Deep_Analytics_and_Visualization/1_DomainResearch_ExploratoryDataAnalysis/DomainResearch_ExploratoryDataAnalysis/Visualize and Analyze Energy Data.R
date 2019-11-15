#############################################################################################
# Mauricio Carvajal
#############################################################################################


#############################################################################################
# Install packages
#############################################################################################
install.packages("RMySQL")
install.packages("lubridate")
install.packages("forcats")
install.packages("plotly")
install.packages("ggfortify")
install.packages("forecast")


#############################################################################################
# Libraries
#############################################################################################
library(RMySQL)
library(dplyr)
library(lubridate)
library(ggplot2)
library(forcats)
library(RColorBrewer)
library(plotly)
library(ggfortify)
library(forecast)

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
# Plan of attack
#############################################################################################
## Plot all of sub-meter 1
plot(unifyDataFrame$Sub_metering_1)


#############################################################################################
#############################################################################################
# Part 1, Subsetting and Meaningful Time Periods
#############################################################################################
#############################################################################################
## Subset the second week of 2008 - All Observations
houseWeek <- filter(unifyDataFrame, year == 2008 & week == 2)
## Plot subset houseWeek
plot(houseWeek$Sub_metering_1)

#############################################################################################
# Visualize a Single Day with Plotly
#############################################################################################

## Subset the 9th day of January 2008 - All observations
houseDay <- filter(unifyDataFrame, year == 2008 & month == 1 & day == 9)
## Plot sub-meter 1
plot_ly(houseDay, x = ~houseDay$DateTime, y = ~houseDay$Sub_metering_1, type = 'scatter', mode = 'lines')

## Plot sub-meter 1, 2 and 3 with title, legend and labels - All observations 
plot_ly(houseDay, x = ~houseDay$DateTime, y = ~houseDay$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseDay$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseDay$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption January 9th, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

#############################################################################################
# Reducing Granularity
#############################################################################################
## Subset the 9th day of January 2008 - 10 Minute frequency
houseDay10 <- filter(unifyDataFrame, year == 2008 & month == 1 & day == 9 & (minute == 0 | minute == 10 | minute == 20 | minute == 30 | minute == 40 | minute == 50))

## Plot sub-meter 1, 2 and 3 with title, legend and labels - 10 Minute frequency
plot_ly(houseDay10, x = ~houseDay10$DateTime, y = ~houseDay10$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseDay10$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseDay10$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption January 9th, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

#############################################################################################
# Adding the not submting measure
#############################################################################################

## Subset the 9th day of January 2008 - 10 Minute frequency
houseDay10 <- filter(unifyDataFrame, year == 2008 & month == 1 & day == 9 & (minute == 0 | minute == 10 | minute == 20 | minute == 30 | minute == 40 | minute == 50))

#Not measure by submeter
notMeasureHouseDay10   <-(((1000/60)*houseDay10$Global_active_power)-houseDay10$Sub_metering_1-houseDay10$Sub_metering_2 -houseDay10$Sub_metering_3)
global_Active_PowerHouseDay10<-(1000/60)*houseDay10$Global_active_power
## Plot sub-meter 1, 2 and 3 with title, legend and labels - 10 Minute frequency
plot_ly(houseDay10, x = ~houseDay10$DateTime, y = ~houseDay10$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseDay10$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseDay10$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  add_trace(y = ~notMeasureHouseDay10, name = 'Not measure by submemter', mode = 'lines') %>%
  add_trace(y = ~global_Active_PowerHouseDay10, name = 'Global Active power', mode = 'lines') %>%
  layout(title = "Power Consumption January 9th, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

plot_ly(houseDay10, x = ~houseDay10$DateTime, y = ~houseDay10$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseDay10$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseDay10$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  add_trace(y = ~notMeasureHouseDay10, name = 'Not measure by submemter', mode = 'lines') %>%
  layout(title = "Power Consumption January 9th, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

#############################################################################################
# Visualization per week
#############################################################################################
#Create a visualization with plotly for a Week of your choosing. Use all three sub-meters 
#and make sure to label. Experiment with granularity. 
## Subset the 9th day of January 2008 - 10 Minute frequency
#houseWeek1 <- filter(unifyDataFrame, year == 2008 & week == 2 & (minute == 0 | minute == 10 | minute == 20 | minute == 30 | minute == 40 | minute == 50))

houseWeek1 <- filter(unifyDataFrame, year == 2008  & week == 2  & (hour== 0| hour== 1| hour== 2| hour== 3| hour== 4| hour== 5| hour== 6| hour== 7| hour== 8| hour== 9| hour== 10| hour== 11| hour== 12| hour== 13| hour== 14| hour== 15| hour== 16| hour== 17| hour== 18| hour== 19| hour== 20| hour== 21| hour== 22| hour== 23))

## Plot sub-meter 1, 2 and 3 with title, legend and labels - 10 Minute frequency
plot_ly(houseWeek1, x = ~houseWeek1$DateTime, y = ~houseWeek1$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseWeek1$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseWeek1$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption Week 2, January 8th-14, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

#############################################################################################
# Visualization per week
#############################################################################################
#Create a visualization with plotly for a Week of your choosing. Use all three sub-meters 
#and make sure to label. Experiment with granularity. 
## Subset the 9th day of January 2008 - 10 Minute frequency
#houseWeek1 <- filter(unifyDataFrame, year == 2008 & week == 2 & (minute == 0 | minute == 10 | minute == 20 | minute == 30 | minute == 40 | minute == 50))

houseWeek1 <- filter(unifyDataFrame, year == 2008  & month == 2  & (day== 0| day== 1| day== 2| day== 3| day== 4| day== 5| day== 6| day== 7| day== 8| day== 9| day== 10| day== 11| day== 12| day== 13| day== 14| day== 15| day== 16| day== 17| day== 18| day== 19| day== 20| day== 21| day== 22| day== 23))

## Plot sub-meter 1, 2 and 3 with title, legend and labels - 10 Minute frequency
plot_ly(houseWeek1, x = ~houseWeek1$DateTime, y = ~houseWeek1$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseWeek1$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseWeek1$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption Week 2, January 8th-14, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))


#############################################################################################
# Pie charts  
#############################################################################################
#BY DAY
houseDay14_2009<- filter(unifyDataFrame, year == 2009, day== 14 )
dataAverageSub1<-mean(houseDay14_2009$Sub_metering_1, na.rm = TRUE)
dataAverageSub2<-mean(houseDay14_2009$Sub_metering_2, na.rm = TRUE)
dataAverageSub3<-mean(houseDay14_2009$Sub_metering_3, na.rm = TRUE)
dataAverageGlobalActive<-mean(houseDay14_2009$Global_active_power, na.rm = TRUE)
dataAverageNotMeseaure<-((1000/60)*dataAverageGlobalActive)-dataAverageSub1-dataAverageSub2-dataAverageSub3
# Create test data.
data <- data.frame(
  category=c("Sub_meter1", "Sub_meter2", "Sub_meter3"),
  count=c(dataAverageSub1, dataAverageSub2, dataAverageSub3)
)

# Compute percentages
data$fraction <- data$count / sum(data$count)
# Compute the cumulative percentages (top of each rectangle)
data$ymax <- cumsum(data$fraction)

# Compute the bottom of each rectangle
data$ymin <- c(0, head(data$ymax, n=-1))

# Compute label position
data$labelPosition <- (data$ymax + data$ymin) / 2

# Compute a good label
#data$label <- paste0(data$category, "\n Porcentage: ", round(data$fraction*100,digits = 1))
data$label <- paste0(round(data$fraction*100,digits = 1),"%")

# Make the plot
ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
  geom_rect() +
  geom_text( x=2, aes(y=labelPosition, label=label, color=category), size=4) + # x here controls label position (inner / outer)
  scale_fill_brewer(palette=3) +
  scale_color_brewer(palette=3) +
  coord_polar(theta="y") +
  xlim(c(-1, 4)) +
  theme_void() +
  #theme(legend.position = "none")
  ggtitle("                                 Porcentaje de consumo en el dia14 del año 2009")



#BY YEAR=2009
houseYear2009 <- filter(unifyDataFrame, year == 2009 )
dataAverageSub1<-mean(houseYear2009$Sub_metering_1, na.rm = TRUE)
dataAverageSub2<-mean(houseYear2009$Sub_metering_2, na.rm = TRUE)
dataAverageSub3<-mean(houseYear2009$Sub_metering_3, na.rm = TRUE)
dataAverageGlobalActive<-mean(houseYear2009$Global_active_power, na.rm = TRUE)
dataAverageNotMeseaure<-((1000/60)*dataAverageGlobalActive)-dataAverageSub1-dataAverageSub2-dataAverageSub3
# Create test data.
data <- data.frame(
  category=c("Sub_meter1", "Sub_meter2", "Sub_meter3"),
  count=c(dataAverageSub1, dataAverageSub2, dataAverageSub3)
)

# Compute percentages
data$fraction <- data$count / sum(data$count)
# Compute the cumulative percentages (top of each rectangle)
data$ymax <- cumsum(data$fraction)

# Compute the bottom of each rectangle
data$ymin <- c(0, head(data$ymax, n=-1))

# Compute label position
data$labelPosition <- (data$ymax + data$ymin) / 2

# Compute a good label
#data$label <- paste0(data$category, "\n Porcentage: ", round(data$fraction*100,digits = 1))
data$label <- paste0(round(data$fraction*100,digits = 1),"%")

# Make the plot
ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
  geom_rect() +
  geom_text( x=2, aes(y=labelPosition, label=label, color=category), size=4) + # x here controls label position (inner / outer)
  scale_fill_brewer(palette=3) +
  scale_color_brewer(palette=3) +
  coord_polar(theta="y") +
  xlim(c(-1, 4)) +
  theme_void() +
  #theme(legend.position = "none")
  ggtitle("                                         Porcentaje de consumo en el año 2009")

#Adding all the info
# Create test data.
data <- data.frame(
  category=c("Sub_meter1", "Sub_meter2", "Sub_meter3", "NoMeasurebySub"),
  count=c(dataAverageSub1, dataAverageSub2, dataAverageSub3,dataAverageNotMeseaure)
)

# Compute percentages
data$fraction <- data$count / sum(data$count)

# Compute the cumulative percentages (top of each rectangle)
data$ymax <- cumsum(data$fraction)

# Compute the bottom of each rectangle
data$ymin <- c(0, head(data$ymax, n=-1))

# Compute label position
data$labelPosition <- (data$ymax + data$ymin) / 2

# Compute a good label
#data$label <- paste0(data$category, "\n Porcentage: ", round(data$fraction*100,digits = 1))
data$label <- paste0(round(data$fraction*100,digits = 1),"%")

# Make the plot
ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
  geom_rect() +
  geom_text( x=2, aes(y=labelPosition, label=label, color=category), size=4) + # x here controls label position (inner / outer)
  scale_fill_brewer(palette=3) +
  scale_color_brewer(palette=3) +
  coord_polar(theta="y") +
  xlim(c(-1, 4)) +
  theme_void() +
  #theme(legend.position = "none")
  ggtitle("                             Porcentaje de consumo en el año 2009")

#BY ALL TIME
dataAverageSub1<-mean(unifyDataFrame$Sub_metering_1, na.rm = TRUE)
dataAverageSub2<-mean(unifyDataFrame$Sub_metering_2, na.rm = TRUE)
dataAverageSub3<-mean(unifyDataFrame$Sub_metering_3, na.rm = TRUE)
dataAverageGlobalActive<-mean(unifyDataFrame$Global_active_power, na.rm = TRUE)
dataAverageNotMeseaure<-((1000/60)*dataAverageGlobalActive)-dataAverageSub1-dataAverageSub2-dataAverageSub3
# Create test data.
data <- data.frame(
  category=c("Sub_meter1", "Sub_meter2", "Sub_meter3"),
  count=c(dataAverageSub1, dataAverageSub2, dataAverageSub3)
)

# Compute percentages
data$fraction <- data$count / sum(data$count)
# Compute the cumulative percentages (top of each rectangle)
data$ymax <- cumsum(data$fraction)

# Compute the bottom of each rectangle
data$ymin <- c(0, head(data$ymax, n=-1))

# Compute label position
data$labelPosition <- (data$ymax + data$ymin) / 2

# Compute a good label
#data$label <- paste0(data$category, "\n Porcentage: ", round(data$fraction*100,digits = 1))
data$label <- paste0(round(data$fraction*100,digits = 1),"%")

# Make the plot
ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
  geom_rect() +
  geom_text( x=2, aes(y=labelPosition, label=label, color=category), size=4) + # x here controls label position (inner / outer)
  scale_fill_brewer(palette=3) +
  scale_color_brewer(palette=3) +
  coord_polar(theta="y") +
  xlim(c(-1, 4)) +
  theme_void() +
  #theme(legend.position = "none")
  ggtitle("                            Porcentaje de consumo de todas las mediciones")


#############################################################################################
# Preparate data, time series
#############################################################################################
## Subset to one observation per week on Mondays at 8:00pm for 2007, 2008 and 2009
house070809weekly <- filter(unifyDataFrame, weekdays == "Monday" & hour == 20 & minute == 1)

## Create TS object with SubMeter3
tsSM3_070809weekly <- ts(house070809weekly$Sub_metering_3, frequency=52, start=c(2007,1))

## Plot sub-meter 3 with autoplot (you may need to install these packages)
autoplot(tsSM3_070809weekly)
## Plot sub-meter 3 with autoplot - add labels, color
autoplot(tsSM3_070809weekly, ts.colour = 'red', xlab = "Time", ylab = "Watt Hours", main = "Sub-meter 3")
## Plot sub-meter 3 with plot.ts
plot.ts(tsSM3_070809weekly)
#############################################################################################

## Subset to one observation per week on Tuesday at 8:00pm for 2007, 2008 and 2009
houseweeklyTuesday <- filter(unifyDataFrame, weekdays == "Tuesday" & hour == 21 & minute == 1)

## Create TS object with SubMeter3
tsS_weeklyTuesday <- ts(houseweeklyTuesday$Sub_metering_2, frequency=52, start=c(2007,1))

## Plot sub-meter 3 with autoplot (you may need to install these packages)
autoplot(tsS_weeklyTuesday)
## Plot sub-meter 3 with autoplot - add labels, color
autoplot(tsS_weeklyTuesday, ts.colour = 'blue', xlab = "Time", ylab = "Watt Hours", main = "Sub-meter 2")
## Plot sub-meter 3 with plot.ts
plot.ts(tsS_weeklyTuesday)

#############################################################################################

## Subset to one observation per week on Wednesday at 8:00pm for 2007, 2008 and 2009
houseweeklyTuesday <- filter(unifyDataFrame, weekdays == "Tuesday" & hour == 21 & minute == 1)

## Create TS object with SubMeter3
tsS_weeklyTuesday <- ts(houseweeklyTuesday$Sub_metering_1, frequency=52, start=c(2007,1))

## Plot sub-meter 3 with autoplot (you may need to install these packages)
autoplot(tsS_weeklyTuesday)
## Plot sub-meter 3 with autoplot - add labels, color
autoplot(tsS_weeklyTuesday, ts.colour = 'blue', xlab = "Time", ylab = "Watt Hours", main = "Sub-meter 1")
## Plot sub-meter 3 with plot.ts
plot.ts(tsS_weeklyTuesday)

#############################################################################################
# Forecasting a time series
#############################################################################################
## Apply time series linear regression to the sub-meter 3 ts object and use summary to obtain R2 and RMSE from the model you built

fitSM3 <- tslm(tsSM3_070809weekly ~ trend + season) 
summary(fitSM3)

## Create the forecast for sub-meter 3. Forecast ahead 20 time periods 
forecastfitSM3 <- forecast(fitSM3, h=20)
## Plot the forecast for sub-meter 3. 
plot(forecastfitSM3)

#################
## Create sub-meter 3 forecast with confidence levels 80 and 90
forecastfitSM3c <- forecast(fitSM3, h=20, level=c(80,90))

## Plot sub-meter 3 forecast, limit y and add labels
plot(forecastfitSM3c, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time")


