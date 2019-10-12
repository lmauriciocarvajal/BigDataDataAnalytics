#############################################################################################
# Mauricio Carvajal
#############################################################################################
#nycflights13::flights. This dataset contains all 336776 flights that departed from New York City 
#in 2013. The data comes from the US Bureau of Transportation Statistics, and is documented in ?nycflights13
#############################################################################################
# Install libs
#############################################################################################
install.packages("dplyr")
install.packages("ggplot")
install.packages("ggplot2")
install.packages("labeling")
install.packages("RMySQL")
library(dplyr)
library(nycflights13)
library(ggplot2)
library(RMySQL)

#############################################################################################
# Checking the data frame
#############################################################################################

dim(flights)

flights
#Tibbles are a modern take on data frames. They keep the features that have stood the test
#of time, and drop the features that used to be convenient but are now frustrating (i.e. 
#converting character vectors to factors).
#############################################################################################
# Single table verbs
#############################################################################################
#Filter rows with filter()
filter(flights, month == 1, day == 1)
flights[flights$month == 1 & flights$day == 2, ]

#Arrange rows with arrange()
arrange(flights, year, month, day)
arrange(flights, sched_arr_time, year, month, day)

#Use desc() to order a column in descending order:

arrange(flights, desc(arr_delay))
#Select columns with select()
# Select columns by name
select(flights, year, month, day)

# Select all columns between year and day (inclusive)
select(flights, year:day)

select(flights, -(year:day))
#There are a number of helper functions you can use within select(), 
#like starts_with(), ends_with(), matches() and contains(). These let you 
#quickly match larger blocks of variables that meet some criterion. See ?select for more details.

#You can rename variables with select() by using named arguments:
select(flights, tail_num = tailnum)

#But because select() drops all the variables not explicitly mentioned, it’s not that useful.
#Instead, use rename():
rename(flights, tail_num = tailnum)

#Add new columns with mutate()
mutate(flights,
       gain = arr_delay - dep_delay,
       speed = distance / air_time * 60
)
#dplyr::mutate() is similar to the base transform(), but allows you to refer to columns that you’ve 
#just created:
  
mutate(flights,
         gain = arr_delay - dep_delay,
         gain_per_hour = gain / (air_time / 60)
  )
#If you only want to keep the new variables, use transmute():
  
transmute(flights,
            gain = arr_delay - dep_delay,
            gain_per_hour = gain / (air_time / 60)
  )

#Summarise values with summarise()
#The last verb is summarise(). It collapses a data frame to a single row.

summarise(flights,
          delay = mean(dep_delay, na.rm = TRUE)
)
#############################################################################################
# Randomly sample rows with sample_n() and sample_frac()
#############################################################################################
#You can use sample_n() and sample_frac() to take a random sample of rows: use sample_n() for a 
#fixed number and sample_frac() for a fixed fraction.

sample_n(flights, 5)

sample_frac(flights, 0.01)
#Use replace = TRUE to perform a bootstrap sample. If needed, you can weight the sample with the 
#weight argument.
select(flights,n)
m<-flights
by_tailnum <- group_by(flights, tailnum)
delay <- summarise(by_tailnum,
                   count = n(),#ASK
                   dist = mean(distance, na.rm = TRUE),
                   delay = mean(arr_delay, na.rm = TRUE))
delay <- filter(delay, count > 20, dist < 3000)

# Interestingly, the average delay is only slightly related to the
# average distance flown by a plane.
ggplot(delay, aes(dist, delay)) +
  geom_point(aes(size = count), alpha = 1/2) +
  geom_smooth() +
  scale_size_area()
  #You use summarise() with aggregate functions, which take a vector of values and return a single number. There are many useful examples of such functions in base R like min(), max(), mean(), sum(), sd(), median(), and IQR(). dplyr provides a handful of others:
  #n(): the number of observations in the current group
  #n_distinct(x):the number of unique values in x.
  #first(x), last(x) and nth(x, n) - these work similarly to x[1], x[length(x)], and x[n] but give you more control over the result if the value is missing.


#When you group by multiple variables, each summary peels off one level of the grouping. That makes it easy to progressively roll-up a dataset:
  
daily <- group_by(flights, year, month, day)
(per_day   <- summarise(daily, flights = n()))

#> # … with 361 more rows
(per_month <- summarise(per_day, flights = sum(flights)))

(per_year  <- summarise(per_month, flights = sum(flights)))

df <- select(flights, year:dep_time)
mutate(df, "year", year+1)
