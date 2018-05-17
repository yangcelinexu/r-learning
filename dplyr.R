#install.packages('hflights')
library(dplyr)
library(hflights)

# explore data
data(hflights)
head(hflights)

#tbl_df creates a “local data frame”
# convert to local data frame
flights <- tbl_df(hflights)
# printing only shows 10 rows and as many columns as can fit on your screen
flights

##Filter:
# base R approach to view all flights on January 1
flights[flights$Month==1 & flights$DayofMonth==1, ] #having rownames
# dplyr approach
# note: you can use comma or ampersand to represent AND condition
filter(flights, Month==1, DayofMonth==1) ##will lost the rownames info, and it is a veiw will not change anything in the original df
# use pipe for OR condition  or also use %in% operator
filter(flights, UniqueCarrier=="AA" | UniqueCarrier=="UA")
filter(flights, UniqueCarrier %in% c("AA", "UA"))

##Select
# base R approach to select DepTime, ArrTime, and FlightNum columns
flights[, c("DepTime", "ArrTime", "FlightNum")]
# dplyr approach
select(flights, DepTime, ArrTime, FlightNum)
##advance conditioning select version
# use colon to select multiple contiguous columns, and use `contains` to match columns by name
# note: `starts_with`, `ends_with`, and `matches` (for regular expressions) can also be used to match columns by name
select(flights, Year:DayofMonth, contains("Taxi"), contains("Delay"))

##Arrange
# base R approach to select UniqueCarrier and DepDelay columns and sort by DepDelay
flights[order(flights$DepDelay), c("UniqueCarrier", "DepDelay")]
# dplyr approach, if that decrese use (arrange(desc(DepDelay)))
flights %>%
  select(UniqueCarrier, DepDelay) %>%
  arrange(DepDelay)

##mutate

# dplyr approach (prints the new variable but does not store it)
flights %>%
  select(Distance, AirTime) %>% ##do not need to select if you just want to add
  mutate(Speed = Distance/AirTime*60)
# store the new variable
flights <- flights %>% mutate(Speed = Distance/AirTime*60) 

##summarise !!!!Reduce variables 
# dplyr approach: create a table grouped by Dest, and then summarise each group by taking the mean of ArrDelay
flights %>%
  group_by(Dest) %>%
  summarise(avg_delay = mean(ArrDelay, na.rm=TRUE))
##`summarise_each()` is deprecated.
#Use `summarise_all()`, `summarise_at()` or `summarise_if()` instead.
#To map `funs` over a selection of variables, use `summarise_at()`

flights %>%
  group_by(UniqueCarrier) %>%
  summarise_at(vars(Cancelled, Diverted), funs(min, max))

flights %>%
  group_by(UniqueCarrier) %>%
  summarise_at(vars(matches("Delay")), funs(min(., na.rm=TRUE), max(., na.rm=TRUE)))

##more advanced version, https://github.com/tidyverse/dplyr/issues/3101

# n() counts the number of rows in a group
# n_distinct(vector) counts the number of unique items in that vector

flights %>%
  group_by(Month, DayofMonth) %>%
  summarise(flight_count = n()) %>% #count how many rows
  arrange(desc(flight_count))

# rewrite more simply with the `tally` function--tally == count the row number and sorting based on group by
flights %>%
  group_by(Month, DayofMonth) %>%
  tally(sort = TRUE)

# for each destination, count the total number of flights and the number of distinct planes that flew there
flights %>%
  group_by(Dest) %>%
  summarise(flight_count = n(), plane_count = n_distinct(TailNum)) ##inthe group find the distict count based on TaiNum

###use group_by without summarise but use table (which showing, how many is canceled and how many is not canceled)
# for each destination, show the number of cancelled and not cancelled flights
flights %>%
  group_by(Dest) %>%
  select(Cancelled) %>%
  table() %>%
  head()

##window funxtions

#Window function takes n inputs and returns n values
#Includes ranking and ordering functions (like min_rank), 
 #offset functions (lead and lag), and cumulative aggregates (like cummean).

# for each carrier, calculate which two days of the year they had their longest departure delays
# note: smallest (not largest) value is ranked as 1, so you have to use `desc` to rank by largest value
# rewrite more simply with the `top_n` function
flights %>%
  group_by(UniqueCarrier) %>%
  select(Month, DayofMonth, DepDelay) %>%
  top_n(2) %>%
  arrange(UniqueCarrier, desc(DepDelay))

##more complex version
flights %>%
  group_by(UniqueCarrier) %>%
  select(Month, DayofMonth, DepDelay) %>%
  filter(min_rank(desc(DepDelay)) <= 2) %>%
  arrange(UniqueCarrier, desc(DepDelay))

#####Love this, only caculate the increametal from last month
# for each month, calculate the number of flights and the change from the previous month
flights %>%
  group_by(Month) %>%
  summarise(flight_count = n()) %>%
  mutate(change = flight_count - lag(flight_count)) ##lead use the next value

##simplify version 
# rewrite more simply with the `tally` function
flights %>%
  group_by(Month) %>%
  tally() %>%
  mutate(change = n - lag(n))

###other useful functions
# randomly sample a fixed number of rows, without replacement
flights %>% sample_n(5)
flights %>% sample_frac(0.25, replace=TRUE) ##choose 25%of the record from population

# dplyr approach: better formatting, and adapts to your screen width
glimpse(flights)

###Connecting to Databases

http://rpubs.com/justmarkham/dplyr-tutorial
https://www.youtube.com/watch?v=jWjqLW-u3hc
!!nice dropbox
https://www.dropbox.com/sh/i8qnluwmuieicxc/AAAgt9tIKoIm7WZKIyK25lh6a

rm(flights)
#install.packages('nycflights13')
library(nycflights13)

# print the flights dataset from nycflights13
flights
# hide a range of columns
flights %>% select(-(dep_time:arr_delay))

# hide any column with a matching name
flights %>% select(-contains("time"))

# rename() does the same thing, except all columns not mentioned are kept
flights %>% rename(tail = tailnum)

###Choosing rows: filter, between, slice, sample_n, top_n, distinct

flights %>% filter(dep_time >= 600, dep_time <= 605)
# between() is a concise alternative for determing i
flights %>% filter(between(dep_time, 600, 605))


# side note: is.na() can also be useful when filtering
flights %>% filter(!is.na(dep_time))
# slice() filters rows by position
flights %>% slice(1000:1005)
# sample three rows from each group
flights %>% group_by(month, day) %>% sample_n(3)
# keep three rows from each group with the top dep_delay
# also sort by dep_delay within each group
flights %>% group_by(month, day) %>% top_n(3, dep_delay) %>% arrange(desc(dep_delay))

# dplyr provides an alternative that is more "efficient"
flights %>% select(origin, dest) %>% distinct()  ##in the base r is unique()
# side note: when chaining, you don't have to include the parentheses if there are no arguments
flights %>% select(origin, dest) %>% distinct


# transmute() only keeps the new variables
flights %>% transmute(speed = distance/air_time*60)

###turn rowname into a variable !!!using mtcars 
mtcars %>% head()
# add_rownames() turns row names into an explicit variable
mtcars %>% add_rownames("model") %>% head()
# side note: dplyr no longer prints row names (ever) for local data frames
mtcars %>% tbl_df()

###use count then do not even need the group option 
flights %>% count(month)
flights %>% count(month, sort=TRUE)
##complext version:
# tally() and count() can do this more concisely
flights %>% group_by(month) %>% tally()
flights %>% group_by(month) %>% summarise(cnt = n())


#### you can sum over a specific variable instead of simply counting rows
flights %>% group_by(month) %>% summarise(dist = sum(distance))
flights %>% count(month, wt = distance)


# n_groups() simply reports the number of groups
flights %>% group_by(month) %>% n_groups()

# ungroup() before arranging to arrange across all groups
flights %>% group_by(month, day) %>% summarise(cnt = n()) %>% ungroup() %>% arrange(desc(cnt))


##Better way of creating DataFrame using data.frame
# data_frame() example
data_frame(a = 1:6, b = a*2, c = 'string', 'd+e' = 1) %>% glimpse()


###semi_join and anti_join
(a <- data_frame(color = c("green","yellow","red"), num = 1:3))
(b <- data_frame(color = c("green","yellow","pink"), size = c("S","M","L")))
# filter "a" to only show observations that match "b"
semi_join(a, b)
# filter "a" to only show observations that don't match "b"
anti_join(a, b)

##if having 2 colomen name have different colounm names
# specify that the join should occur by matching "color" in "a" with "col" in "b"
inner_join(a, b, by=c("color" = "col"))

##iewing more output: print, View
# specify that you want to see more rows
flights %>% print(n = 15)
##specify that you want to see all columns
flights %>% print(width = Inf)

##sets option
http://rpubs.com/justmarkham/dplyr-tutorial-part-2






http://rpubs.com/justmarkham/dplyr-tutorial-part-2

Hardly 2014 course  (command+shift+p:rerun the whole block) 

fligts%>%
  filter(!is.na(arr_delay))%>%
  group_by(dest)%>%
  summarise(mean=mean(arr_delay),n=n())%>%
  filter(n>10)%>%
  arrange(desc(mean))


flights%>%
  group_by(carrier,flights, dest)%>%
  summarise(n=n_distinct(date)) %>%
  filter(n==365)

per_hour <-flights%>%
  filter(cancelled==0)%>%
  mutate(time=hour+minute/60) %>%
  group_by(time) %>%
  summarise(arr_delay=mean(arr_delay,na.rm=TRUE,n=n())
            

##HOUR and time transfer
hour=time%/%100,
minute=time%%100

time=hour+minute/60

ggplot(filter(per_hour,n>30),aes(time,arr_delay))+
  geom_vline(xintercept=5:24,colour='white',size=2) +
  geom_point()



##some statistics
IQR(),mad(),sd(), var()
z-score()--how many sd the point to the mean: z=(x-mean(x))/sd(x)
sum(xxx>5), mean(xxx>5)
##group by +mutate,
planes <- flights%>%
  filter(!is.na(arr_delay))%>%
  group_by(plane)%>%
  filter(n()>30)
planes%>%
  mutate(z_delay = (arr_delay -mean(arr_delay))/sd(arr_delay))%>%
  filter(z_delay >5) ##realy unusal

##window function
planes %>%filter(min_rank(arr_delay)<5) ##showing top 5 least delay for each plan

##1. Ranking and ordering :min_rank(),row_number() and dense_rank()

flights%>%group_by(plane)%>%filter(row_number(desc(arr_delay))>2)

##2.offsets:lead & lag (difference between today and yesterday)
daily <-flights %>%
  group_by(date) %>%
  summarise(delay =mean(dep_delay,na.rm=TRUE)%>%
  mutate(delay-lag(delay),order_by=date)
  
  ##is there a change?
  x!=lag(x)
  #Percent
  (x-lag(x))/x
  x/lag(x)
  ##false before and now is true
  !lag(x)&x
  
##3.cumulative aggregates/rolling aggrigate

location <-airports%>%
  select(dest=iata, name=airport,lat,long) #use select to rename

delays <-flights %>%
  group_by(dest) %>%
  filter(!is.na(arr_delay))%>%
  summarise(arr_delay=mean(arr_delay,na.rm=TRUE),n=n()) %>%
  arrange(desc(arr_delay))
#%>%filter(n>5)

delays <-delays %>% left_join(location)

ggplot(delays,eas(long,lat)) +
  borders('State') +
  geom_point(aes(colour=arr_delay),size=5,alpha=0.9)+
  coord_quickmap()
###dplyr feature engenering 
setwd('/Users/yang.celine.xu/Documents/RW/Rlearning')
##Nested data (tidyr)
##functional programming (purrr)
##Models (broom)
library(gapminder)






