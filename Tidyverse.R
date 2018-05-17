###R advance learning --data exploring and tidyverse
#install.packages('ggbeeswarm')
getwd()
setwd('/Users/yang.celine.xu/Documents/RW/Rlearning')
#install.packages('tidyverse')


library(tidyverse)
#data import
permits_raw <- read_csv("dataPermit_full.csv", na = "null") ## clean null value to NA
permits_raw

str(permits_raw)
# area = metropolitan standard area
# date = month/year (character vector, or strings)
# f1= 1 family house, f24= 2-4 family house, f5 = family have more than 5 , 
# units= number of buildings, change in unites; 
# value = average value of building;


##separate, filter, select tidy, mutate  the dataset

permits <- permits_raw %>%
  separate(date, c("month","year"), "/",convert= TRUE) %>%
  filter (year > 2007) %>%
  select (-ends_with("change"))



##Basic eda

permits %>% count(year) ##count number of permit by year
##count(year, sort=TRUE)
permits %>% count(area) ## count number of permit by area
##double count, to check how many area hae sepcific number of count
permits %>% count(area) %>% count(n)  
##interpretation : 1 area have 49 records, 16 areas have 84 records

## check the NA in col in permits

permits %>% filter (is.na(col)) 
is.na(permits)
##narrow down the scop only focus on the single family homes
##quick visuilizations
## Need to fix the time idex

permits <- permits %>%
  mutate(date = year+ (month-1)/12) #how to change only year and month to date to have the time plote index


ggplot(permits,aes(date,f1units))+
  theme_minimal()+##theme_bw()+
  geom_point(alpha = 0.1)

ggplot(permits,aes(date,f1units))+
  geom_line(aes(group = area), alpha = 0.1)+ ## need to pass the aes(group= ) function otherwise will not be line
  theme_minimal()
## narrow down to the big cities --maybe bias but a good start
## Agregation and drill down
##understand which city has average highest number 
f1units <- permits %>%  ##can create a variable but also can only just for exploration
  group_by(area) %>%
  summarise(mean = mean(f1units)) %>%
  arrange (desc(mean))
f1units  #print out version only have integer but the view(f1units) is float
f1units %>% filter(mean > 100)  ##only interested in the records over a certain threashold

##using summarise_at simplify the multiple summarise
iris %>%
  group_by(Species) %>%
  summarise_at(vars(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width),
               c("min", "max", "mean"))
####

##use this city list to retraive the permits: semi_join

permits_big <- permits%>%
  semi_join(f1units %>% filter(mean >100)) ##best way to filter the records based one kind of the ranking

ggplot(permits_big,aes(date,f1units))+
  geom_line(aes(group = area), alpha = 0.1)+
  theme_minimal()+
  scale_y_log10()+## log tansformation
  geom_smooth(se=FALSE)  ##easy way to find out the long term trends . 


###Model to partisioning the singnal, it is better only use one line to do that
hoston <- permits %>% filter (str_detect(area,"Houston")) ##find all the record in AREA have str as contend Houston

#permits %>% filter (str_detect(area,'LA'))%>%count(area) ##str_detection only can have one str
#hoston1 <- permits %>% filter (area=="Houston")# this does not work becaus that is not acurate name

ggplot(hoston,aes(date,f1units))+
  geom_line(aes(group = area))
## see the seasonal pattern then can change the line to 

ggplot(hoston,aes(month,f1units))+
  geom_line(aes(group = year)) ##break down the  each year line checking seasonal pattern



##quesitons, is the patern everywhere. have the higher at the begining of the year
#waht drives it? is the weather?

library(modelr)
hoston_mod <-lm(log(f1units) ~ factor(month), data = hoston) ##build a lm model against month
##this basically a seasoanl factor capturer
hoston %>% 
  add_predictions(hoston_mod) %>%##add the prediction column to the existing df
## can add the ggplot just after the pipline
  ggplot(aes(date,pred))+
  geom_line()
##more useful to check the residuels
##here this residuel accture the trends without the seasonal fact 
hoston %>% 
  add_residuals(hoston_mod) %>%##add the prediction column to the existing df
  ## can add the ggplot just after the pipline
  ggplot(aes(date,resid))+
  geom_line()

##Q: what drive this trends, is that universal
##what happened in 2010

###!!! extend this model to every city!!!
##create a datafram have dataframe inside, use nest. 
##because we filted by only one area for analysis first so, nest all the other in the 
##same format as what we modeled

by_area <- permits_big %>%
  group_by(area) %>%
  nest()
by_area%>%count(area)


area_model <-function(df) {
  lm(log(f1units) ~ factor(month), data = df)
}

##!!!!!!why here the return() not needed? ##we do not return anything directly. we just build the model

detrended <-by_area %>% 
  mutate(model = map(data, area_model),
         resids =map2(data, model,add_residuals)
  ) %>% unnest(resids)

##!!!what is map and map2 nest and unest??, map is apply the FUN to the data, map2 is apply FUN by taking 2 variable
  
  ##here is the mean trends for all the residule lines by area
  
ggplot(detrended,aes(date,resid))+
  geom_line(aes(group=area),alpha=1/10)+
  geom_smooth(se=FALSE)

###

library(ggbeeswarm)
library(lubridate)

## use tibble as main format

df %>% count(col,sort=TRUE) %>% print(n=10)
permits_raw %>%
  ggplot(aes(date,f1units))+
  geom_quasirandom(size=0.2)  #violine map

  permits_raw %>%
    ggplot(aes(date,f1units))+
    geom_line(aes(group=area)) ##line chart the group trick

##lump other things together only have top 10
  
  permits_raw %>%
    mutate(area = area %>% fct_lump(10)) %>%
    ggplot(aes(date,area)) +
    #geom_quasirandom(size=0.2)
    geom_point(size=0.2)
 ##not useful in this case but it helps 
  
##reorder the fact by using, and most recent one at the top
  permits_raw %>%
    mutate(area = area %>% fct_reorder(date) %>% fct_rev() %>% fct_lump(10)) %>%
    ggplot(aes(date,area)) +
    #geom_quasirandom(size=0.2)
    geom_point(size=0.2)

## drawing info week of the date agast the time
#need to reorder the day of week

df %>%
  mutate(wday = wday(datetime,label=TRUE)) %>%
  fct_shift(1) %>%
  #fct_rev %>%
  ggplot(aes(time,wday))+
  geom_quasirandom()
           

##creating time v.s date graph --round in to week
df %>%
  ggplot(aes(date,time))+
  geom_quasirandom()
  
  
df %>%
  mutate(week = floor_date(date,'week')) %>%
  ggplot(aes(week,time))+
  geom_quasirandom()

##geom_segment
ggplot(df,aes(y=)) +
  geom_segment(aes(x=start,xend=end,yend=country,colour=),size=10)+
  scale_y_continuous(break= 2000 + seq(7,17, by=2))+
  scale_x_date(date_labels = "%b")
                      


###Time manipulation 
time <- Sys.time()
typeof(time)
as.POSIXct(time)  ##double 
date <-as.Date(time)
date <- as.Date(floor_date(time,"day"))
tt<- strftime(time, format="%H:%M:%S")
# advance version of as.Date()

##Not sure why using update
tt <- update(time,year=2016,month=1,mday=1,second=0)
