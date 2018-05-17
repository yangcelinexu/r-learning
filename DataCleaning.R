getwd()
setwd('/Users/yang.celine.xu/desktop')

library(tidyverse)
#install.packages('tidyverse')
options(tz="Europe/Stockholm")
fin <-read.csv('Future-500.csv',na.strings = c('') ) ##if there are ''empty strings will not show as NA but nothing, to avoide this 
head(fin,20)
str(fin)
U1<- sapply(dna, function(x) length(unique(x)) )# unique Value checking
U1
##typeof(vector)

U2 <- lapply(dna, class)
U2
summary(fin)

## changing from nom-factore to the factor
fin$ID <-factor(fin$ID)
fin$Inception <-factor(fin$Inception)

#changing factor to intiger
## z <-factor(c('11','12','11'))
#z_1 <- as.charactor(z) #chang to chractoer first 
#z <- as.numeric(z_1) #change bac to numaric

## t <- as.numeric(as.character(z))

##sub() and gsub() #replacement sub only first insidence 

fin$Expenses <-gsub(' Dollars','',fin$Expenses)
fin$Expenses <-gsub(',','',fin$Expenses)
head(fin)
str(fin)

fin$Revenue <- gsub('\\$','',fin$Revenue) ##\\escape sequence for
fin$Revenue <-gsub(',','',fin$Revenue)

fin$Growth <-gsub('%','',fin$Growth)

##chr to numaric

fin$Expenses <-as.numeric(fin$Expenses)
fin$Revenue <-as.numeric(fin$Revenue)
fin$Growth <-as.numeric(fin$Growth)

summary(fin)

##missing Data handeling

##sepearte with the complete and having NA coloumns
# uodate: fin <-read.csv('Future-500.csv',na.strings = c('') )
fin[!complete.cases(fin),] ##select all the row having missing value 
##fitering 
fin[which(fin$Revenue==9746272),] ##only chose that row, which get the row number
fin[fin$Revenue==9746272,]  ##will pick up some NA row

is.na()
fin[is.na(fin$Revenue),]
## remove the missing value 
fin_backup <- fin
fin[is.na(fin$Industry),]
fin <-fin[!is.na(fin$Industry),]

##reseting datafram index
#rownames(fin) <-1:nrow(fin) option1
rownames(fin) <-NULL #option2 

##Replacing Missing Data: Factual data ## ceat
fin[!complete.cases(fin),]
fin[is.na(fin$State),]


city_list <- fin%>% distinct(State, City) %>% filter(!is.na(State))
##us segemetation imputation median to fill the NA ##city list
city_to_state <- function(city){
  idx = which(city_list$City == city) 
  return(as.character(city_list$State[idx])) ##if do not have as.character will be factor
}
city_to_state('San Francisco')

remove(test)
test = fin %>% mutate(State2= map(State, ~replace(.x,is.na(.x),city_to_state(City) )))


test2 = left_join(fin, city_list, by = 'City')

test2[test2$City=='San Francisco',]
                      
                      
test2[is.na(test2$State.y),]
test[test$City=='New York',]

##use, mutation, median replace


fin[is.na(fin$State) &fin$City=='New York','State'] <-'NY'
fin[is.na(fin$State) &fin$City=='San Francisco','State'] <-'CA'

##do it at once

##test



##pipline is better
library(tidyr)
remove(test)
test =fin%>%
  group_by(Industry) %>% 
  mutate(Employees= ifelse(is.na(Employees), median(Employees, na.rm=TRUE), Employees))%>%
  mutate(Revenue= ifelse(is.na(Revenue), median(Revenue, na.rm=TRUE), Revenue))%>%
  mutate(Growth= ifelse(is.na(Growth), median(Growth, na.rm=TRUE), Growth))%>%
  mutate(Expenses= ifelse(is.na(Expenses) & is.na(Profit), median(Expenses, na.rm=TRUE), Expenses))
  
test[!complete.cases(test),]
fin[!complete.cases(fin),]

test= fin%>%
  mutate(Profit=ifelse(!is.na(Expenses),Revenue-Expenses,Profit))%>%
  mutate(Expenses=ifelse(is.na(Expenses),Revenue-Profit,Expenses))

fin = test

###more basical methord
##old school for individual putation
median(fin[,'Employees'],na.rm=TRUE)

med_empl_retail <-median(fin[fin$Industry=='Retail','Employees'],na.rm=TRUE)
mean(fin[fin$Industry=='Retail','Employees'],na.rm=TRUE)

fin[is.na(fin$Employees) & fin$Industry=='Retail',]
fin[is.na(fin$Employees) & fin$Industry=='Retail','Employees'] <- med_empl_retail


##define the function?? could I use map for this??
med_empl_finserv <-median(fin[fin$Industry=='Financial Services','Employees'],na.rm=TRUE)
med_empl_finserv

fin[is.na(fin$Employees) & fin$Industry =='Financial Services', ]
fin[is.na(fin$Employees) & fin$Industry =='Financial Services', ] <- med_empl_finserv

####visulization
library(ggplot2)
ggplot(data=fin) + geom_point(aes(x=Revenue, y=Expenses,
                                  colour=Industry, size=Profit))

ggplot(data=fin) + geom_point(aes(x=Revenue, y=Expenses,
                                  colour=Industry))+
  geom_smooth(aes(x=Revenue, y=Expenses,
                  colour=Industry), fill=NA, zise=1.2)


ggplot(data=fin, aes(x=Industry, y=Growth,
                     colour=Industry)) + 
  geom_jitter()+
  geom_boxplot(alpha=0.5,outlier.color=NA)


