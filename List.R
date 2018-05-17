getwd()
setwd('/Users/yang.celine.xu/desktop')

util<-read.csv('Machine-Utilization.csv')
head(util,50)
str(util)
summary(util)

##Derive utilization
util_new <-util%>%
  mutate(Utilization=1-Percent.Idle)%>%
  mutate(PosixTime = as.POSIXct(Timestamp,format='%d/%m/%Y %H:%M'))

#?POSIXct
#as.POSIXct(util$Timestamp,format='%d/%m/%Y %H:%M')
head(util_new,50)
util<-util_new
util <- util[,c(4,1,2,3,5)] ##rearrange the coloumn

#write.csv(util, file = "Test.csv",row.names=FALSE)

RL1 <-util[util$Machine=='RL1',]
summary(RL1)
##if originally there is a Factor when do the subseting need to clean the facto memary
RL1$Machine <- factor(RL1$Machine)
summary(RL1)
#Construct List
##character: Machine name
##Vector: (min,mean,max) Utilization for the month (excluding unknown hours)
## Logical: has utilization ever fallen below 90% TURE/FALSE

how to use map function to process all the mechine at the same time


util_stats_rl1 <-c(min(RL1$Utilization,na.rm=T),
                   mean(RL1$Utilization,na.rm=T),
                   max(RL1$Utilization,na.rm=T))

which(RL1$Utilization<0.09)  ##which function ignor NA 

util_under_90 <- length(which(RL1$Utilization<0.90)) >0
util_under_90 
list_rl1 <-list('RL1',util_stats_rl1,util_under_90)

###dplyr version
head(util)
t <-util%>%
  group_by(Machine)%>%
  summarise(Min=min(Utilization,na.rm=T),
            Max=max(Utilization,na.rm=T),
            Mean=mean(Utilization,na.rm=T),
            flag=length(which(Utilization<0.90))>0)

#####
iris %>%
  group_by(Species) %>%
  summarise_at(vars(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width),
               c("min", "max", "mean"))
###
MN <- as.character(unique(util$Machine))
get_name <- function (n){
  paste('list',n,sep='_')
}

  
for (n in MN){
  print(get_name(n))}

###Naming componets of a list
list_rl1
names(list_rl1) <-c('Machine','Stats','LowThreshold')
list_rl1$Data <-RL1
#another way to name the list
#list_rl1 <-list(Machine='RL1',Stats=util_stats_rl1,LowThreshold=util_under_90)

#Extracting compomets of a list
#three ways:
#[] -will always return a list
#[[]] -will return the actural object
#$ -same as [[]] but prettier
summary(list_rl1)
str(list_rl1)
##subsetting a list

list_rl1[[1]]
list_rlq[[2]][3]

##Adding and deleting list components

list_rl1[4] <-'NewInfo'

##adding the all hours where utilization is unkonw

list_rl1$UnknowHours<-RL1[is.na(RL1$Utilization),'PosixTime']

list_rl1[10] <-'Test'  ##will adding NULL in between

##Remove one component ##Notice: numeration has shifted, #not like the df, if delete the element the rownumber will not change
#
list_rl1[4] <-NULL
list_rl1[5:9] <-NULL

###ggplot of timeserios line

p <- ggplot(data = util)
plot <-p + geom_line(aes(x=PosixTime, y= Utilization,
                  colour=Machine),size=1)+
  facet_grid(Machine~.)+
  geom_hline(yintercept = 0.90, colour='Gray',
             size=1.2,linetype=3)

list_rl1$plot <-plot