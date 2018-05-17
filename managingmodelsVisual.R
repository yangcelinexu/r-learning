install.packages('gapminder')
library(tidyverse)
getwd()
#setwd('/Users/yang.celine.xu/Documentsr')
# dt <- read_excel("Data_Cortex_Nuclear.xls", sheet = 1, col_names= T)
# dt <- data.frame(dt)
# write.csv(dt, file = "Data_Cortex_Nuclear.csv",row.names=FALSE)

seq_along(x) ##the length of the X

##thinking functions

library(purrr)
##statistical checking

head(mtcars)
str(mtcars) ##heads on check up
U1<- sapply(dna, function(x) length(unique(x)) )# unique Value checking
U1

U2 <- lapply(dna, class)
U2

###caculating the group statistics

means <-map_dbl(mtcars,mean)
medians <- map_dbl(mtcars, median)

##PURRR learning map, map_dbl,map2

##advance version, only 2 line caculate the mean, median and sd for each colunm in DF

funs <- list(mean,median,sd)
funs %>% map (~ mtcars %>% map_dbl(.x))



library(gapminder)
library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)

gapminder <- gapminder %>% mutate(year1950 = year - 1950)

##Net datafram
by_country <- gapminder %>%
  group_by(continent,country) %>%
  nest()
#using $data to understand what is nested
#for this datafram each counytry have the same length of year
by_country$data[[1]]
##then we achiive having each country related data in on cell as a list
#defind a model for single on df
country_model <- function(df){
  lm(lifeExp ~ year1950, data = df )
}

#using map apply the lm model to all the country and store the reasult in to mod

models <- by_country %>%
  mutate(
    mod = map(data,country_model)
  )
##focus on  reuslts in Africa

models %>% filter(continent=='Africa')

###using Broom to compare the model
##get model statistics, tidy, glance, augument

models <- models %>%
  mutate (
    tidy    = map(mod, broom::tidy),
    glance  = map(mod, broom::glance),
    rsq     = map_dbl(glance, "r.squared"),
    augment = map(mod, broom::augment)
  )

models %>% arrange(desc(rsq))

models %>% 
  ggplot(aes(rsq, country))+
  geom_point(aes(colour=continent))
##use reorder function arrange the country based on rsq
models %>% 
  ggplot(aes(rsq, reorder(country,rsq)))+
  geom_point(aes(colour=continent))
##arrange the models by rsq desendence
# can use the shiny to do the exploration, 
#each country each plot and use R squere to filter

##unest data

unnest(models,data)  ##unnest data and add the rsq per row
unnest(models, glance,.drop=TRUE) %>% View()
unnest(models,tidy) 
#the scond variable in the () is the columns you want to unnest

##print the model parameters

models %>%
  unnest(tidy) %>%
  select(continent, country, term, estimate, rsq) %>%
  spread (term,estimate) %>%
  ggplot(aes(`(Intercept)`, year1950))+
   geom_point(aes(colour= continent, size=rsq))+
   geom_smooth(se= FALSE)+
   xlab('Life Expectancy (1950)')+
   ylab('Yearly improvement')+
   scale_size_area()
##size the points according to the Rsqure
##not sure what is matter of scale_size_area 
  

#augment is the row level info have, residuls and others
models  %>%
  unnest(augment) %>%
  ggplot(aes(year1950,.resid))+
  geom_line(aes(group =country),alpha=1/3)+
  geom_smooth(se=FALSE)+
  geom_hline(yintercept =0, colour='white')+
  facet_wrap(~continent)
##different using facet_wrap and facet_grid


##spread(a,b) a is the columns has the tags; b is the column has the number
#the opposite side of spread is gather
  
  
df <- data.frame(xyz = "a")
df$x
fct_reorder()

ggrepel ###the lable of the datapoint not overlapo
ggbeeswarm  ##the point in scatte plot is not overlap
library(swambee)
ggplot()+
  geom_quasirandom()


t2 <-as_tibble(iris) %>% nest(Petal.Width, Sepal.Width)
as_tibble(chickwts) %>% nest(weight)

if (require("gapminder")) {
  gapminder %>%
    group_by(country, continent) %>%
    nest()
  
  gapminder %>%
    nest(-country, -continent)
}

stocks <- data.frame(
  time = as.Date('2009-01-01') + 0:9,
  X = rnorm(10, 0, 1),
  Y = rnorm(10, 0, 2),
  Z = rnorm(10, 0, 4)
)
stocksm <- stocks %>% gather(stock, price, -time)
stocksm %>% spread(stock, price)
stocksm %>% spread(time, price)
