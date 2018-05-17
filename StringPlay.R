
library("stringr")
##str_detect() and str_subset()

jr = c("Theo is first", "Esther is second", "Colin - third")
str_detect(jr, "Theo")
str_detect(jr, "is")

str_subset(jr, "Theo")
str_subset(jr, "is")

files = c(
  "tmp-project.csv", "project.csv", 
  "project2-csv-specs.csv", "project2.csv2.specs.xlsx", 
  "project_cars.ods", "project-houses.csv", 
  "Project_Trees.csv","project-cars.R",
  "project-houses.r", "project-final.xls", 
  "Project-final2.xlsx"
)


##.{()\^$|?*+ need to escape
str_subset(files, "\\.csv")

#hat and dollar are used to specify the start and end of a line respectively. 
#all file names that start with “Pro
str_subset(files, "^Proj")

##specifically just “.csv” or “.ods” files
str_subset(files, "\\.csv$")

##Round parentheses,(), and the pipe, |, ;  parentheses specify a group and the pipe means “or”. 
str_subset(files, "\\.csv$|\\.ods$")
#the other way
str_subset(files, "\\.csv$|\\.ods$")
##We can match a group of characters or digits using the square parentheses.

#the last lower case letter in each element of the vector, if such a thing exists
str_extract(files, "[a-z]$")


#To include this we add “A-Z” (to add numbers we add 0-9 and to add metacharacters we write them without escaping them)
str_extract(files, "[a-zA-Z]$")

##The asterisk is what is called a quantifier. There are three other quantifiers (+, ? and {}),
str_extract(files, "[a-zA-Z]*$")
str_subset(files, "[a-zA-Z]*\\.(csv|ods)$")
str_subset(files, "(\\_|\\-)[a-zA-Z]*\\.(csv|ods)$")





