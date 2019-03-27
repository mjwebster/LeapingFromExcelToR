
#this is an R Script
#Anything with a hashtag in front of it are comments (not code)


#Download R
#  https://www.r-project.org/


#Download RStudio
#https://www.rstudio.com/products/rstudio/download/



#Install packages (if you haven't already done so)-  CRAN
#  https://cran.r-project.org/


#The syntax for installing a package
#install.packages(c("rmarkdown", "kableExtra", "lubridate", "knitr", "tidyverse", "ggplot2",  "janitor", "scales", "ggthemes", "DT"))



# LOAD PACKAGES -----------------------------------------------------------


#Need to load packages each time you work in R
library(tidyverse)
#packages we will be using from within tidyverse: 
 #readr --  importing csv files 
#dplyr --  #general analysis 
#gpplot2 --  #making charts 


getwd()  #how to check what directory we are working from


# IMPORT DATA -------------------------------------------------------------

#Death records for opioid-related deaths

#first we'll set the name of our new tibble (tidyverse version of a data.frame)
#then we'll use the read_csv, which is part of the readr package
#and we'll tell it where to find csv file

deaths <- read_csv('./data/opiate_deaths.csv')


# REVIEW DATA -------------------------------------------------------------

head(deaths)

#this shows us the column names
names(deaths)


#What does the structure look like?
str(deaths)


#look to see the columns are formatted the way you want 
#(dates are date formats, numbers are numeric, etc)

#notice that all the date fields came in as characters (not what we want!)
#birthdate
#deathdate
#injury_date

#Let's go back and add some details to the import code
#replace the previous import code with this
#the col_types function allows you to set formats for specific fields
#in this case, I also told it the default should be character
#for any fields that I don't specify
#here, we need to tell R how the dates in our underlying data are structured
#more info about date formats using readr here: https://readr.tidyverse.org/reference/parse_datetime.html


#we can fix that easily using a package called "janitor"
#first we need to load janitor 
# https://cran.r-project.org/web/packages/janitor/vignettes/janitor.html#clean-data.frame-names-with-clean_names

library(janitor)

#we will overwrite our dataframe with one of the same name
# the pipe (%>%) is how we tell R to connect operations
#in this case we're telling it to make a new data frame, use the old data frame and then clean the column names




deaths <- read_csv('./data/opiate_deaths.csv',
                   col_types=cols(.default="c", BIRTHDATE=col_date("%m/%d/%Y"),
                                  DEATHDATE=col_date("%m/%d/%Y"),
                                  INJURY_DATE=col_date("%m/%d/%Y"),
                                  AGEYEARS="i")) %>% clean_names()

#let's look at it again
head(deaths)




# ANALYSIS ----------------------------------------------------------------


#One of the packages that comes with tidyverse is called "dplyr"
#this is, hands-down, the most useful package for analyzing data
#it works very much like Structured Query Language (SQL)
#Its closest equivalent in Excel is filtering, sorting and Pivot Tables all wrapped into one
#but with more versatility


#showing only select columns- let's check those date fields

deaths %>%  select(firstname, lastname, birthdate, deathdate, gender)

#can use names() or head() to check column names

#filtering only select rows
 deaths %>% filter(gender=='F')%>%  select(firstname, lastname, gender, race)
 
 #How do we know that "F" is the right value?
 deaths %>% count(gender)
 
 #Let's check out the race field
 deaths %>% count(race)
 
 #Now select all the records where Chinese or Japanese
 deaths %>%
   filter(race=='Chinese' | race=='Japanese') %>%
   select(firstname, gender, race, deathdate)

 #notice how the code is indented?
 
 #As you've probably figured out, seeing results of your analysis
 #in a script page doesn't work out too well
 #let's move over to an RMarkdown page where we can make it look better
 

 