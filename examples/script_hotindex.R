

#install.packages("jsonlite")

# load required packages
library(readr) #importing csv files
#library(plyr) #needed for stacked bar chart label positions
library(dplyr) #general analysis 
library(ggplot2) #making charts
library(lubridate) #date functions
library(reshape2) #use this for melt function to create one record for each team
library(tidyr)
library(janitor) #use this for doing crosstabs

library(scales) #needed for stacked bar chart axis labels
library(knitr) #needed for making tables in markdown page
library(car)
library(aws.s3) #needed for uploading to amazon S3 server
library(rmarkdown)
library(DT) #needed for making  searchable sortable data tble
library(jsonlite)



currentyear='2018'
lastyear='2017'
twoyearsago <- '2016'
threeyearsago <- '2015'
fouryearsago <- '2014'


#Make sure the PPSF file has at least 5 years of data in it
#The first column in each file should be "Place" capitalized
#dom and ppsf files need to have a metro-level record in them

# LOAD DATA ---------------------------------------------------------------



#load the city croswalk file
#filter to exclude records for cities that are outside the 13-county metro
#note: this also includes neighborhood names
#Create a new field that only grabs the state code and county subidivision code
#this is needed for joining to census data later
cities <- read_csv("./data/city_crosswalk.csv")  %>% filter(County13!='n')%>% mutate(geoid2=substr(GEOID,8,14))



#load this year's data files and melt them (normalize)
#closed sales data for all years by community
closed <- melt(read_csv("./data/closedsales.csv"), id.vars="Place")  %>% mutate(type='city') 
dom <- melt(read_csv("./data/dom.csv"), id.vars="Place")  %>% mutate(type='city')  #days on market data for all years by community
polp <- melt(read_csv("./data/polp.csv"), id.vars="Place")  %>% mutate(type='city')  #pct of original list price data for all years by community
ppsf <- melt(read_csv("./data/ppsf.csv"), id.vars = "Place")  %>% mutate(type='city')  #price per sq foot data for all years by community 
inventory <- melt(read_csv("./data/inventory.csv"), id.vars="Place")  %>% mutate(type='city')  #INVENTORY ; this is not used for the index

#load other data files that don't need to be melted
#UPDATE THIS!!!!
other <- read_csv("./data/othermetrics2018.csv")  %>% mutate(type='city') 
other2017 <- read_csv("./data/othermetrics2017.csv") %>% mutate(type='city') #other metrics for 2017 (pct new construction, pct townhouse, pct distressed) 
other2016 <- read_csv("./data/othermetrics2016.csv")   %>% mutate(type='city') #other metrics for 2016
lastindex <- read_csv("./data/hotindex2017_revised.csv", col_types=cols(geoid2=col_character(), index_rank=col_double())) %>% mutate(type='city')  #final index scores for last index we ran 


#fix city name for Minneapolis in the other metrics file
other$place[other$place =="Minneapolis - (Citywide)"] <- "Minneapolis"


#Load neighborhood data and melt
closed_neighborhood <- melt(read_csv("./data/closedsales_neighborhoods.csv"), id.vars="Place") %>% mutate(type='neighborhood') 
dom_neighborhood <- melt(read_csv("./data/dom_neighborhoods.csv"), id.vars="Place")  %>% mutate(type='neighborhood')   #days on market data for all years by neighborhood
ppsf_neighborhood <- melt(read_csv("./data/ppsf_neighborhood.csv"), id.vars = "Place") %>% mutate(type='neighborhood')   #price per sq foot data for all years by neighborhood 
inventory_neighborhood <- melt(read_csv("./data/inventory_neighborhoods.csv"), id.vars="Place") %>% mutate(type='neighborhood')   #inventory by neighborhood



#import census data on tenure and housing costs - #b25106

census_tenure <-  read_csv("./data/census_tenure_costs.csv", 
                           col_types=cols(geoid=col_character(),Geography=col_character(),
                                          TotalHousingUnits=col_integer(),
                                          PctOwner=col_double(), OwnerCostBurdened=col_integer(),
                                          PctCostBurdenedOwners=col_double(),
                                          OwnerOccupiedUnits=col_integer()))

#import census data on median household income - B19013
census_income <-  read_csv("./data/ACS_17_5YR_B19013.csv", col_types=cols(geoid=col_character(),
                                                                          HD01_VD01=col_integer())) %>% rename(MedianHHIncome=HD01_VD01)


#Import census data on median value of owner-occupied homes - B25077
census_value <-  read_csv("./data/ACS_17_5YR_B25077.csv", col_types=cols(geoid=col_character(),
                                                                         HD01_VD01=col_integer())) %>%
  rename(MedianValue=HD01_VD01)









# TIME SERIES -------------------------------------------------------------

#append neighborhood data to city data for time series table
closed2 <-  rbind(closed, closed_neighborhood)
dom2 <- rbind(dom, dom_neighborhood)
ppsf2 <-  rbind(ppsf, ppsf_neighborhood)
inventory2 <-  rbind(inventory, inventory_neighborhood)



#generate timeseries table to export for interactive (exported as JSON at bottom of script)
#the dom file needs to go first to ensure the metro area records gets included
timeseries <- inner_join(dom2, cities %>%
                           select(stribID, NameInRealtorsData, geoid2, FullName, location, type, neighborhoodname), 
                         by=c("Place"="NameInRealtorsData", "type"="type"))%>% 
  rename(dom="value")



#the remainder of these are set up as a left join to ensure the timeseries table has all the places it started with
#there is no record for metro area in the closed and inventory tables
timeseries <- left_join(timeseries, ppsf2, by=c("Place"="Place", "variable"="variable", "type"="type")) %>% 
  rename(ppsf="value")

timeseries <- left_join(timeseries, closed2, by=c("Place"="Place", "variable"="variable", "type"="type")) %>% 
  rename(closed="value")



timeseries <- left_join(timeseries, inventory2, by=c("Place"="Place", "variable"="variable", "type"="type")) %>% 
  rename(inventory="value")








# BUILD INDEX -------------------------------------------------------------



#this goes back and uses the original city-level data (without neighborhoods)

#CLOSED SALES -- need most recent two years
closednew <- closed2%>%filter(variable ==currentyear | variable==lastyear)

#the variable in dcast refers to the field in closednew; syntax is dataframe, what to make rows ~ what to make columns
#cast by multiple variables -- so each row is a place/type combination, with years ("variable") as the columns
closednew <- dcast(closednew, Place + type ~ variable, value.var="value")

#join to the cities table, but make sure to only include the cities in 13-county metro that we are using
index_table <- inner_join(cities %>% 
                            filter(County13!='n') %>% 
                            select(stribID, NameInRealtorsData, FullName, CityName, geoid2, location, COUNTY, STATE, type), closednew,
                          by=c("NameInRealtorsData"="Place", "type"="type"))%>%
  select(Place=NameInRealtorsData, stribID, geoid2, FullName, CityName,location, COUNTY, STATE, cs_prev=lastyear, cs_curr=currentyear, type)





#repeat that for other data tables

#DAYS ON MARKET -- need most recent two years
domnew <- dom2%>%filter(variable ==lastyear | variable==currentyear)
domnew <- dcast(domnew, Place + type ~variable)
domnew <- domnew%>%select(Place, type, dom_prev=lastyear, dom_curr=currentyear)

#PCT ORIG LIST PRICE -- need most recent year only
polpnew <- polp%>%filter(variable==currentyear)%>%select(Place, type, pctorigprice=value)


#Price per square foot (PPSF) -- at least last 5 years

ppsfnew <- dcast(ppsf2, Place + type ~ variable)
ppsfnew <- ppsfnew%>%select(Place, type, ppsf_yr1=fouryearsago, ppsf_yr2=threeyearsago, ppsf_yr3=twoyearsago,
                            ppsf_y4=lastyear, ppsf_yr5=currentyear)



#distressed, new construction, townhousecondo -- most recent year --from othermetrics
#make sure the fields are decimals (without percent signs)
distress <- other%>%select(place, type) %>%
  mutate(NewConstruct=round(other$NewConstruction,3), 
         TownCondo=round(other$TownhouseCondo,3), 
         PctDistressed=round(other$Distressed,3))


#join the index_table with other metrics
index_table <- left_join(index_table, ppsfnew, by=c("Place"="Place", "type"="type"))
index_table <- left_join(index_table, domnew, by=c("Place"="Place", "type"="type"))
index_table <- left_join(index_table, polpnew, by=c("Place"="Place", "type"="type"))
index_table <- left_join(index_table, distress, by=c("Place"="place", "type"="type"))

#some of the key fields in index_table are not populated for all the cities




#ADD VARIABLES:
#pct change in closed sales
#diff in days on market
#average PPSF for previous four years (columns 12 through 15 in the table, yrs1-4)
index_table <- index_table%>%
  mutate(cs_pctchange= (cs_curr-cs_prev)/cs_prev, 
  dom_diff=dom_curr-dom_prev,
  avgPPSF= rowMeans(index_table[,12:15]))

#ADD VARIABLE:
#Pct change between PPSF for most recent year and prior 4-yr average
index_table <- index_table%>%
  mutate(ppsf_pctchange = round((ppsf_yr5-avgPPSF)/avgPPSF,3))



#RANKINGS:
#rank days on market (1=highest number) Notice the minus sign in front of the field name
#rank Pct of original price (1=low percentage)
#rank PPSF change (1=lowest percentage)  
#rank distressed (1=high percentage) Notice the minus sign in front of the field name
#the na.last=false on the distressed one is to look for NULL values in the PctDistressed field; if so, they get a low rank score

#This winnows it down to only cities that are eligible for rankings (75 sales or more)
index_table_rankings <- index_table%>%
  filter(cs_curr>=75, type=='city') %>% 
  mutate(dom_rank=rank(-dom_curr),
         polp_rank=rank(pctorigprice),
         ppsf_rank=rank(ppsf_pctchange),
         distress_rank=rank(-PctDistressed, na.last=FALSE))


#total index score -combine the ranking scores
index_table_rankings <- index_table_rankings%>%
  mutate(index_score = dom_rank+polp_rank+ppsf_rank+distress_rank)

#rank the index score (1=highest)
#notice the minus sign in front of index_score so that the highest score gets the rank of #1
#the ties.method=c("max") ensures that you don't have decimal points in the rank number
index_table_rankings <- index_table_rankings%>%
  mutate(index_rank = rank(-index_score, ties.method = c("max")))



#add last year's index position info
#and add back to all the cities
lastindex_results <- lastindex%>%select(Place, type, geoid2, index_rank) %>% rename(LastRank=index_rank)

final_table <-  left_join(index_table, index_table_rankings %>% select(geoid2, dom_rank, polp_rank, ppsf_rank, 
                                                                       distress_rank, index_score, index_rank),
                          by=c("geoid2"="geoid2"))

final_table <- left_join(final_table, lastindex_results, by=c("Place"="Place", "type"="type"))





# ADD CENSUS DATA ---------------------------------------------------------


#Join Census metrics to the final_table
final_table <- left_join(final_table, census_tenure %>%
                           select(geoid, PctOwner, PctCostBurdenedOwners), by=c("geoid2.x"="geoid"))

final_table <- left_join(final_table, census_income %>%
                           select(geoid, MedianHHIncome), by=c("geoid2.x"="geoid"))

final_table <-  left_join(final_table, census_value %>% 
                            select(geoid, MedianValue), by=c("geoid2.x"="geoid"))




# CREATE FINAL HOT HOUSING TABLE ------------------------------------------



final_table_export <-  final_table %>%
  select(Place, geoid2.x, FullName, CityName,location,
                                            COUNTY, STATE, cs_curr, ppsf_pctchange, dom_diff,pctorigprice,
                                            PctDistressed,NewConstruct, index_score, index_rank, LastRank,
                                            PctOwner, PctCostBurdenedOwners, MedianHHIncome, MedianValue, stribID, type) %>% 
  rename(geoid2=geoid2.x)






write.csv(final_table_export, "hotindex2018_testing.csv", row.names=FALSE)




# EXPORT JSON -------------------------------------------------------------



hot_housing_index_json <-  toJSON(final_table_export, pretty=TRUE)
write(hot_housing_index_json, "hot_housing_index.json")



timeseries_json <-  toJSON(timeseries, pretty=TRUE)
write(timeseries_json, "timeseries.json")



