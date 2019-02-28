#install.packages(c("rmarkdown",  "lubridate", "knitr", "car", "aws.s3", "dplyr", "rehsape2", "ggplot2", "readr", "janitor", "scales", "tidyr", "htmltools", "tidyverse", "readxl", "ggthemes", "waffle"))


#install.packages("foreign") #this is for loading an SPSS file

#install.packages(c("kableExtra","DT" ))



# load_libraries ----------------------------------------------------------

library(readr) #importing csv files
library(dplyr) #general analysis 
library(ggplot2) #making charts
library(lubridate) #date functions
library(reshape2) #use this for melt function to create one record for each team
library(tidyr)
library(janitor) #use this for doing crosstabs
library(scales) #needed for stacked bar chart axis labels
library(knitr) #needed for making tables in markdown page
#library(car)
library(aws.s3)
library(htmltools)#this is needed for Rstudio to display kable and other html code
library(rmarkdown)
library(readxl)
library(DT) #needed for making  searchable sortable data tble
library(kableExtra)
library(ggthemes)
library(waffle)
library(foreign)



# import_data -------------------------------------------------------------

#use readr package to bring in csv file
#col_names=FALSE ignores the headers and gives it X1, X2, etc
#skip=1 means to not import that header row
#col_types tells it to either set field to character (c) or Date (D) or skip ("_")
#trim_ws trims leading and trailing spaces
rape_import <-  read_csv("./data/RapeProject2018-MaryJoExportView.csv", col_names=FALSE, skip=1,
                         col_types=cols(X1=col_date("%m/%d/%Y"),
                                        X2='c',X3='c',X4='c',X5='_',X6='_',X7='c',X8='c',X9='_',X10='_',X11='_',X12='_',
                                        X13='c',X14='_',X15='c',X16='c',X17=col_date("%m/%d/%Y"),X18=col_date("%m/%d/%Y"),
                                        X19=col_date("%m/%d/%Y"),X20='c',X21='c',X22='c',X23='_',X24='c',X25='c',X26='c',X27='c',
                                        X28='c',X29='c',X30='c',X31='c',X32='c',X33='c',X34='c',X35='c',X36='c',X37='c',X38='c',X39='c',
                                        X40='c',X41='c',X42='c',X43='_',X44='_',X45='_',X46='_',X47='_',X48='_',X49='c',X50='c',X51='_',
                                        X52='_',X53='c',X54='c',X55='_',X56='c',X57='c',X58='c',X59='c',X60='c',X61='c',
                                        X62='c', X63='c', X64='c' ,X65='c'),   trim_ws=TRUE)



#this assigns new column names
colnames(rape_import) <- c("timestamp","department","casenumber","vulnerable","closurecode","investigatorsassigned","casenotes", "whocreated",
                           "opencase",
                           "dateclosed","incidentdate","datereported","suspectnames", "suspectDOB", "suspect_prior","collegestudents","kitdestroyed","casetype",
                           "legaltype","victiminvolve","reportingdelay","suspectarrested","sentforprosecution", "county", "chargesfiled","conviction", "sentence",
                           "victiminterviewed","interview_policestation","suspectinterviewed",
                           "potentialwitnesses_notinterviewed","rapeexam","otherphysicalevidence","testingresults","pretextcall","police_scene",
                           "victimintoxicated","victim_affectmemory","suspect_personofcolor","statutorycase","malevictim",
                           "threecontactattempts","potentialevidence_notcollected","significanteffort_identifysuspect","
                           Kit_testing", "doublevictim", "repeat", "victim_age")





# winnow_records ----------------------------------------------------------


#identify the records that are tagged as unfounded and create new variable

rape_import <- rape_import %>%
  mutate(unfounded=ifelse(grepl("unfounded", closurecode), "unfounded", "NA")) 


## WINNOW OUT RECORDS WE DON'T WANT TO INCLUDE
#First create field indicating year case was reported to police
#create another field to identify records with a delay in reporting
#exclude cases flagged as open
#include only cases where year reported is 2015 or more recent
#exclude unfounded cases

##excludes Wright County because we discovered their data only 
#included cases that did NOT result in arrest


rape <- rape_import %>%
  mutate(yr=year(datereported),diff=difftime(datereported, incidentdate, units=c("days"))) %>%
  filter(is.na(opencase), yr>=2015, unfounded=="NA")

#rape_import %>% group_by(year(datereported)) %>% summarise(count=n())


#create new variable that collapses departments
#and identifies ones we are not including
#excluded ones tend to be one-off cases that aren't in our top 20 agencies

rape$department_collapse <- "NA"
rape$department_collapse[rape$department=="Minneapolis"] <- "Minneapolis"
rape$department_collapse[rape$department=="St. Paul"] <- "St. Paul"
rape$department_collapse[rape$department=="Roseville"] <- "Exclude"
rape$department_collapse[rape$department=="Winona"] <- "Exclude"
rape$department_collapse[rape$department=="Willmar"] <- "Exclude"
rape$department_collapse[rape$department=="Other"] <- "Exclude"
rape$department_collapse[rape$department=="Wright County"] <- "All other"
rape$department_collapse[rape$department_collapse=="NA"] <- "All other"

#filter out the departments/cases we're excluding from the analysis
rape <-  rape %>%  filter(department_collapse!="Exclude")


# replace_null_values -----------------------------------------------------


rape$conviction[is.na(rape$conviction)] <- "N/A"
rape$chargesfiled[is.na(rape$chargesfiled)] <- "N/A"
rape$victiminvolve[is.na(rape$victiminvolve)] <- "N/A"
rape$suspectarrested[is.na(rape$suspectarrested)] <- "N/A"
#rape$casetype[is.na(rape$casetype)] <- "N/A"
rape$legaltype[is.na(rape$legaltype)] <- "N/A"
rape$potentialevidence_notcollected[is.na(rape$potentialevidence_notcollected)] <- "N/A"
rape$potentialwitnesses_notinterviewed[is.na(rape$potentialwitnesses_notinterviewed)] <- "N/A"
rape$sentforprosecution[is.na(rape$sentforprosecution)] <- "N/A"
rape$investigatorsassigned[is.na(rape$investigatorsassigned)] <- "Cannot determine"
rape$victiminterviewed[is.na(rape$victiminterviewed)] <- "N/A"
rape$police_scene[is.na(rape$police_scene)] <- "N/A"
rape$significanteffort_identifysuspect[is.na(rape$significanteffort_identifysuspect)] <- "N/A"
rape$suspectinterviewed[is.na(rape$suspectinterviewed)] <- "N/A"
rape$rapeexam[is.na(rape$rapeexam)] <- "Unknown"
rape$victim_affectmemory[is.na(rape$victim_affectmemory)] <- "Unclear"



# Recoding ----------------------------------------------------------------


#add field indicating if there is a delay of 2 days or more between when incident occurs and when reported to police
rape <- rape %>% mutate(delay= case_when(is.na(datereported)~"no date reported", 
                                         diff>=2~ "delay", 
                                         diff<2 ~ "no delay",
                                         TRUE~ "unknown"))



#Create groups & assign suspect flag, for analysis purposes
rape <- rape %>% mutate(group= case_when(investigatorsassigned=="Yes" & sentforprosecution=="No" ~ "Investigated/Not sent for Prosecution", 
                                         sentforprosecution=="Yes" ~ "Sent for prosecution",
                                         investigatorsassigned=="No" ~ "Not assigned to investigators",
                                         investigatorsassigned=="Yes" & sentforprosecution=="Cannot determine"~ "Investigated/Not sent for Prosecution"), 
                        suspectflag= case_when(grepl(",", suspectnames)~ "Named suspect",
                                               grepl("unknown", suspectnames)~ "None",
                                               grepl("redacted", suspectnames)~"Named suspect",
                                               grepl("Unknown", suspectnames)~"None",
                                               grepl("Redacted", suspectnames)~"Named suspect",
                                               is.na(suspectnames)~"None",
                                               TRUE ~ "Partial name"
                        ))






#identify cases that were declined for prosecution in a new field
rape$declinedcase <- "NA"
rape$declinedcase[rape$sentforprosecution=="Yes" & rape$chargesfiled=="No"] <- "Declined"


#This lumps all the "not prosecuted" cases together into one group
rape <-  rape %>% mutate(group2 = case_when(group=="Investigated/Not sent for Prosecution" | group=="Not assigned to investigators"~ "Not sent for prosecution", 
                                            group=="Sent for prosecution"~"Sent for prosecution"))


#These recode victim and suspect interview fields
ELSE <- TRUE

rape <-  rape %>% mutate(victim_interview_combine = case_when(victiminterviewed=="No" | victiminterviewed=="Attempted"~"No/Attempted", victiminterviewed=="Yes"~"Yes", ELSE~"Unk"))

rape <-  rape %>% mutate(suspect_interview_combine = case_when(suspectinterviewed=="No" | suspectinterviewed=="Attempted"~"No/Attempted", suspectinterviewed=="Yes"~"Yes", ELSE~"Unk"))

rape$victim_interview_combine <- factor(rape$victim_interview_combine, levels=c("Yes", "No/Attempted", "Unk"))

rape$suspect_interview_combine <- factor(rape$suspect_interview_combine, levels=c("Yes", "No/Attempted", "Unk"))


#Create a field that creates the top 4 bucket for departments
rape <-  rape %>%
  mutate(top4=case_when(department=="Anoka" | department=="Minneapolis" | department=="St. Paul" | department=="Duluth"~"Top 4", TRUE~"All others"))


#Create a field that puts departments in buckets
rape <-  rape %>% mutate(top2=case_when( department=="Minneapolis" | department=="St. Paul" ~"Top 2", TRUE~"All others"))


#Create a field that marries rapeexam and otherphysical evidence together
rape <-  rape %>% mutate(physicalevidence = case_when(rapeexam=="No" & otherphysicalevidence=="No"~"None", 
                                                      rapeexam=="Not applicable" & otherphysicalevidence=="No"~"None",
                                                      rapeexam=="victim refused" & otherphysicalevidence=="No"~"None",
                                                      rapeexam=="Unknown" & otherphysicalevidence=="No"~"None",
                                                      rapeexam=="Unknown" & is.na(otherphysicalevidence)~"None",
                                                      rapeexam=="No" & is.na(otherphysicalevidence)~"None", 
                                                      rapeexam=="Yes" & otherphysicalevidence=="No"~"Kit only",
                                                      rapeexam=="Yes" & is.na(otherphysicalevidence)~"Kit only",
                                                      rapeexam=="No" & otherphysicalevidence=="Yes"~"Other evidence only",
                                                      rapeexam=="Not applicable" & otherphysicalevidence=="Yes"~"Other evidence only",
                                                      rapeexam=="victim refused" & otherphysicalevidence=="Yes"~"Other evidence only",
                                                      rapeexam=="Unknown" & otherphysicalevidence=="Yes"~"Other evidence only",
                                                      rapeexam=="Yes" & otherphysicalevidence=="Yes"~"Both"))





# set_factor_order --------------------------------------------------------

#set the order for  variables in key fields
rape$conviction <- factor(rape$conviction, levels=c("Yes", "No", "Case still pending", "Cannot determine"))
rape$chargesfiled <- factor(rape$chargesfiled, levels=c("Yes", "No", "Cannot determine",  "NEED TO CHECK", "N/A"))
rape$casetype <- factor(rape$casetype, levels=c("Acquaintance", "Stranger", "Unclear"))
rape$legaltype <- factor(rape$legaltype, levels=c("Force/Threat/Fear", "Incapacitation", "Both", "Unknown", "N/A"))
rape$potentialevidence_notcollected <- factor(rape$potentialevidence_notcollected, levels=c("Yes", "No", "Does not apply", "Unclear", "N/A"))
rape$potentialwitnesses_notinterviewed <- factor(rape$potentialwitnesses_notinterviewed, levels=c("Yes", "No", "Not applicable", "N/A"))
rape$sentforprosecution <- factor(rape$sentforprosecution, levels=c("Yes", "No","Cannot determine", "N/A"))
rape$suspectarrested <- factor(rape$suspectarrested, levels=c("Yes", "No", "Cannot determine", "N/A"))
rape$victiminvolve <- factor(rape$victiminvolve, levels=c("Withdrew at some point", "Cooperated throughout", "Unknown", "N/A"))
rape$investigatorsassigned <- factor(rape$investigatorsassigned, levels=c("Yes", "No"))
rape$victiminterviewed <- factor(rape$victiminterviewed, levels=c("Yes", "Attempted", "No", "Other", "N/A"))
rape$police_scene <- factor(rape$police_scene, levels=c("Yes", "No", "Does not apply", "Cannot determine", "N/A"))
rape$victimintoxicated <- factor(rape$victimintoxicated, levels=c("Yes", "No", "Unknown"))
rape$suspectinterviewed <- factor(rape$suspectinterviewed, levels=c("Yes", "Attempted", "No", "Other", "Suspect unknown", "N/A"))
rape$significanteffort_identifysuspect <-  factor(rape$significanteffort_identifysuspect, levels=c("Yes", "No", "Does not apply", "N/A" ))



# calculate_scores --------------------------------------------------------
#This section attempts to generate a "score" for how well investigators handled the case
#we didn't end up using this

ELSE <- TRUE

rape <- rape %>%mutate(score_victim= case_when(victiminterviewed=="No"~1, is.na(victiminterviewed)~0, ELSE~0))
rape <- rape %>%mutate(score_suspect= case_when(suspectflag=="Named suspect" & suspectinterviewed=="No" ~ 1, ELSE~0))
rape <- rape %>%mutate(score_witness= case_when(is.na(potentialwitnesses_notinterviewed)~0, potentialwitnesses_notinterviewed=="Yes"~1, potentialwitnesses_notinterviewed=="No"~0, ELSE~0))
rape <- rape %>%mutate(score_evidence= case_when(delay=="no delay" & potentialevidence_notcollected=="Yes"~1, is.na(potentialevidence_notcollected)~0, ELSE ~0))
#rape <- rape %>%mutate(score_scene= case_when(is.na(police_scene)~0, police_scene=="No"~1, ELSE~0))


#rape <- rape %>% mutate(finalscore = score_victim+score_suspect+score_witness+score_evidence+score_scene)
rape <- rape %>% mutate(finalscore = score_victim+score_suspect+score_witness+score_evidence)

#rape %>% group_by(finalscore) %>% summarise(count=n())

#rape %>% filter(is.na(finalscore)) %>% select(delay, suspectflag, victiminterviewed,suspectinterviewed, potentialwitnesses_notinterviewed, potentialevidence_notcollected, police_scene, score_victim, score_suspect, score_witness, score_evidence, score_scene)


rape <-  rape %>% mutate(finalscore_groups = case_when(finalscore>=3~"Failed to do 3 or more basics",
                                                                 finalscore<3 ~ "Other cases"))


#flip the score to positive spin

ELSE <- TRUE

rape <- rape %>%mutate(score_victim2= case_when(victiminterviewed=="Yes"~1, is.na(victiminterviewed)~0, ELSE~0))
rape <- rape %>%mutate(score_suspect2= case_when(suspectflag=="Named suspect" & suspectinterviewed=="Yes" ~ 1, ELSE~0))
rape <- rape %>%mutate(score_witness2= case_when(is.na(potentialwitnesses_notinterviewed)~0, potentialwitnesses_notinterviewed=="No"~1, potentialwitnesses_notinterviewed=="Yes"~0, ELSE~0))
rape <- rape %>%mutate(score_evidence2= case_when(delay=="no delay" & potentialevidence_notcollected=="No"~1, is.na(potentialevidence_notcollected)~0, ELSE ~0))

rape <- rape %>% mutate(positivescore = score_victim2+score_suspect2+score_witness2+score_evidence2)

rape <-  rape %>% mutate(positivescore_groups = case_when(positivescore>=3~"Did 3 or more basics",
                                                          positivescore<3 ~ "Other cases"))


rape <-  rape %>% mutate(evidence2=case_when(potentialevidence_notcollected=="Yes"~"Not all collected", potentialevidence_notcollected=="No"~"All collected", TRUE~"Unknown"))


rape <-  rape %>% mutate(witness2 = case_when(potentialwitnesses_notinterviewed=="Yes"~"Not all witnesses interviewed", potentialwitnesses_notinterviewed=="No"~"All witnesses interviewed", TRUE~"Unknown"))



#new rape exam variable for "explore3" page
rape <-  rape %>% mutate(rapeexam2 = case_when(rapeexam=='No' | rapeexam=='victim refused'~'no', rapeexam=='Yes'~'yes', rapeexam=='Not applicable'~'NA', rapeexam=='Unknown'~'unknown'))


#write.csv(rape, "rape_analysiscases.csv", row.names = FALSE)




