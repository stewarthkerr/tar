#Data pull for visiting scholar
library(RCurl)
library(dplyr)
library(readr)
library(readxl)
library(tidyr)
library(lubridate)
source('./Code/scoring_code.R')

#Create the function that pulls and cleans data from the database
data_pull <- function(dbn,api_token){
  
dbn_api <- postForm(uri='https://redcap.duke.edu/redcap/api/',token=api_token,content='report',format='csv',report_id='ALL',rawOrLabel='raw',rawOrLabelHeaders='raw',exportCheckboxLabel='false',returnFormat='csv')
dbn <- read.csv(textConnection(dbn_api),stringsAsFactors = FALSE)

return(dbn)
}

#Run the function for tar and tar_licensed databases
tar<-data_pull(tar,'APITOKEN')
tar_lic<-data_pull(tar_lic,'APITOKEN') %>% select(colnames(tar)) #This removes the columns that are specific to tar_lic so we can vertically append

#Vertically append the two datasets and remove duplicate rows
tar <- bind_rows(tar,tar_lic) %>%
  distinct(studyid,redcap_event_name,.keep_all=TRUE) 
rm(tar_lic)

#Select only records from arm 1, only the variables we care about
tar <- filter(tar,grepl('arm_1',redcap_event_name),surgery_date<='2013-03-30',surgery_date!='',grepl('pre',redcap_event_name)) %>%
  select(mrn,dob,surgery_date,gender,weight,height,bmi,diabetic,diabetes_type,smoking_history,packs_per_day,quit_date,packs_year,tourniquet_start_time,tourniquet_end_time)

#Output
write.csv(tar,file="./Data Pulls/Cody_030518/output/030518_cody.csv",na='',row.names=FALSE)