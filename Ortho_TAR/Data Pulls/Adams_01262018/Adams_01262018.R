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

#Put mrn, sx_side,sx_side_v2,surgery_date on each record
work <- select(dbn,studyid,mrn,sx_side,sx_side_v2,surgery_date) %>% distinct(studyid,.keep_all=TRUE)
dbn <- merge(dbn,work,by='studyid',suffixes=c('.DROP','')) %>% 
  select(-contains('.DROP'))
rm(work)

#Do the scoring and translate sx_side_v2, also create index for ordering
dbn_scored <- fn_tar_scoring(dbn)
dbn <- inner_join(dbn,dbn_scored, by=c('studyid','redcap_event_name'),suffix = c('.DROP','')) %>%
  mutate(sx_side_v2=ifelse(sx_side_v2=='1','R','L'),index=eventindex(redcap_event_name))

return(dbn)
}

#Run the function for tar and tar_licensed databases
tar<-data_pull(tar,'APITOKEN')
tar_lic<-data_pull(tar_lic,'APITOKEN') %>% select(colnames(tar)) #This removes the columns that are specific to tar_lic so we can vertically append

#Vertically append the two datasets and remove duplicate rows
tar <- bind_rows(tar,tar_lic) %>%
  distinct(studyid,redcap_event_name,.keep_all=TRUE) 
rm(tar_lic)

#Removes records that aren't preop or year 2 and that don't have a year 2
tar <- inner_join(tar,filter(tar,index==5) %>% select(studyid,has_year_2 = index),by='studyid') %>%
  select(-has_year_2)

#Filter off records that aren't from preop or year 2 
tar <- filter(tar,grepl('arm_1',redcap_event_name) & (index==1 | index==5))

#Arrange and select the variables we want
tar <- arrange(tar,mrn,sx_side_v2,surgery_date,index) %>%
  select(-contains('.DROP'),-contains('imp_'),-matches('fadi_\\d+'),-matches('smfa_\\d+'),-matches('sf36_\\d+'),-matches('._complete'),-matches('aofas_\\d+'),-matches('faos_[[:lower:]]+\\d')) %>%
  select(mrn,studyid,sx_side_v2,redcap_event_name:sf36_men_hea_dim)

#Pull out the two year records
work <- filter(tar,grepl('year',redcap_event_name)) %>% select(studyid,visit_date:sf36_men_hea_dim)

#Join back into the preop records so we can have 1 row per patient
tar <- inner_join(filter(tar,grepl('pre',redcap_event_name)) %>% select(mrn:sx_side_v2,visit_date:sf36_men_hea_dim),work,by='studyid',suffix = c(".preop",".2year"))

#Output
write.csv(tar,file="./Data Pulls/Adams_01262018/output/01262018_adams.csv",na='',row.names=FALSE)