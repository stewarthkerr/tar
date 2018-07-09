#Data pull for visiting scholar
library(RCurl)
library(dplyr)
library(readr)
library(readxl)
library(tidyr)
library(lubridate)
source('./Code/scoring_code.R')

#Load the list of mrns we are interested in
mrn_list <- read_csv("./Data Pulls/Lazarides_01092018/data/mrn.csv") %>% 
  distinct()

#Create the function that pulls and cleans data from the database
data_pull <- function(dbn,api_token){
  
dbn_api <- postForm(uri='https://redcap.duke.edu/redcap/api/',token=api_token,content='report',format='csv',report_id='ALL',rawOrLabel='raw',rawOrLabelHeaders='raw',exportCheckboxLabel='false',returnFormat='csv')
dbn <- read.csv(textConnection(dbn_api),stringsAsFactors = FALSE)

#Put mrn, sx_side,sx_side_v2,surgery_date on each record
work <- select(dbn,studyid,mrn,sx_side,sx_side_v2,surgery_date) %>% distinct(studyid,.keep_all=TRUE)
dbn <- merge(dbn,work,by='studyid',suffixes=c('.DROP','')) %>% 
  select(-contains('.DROP'))
rm(work)

#Merge with the mrn list and filter out missing and records after 11-14-17
dbn <- mutate(dbn,mrn=gsub(' ','',toupper(mrn))) %>%
  merge(mrn_list,by='mrn',all.y=TRUE) %>% 
  filter(!is.na(studyid),visit_date=='' | ymd(visit_date)<"2017-11-14")

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
  distinct(studyid,redcap_event_name,.keep_all=TRUE) %>%
  arrange(mrn,sx_side_v2,surgery_date,index) %>%
  select(-contains('.DROP'),-contains('imp_'),-matches('fadi_\\d+'),-matches('smfa_\\d+'),-matches('sf36_\\d+'),-matches('._complete'),-matches('aofas_\\d+'),-matches('faos_[[:lower:]]+\\d'),-index) %>%
  select(mrn,studyid,sx_side_v2,redcap_event_name:sf36_men_hea_dim)
rm(tar_lic)

#Pull the list of missing mrns after the tar_lic and tar pulls have been added together
missing_mrn <- merge(select(tar,studyid,mrn),mrn_list,by='mrn',all.y=TRUE) %>% 
  filter(is.na(studyid)) %>% select(-studyid)

#Output
write.csv(missing_mrn,file="./Data Pulls/Lazarides_01092018/output/01092018_tar_missing_mrn.csv",na='',row.names=FALSE)
write.csv(tar,file="./Data Pulls/Lazarides_01092018/output/01052018_lazarides.csv",na='',row.names=FALSE)