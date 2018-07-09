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

#Need to put prosthesis, MRN, surgery_date on each row
work <- select(dbn,studyid,mrn,sx_side,sx_side_v2,gender) %>% distinct(studyid,.keep_all=TRUE)
dbn <- merge(dbn,work,by='studyid',suffixes=c('.DROP','')) %>% 
  select(-contains('.DROP'))
rm(work)

return(dbn)
}

#Run the function for tar and tar_licensed databases
tar <- data_pull(tar,'APITOKEN')
tar_lic <- data_pull(tar_lic,'APITOKEN')

#Choose all the patients from arm 1, remove duplicates
tar <- bind_rows(tar,tar_lic) %>% 
  filter(grepl('arm_1',redcap_event_name) & !grepl('3_month',redcap_event_name)) %>% 
  distinct(mrn,sx_side_v2,visit_date,.keep_all=TRUE)
rm(tar_lic)

#Scoring
tar_scored <- fn_tar_scoring(tar)
tar <- inner_join(tar,tar_scored, by=c('studyid','redcap_event_name'),suffix = c('.DROP','')) %>%
  select(-contains('.DROP')) %>%
  mutate(event = eventindex(redcap_event_name)) %>%
  arrange(mrn,sx_side_v2,event)
rm(tar_scored)

#Select the columns we want
tar <- select(tar,mrn,sx_side_v2,redcap_event_name,visit_date,dob,age_at_sugery,gender,race___0:race___999,race_other,height,weight,bmi,asa,smoking_history,packs_per_day,quit_date,packs_year,diabetic,diabetes_type,primary_diagnosis,primary_diagnosis_v2,prosthesis,patient_notes,prior_sx___1:prior_sx___5,surgery_date,surgery_start_time,surgery_end_time,tourniquet_start_time,tourniquet_end_time,attending,vas,smfa_fun_count:sf36_pcs)

write.csv(tar,file='./Data Pulls/Bradley_20180508/output/Bradley_20180508.csv',na='',row.names=FALSE)