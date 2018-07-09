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
work <- select(dbn,studyid,mrn,sx_side,sx_side_v2) %>% distinct(studyid,.keep_all=TRUE)
dbn <- merge(dbn,work,by='studyid',suffixes=c('.DROP','')) %>% 
  select(-contains('.DROP'))
rm(work)

return(dbn)
}

#Run the function for tar and tar_licensed databases
tar <- data_pull(tar,'APITOKEN')
tar_lic <- data_pull(tar_lic,'APITOKEN')

#Choose all the patients from arm 1, remove duplicates
tar <- bind_rows(tar,tar_lic) 
rm(tar_lic)

#Scoring
tar_scored <- fn_tar_scoring(tar)
tar <- inner_join(tar,tar_scored, by=c('studyid','redcap_event_name'),suffix = c('.DROP','')) %>%
  select(-contains('.DROP')) %>%
  mutate(event = eventindex(redcap_event_name))
rm(tar_scored)

#Select only the MRNs and events we want
tar <- filter(tar, mrn %in% c('XX3420','FY9954','FW4106','PG8944','GA2620','GJ6075','KF5269')) %>%
  filter(event <= 5 & event != 2)  %>%
  arrange(mrn,sx_side_v2,event) %>% 
  distinct(mrn,sx_side_v2,visit_date,.keep_all=TRUE)

#Select the columns we want
tar <- select(tar,studyid,mrn,sx_side_v2,redcap_event_name,visit_date,dob,age_at_sugery,smoking_history,packs_per_day,quit_date,packs_year,diabetic,diabetes_type,primary_diagnosis,primary_diagnosis_v2,vas,smfa_fun_count:sf36_pcs)

write.csv(tar,file='./Data Pulls/Lachman_20180515/output/Lachman_20180515.csv',na='',row.names=FALSE)