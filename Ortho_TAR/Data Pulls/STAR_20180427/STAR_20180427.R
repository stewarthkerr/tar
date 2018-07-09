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
work <- select(dbn,studyid,mrn,sx_side,sx_side_v2,surgery_date,prosthesis,dob,age_at_sugery,height,weight,bmi,gender,diabetic,diabetes_type,smoking_history,packs_per_day,quit_date,packs_year) %>% distinct(studyid,.keep_all=TRUE)
dbn <- merge(dbn,work,by='studyid',suffixes=c('.DROP','')) %>% 
  select(-contains('.DROP'))
rm(work)

return(dbn)
}

#Run the function for tar and tar_licensed databases
tar <- data_pull(tar,'APITOKEN')
tar_lic <- data_pull(tar_lic,'APITOKEN')

#Choose all the STAR patients from arm 1, remove duplicates
tar <- bind_rows(tar,tar_lic) %>% 
  filter(prosthesis=='2',grepl('arm_1',redcap_event_name)) %>% 
  distinct(mrn,sx_side_v2,visit_date,.keep_all=TRUE)
rm(tar_lic)

#Scoring
tar_scored <- fn_tar_scoring(tar)
tar <- inner_join(tar,tar_scored, by=c('studyid','redcap_event_name'),suffix = c('.DROP','')) %>%
  select(-contains('.DROP')) %>%
  mutate(event = eventindex(redcap_event_name))
rm(tar_scored)

#Determine most recent event
tar <- mutate(group_by(tar,mrn,sx_side_v2), most_recent_fu = max(event)) %>%
  mutate(most_recent_fu = case_when(
    most_recent_fu == '1' ~ 'Pre-Op',
    most_recent_fu == '2' ~ '3 Month',
    most_recent_fu == '3' ~ '6 Month',
    most_recent_fu == '4' ~ '1 Year',
    most_recent_fu == '5' ~ '2 Year',
    most_recent_fu == '6' ~ '3 Year',
    most_recent_fu == '7' ~ '4 Year',
    most_recent_fu == '8' ~ '5 Year',
    most_recent_fu == '9' ~ '6 Year',
    most_recent_fu == '10' ~ '7 Year',
    most_recent_fu == '11' ~ '8 Year',
    most_recent_fu == '12' ~ '9 Year',
    most_recent_fu == '13' ~ '10 Year'
  )) %>%
  arrange(mrn,sx_side_v2,event)

#Select the columns we want
tar <- select(tar,mrn,sx_side_v2,prosthesis,redcap_event_name,most_recent_fu,visit_date,mrn,surgery_date,dob,age_at_sugery,height,weight,bmi,gender,diabetic,diabetes_type,smoking_history,packs_per_day,quit_date,packs_year,vas,smfa_fun_count:sf36_pcs)

write.csv(tar,file='./Data Pulls/STAR_20180427/output/STAR_20180427.csv',na='',row.names=FALSE)