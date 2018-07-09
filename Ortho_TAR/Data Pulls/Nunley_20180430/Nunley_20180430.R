#Data merge for Dr. Nunley on 11/2`/2017
library(dplyr)
library(readr)
library(readxl)
library(tidyr)
library(lubridate)
library(RCurl)
source('./Code/scoring_code.R')

#Create the function that pulls and cleans data from the database
data_pull <- function(dbn,api_token){
  
  dbn_api <- postForm(uri='https://redcap.duke.edu/redcap/api/',token=api_token,content='report',format='csv',report_id='ALL',rawOrLabel='raw',rawOrLabelHeaders='raw',exportCheckboxLabel='false',returnFormat='csv')
  dbn <- read.csv(textConnection(dbn_api),stringsAsFactors = FALSE)
  
  #Need to put prosthesis, MRN, surgery_date on each row
  work <- select(dbn,studyid) %>% distinct(studyid,.keep_all=TRUE)
  dbn <- merge(dbn,work,by='studyid',suffixes=c('.DROP','')) %>%
    select(-contains('.DROP'))
  rm(work)
  
  return(dbn)
}

#Load the list of mrns we are interested in and the tar data set
prosthesis <- read_csv(file='./Data Pulls/Nunley_20180430/raw/prosthesis.csv') %>% select(studyid)
tar<-data_pull(tar,'APITOKEN')

#Take only the patients we care about
tar <- inner_join(prosthesis,tar,by='studyid')

#Scoring 
tar_scored <- fn_tar_scoring(tar)
tar <- inner_join(tar,tar_scored, by=c('studyid','redcap_event_name'),suffix = c('.DROP','')) %>%
  select(-contains('.DROP')) %>%
  mutate(event = eventindex(redcap_event_name)) 
rm(tar_scored)

#Determine most recent event, only want to keep pre-op, mrf, 1year, and 2year
tar <- mutate(group_by(tar,studyid), most_recent_fu = max(event))

#We want pre-op, 1year, 2year, and most recent follow-up -- we want the data to be wide
pre <- filter(tar,event == '1') %>% 
  select(studyid,mrn,prosthesis,dob,gender,race___0:race_other,height,weight,bmi,asa,smoking_history,packs_per_day,quit_date,packs_year,diabetic,diabetes_type,heart_disease,heart_problems___1:heart_problems_v2,study_status,study_status_reason,withdraw_date,study_status_reason_v2,sx_side,sx_side_v2,prior_sx___1:prior_sx___5,surgery_date,visit_date,vas,fadi_total,smfa_fun_atotal,smfa_fun_index,smfa_bother_atotal,smfa_bother_index,aofas_hfoot,aofas_hfoot_alignment,aofas_hfoot_function,aofas_hfoot_pain,sf36_phy_hea_dim,sf36_men_hea_dim,sf36_total)
mrf <- filter(tar,event == most_recent_fu) %>%
  select(studyid,most_recent_fu,visit_date,vas,fadi_total,smfa_fun_atotal,smfa_fun_index,smfa_bother_atotal,smfa_bother_index,aofas_hfoot,aofas_hfoot_alignment,aofas_hfoot_function,aofas_hfoot_pain,sf36_phy_hea_dim,sf36_men_hea_dim,sf36_total)
year1 <- filter(tar,event == '4') %>%
  select(studyid,visit_date,vas,fadi_total,smfa_fun_atotal,smfa_fun_index,smfa_bother_atotal,smfa_bother_index,aofas_hfoot,aofas_hfoot_alignment,aofas_hfoot_function,aofas_hfoot_pain,sf36_phy_hea_dim,sf36_men_hea_dim,sf36_total)
year2 <- filter(tar,event == '5') %>%
  select(studyid,visit_date,vas,fadi_total,smfa_fun_atotal,smfa_fun_index,smfa_bother_atotal,smfa_bother_index,aofas_hfoot,aofas_hfoot_alignment,aofas_hfoot_function,aofas_hfoot_pain,sf36_phy_hea_dim,sf36_men_hea_dim,sf36_total)

#Merge the data together so we have 1 row per studyid
tar <- left_join(pre,mrf,by='studyid',suffix = c('','_mrf')) %>%
  left_join(year1,by='studyid',suffix = c('','_year1')) %>%
  left_join(year2, by = 'studyid', suffix = c('_pre','_year2'))

#Translate most-recent followup so it's legible
tar <- mutate(tar,most_recent_fu = case_when(
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
))

#Split into two files -- salto and star
salto <- filter(tar,prosthesis==1) %>% 
  mutate(prosthesis='Salto-Talaris')
star <- filter(tar,prosthesis==2) %>%
  mutate(prosthesis='STAR')

write.csv(salto,file="./Data Pulls/Nunley_20180430/output/20180430_SALTO.csv",na='',row.names=FALSE)
write.csv(star,file="./Data Pulls/Nunley_20180430/output/20180430_STAR.csv",na='',row.names=FALSE)