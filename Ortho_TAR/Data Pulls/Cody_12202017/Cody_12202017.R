#Match patients that have no TKA for the 15 in the Ortho database that had TKA then TAA
library(dplyr)
library(readr)
library(readxl)
library(tidyr)
library(lubridate)
source('P://Outside DUSON RMT Projects//Department_ORTHO//Ortho TAR - Stewart Folder//Code//scoring_code.R')
#Define a function we will use later
eventindex <- function(ren){
  return(ifelse(grepl('pre',ren),1,ifelse(
    grepl('3_month',ren),2,ifelse(
      grepl('6_month',ren),3,ifelse(
        grepl('1_year',ren),4,ifelse(
          grepl('2_year',ren),5,ifelse(
            grepl('3_year',ren),6,ifelse(
              grepl('4_year',ren),7,ifelse(
                grepl('5_year',ren),8,ifelse(
                  grepl('6_year',ren),9,ifelse(
                    grepl('7_year',ren),10,ifelse(
                      grepl('8',ren),11,ifelse(
                        grepl('9',ren),12,ifelse(
                          grepl('10',ren),13,''))))))))))))))}
age <- function(from, to) {
  from_lt = as.POSIXlt(from)
  to_lt = as.POSIXlt(to)
  
  age = to_lt$year - from_lt$year
  
  ifelse(to_lt$mon < from_lt$mon |
           (to_lt$mon == from_lt$mon & to_lt$mday < from_lt$mday),
         age - 1, age)
}
keep<-c('mrn','sx_side_v2','redcap_event_name','visit_date','surgery_date','dob','age_at_surgery','age_at_visit','gender','weight','bmi','vas','smfa_fun_count','smfa_fun_avg','smfa_fun_atotal','smfa_fun_index','smfa_bother_count','smfa_bother_avg','smfa_bother_atotal','smfa_bother_index','sf36_body_pain','sf36_general_health','sf36_mental_health','sf36_physical_function','sf36_role_emotional','sf36_role_physical','sf36_social_fun','sf36_vitality','sf36_rep_health','aofas_hfoot','aofas_hfoot_function','aofas_hfoot_pain','aofas_hfoot_alignment','faos_pain','faos_symptoms','faos_adl','faos_spandrecpain','faos_qol','sf36_total','sf36_phy_hea_dim','sf36_men_hea_dim')

setwd('P://Outside DUSON RMT Projects//Department_ORTHO//Ortho TAR - Stewart Folder//Data Pulls//Cody_12202017')

#Load the list of mrns we are interested in and the tar data sets
mrn <- read_csv(file='.//Raw data//mrn.csv') %>% distinct(mrn)
tar <- read_csv(file='.//Raw data//ORTHOTAR_DATA_2017-12-20_1617.csv')

#Puts an MRN,sx_side_v2,surgery_date,dob on each record in TAR, and then only keep patients with no tka
tar_mrn <- select(tar,studyid,mrn,sx_side_v2,surgery_date,dob) %>% distinct(studyid,.keep_all=TRUE)
tar <- merge(tar,tar_mrn,by='studyid',suffixes=c('.DROP','')) %>% select(-contains('.DROP'))

#Merge with the mrn list and filter out missing
tar <- merge(tar,mrn,by='mrn',all.y=TRUE) 
missing_mrn <- filter(tar,is.na(studyid)) %>% select(mrn)
tar <- filter(tar,!is.na(studyid))

tar<-mutate(tar,age_at_surgery=age(ymd(dob),ymd(surgery_date)),
            age_at_visit=age(ymd(dob),ymd(visit_date))) #calculate age at time of surgery and visit

  #Do the scoring and translate sx_side_v2
    tar_scored <- fn_tar_scoring(tar)
    tar <- inner_join(tar,tar_scored, by=c('studyid','redcap_event_name'),suffix = c('.DROP','')) %>%
    select(keep) %>%
    mutate(sx_side_v2=ifelse(sx_side_v2=='1','R','L'))

  # #Determine which records don't have event max < 1 year and remove
  #   work_tar_recent <- select(work_tar,mrn,sx_side_v2,surgery_date,redcap_event_name) %>% mutate(index=eventindex(redcap_event_name)) %>%
  #     group_by(mrn,sx_side_v2,surgery_date) %>% summarise(eventmax = max(index))
  # 
  #   work_tar <- merge(work_tar,work_tar_recent,by=c('mrn','sx_side_v2','surgery_date')) %>% filter(eventmax>3)
# 
#   #Determine which records don't have preop scoring and remove
#     tar_remove <- filter(work_tar,grepl('pre',redcap_event_name)&(is.na(vas)|is.na(smfa_fun_index)|is.na(smfa_bother_index)|is.na(sf36_total)|is.na(sf36_phy_hea_dim)|is.na(sf36_men_hea_dim)|is.na(aofas_hfoot))) %>%
#       select(mrn,sx_side_v2,surgery_date) %>%
#       mutate(remove='1')
# 
#     work_tar <- left_join(work_tar,tar_remove,by=c('mrn','sx_side_v2','surgery_date')) %>%
#       mutate(index=eventindex(redcap_event_name)) %>%
#       filter(is.na(remove)& (index==1|index>3)) %>%
#       arrange(mrn,sx_side_v2,surgery_date,index) %>%
#       select(-contains('faos'))
# 
#     assign(an,work_tar)
#     write.csv(get(an),file=paste("P://Outside DUSON RMT Projects//Department_ORTHO//Ortho TAR - Stewart Folder//Data Pulls//Ortho_11302017//Output//OrthoTAR Data//All data//",an,'.csv',sep=''),na='',row.names=FALSE)

  #Figure out which is the most recent event (that has all the scores we care about)
    tar_recent <- filter(tar,(grepl('pre',redcap_event_name) | !(is.na(vas)|is.na(smfa_fun_index)|is.na(smfa_bother_index)|is.na(sf36_total)|is.na(sf36_phy_hea_dim)|is.na(sf36_men_hea_dim)|is.na(aofas_hfoot)))) %>%
      select(mrn,sx_side_v2,surgery_date,redcap_event_name) %>% mutate(index=eventindex(redcap_event_name)) %>%
      group_by(mrn,sx_side_v2,surgery_date) %>% summarise(eventmax = max(index))

  #Merge most recent back in
    tar <- merge(tar,tar_recent,by=c('mrn','sx_side_v2','surgery_date')) %>%
      filter(eventmax > 3) %>%
      mutate(index=eventindex(redcap_event_name)) %>%
      filter(index=='1' | index==eventmax) %>%
      arrange(mrn,sx_side_v2,surgery_date,index) %>%
      select(-index,-eventmax)

write.csv(tar,file='.//Output//Cody_12202017.csv',na='',row.names=FALSE)
write.csv(missing_mrn,file='.//Output//Cody_12202017_MISSING_MRN.csv',na='',row.names=FALSE)
