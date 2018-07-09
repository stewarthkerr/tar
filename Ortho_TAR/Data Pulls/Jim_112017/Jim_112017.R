#Data pull for Dr. Lachman on 11/20/2017
library(dplyr)
library(readr)
library(readxl)
library(tidyr)
library(lubridate)
source('P://Outside DUSON RMT Projects//Department_ORTHO//Ortho TAR - Stewart Folder//Code//scoring_code.R')

#Load the list of mrns we are interested in and the tar data set
mrn <- read_csv(file='P://Outside DUSON RMT Projects//Department_ORTHO//Ortho TAR - Stewart Folder//Data Pulls//Jim_112017//mrn.csv')
tar <- read_csv(file='P://Outside DUSON RMT Projects//Department_ORTHO//Ortho TAR - Stewart Folder//Data Pulls//Jim_112017//Raw data//tar.csv')
arm3 <- read_csv(file='P://Outside DUSON RMT Projects//Department_ORTHO//Ortho TAR - Stewart Folder//Data Pulls//Jim_112017//Raw data//arm3.csv')

#Linking mrn and study id for arm 1
mrn_rn <- merge(mrn,tar,by='mrn',all.x=TRUE) %>% select(mrn,studyid,revision_date,second_revision_date) %>% distinct(mrn,.keep_all=TRUE)

#linking mrn and studyid for arm 3
mrn_a3 <- merge(arm3,mrn,by='mrn') %>% select(-redcap_event_name)

#vertically append mrn_a3 and mrn_rn
mrn_rn <- rbind(mrn_rn,mrn_a3)

#Creating a list that aren't in tar database
mrn_miss <- filter(mrn_rn,is.na(studyid))
mrn_rn <- filter(mrn_rn,!is.na(studyid))

b4split <- merge(mrn_rn,tar,by='studyid') %>%
  select(mrn=mrn.x,studyid,redcap_event_name,revision_date,second_revision_date,visit_date,bmi,smoking_history,diabetic,diabetes_type,primary_diagnosis,primary_diagnosis_v2,vas,smfa_fun_atotal:imp_smfa_bother_index,sf36_body_pain:imp_sf36_total,aofas_hfoot:imp_aofas_hfoot_alignment,faos_p1:faos_q4) %>%
  #filter(grepl('preop',redcap_event_name)|grepl('pre_op',redcap_event_name)|grepl('6_month',redcap_event_name)|grepl('1_year',redcap_event_name)|grepl('2_year',redcap_event_name)) %>%
  mutate(
    `FAOS Pain Score` = 100 - (faos_p1+faos_p2+faos_p3+faos_p4+faos_p5+faos_p6+faos_p7+faos_p8+faos_p9)*100/36,
    `FAOs Symptoms Score` = 100 - (faos_s1+faos_s2+faos_s3+faos_s4+faos_s5+faos_s6+faos_s7)*100/28,
    `FAOS ADL Score` = 100 - (faos_a1+faos_a2+faos_a3+faos_a4+faos_a5+faos_a6+faos_a7+faos_a8+faos_a9+faos_a10+faos_a11+faos_a12+faos_a13+faos_a14+faos_a15+faos_a16+faos_a17)*100/68,
    `FAOS Sport & Rec Score` = 100 - (faos_sp1+faos_sp2+faos_sp3+faos_sp4+faos_sp5)*100/20,
    `FAOS QOL Score` = 100 - (faos_q1+faos_q2+faos_q3+faos_q4)*100/16
    )
  
b4split<-mutate(b4split,visit_date=ymd(visit_date),
               revision_date=mdy(revision_date),
               second_revision_date=mdy(second_revision_date)
               )

b4split = within(b4split, {
  
  redcap_event_name = gsub('_arm_1','',redcap_event_name)
  redcap_event_name = gsub('_arm_2','',redcap_event_name)
  redcap_event_name = gsub('_arm_3','',redcap_event_name)
  
  eventindex = ifelse(grepl('pre',redcap_event_name),1,ifelse(
    grepl('3_month',redcap_event_name),2,ifelse(
      grepl('6_month',redcap_event_name),3,ifelse(
        grepl('1_year',redcap_event_name),4,ifelse(
          grepl('2_year',redcap_event_name),5,ifelse(
            grepl('3_year',redcap_event_name),6,ifelse(
              grepl('4_year',redcap_event_name),7,ifelse(
                grepl('5_year',redcap_event_name),8,ifelse(
                  grepl('6_year',redcap_event_name),9,ifelse(
                    grepl('7_year',redcap_event_name),10,ifelse(
                      grepl('8',redcap_event_name),11,ifelse(
                        grepl('9',redcap_event_name),12,ifelse(
                          grepl('10',redcap_event_name),13,'')))))))))))))
  
  smoking_history[smoking_history==1] <- 'Current smoker'
  smoking_history[smoking_history==2] <- 'Previous smoker'
  smoking_history[smoking_history==0] <- 'Never a smoker'
  diabetic[diabetic==1] <- 'Yes'
  diabetic[diabetic==0] <- 'No'
  diabetes_type[diabetes_type==1] <-'T1DM'
  diabetes_type[diabetes_type==2] <-'T2DM'
  primary_diagnosis[primary_diagnosis==1] <- 'Post Traumatic'
  primary_diagnosis[primary_diagnosis==2] <- 'Rheumatoid'
  primary_diagnosis[primary_diagnosis==3] <- 'Osteoarthritis'
  primary_diagnosis[primary_diagnosis==4] <- 'Other'
  
  #If the values are blank, check to see if they are in the imported value
  smfa_fun_index = ifelse(is.na(smfa_fun_index),imp_smfa_fun_index,smfa_fun_index)
  smfa_bother_index = ifelse(is.na(smfa_bother_index),imp_smfa_bother_index,smfa_bother_index)
  
  sf36_body_pain = ifelse(is.na(sf36_body_pain),imp_sf36_body_pain,sf36_body_pain)
  sf36_general_health = ifelse(is.na(sf36_general_health),imp_sf36_general_health,sf36_general_health)
  sf36_mental_health = ifelse(is.na(sf36_mental_health),imp_sf36_mental_health,sf36_mental_health)
  sf36_physical_function = ifelse(is.na(sf36_physical_function),imp_sf36_physical_function,sf36_physical_function)
  sf36_role_emotional = ifelse(is.na(sf36_role_emotional),imp_sf36_role_emotional,sf36_role_emotional)
  sf36_role_physical = ifelse(is.na(sf36_role_physical),imp_sf36_role_physical,sf36_role_physical)
  sf36_social_fun = ifelse(is.na(sf36_social_fun),imp_sf36_social_fun,sf36_social_fun)
  sf36_vitality = ifelse(is.na(sf36_vitality),imp_sf36_vitality,sf36_vitality)
  sf36_total = ifelse(is.na(sf36_total),imp_sf36_total,sf36_total)
  
  aofas_hfoot = ifelse(is.na(aofas_hfoot),imp_aofas_hfoot,aofas_hfoot)
  aofas_hfoot_pain = ifelse(is.na(aofas_hfoot_pain),imp_aofas_hfoot_pain,aofas_hfoot_pain)
  aofas_hfoot_function = ifelse(is.na(aofas_hfoot_function),imp_aofas_hfoot_function,aofas_hfoot_function)
  aofas_hfoot_alignment = ifelse(is.na(aofas_hfoot_alignment),imp_aofas_hfoot_alignment,aofas_hfoot_alignment)
  
  after_revision = ifelse(visit_date>revision_date,'Yes','No')
})

b4split <- select(b4split,-contains("imp_"),-contains("faos",ignore.case=FALSE)) %>%
  arrange(mrn,eventindex) %>%
  rename(event_timepoint = redcap_event_name)

demomerg <- select(b4split,studyid,bmi,smoking_history,diabetic,diabetes_type,primary_diagnosis,primary_diagnosis_v2) %>%
  distinct(studyid,.keep_all=TRUE)

b4split <- select(b4split,-bmi,-smoking_history,-diabetic,-diabetes_type,-primary_diagnosis,-primary_diagnosis_v2)

b4split <- inner_join(b4split,demomerg,by='studyid') %>% select(studyid,mrn,event_timepoint,visit_date,revision_date,second_revision_date,after_revision,bmi,smoking_history,diabetic,diabetes_type,primary_diagnosis,primary_diagnosis_v2,vas:`FAOS QOL Score`,-eventindex)

write.csv(b4split,file="P://Outside DUSON RMT Projects//Department_ORTHO//Ortho TAR - Stewart Folder//Data Pulls//Jim_112017//Output//112117_TAR_Output.csv",na='',row.names=FALSE)
write.csv(mrn_miss,file="P://Outside DUSON RMT Projects//Department_ORTHO//Ortho TAR - Stewart Folder//Data Pulls//Jim_112017//Output//112117_MRN_Missing.csv",na='',row.names=FALSE)