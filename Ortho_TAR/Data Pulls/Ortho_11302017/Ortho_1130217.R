#Data pull for visiting scholar
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

#Load the list of mrns we are interested in and the tar data sets
mrn <- read_csv(file='P://Outside DUSON RMT Projects//Department_ORTHO//Ortho TAR - Stewart Folder//Data Pulls//Ortho_11302017//Raw data//88988 - MRNs Identified.csv')
tar <- read_csv(file='P://Outside DUSON RMT Projects//Department_ORTHO//Ortho TAR - Stewart Folder//Data Pulls//Ortho_11302017//Raw data//ORTHOTAR_DATA_2017-12-06.csv')
tar_lic <- read_csv(file='P://Outside DUSON RMT Projects//Department_ORTHO//Ortho TAR - Stewart Folder//Data Pulls//Ortho_11302017//Raw data//LicensedORTHOTAR_DATA_2017-12-06.csv')

#These are the variables we are interested in
keep<-c('mrn','sx_side_v2','surgery_date','studyid','redcap_event_name','last_name','first_name','dob','gender','race___0','race___1','race___2','race___3','race___4','race___5','race___999','race_other','bmi','asa','smoking_history','quit_date','packs_year','diabetic','diabetes_type','heart_disease','heart_problems___1','heart_problems___2','heart_problems___3','heart_problems___4','heart_problems___5','heart_problems___6','heart_problems_v2','age_at_sugery','attending','primary_diagnosis','primary_diagnosis_v2','prosthesis','prior_sx___1','prior_sx___2','prior_sx___3','prior_sx___4','prior_sx___5','patient_notes','tha','tha_right_date','tha_left_date','tka','tka_right_date','tka_left_date','intraop_comp','intraop_comp_type___1','intraop_comp_type___2','intraop_comp_type___3','intraop_comp_type___4','intra_op_mal_frac_type','intra_op_other','complication','complication_specify___0','complication_specify___1','complication_specify___2','complication_specify___3','complication_specify___4','complication_specify___5','complication_specify___6','complication_specify___7','complication_specify___12','complication_specify___8','complication_specify___9','complication_specify___13','complication_specify___14','complication_specify___10','complication_specify___11','infection_type___1','infection_type___2','skin_infection_date','deep_infection_date','amputation_date','ankle_pain_date','swelling_date','implant_failure_date','wound_break_down_date','wound_break_down_type___1','wound_break_down_type___2','wound_break_down_type___3','wound_vac_date','free_flap_date','split_thickness_date','medial_lateral_type___1','medial_lateral_type___2','medial_fracture_date','lateral_fracture_date','talar_subsidence_date','impingement_date','debridement','neurovascular_injury_date','tendon_injury_date','dvt_date','pe_date','death_date','other_date','component_removal_type___1','component_removal_type___2','complication_other','complication_date','visit_date','vas','smfa_fun_count','smfa_fun_avg','smfa_fun_atotal','smfa_fun_index','smfa_bother_count','smfa_bother_avg','smfa_bother_atotal','smfa_bother_index','sf36_body_pain','sf36_general_health','sf36_mental_health','sf36_physical_function','sf36_role_emotional','sf36_role_physical','sf36_social_fun','sf36_vitality','sf36_rep_health','aofas_hfoot','aofas_hfoot_function','aofas_hfoot_pain','aofas_hfoot_alignment','faos_pain','faos_symptoms','faos_adl','faos_spandrecpain','faos_qol','sf36_total','sf36_phy_hea_dim','sf36_men_hea_dim')

#Puts an MRN and on each record in TAR
tar_mrn <- select(tar,studyid,mrn,sx_side_v2,surgery_date) %>% distinct(studyid,.keep_all=TRUE)
tar <- merge(tar,tar_mrn,by='studyid') %>% select(-mrn.x,-sx_side_v2.x,-surgery_date.x) %>% rename(mrn=mrn.y,sx_side_v2=sx_side_v2.y,surgery_date=surgery_date.y)

#Puts an MRN on each record in TAR lic
lic_mrn <- select(tar_lic,studyid,mrn,sx_side_v2,surgery_date) %>% distinct(studyid,.keep_all=TRUE)
tar_lic <- merge(tar_lic,lic_mrn,by='studyid') %>% select(-mrn.x,-sx_side_v2.x,-surgery_date.x) %>% rename(mrn=mrn.y,sx_side_v2=sx_side_v2.y,surgery_date=surgery_date.y)

#Selecting only the mrn we want
tar <- merge(mrn,tar,by=c('mrn','sx_side_v2'),all.x=TRUE) 
tar_lic <- merge(mrn,tar_lic,by=c('mrn','sx_side_v2'),all.x=TRUE)

#Filter off the records that aren't in the database
mrn_TAR_MISS <- select(tar,mrn,studyid,sx_side_v2) %>% filter(is.na(studyid)) %>% distinct(mrn,sx_side_v2,.keep_all=TRUE)
mrn_LIC_MISS <- select(tar_lic,mrn,studyid,sx_side_v2) %>% filter(is.na(studyid)) %>% distinct(mrn,sx_side_v2,.keep_all=TRUE)
mrn_miss <- inner_join(mrn_TAR_MISS,mrn_LIC_MISS,by=c('mrn','sx_side_v2')) %>% select(-contains('studyid'))
#Translating
mrn_miss <- mutate(mrn_miss,sx_side_v2=ifelse(sx_side_v2=='1','R','L'))

tar <- filter(tar,!is.na(studyid))
tar_lic <- filter(tar_lic,!is.na(studyid))

###TAR
#Determine which visit is the most recent after removing later visits that are null (using filter)
TAR_RECENT <- filter(tar,grepl('pre',redcap_event_name) | !is.na(vas)) %>% select(mrn,sx_side_v2,surgery_date,redcap_event_name) %>% mutate(index=eventindex(redcap_event_name)) %>%
                group_by(mrn,sx_side_v2,surgery_date) %>% summarise(eventmax = max(index))

#Merge the most recent visit back into the data set, also add the index column for later filtering
tar <- merge(tar,TAR_RECENT,by=c('mrn','sx_side_v2','surgery_date')) %>% mutate(index=eventindex(redcap_event_name))

#Filter off records that aren't pre-op, 1-year, 2-year, or most recent
tar <- filter(tar,index=='1' | index==eventmax)

#Perform the scoring, drop the old scores from REDCap
tar_scored <- fn_tar_scoring(tar)
tar <- inner_join(tar,tar_scored, by=c('studyid','redcap_event_name'),suffix = c('.DROP','')) %>% arrange(mrn,sx_side_v2,surgery_date,index) %>%
          select(keep) %>%
          mutate(sx_side_v2=ifelse(sx_side_v2=='1','R','L'))

###LICENSE
#Determine which visit is the most recent
LIC_RECENT <- filter(tar_lic,!grepl('pre',redcap_event_name) | !is.na(vas)) %>% select(mrn,sx_side_v2,surgery_date,redcap_event_name) %>% mutate(index=eventindex(redcap_event_name)) %>%
  group_by(mrn,sx_side_v2,surgery_date) %>% summarise(eventmax = max(index))

#Merge the most recent visit back into the data set, also add the index column for later filtering
tar_lic <- merge(tar_lic,LIC_RECENT,by=c('mrn','sx_side_v2','surgery_date')) %>% mutate(index=eventindex(redcap_event_name))

#Filter off records that aren't pre-op, 1-year, 2-year, or most recent
tar_lic <- filter(tar_lic,index=='1' | index==eventmax)

#Perform the scoring, drop the old scores from REDCap
tar_lic_scored <- fn_tar_scoring(tar_lic)
tar_lic <- inner_join(tar_lic,tar_lic_scored, by=c('studyid','redcap_event_name'),suffix = c('.DROP','')) %>% arrange(mrn,sx_side_v2,surgery_date,index) %>%
  select(keep) %>%
  mutate(sx_side_v2=ifelse(sx_side_v2=='1','R','L'))

#OLD SELECT:
#  select(-contains('.DROP'),-contains('imp_'),-matches('fadi_\\d+'),-matches('smfa_\\d+'),-matches('sf36_\\d+'),-matches('._complete'),-matches('aofas_\\d+'),-matches('faos_[[:lower:]]+\\d'),-index,-eventmax) %>%
#  select(-contains('.DROP'),-contains('imp_'),-matches('fadi_\\d+'),-matches('smfa_\\d+'),-matches('sf36_\\d+'),-matches('._complete'),-matches('aofas_\\d+'),-matches('faos_[[:lower:]]+\\d'),-index,-eventmax) %>%

write.csv(tar,file="P://Outside DUSON RMT Projects//Department_ORTHO//Ortho TAR - Stewart Folder//Data Pulls//Ortho_11302017//Output//Ortho_11302017_TAR_Output.csv",na='',row.names=FALSE)
write.csv(tar_lic,file="P://Outside DUSON RMT Projects//Department_ORTHO//Ortho TAR - Stewart Folder//Data Pulls//Ortho_11302017//Output//Ortho_11302017_TAR_License_Output.csv",na='',row.names=FALSE)
write.csv(mrn_miss,file="P://Outside DUSON RMT Projects//Department_ORTHO//Ortho TAR - Stewart Folder//Data Pulls//Ortho_11302017//Output//Ortho_11302017_Missing_MRN.csv",na='',row.names=FALSE)