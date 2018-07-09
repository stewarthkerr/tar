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
keep<-c('mrn','sx_side_v2','tka','attending','prosthesis','primary_diagnosis','surgery_date','age_at_surgery','redcap_event_name','last_name','first_name','dob','gender','race___0','race___1','race___2','race___3','race___4','race___5','race___999','race_other','bmi','asa','smoking_history','quit_date','packs_year','diabetic','diabetes_type','heart_disease','heart_problems___1','heart_problems___2','heart_problems___3','heart_problems___4','heart_problems___5','heart_problems___6','heart_problems_v2','age_at_sugery','primary_diagnosis_v2','prior_sx___1','prior_sx___2','prior_sx___3','prior_sx___4','prior_sx___5','patient_notes','tha','tha_right_date','tha_left_date','tka_right_date','tka_left_date','intraop_comp','intraop_comp_type___1','intraop_comp_type___2','intraop_comp_type___3','intraop_comp_type___4','intra_op_mal_frac_type','intra_op_other','complication','complication_specify___0','complication_specify___1','complication_specify___2','complication_specify___3','complication_specify___4','complication_specify___5','complication_specify___6','complication_specify___7','complication_specify___12','complication_specify___8','complication_specify___9','complication_specify___13','complication_specify___14','complication_specify___10','complication_specify___11','infection_type___1','infection_type___2','skin_infection_date','deep_infection_date','amputation_date','ankle_pain_date','swelling_date','implant_failure_date','wound_break_down_date','wound_break_down_type___1','wound_break_down_type___2','wound_break_down_type___3','wound_vac_date','free_flap_date','split_thickness_date','medial_lateral_type___1','medial_lateral_type___2','medial_fracture_date','lateral_fracture_date','talar_subsidence_date','impingement_date','debridement','neurovascular_injury_date','tendon_injury_date','dvt_date','pe_date','death_date','other_date','component_removal_type___1','component_removal_type___2','complication_other','complication_date','visit_date','vas','smfa_fun_count','smfa_fun_avg','smfa_fun_atotal','smfa_fun_index','smfa_bother_count','smfa_bother_avg','smfa_bother_atotal','smfa_bother_index','sf36_body_pain','sf36_general_health','sf36_mental_health','sf36_physical_function','sf36_role_emotional','sf36_role_physical','sf36_social_fun','sf36_vitality','sf36_rep_health','aofas_hfoot','aofas_hfoot_function','aofas_hfoot_pain','aofas_hfoot_alignment','faos_pain','faos_symptoms','faos_adl','faos_spandrecpain','faos_qol','sf36_total','sf36_phy_hea_dim','sf36_men_hea_dim')
#Load the list of mrns we are interested in and the tar data sets
base <- read_csv(file='P://Outside DUSON RMT Projects//Department_ORTHO//Ortho TAR - Stewart Folder//Data Pulls//Ortho_11302017//Raw data//Round2.csv')
tar <- read_csv(file='P://Outside DUSON RMT Projects//Department_ORTHO//Ortho TAR - Stewart Folder//Data Pulls//Ortho_11302017//Raw data//ORTHOTAR_DATA_2017-12-06.csv')
#Puts an MRN,sx_side_v2,surgery_date,tka on each record in TAR, and then only keep patients with no tka
tar_mrn <- select(tar,studyid,mrn,sx_side_v2,surgery_date,tka,dob,attending,prosthesis,primary_diagnosis) %>% distinct(studyid,.keep_all=TRUE)
tar <- merge(tar,tar_mrn,by='studyid',suffixes=c('.DROP','')) %>% select(-contains('.DROP'))
work_tar <- filter(tar,mrn %in% c('M33110','K32471','W36087','EP5747','K32471','J90623','WG6389','FK3496','FS8935','DJ0764','X71343','EA7705','FS3037','RJ6659'))
work_tar<-mutate(work_tar,age_at_surgery=((year(mdy(surgery_date)))-(year(mdy(dob))))) #calculate age at time of surgery
#Do the scoring
work_tar_scored <- fn_tar_scoring(work_tar)
work_tar <- inner_join(work_tar,work_tar_scored, by=c('studyid','redcap_event_name'),suffix = c('.DROP','')) %>%
  select(keep) %>%
  mutate(sx_side_v2=ifelse(sx_side_v2=='1','R','L'))
# #Determine which records don't have event max < 1 year and remove
# work_tar_recent <- select(work_tar,mrn,sx_side_v2,surgery_date,redcap_event_name) %>% mutate(index=eventindex(redcap_event_name)) %>%
#   group_by(mrn,sx_side_v2,surgery_date) %>% summarise(eventmax = max(index))
# work_tar <- merge(work_tar,work_tar_recent,by=c('mrn','sx_side_v2','surgery_date')) %>% filter(eventmax>3)
# #Determine which records don't have preop scoring and remove
# tar_remove <- filter(work_tar,grepl('pre',redcap_event_name)&(is.na(vas)|is.na(smfa_fun_index)|is.na(smfa_bother_index)|is.na(sf36_total)|is.na(sf36_phy_hea_dim)|is.na(sf36_men_hea_dim)|is.na(aofas_hfoot))) %>%
#   select(mrn,sx_side_v2,surgery_date) %>%
#   mutate(remove='1')
# work_tar <- left_join(work_tar,tar_remove,by=c('mrn','sx_side_v2','surgery_date')) %>%
 work_tar <- mutate(work_tar,index=eventindex(redcap_event_name)) %>%
#   filter(is.na(remove)& (index==1|index>3)) %>%
  arrange(mrn,sx_side_v2,surgery_date,index) %>%
  select(-contains('faos'))
assign(an,work_tar)
write.csv(get(an),file=paste("P://Outside DUSON RMT Projects//Department_ORTHO//Ortho TAR - Stewart Folder//Data Pulls//Ortho_11302017//Output//OrthoTAR Data//All data//",an,'.csv',sep=''),na='',row.names=FALSE)
#Remove eventmax and index for next check
#work_tar <- select(work_tar,-eventmax,-index)
#Figure out which is the most recent event (that has all the scores we care about)
work_tar_recent <- filter(work_tar,(grepl('pre',redcap_event_name) | !(is.na(vas)|is.na(smfa_fun_index)|is.na(smfa_bother_index)|is.na(sf36_total)|is.na(sf36_phy_hea_dim)|is.na(sf36_men_hea_dim)|is.na(aofas_hfoot)))) %>%
  select(mrn,sx_side_v2,surgery_date,redcap_event_name) %>% mutate(index=eventindex(redcap_event_name)) %>%
  group_by(mrn,sx_side_v2,surgery_date) %>% summarise(eventmax = max(index))
#Merge most recent back in
work_tar <- merge(work_tar,work_tar_recent,by=c('mrn','sx_side_v2','surgery_date')) %>%
  filter(eventmax > 3) %>%
  mutate(index=eventindex(redcap_event_name)) %>%
  filter(index=='1' | index==eventmax) %>%
  arrange(mrn,sx_side_v2,surgery_date,index)
assign(dn,work_tar)

write.csv(get(dn),file=paste("P://Outside DUSON RMT Projects//Department_ORTHO//Ortho TAR - Stewart Folder//Data Pulls//Ortho_11302017//Output//OrthoTAR Data//Most recent post-op//",dn,'.csv',sep=''),na='',row.names=FALSE)