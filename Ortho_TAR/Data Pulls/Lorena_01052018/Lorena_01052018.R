#Data pull for visiting scholar
library(RCurl)
library(dplyr)
library(readr)
library(readxl)
library(tidyr)
library(lubridate)
source('./Code/scoring_code.R')
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

#These are the variables we are interested in in the right order
keep<-c('studyid','mrn','last_name','first_name','sx_side','sx_side_v2','surgery_date','dob','age_at_sugery','gender','race___0','race___1','race___2','race___3','race___4','race___5','race___999','race_other','height','weight','bmi','asa','smoking_history','quit_date','packs_year','diabetic','diabetes_type','heart_disease','heart_problems___1','heart_problems___2','heart_problems___3','heart_problems___4','heart_problems___5','heart_problems___6','heart_problems_v2','primary_diagnosis','primary_diagnosis_v2','prosthesis','prior_sx___1','prior_sx___2','prior_sx___3','prior_sx___4','prior_sx___5','patient_notes','implant_failure_date','complication_other')

#Load the list of mrns we are interested in
mrn_list <- read_excel("./Data Pulls/Lorena_01052018/data/Consented TAR Patients -10-26-17.xlsx",skip = 1) %>% 
  distinct() %>%
  mutate(mrn=gsub('\\.','',toupper(`MRN:`))) %>%
  select(-`MRN:`) %>%
  filter(mrn!='')

####LABEL####
#Pull the data from REDCap
tar_api <- postForm(uri='https://redcap.duke.edu/redcap/api/',token='APITOKEN',content='report',format='csv',report_id='9655',rawOrLabel='label',rawOrLabelHeaders='raw',exportCheckboxLabel='true',returnFormat='csv')
tar <- read.csv(textConnection(tar_api),stringsAsFactors = FALSE)

#Put implant failure date on each record - this can give revision information, then remove non-demo records
rev_date <- select(tar,studyid,implant_failure_date) %>% distinct(studyid,.keep_all=TRUE)
tar <- merge(tar,rev_date,by='studyid',suffixes=c('.DROP','')) %>% 
  select(-contains('.DROP')) %>%
  filter(redcap_event_name=='Pre Op (Arm 1: TAR)') %>%
  select(keep)

#Merge with the mrn list and filter out missing
tar <- mutate(tar,mrn=gsub(' ','',toupper(mrn))) %>%
  merge(mrn_list,by='mrn',all.y=TRUE) 
missing_mrn <- filter(tar,is.na(studyid)) %>% select(mrn)
tar <- filter(tar,!is.na(studyid))

####RAW####
#Pull the data from REDCap
tar_api_raw <- postForm(uri='https://redcap.duke.edu/redcap/api/',token='APITOKEN',content='report',format='csv',report_id='9655',rawOrLabel='raw',rawOrLabelHeaders='raw',exportCheckboxLabel='false',returnFormat='csv')
tar_raw <- read.csv(textConnection(tar_api_raw),stringsAsFactors = FALSE)

#Put implant failure date on each record - this can give revision information, then remove non-demo records
tar_raw <- merge(tar_raw,rev_date,by='studyid',suffixes=c('.DROP','')) %>% 
  select(-contains('.DROP')) %>%
  filter(redcap_event_name=='pre_op_arm_1') %>%
  select(keep)

#Merge with the mrn list and filter out missing
tar_raw <- mutate(tar_raw,mrn=gsub(' ','',toupper(mrn))) %>%
  merge(mrn_list,by='mrn',all.y=TRUE) %>%
  filter(!is.na(studyid))

#output
write.csv(missing_mrn,file="./Data Pulls/Lorena_01052018/output/01052018_tar_missing_mrn.csv",na='',row.names=FALSE)
write.csv(tar,file="./Data Pulls/Lorena_01052018/output/01052018_tar_demo_label.csv",na='',row.names=FALSE)
write.csv(tar_raw,file="./Data Pulls/Lorena_01052018/output/01052018_tar_demo_raw.csv",na='',row.names=FALSE)