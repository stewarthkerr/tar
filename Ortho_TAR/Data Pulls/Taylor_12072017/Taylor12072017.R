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

#Load the list of mrns we are interested in and the tar data sets
mrn <- read_csv(file='P://Outside DUSON RMT Projects//Department_ORTHO//Ortho TAR - Stewart Folder//Data Pulls//Taylor_12072017//Raw data//mrn_pull.csv')
tar <- read_csv(file='P://Outside DUSON RMT Projects//Department_ORTHO//Ortho TAR - Stewart Folder//Data Pulls//Taylor_12072017//Raw data//ORTHOTAR_DATA_2017-12-08.csv')

#Only care about arm 1
tar <- filter(tar,grepl('arm_1',redcap_event_name))

#Put an mrn on each row
tar_mrn <- select(tar,studyid,mrn,sx_side_v2) %>% distinct(studyid,.keep_all=TRUE)
tar <- merge(tar,tar_mrn,by='studyid',suffixes=c('.DROP','')) %>% select(-contains('.DROP'))

#Merge with the mrn list and filter out missing
tar <- merge(tar,mrn,by='mrn',all.y=TRUE) 
missing_mrn <- filter(tar,is.na(studyid)) %>% select(mrn)
tar <- filter(tar,!is.na(studyid))

#Perform scoring
tar_scored <- fn_tar_scoring(tar)
tar <- inner_join(tar,tar_scored, by=c('studyid','redcap_event_name'),suffix = c('.DROP','')) %>%
  select(-contains('.DROP'),-contains('imp_'))#,-matches('fadi_\\d+'),-matches('smfa_\\d+'),-matches('sf36_\\d+'),-matches('._complete'),-matches('aofas_\\d+'),-matches('faos_[[:lower:]]+\\d'))
  
out <- mutate(tar,index=eventindex(redcap_event_name)) %>%
  arrange(mrn,sx_side_v2,index) %>%
  select(mrn,sx_side_v2,studyid:sf36_men_hea_dim,-index) %>%
  mutate(sx_side_v2=ifelse(sx_side_v2=='1','R','L'))

write.csv(out,file='P://Outside DUSON RMT Projects//Department_ORTHO//Ortho TAR - Stewart Folder//Data Pulls//Taylor_12072017//Output//ORTHOTAR_Taylor_12072017.csv',na='',row.names=FALSE)
write.csv(missing_mrn,file='P://Outside DUSON RMT Projects//Department_ORTHO//Ortho TAR - Stewart Folder//Data Pulls//Taylor_12072017//Output//ORTHOTAR_Missing_MRN_12072017.csv',na='',row.names=FALSE)


