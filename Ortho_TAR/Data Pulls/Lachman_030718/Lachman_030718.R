#Data pull for visiting scholar
library(RCurl)
library(dplyr)
library(readr)
library(readxl)
library(tidyr)
library(lubridate)
source('./Code/scoring_code.R')

#Function for numbering the events
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

#Create the function that pulls and cleans data from the database
data_pull <- function(dbn,api_token){
  
dbn_api <- postForm(uri='https://redcap.duke.edu/redcap/api/',token=api_token,content='report',format='csv',report_id='ALL',rawOrLabel='raw',rawOrLabelHeaders='raw',exportCheckboxLabel='false',returnFormat='csv')
dbn <- read.csv(textConnection(dbn_api),stringsAsFactors = FALSE)

return(dbn)
}

#Run the function for tar and tar_licensed databases
tar<-data_pull(tar,'APITOKEN')
tar_lic<-data_pull(tar_lic,'APITOKEN') %>% select(colnames(tar)) #This removes the columns that are specific to tar_lic so we can vertically append

#Vertically append the two datasets and remove duplicate rows
tar <- bind_rows(tar,tar_lic) %>%
  distinct(studyid,redcap_event_name,.keep_all=TRUE) 
rm(tar_lic)

#Select only records from arm 1, only the variables we care about, put event index in, and remove events that aren't pre-op or 1 year
tar <- filter(tar,grepl('arm_1',redcap_event_name)) %>%
        mutate(event = eventindex(redcap_event_name)) %>%
        filter(event==1 | event==4)

#Do scoring, remove NA scores,
tar_scored <- fn_tar_scoring(tar)
tar <- inner_join(tar,tar_scored, by=c('studyid','redcap_event_name'),suffix = c('.DROP','')) %>%
  select(-contains('.DROP')) %>%
  filter(!is.na(sf36_pcs) & !is.na(sf36_mcs)) %>%
  group_by(studyid) %>%
  mutate(event_total = sum(as.numeric(event))) %>%
  filter(event_total == 5) #This means the record has both pre and 1 year scores

#Let's pick just the variables we want
tar <- select(tar,studyid,redcap_event_name,mrn,sx_side_v2,dob,age_at_sugery,gender,weight,height,bmi,sf36_body_pain:sf36_rep_health,sf36_total,sf36_phy_hea_dim,sf36_men_hea_dim,sf36_mcs,sf36_pcs)

#Now let's convert to wide data
tar_1yr <- filter(tar,redcap_event_name == '1_year_arm_1') %>%
  select(studyid,sf36_body_pain:sf36_pcs)
tar <- filter(tar,redcap_event_name == 'pre_op_arm_1')
tar <- inner_join(tar,tar_1yr,by='studyid',suffix = c('_pre_op','_1_yr'))

#Let's remove patients that are missing data, have erroneous data, and remove patients that had more than one procedure
tar <- filter(tar,!is.na(age_at_sugery),!is.na(bmi),!is.na(gender),age_at_sugery>18,bmi<60) %>% distinct(mrn,.keep_all=TRUE)

#This is the sample of 500 random patients we will export
out <- sample_n(ungroup(tar),500) %>% select(-redcap_event_name)

#Output
write.csv(out,file="./Data Pulls/Lachman_030718/output/030718_lachman.csv",na='',row.names=FALSE)