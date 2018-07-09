#Data pull for visiting scholar
library(RCurl)
library(dplyr)
library(readr)
library(readxl)
library(tidyr)
library(lubridate)
source('./Code/scoring_code.R')
eventindex <- function(ren = redcap_event_name){
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

#Load the list of mrns we are interested in
mrn_list <- read_csv("./Data Pulls/Lazarides_030718/data/mrn.csv") %>% 
  distinct()

#Create the function that pulls and cleans data from the database
data_pull <- function(dbn,api_token){
  
dbn_api <- postForm(uri='https://redcap.duke.edu/redcap/api/',token=api_token,content='report',format='csv',report_id='ALL',rawOrLabel='raw',rawOrLabelHeaders='raw',exportCheckboxLabel='false',returnFormat='csv')
dbn <- read.csv(textConnection(dbn_api),stringsAsFactors = FALSE)

#Put mrn, sx_side,sx_side_v2,surgery_date on each record
work <- select(dbn,studyid,mrn,sx_side,sx_side_v2,surgery_date) %>% distinct(studyid,.keep_all=TRUE)
dbn <- merge(dbn,work,by='studyid',suffixes=c('.DROP','')) %>% 
  select(-contains('.DROP'))
rm(work)

return(dbn)
}

#Run the function for tar and tar_licensed databases
tar<-data_pull(tar,'APITOKEN')
tar_lic<-data_pull(tar_lic,'APITOKEN') %>% select(colnames(tar)) #This removes the columns that are specific to tar_lic so we can vertically append
star_qi <- data_pull(star_qi,'APITOKEN')

#Vertically append the two datasets and remove duplicate rows
tar <- bind_rows(tar,tar_lic) %>%
  distinct(studyid,redcap_event_name,.keep_all=TRUE) 
rm(tar_lic)

#Create the visit date on star_qi
star_qi <- mutate(star_qi,visit_date = case_when(
  !is.na(vas_date) ~ vas_date,
  !is.na(sf_36_date) ~ sf_36_date,
  !is.na(aofas_date) ~ aofas_date,
  !is.na(bp_date) ~ bp_date
))

#Put the most recent visit date on each row
tar <- mutate(group_by(tar,studyid),most_recent_fu = max(visit_date))
star_qi <- mutate(group_by(star_qi,studyid),most_recent_fu = max(visit_date))

#Merge with the mrn list and filter out missing and records after 11-14-17
tar <- mutate(tar,mrn=gsub(' ','',toupper(mrn))) %>%
  merge(mrn_list,by='mrn',all.y=TRUE) %>% 
  filter(!is.na(studyid),ymd(visit_date)<"2017-11-14")
star_qi <- mutate(star_qi,mrn=gsub(' ','',toupper(mrn))) %>%
  merge(mrn_list,by='mrn',all.y=TRUE) %>% 
  filter(!is.na(studyid),ymd(visit_date)<"2017-11-14")

#Do the scoring and translate sx_side_v2, also create index for ordering
tar_scored <- fn_tar_scoring(tar)
tar <- inner_join(tar,tar_scored, by=c('studyid','redcap_event_name'),suffix = c('.DROP','')) %>%
  select(-contains('.DROP')) %>%
  mutate(event = eventindex(redcap_event_name))

#Pull the list of missing mrns after the tar_lic and tar pulls have been added together
missing_mrn <- right_join(select(tar,studyid,mrn),mrn_list,by='mrn',all.y=TRUE) %>% 
  right_join(select(star_qi,mrn),mrn_list,by='mrn',all.y = TRUE) %>%
  filter(is.na(studyid)) %>%
  distinct(mrn)

#Prepare the files for output
tar <- arrange(tar,studyid,as.numeric(event)) %>%
  select(-event)
star_qi <- arrange(star_qi,studyid,visit_date)

#Output
write.csv(star_qi,file="./Data Pulls/Lazarides_030718/output/030718_starqi_lazarides.csv",na='',row.names=FALSE)
write.csv(missing_mrn,file="./Data Pulls/Lazarides_030718/output/030718_missing_mrn_lazarides.csv",na='',row.names=FALSE)
write.csv(tar,file="./Data Pulls/Lazarides_030718/output/030718_tar_lazarides.csv",na='',row.names=FALSE)