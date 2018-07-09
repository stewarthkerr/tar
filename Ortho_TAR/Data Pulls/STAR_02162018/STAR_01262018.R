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

return(dbn)
}

#Run the function for tar and tar_licensed databases
tar <- data_pull(tar,'APITOKEN') %>%
  filter(!is.na(prosthesis)) %>%
  select(mrn,prosthesis,surgery_date)
tar_lic <- data_pull(tar_lic,'APITOKEN') %>%
  filter(!is.na(prosthesis)) %>%
  select(mrn,prosthesis,surgery_date)

#Choose all the STAR patients, remove duplicates
tar_all <- bind_rows(tar,tar_lic) %>% 
  filter(prosthesis=='2') %>% 
  distinct(mrn,.keep_all=TRUE)

#Choose only the ones in 2009 or later
after2009 <- filter(tar_all,surgery_date>'2008-12-31')
  
