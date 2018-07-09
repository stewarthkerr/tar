#Data merge for Dr. Nunley on 11/2`/2017
library(dplyr)
library(readr)
library(readxl)
library(tidyr)
library(lubridate)

#Load the list of mrns we are interested in and the tar data set
prosthesis <- read_csv(file='P://Outside DUSON RMT Projects//Department_ORTHO//Ortho TAR - Stewart Folder//Data Pulls//Nunley_11212017//Raw Data//prosthesis.csv') %>% select(-redcap_event_name)
tar <- read_csv(file='P://Outside DUSON RMT Projects//Department_ORTHO//Ortho TAR - Stewart Folder//Data Pulls//Nunley_11212017//Raw Data//tar_random_output_20171018.csv')

work <- merge(prosthesis,tar,on='mrn')

needprosthesis <- filter(work,is.na(prosthesis)) %>% select(studyid,mrn,prosthesis)

write.csv(needprosthesis,file="P://Outside DUSON RMT Projects//Department_ORTHO//Ortho TAR - Stewart Folder//Data Pulls//Nunley_11212017//Output//11212017_need_prosthesis.csv",na='',row.names=FALSE)

salto <- filter(work,prosthesis==1) %>% mutate(prosthesis='Salto-Talaris')
star <- filter(work,prosthesis==2) %>% mutate(prosthesis='STAR')

write.csv(salto,file="P://Outside DUSON RMT Projects//Department_ORTHO//Ortho TAR - Stewart Folder//Data Pulls//Nunley_11212017//Output//11212017_SALTO.csv",na='',row.names=FALSE)
write.csv(star,file="P://Outside DUSON RMT Projects//Department_ORTHO//Ortho TAR - Stewart Folder//Data Pulls//Nunley_11212017//Output//11212017_STAR.csv",na='',row.names=FALSE)
