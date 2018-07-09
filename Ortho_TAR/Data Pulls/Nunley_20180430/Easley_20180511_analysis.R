library(dplyr)
library(readr)
library(readxl)
library(tidyr)
library(lubridate)
library(RCurl)

salto <- read.csv('./Data Pulls/Nunley_20180430/output/20180430_SALTO.csv')
star <- read.csv("./Data Pulls/Nunley_20180430/output/20180430_STAR.csv")

salto <- filter(salto,grepl('Year',most_recent_fu) & !grepl('1 Year',most_recent_fu)) %>%
  mutate(most_recent_fu = as.numeric(most_recent_fu),
         vas_improve = vas_mrf - vas_pre,
         aofas_improve = aofas_hfoot_mrf - aofas_hfoot_pre,
         sf36_improve = sf36_total_mrf - sf36_total_pre,
         fadi_improve = fadi_total_mrf - fadi_total_pre,
         smfa_improve = smfa_fun_atotal_mrf - smfa_fun_atotal_pre)

star <- filter(star,grepl('Year',most_recent_fu) & !grepl('1 Year',most_recent_fu)) %>%
  mutate(most_recent_fu = as.numeric(most_recent_fu),
         vas_improve = vas_mrf - vas_pre,
         aofas_improve = aofas_hfoot_mrf - aofas_hfoot_pre,
         sf36_improve = sf36_total_mrf - sf36_total_pre,
         fadi_improve = fadi_total_mrf - fadi_total_pre,
         smfa_improve = smfa_fun_atotal_mrf - smfa_fun_atotal_pre)

star_sum <- summarise(star,avg_preop_vas = mean(vas_pre, na.rm = TRUE), avg_mrf_vas = mean(vas_mrf, na.rm = TRUE),
                      avg_preop_aofas = mean(aofas_hfoot_pre, na.rm = TRUE), avg_mrf_aofas = mean(aofas_hfoot_mrf, na.rm = TRUE),
                      avg_preop_sf36 = mean(sf36_total_pre, na.rm = TRUE), avg_mrf_sf36 = mean(sf36_total_mrf, na.rm = TRUE),
                      avg_preop_fadi = mean(fadi_total_pre, na.rm = TRUE), avg_mrf_fadi = mean(fadi_total_mrf, na.rm = TRUE),
                      avg_preop_smfa = mean(smfa_fun_atotal_pre, na.rm = TRUE), avg_mrf_smfa = mean(smfa_fun_atotal_mrf, na.rm = TRUE),
                      avg_vas_improve = mean(vas_improve, na.rm = TRUE),
                      avg_aofas_improve = mean(aofas_improve, na.rm = TRUE),
                      avg_sf36_improve = mean(sf36_improve, na.rm = TRUE),
                      avg_fadi_improve = mean(fadi_improve, na.rm = TRUE),
                      avg_smfa_improve = mean(smfa_improve,na.rm = TRUE))

salto_sum <- summarise(salto,avg_preop_vas = mean(vas_pre, na.rm = TRUE), avg_mrf_vas = mean(vas_mrf, na.rm = TRUE),
                      avg_preop_aofas = mean(aofas_hfoot_pre, na.rm = TRUE), avg_mrf_aofas = mean(aofas_hfoot_mrf, na.rm = TRUE),
                      avg_preop_sf36 = mean(sf36_total_pre, na.rm = TRUE), avg_mrf_sf36 = mean(sf36_total_mrf, na.rm = TRUE),
                      avg_preop_fadi = mean(fadi_total_pre, na.rm = TRUE), avg_mrf_fadi = mean(fadi_total_mrf, na.rm = TRUE),
                      avg_preop_smfa = mean(smfa_fun_atotal_pre, na.rm = TRUE), avg_mrf_smfa = mean(smfa_fun_atotal_mrf, na.rm = TRUE),
                      avg_vas_improve = mean(vas_improve, na.rm = TRUE),
                      avg_aofas_improve = mean(aofas_improve, na.rm = TRUE),
                      avg_sf36_improve = mean(sf36_improve, na.rm = TRUE),
                      avg_fadi_improve = mean(fadi_improve, na.rm = TRUE),
                      avg_smfa_improve = mean(smfa_improve,na.rm = TRUE))
