##### Packages -----
rm(list = ls());gc()
library(RPostgreSQL)
library(tidyverse)
library(stringr)
library(lubridate)
library(dplyr)
library(gtsummary)
library(ivs)

########################################################################################################################
##### STEP 1: Connect with SQL database -----
########################################################################################################################
##### STEP 2: Subject selection -----
df_covid_related_icd <- tbl(mimic_hosp, "d_icd_diagnoses") %>% 
  filter(str_detect(str_to_lower(long_title), "")) %>%
  collect() 
vt_covid_related_icd <- df_covid_related_icd %>% pull(icd_code) 
df_covid_dignoses <- tbl(mimic_hosp, "diagnoses_icd") %>% 
  filter(icd_code %in% vt_covid_related_icd) %>% collect()
colnames(df_covid_dignoses)
df_covid_dignoses %>% filter(duplicated(hadm_id))
df_covid_dignoses %>%
  group_by(icd_code) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  left_join(df_covid_related_icd %>% select(icd_code, long_title), by = "icd_code")
df_covid_raw <- df_covid_dignoses %>%
  mutate(icd_code = str_trim(icd_code)) %>%  
  filter(icd_code == "")
df_covid_raw %>%
  distinct(subject_id)
df_covid_raw %>% filter(duplicated(hadm_id))
df_covid_raw %>% filter(duplicated(subject_id))
df_icustay_detail <-  tbl(mimic_derived, "icustay_detail") %>% 
  filter(hadm_id %in% df_covid_raw$hadm_id) %>% collect()
df_icustay_detail %>%
  distinct(subject_id)

df_covid <- df_icustay_detail %>%
  filter(first_icu_stay == TRUE)
df_covid %>% filter(duplicated(hadm_id))
df_covid %>% filter(duplicated(subject_id))
glimpse(df_covid)
df_covid <- df_covid %>%  group_by(subject_id) %>% 
  filter(icu_intime == min(icu_intime)) %>% ungroup()
write_csv(df_covid, "E:/R2023/R_SQL/data/df_covid.csv")
