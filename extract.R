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
df_variable_related_icd <- tbl(mimic_hosp, "d_icd_diagnoses") %>% 
  filter(str_detect(str_to_lower(long_title), "")) %>%
  collect() 
vt_variable_related_icd <- df_variable_related_icd %>% pull(icd_code) 
df_variable_dignoses <- tbl(mimic_hosp, "diagnoses_icd") %>% 
  filter(icd_code %in% vt_variable_related_icd) %>% collect()
colnames(df_variable_dignoses)
df_variable_dignoses %>% filter(duplicated(hadm_id))
df_variable_dignoses %>%
  group_by(icd_code) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  left_join(df_variable_related_icd %>% select(icd_code, long_title), by = "icd_code")
df_variable_raw <- df_variable_dignoses %>%
  mutate(icd_code = str_trim(icd_code)) %>%  
  filter(icd_code == "")
df_variable_raw %>%
  distinct(subject_id)
df_variable_raw %>% filter(duplicated(hadm_id))
df_variable_raw %>% filter(duplicated(subject_id))
df_icustay_detail <-  tbl(mimic_derived, "icustay_detail") %>% 
  filter(hadm_id %in% df_variable_raw$hadm_id) %>% collect()
df_icustay_detail %>%
  distinct(subject_id)

df_variable <- df_icustay_detail %>%
  filter(first_icu_stay == TRUE)
df_variable %>% filter(duplicated(hadm_id))
df_variable %>% filter(duplicated(subject_id))
glimpse(df_variable)
df_variable <- df_variable %>%  group_by(subject_id) %>% 
  filter(icu_intime == min(icu_intime)) %>% ungroup()
write_csv(df_variable, "E:/R2023/R_SQL/data/df_variable.csv")
