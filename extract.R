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
##### STEP 1: Connect with SQL database -----
mimic_icu <- dbConnect(RPostgres::Postgres(),
                       host     = "localhost",
                       dbname   = "mimiciv3",
                       user     = "postgres",
                       password = "3027291",
                       bigint   = "integer",
                       port="5432",
                       options  = "-c search_path=mimiciv_icu")

mimic_hosp <- dbConnect(RPostgres::Postgres(),
                        host     = "localhost",
                        dbname   = "mimiciv3",
                        user     = "postgres",
                        password = "3027291",
                        bigint   = "integer",
                        port="5432",
                        options  = "-c search_path=mimiciv_hosp")

mimic_derived <- dbConnect(RPostgres::Postgres(),
                           host     = "localhost",
                           dbname   = "mimiciv3",
                           user     = "postgres",
                           password = "3027291",
                           bigint   = "integer",
                           port="5432",
                           options  = "-c search_path=public")
########################################################################################################################
##### STEP 2: Subject selection -----
##筛选d_icd_diagnoses表中包含“covid”的记录
df_covid_related_icd <- tbl(mimic_hosp, "d_icd_diagnoses") %>% 
  filter(str_detect(str_to_lower(long_title), "covid")) %>%
  collect() 
##查看筛选到的covid相关的icd编码
vt_covid_related_icd <- df_covid_related_icd %>% pull(icd_code) 
##导入diagnoses_icd表中包含vt_covid_related_icd的所有记录
df_covid_dignoses <- tbl(mimic_hosp, "diagnoses_icd") %>% 
  filter(icd_code %in% vt_covid_related_icd) %>% collect()
colnames(df_covid_dignoses)
##要查看df_covid_dignoses表以 icd_code 作为分组，并统计各组的记录数
df_covid_dignoses %>% filter(duplicated(hadm_id))
df_covid_dignoses %>%
  group_by(icd_code) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  left_join(df_covid_related_icd %>% select(icd_code, long_title), by = "icd_code")
##将所有icd_code == "U071"的记录导出
df_covid_raw <- df_covid_dignoses %>%
  mutate(icd_code = str_trim(icd_code)) %>%  # 去除前后空格
  filter(icd_code == "U071")
##查看一下总共有多少诊断为covid-19的患者
df_covid_raw %>%
  distinct(subject_id)
##把df_covid_raw 中的hadm_id提取出来作为icustay_detail表合并用
df_covid_raw %>% filter(duplicated(hadm_id))
df_covid_raw %>% filter(duplicated(subject_id))
df_icustay_detail <-  tbl(mimic_derived, "icustay_detail") %>% 
  filter(hadm_id %in% df_covid_raw$hadm_id) %>% collect()
##查看一下总共有多少诊断为covid-19并入住icu的患者
df_icustay_detail %>%
  distinct(subject_id)
##提取每个hadm_id首次入icu的记录
df_covid <- df_icustay_detail %>%
  filter(first_icu_stay == TRUE)
df_covid %>% filter(duplicated(hadm_id))
df_covid %>% filter(duplicated(subject_id))
##提取每个subject_id首次入院的记录
glimpse(df_covid)
df_covid <- df_covid %>%  group_by(subject_id) %>% 
  filter(icu_intime == min(icu_intime)) %>% ungroup()
write_csv(df_covid, "E:/R2023/R_SQL/data/df_covid.csv")
