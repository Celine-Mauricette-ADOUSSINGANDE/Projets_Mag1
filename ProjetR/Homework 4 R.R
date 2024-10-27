# Data wrangling: Working with IPUMS in R

## 1 The IPUMS Census Data Archive

## 2 R

#Import library

library(tidyverse)
library(ipumsr)

ddi_path <- "C:/Users/AMSE/Documents/R/R_in_class/Data/usa_1980_extract_bpl.xml"
ddi <- read_ipums_ddi(ddi_path)

data_path <- "C:/Users/AMSE/Documents/R/R_in_class/Data/usa_1980_extract_bpl.dat"
dat80 <- read_ipums_micro(ddi, data_file = data_path)
ipums_val_labels(dat80$BPL)

## 3 Tasks: Load an extract from the 1980 Census and conduct an Exploratory Data Analysis

### 3.1 The data extract

file.info(paste0("C:/Users/AMSE/Documents/R/R_in_class/Data/usa_1980_extract_bpl.xml"))$size / 1e3
dat80
dat80 %>%
  filter(SEX == 1,  BIRTHYR >= 1930 & BIRTHYR < 1940,
         INCWAGE > 0,  WKSWORK1>0, RACE == 1 | RACE == 2) %>%
  mutate(YEAR:=NULL, SAMPLE:=NULL, GQ:=NULL, RACED:=NULL,
         RACE:=NULL, SEX:=NULL)  ->
  dat80

###3.2 Sample and the EDA

dat80 %>%
  mutate(LWKLYWGE = log(INCWAGE / WKSWORK1)) %>%
  mutate(INCWAGE:=NULL,WKSWORK1:=NULL) %>%
  mutate(EDUC = NA) %>%
  mutate(EDUC = ifelse(EDUCD == 14, 1, EDUC)) %>%
  mutate(EDUC = ifelse(EDUCD == 15, 2, EDUC)) %>%
  mutate(EDUC = ifelse(EDUCD == 16, 3, EDUC)) %>%
  mutate(EDUC = ifelse(EDUCD == 17, 4, EDUC)) %>%
  mutate(EDUC = ifelse(EDUCD == 22, 5, EDUC)) %>%
  mutate(EDUC = ifelse(EDUCD == 23, 6, EDUC)) %>%
  mutate(EDUC = ifelse(EDUCD == 25, 7, EDUC)) %>%
  mutate(EDUC = ifelse(EDUCD == 26, 8, EDUC)) %>%
  mutate(EDUC = ifelse(EDUCD == 30, 9, EDUC)) %>%
  mutate(EDUC = ifelse(EDUCD == 40, 10, EDUC)) %>%
  mutate(EDUC = ifelse(EDUCD == 50, 11, EDUC)) %>%
  mutate(EDUC = ifelse(EDUCD == 60, 12, EDUC)) %>%
  mutate(EDUC = ifelse(EDUCD == 65 | EDUCD == 70, 13, EDUC)) %>%
  mutate(EDUC = ifelse(EDUCD == 80, 14, EDUC)) %>%
  mutate(EDUC = ifelse(EDUCD == 90, 15, EDUC)) %>%
  mutate(EDUC = ifelse(EDUCD == 100, 16, EDUC)) %>%
  mutate(EDUC = ifelse(EDUCD == 110, 17, EDUC)) %>%
  mutate(EDUC = ifelse(EDUCD == 111, 18, EDUC)) %>%
  mutate(EDUC = ifelse(EDUCD == 112, 19, EDUC)) %>%
  mutate(EDUC = ifelse(EDUCD >= 113, 20, EDUC)) %>%
  mutate(EDUCD := NULL) ->
  dat80

glimpse(dat80)

### 3.3 A brief look ahead: The returns-to-schooling in the 1980 census

summary(lm(LWKLYWGE ~ EDUC + factor(BIRTHYR), data=dat80))

## 4 Version control

library(sessioninfo)
platform_info()

package_info(c("ipumsr", "tidyverse"), dependencies=FALSE)

