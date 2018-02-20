rm(list=ls())
library(foreign); library(sas7bdat)
library(dplyr); library(magrittr)

#### data entry ############################################################################
data.entire = read.spss("../data/ENTIRE_dataset_MIBS_2013_04_12.sav", to.data.frame=TRUE)
data.full = read.sas7bdat("../data/fulldata.sas7bdat")
data.anger = read.spss("../data/MIBS_Anger_2013_04_12.sav", to.data.frame=TRUE)
data.anger.trt = read.spss("../data/MIBS_Anger_TreatedOnly_2013_04_12.sav", to.data.frame=TRUE)
data.entire$id %>% as.character %>% gsub(pattern=" ",replacement="") -> data.entire$id
data.full$id %<>% as.character
data.anger$id %<>% as.character
data.anger.trt$id %<>% as.character
names(data.entire)[1030] <- "filter__"


#### data description ############################################################################
dim(data.entire)  # 97 x 1608
names(data.entire)
data.entire$treatm_group %>% table

dim(data.anger)  # 97 x 1602
# Guess data.anger is a subset of data.entire
  all(names(data.anger) %in% names(data.entire)) # every variable in data.anger is in data.entire
  all(data.anger$id %in% data.entire$id) # every subject id in data.anger is in data.entire
  data.anger$treatm_group %>% table

dim(data.full) # 97 x 1608
identical(data.full, data.entire) #FALSE
# something is different btw two datasets
  all(names(data.full) %in% names(data.entire)) # every variable in data.anger is in data.entire
  all(data.full$id %in% data.entire$id) # every subject id in data.anger is in data.entire
  data.full$treatm_group %>% table
