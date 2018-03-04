rm(list=ls())
library(foreign)
library(dplyr); library(magrittr)
library(psych) # Chronbach's alpha
source("F0001-basic.R")

#### 0. Data Step ##############################################################################
#### 0.0 data entry ############################################################################
data = read.spss("../data/ENTIRE_dataset_MIBS_2013_04_12.sav", to.data.frame=TRUE)
data$id %>% as.character %>% gsub(pattern=" ",replacement="") -> data$id
dim(data)  # 97 x 1608

#### 0.1 variables list-up ############################################################################
  # Getting data lables
    attr(data,"variable.labels") -> data.label
    data.label <- data.frame(name = names(data.label), label = data.label)
    data.label$name %<>% as.character
    data.label$label %<>% as.character
    rownames(data.label) <- NULL
    
    data.label$name[1:9] # main variables
    c('id', 'treatm_group', 'treatmgroup_nr', 'cohort', 'dropouts', 'treatm_sessions', 'treated', 'PRE_TREATMENT_QUEST', 'date')
  
  # what type of variables are there?  
    tmp <- gsub("[0-9]","",data.label$name[-(1:9)]) # numbers removed
    tmp <- gsub("\\_.*","",tmp)               # anything after underbar removed
    table(tmp)                                # results
    tmp %>% unique %>% length                 # 117 unique categories + main category

#### 0.2 data scoping ############################################################################
  ## 0.2.1 IBSS
    extractVar("IBS_severit")
    extractVar("IBS_severit.*_ITT")
    var.IBSS = data.frame(variable = c('IBS_severity_bl', 'IBS_severity_fu', 'IBS_severity_3mo_fu', 'IBS_severity_6mo', 'IBS_severity_12mo'),
                          category = "IBSS",
                          time = c("bl", "fu", "3mo", "6mo", "12mo"))
                      
  # "...ITT" in variable names is not actually ITT, but stands for LVCF (last value carry forward).

  #### 0.2.2 anger ############################################################################
    # reverse coding
      data$bax_1.re <- 5 - data$bax_1 %>% as.numeric  #will be reversed next time
      data$ax_1_1.re <- 5 - data$ax_1_1 %>% as.numeric
      data$ax1_3mo.re <- 5 - data$ax1_3mo %>% as.numeric
      data$ax_1_6mo.re <- 5 - data$ax_1_6mo %>% as.numeric
      data$ax1_12mo.re <- 5 - data$ax1_12mo %>% as.numeric
  
    # anger variable - construct mapping
      anger <- list(unexp = NA, extexp = NA, emoreg = NA, probsol = NA)
        anger$unexp <- data.frame(bl = paste0("bax_", c(39, 40, 25, 3, 15, 10, 18, 22, 6)),
                                  fu = paste0("ax_", c(391, 401, 251, "3_1", 151, 101, 181, 221, "6_1")),
                                  mo3 = paste0("ax", c(39, 40, 25, 3, 15, 10, 18, 22, 6), "_3mo"),
                                  mo6 = paste0("ax_", c(39, 40, 25, 3, 15, 10, 18, 22, 6), "_6mo"),
                                  mo12 = paste0("ax", c(39, 40, 25, 3, 15, 10, 18, 22, 6), "_12mo"),
                                  stringsAsFactors=FALSE)
        anger$extexp <- data.frame(bl = paste0("bax_", c(19, 2, 37, 17, 4, 9, "1.re")),
                                   fu = paste0("ax_", c(191, "2_1", 371, 171, "4_1", "9_1", "1_1.re")),
                                   mo3 = paste0("ax", c(19, 2, 37, 17, 4, 9, 1), c(rep("_3mo",6), "_3mo.re")),
                                   mo6 = paste0("ax_", c(19, 2, 37, 17, 4, 9, 1), c(rep("_6mo",6), "_6mo.re")),
                                   mo12 = paste0("ax", c(19, 2, 37, 17, 4, 9, 1), c(rep("_12mo",6), "_12mo.re")),
                                   stringsAsFactors=FALSE)
        anger$emoreg <- data.frame(bl = paste0("bax_", c(27, 34, 21, 26, 28)),
                                   fu = paste0("ax_", c(271, 341, 211, 261, 281)),
                                   mo3 = paste0("ax", c(27, 34, 21, 26, 28), "_3mo"),
                                   mo6 = paste0("ax_", c(27, 34, 21, 26, 28), "_6mo"),
                                   mo12 = paste0("ax", c(27, 34, 21, 26, 28), "_12mo"),
                                   stringsAsFactors=FALSE)
        anger$probsol <- data.frame(bl = paste0("bax_", c(31, 38, 20, 35, 36)),
                                    fu = paste0("ax_", c(311, 381, 201, 351, 361)),
                                    mo3 = paste0("ax", c(31, 38, 20, 35, 36), "_3mo"),
                                    mo6 = paste0("ax_", c(31, 38, 20, 35, 36), "_6mo"),
                                    mo12 = paste0("ax", c(31, 38, 20, 35, 36), "_12mo"),
                                    stringsAsFactors=FALSE)
  
      # anger variables in a vector
        anger.vector <- do.call(c, do.call(c, anger)) %>% as.vector
        var.anger <- data.frame(variable = anger.vector,
                                category = "anger",
                                time = do.call(c, sapply(c(9,7,5,5), function(s) rep(c("bl", "fu", "3mo", "6mo", "12mo"), each=s))))
      # factor into numeric
        for (i in anger.vector) data[, i] %<>% as.numeric  
      
    # confidence intervals of cronbach's alpha
      sapply(anger$unexp, function(s) alpha(data[,s]) %>% alpha.ci) # unexp
      sapply(anger$extexp, function(s) alpha(data[,s]) %>% alpha.ci) # extexp
      sapply(anger$emoreg, function(s) alpha(data[,s]) %>% alpha.ci) # emoreg
      sapply(anger$probsol, function(s) alpha(data[,s]) %>% alpha.ci) # probsol
  
  
    # mean scores
      if (FALSE) { ##### !!!!This should be moved to after imputation!!!!! #####
        # apply(data[ ,anger$unexp$bl], 1, mean) # simple coding for illustration purpose
        anger.mean <- lapply(anger, function(i) sapply(i, function(j) apply(data[ ,j], 1, mean)))
        anger.mean <- do.call(cbind, anger.mean) %>% as.data.frame
        names(anger.mean) <- paste0(rep(c("unexp", "extexp", "emoreg", "probsol"), each=5), c(".bl", ".fu", ".3mo", ".6mo", ".12"))
      }

  ###############################################################################################

  ## 0.2.3 FILE (family inventory of life events and changes): something like trauma  
    FILE <- extractVar("^q[0-9]")  #^: begins with, [0-9]: numbers
    tmp <- which(grepl("_b", FILE[,2]))
    FILE <- list(bl = FILE[tmp, 2], hist = FILE[-tmp, 2])
      # bl: during last 12 months, hist: before last 12 months.
      order(gsub("q", "", gsub("_b", "", FILE$bl)) %>% as.numeric) == 1:71 # check if they are in the order
      order(gsub("q", "", FILE$hist) %>% as.numeric) == 1:71 # check if they are in the order
    FILE.vector = do.call(c, FILE)
    var.FILE = data.frame(variable = FILE.vector, category = "FILE", time = rep(c("common", "common-hist"), each=71))
    
    # factor into numeric
    for (i in FILE.vector) data[, i] %<>% as.numeric  # yes = 1, no = 2
    data[,FILE.vector] = 2 - data[,FILE.vector]       # recoding: yes = 1, no = 0
    
    # summed scores
    if (FALSE) { ##### !!!!This should be moved to after imputation!!!!! #####
      FILE.sum <- sapply(FILE, function(i) apply(data[ ,i], 1, sum)) %>% as.data.frame
      names(FILE.sum) <- c("FILE.bl", "FILE.hist")
    }
   
  ## 0.2.4 comorbidity
    (ADD codes HERE)  
    var.comorbid = data.frame(variable = ,
                              category = ,
                              time = )
    
  ## 0.2.5 work productivity
    (ADD codes HERE)  
    var.work = data.frame(variable = ,
                              category = ,
                              time = )
    
  ## 0.2.6 coping strategy
    (ADD codes HERE)  
    var.coping = data.frame(variable = ,
                              category = ,
                              time = )
    
  ## var.include, sample.include ################################################################
    var.include = data.frame(variable = c('id', 'treatmgroup_nr', 'cohort', 'treatm_sessions'), 
                             category = "base", 
                             time = "common") ## include basic variables
    var.include = rbind(var.include, var.IBSS)  # adding IBSS variables
    var.include = rbind(var.include, var.anger) # adding anger variables
    var.include = rbind(var.include, var.FILE)  # adding FILE variables
    var.include = rbind(var.include, var.comorbid)  # adding comorbidity variables
    var.include = rbind(var.include, var.work)  # adding work variables
    var.include = rbind(var.include, var.coping)  # adding coping variables
    
    sample.include = which(!is.na(data$treatmgroup_nr))  # subject numbers of both arms
  ###############################################################################################

  data.working = data[sample.include, var.include]
  saveRDS(data.working, "data.working.rds")