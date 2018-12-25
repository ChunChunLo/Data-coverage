#-----------------------
#title: "Data coverage"
#author: "Chun"
#date: "19 Dec. 2018"
#-----------------------

#--------------------------------------
# Data source: TaiBIF.......4 datasets
#              GBIF.........4 datasets
#              TBN
#              Road kill
#              ebird
#--------------------------------------

#---- load packages
library(data.table)
library(magrittr)
library(rgdal)
library(tidyr)
library(readxl)
#---- Set environment
D2018 <- "D:/Chun/Analysis/RRR/Dataset"


#--------------------------------------####
 GBIF <- "D:/Chun/Analysis/RRR/Dataset/GBIF"
 TBIF <- "D:/Chun/Analysis/RRR/Dataset/TaiBIF"
 D2018 <- "D:/Chun/Analysis/RRR/Dataset"
 ebd <- file.path( "D:/Chun/Analysis/RRR/Dataset/ebd_TW_200001_201712_prv_relMay-2017")
 CWBF <- file.path( "D:/Chun/Analysis/RRR/Dataset/eBird historic data")
 # get RDS for each dataset
 GBIF.L <-
   list.files(GBIF, full.names = T) %>%
   .[c(2, 5:7)]
 TBIF.L <-
   list.files(TBIF, full.names = T) %>%
   .[c(1:2,4,6)]

 # 讀入所有資料集+增加資料來源(Project)&調查時間欄位(Date)##
 Data_ori <-
   lapply(c(GBIF.L, TBIF.L, ebd, CWBF, D2018), function(x){
     readRDS(file.path(x, "data.rds"))}) %>%
   do.call("rbind", .) %>%
   .[, eventID1 := eventID] %>%
   separate(eventID1, c("Project", "Date", "T","ID"), "_") %>%
   .[,c( "T", "ID"):= NULL ] %>%
   setnames(., c("common_name_T", "scientific_name"), c("Ori_common_name_T", "Ori_scientific_name") )
saveRDS(Data_ori, file.path(D2018, "Data_ori.rds"))

Data_ori <- readRDS(file.path(D2018, "Data_ori.rds"))
#--------------------------------------####

#變量轉為數值 (as.numeric針對vector)
Data_ori$Year %<>% as.numeric
Data_ori$Month %<>% as.numeric
Data_ori$Latitude %<>% as.numeric
Data_ori$Longitude %<>% as.numeric
Data_ori$individual_count %<>% as.numeric

#----Summary
Data <- Data_ori

Data %>%
  .[, list(Project, accepted_name_code, individual_count)] %>%
  .[, .(nRecord = .N,
        nSpecies = uniqueN( accepted_name_code)), by = list(Project)]

summary(Data_ori)


#-- 物種數=0 視為NA (n=0)
Data[individual_count==0] %>% nrow
Data %<>% .[individual_count==0, individual_count:= NA]

#---- 對照物種資訊
# 載入物種清單資訊 (class欄位) [list.s]
list.s <-
  read_xlsx(file.path(D2018,"data coverage_species list.xlsx"),sheet = 1) %>%
  setDT %>%
  .[,c("accepted_name_code", "common_name_T",
       "class_c", "order_c", "family_c", "genus_c", "scientific_name",
       "is_endemic", "is_alien", "is_invasive")] %>%
  unique

# 對照物種清單
Data %<>%
  list.s[. , on = "accepted_name_code"]

Data %<>%
  .[ !is.na(individual_count) ] %>% 
  .[, individual_count := as.numeric(individual_count)] %>%
  .[ , .(individual_count = sum(individual_count )), 
     by = list(accepted_name_code, common_name_T,
               class_c, order_c, family_c, genus_c,scientific_name,
               Ori_common_name_T, Ori_scientific_name, 
               Year, Month, 
               Longitude, Latitude,
               eventID, Project, Date,
               is_endemic, is_alien, is_invasive) ] %>%
  rbind(., Data[ is.na(individual_count) ]) %>% 
  unique 

## 無物種資訊之紀錄
Data_NA <- Data %>%
  .[is.na(scientific_name)]

Data_NA %>%
  .[, list(Project, accepted_name_code, individual_count)] %>%
  .[, .(nRecord = .N,
        nSpecies = uniqueN(accepted_name_code)), by = list(Project)]

## 對上物種資訊之紀錄
Data_sp <- Data %>%
  .[!is.na(scientific_name)]

Data_sp %>%
  .[, list(Project, accepted_name_code, individual_count)] %>%
  .[, .(nRecord = .N,
        nSpecies = uniqueN(accepted_name_code)), by = list(Project)]


#---- export as .RData
saveRDS(Data_sp, 
     file= file.path(D2018,"data_coverage_20181219.rds"))
#================= End

readRDS(file.path(D2018,"data_coverage_20181219.rds"))


