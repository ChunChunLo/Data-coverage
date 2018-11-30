#-----------------------
#title: "Data coverage"
#author: "Chun"
#date: "10 April. 2018"
#-----------------------

#--------------------------------------
# Data source: TaiBIF.......8 datasets
#              GBIF.........4 datasets
#              TBN
#              Road kill
#              ebird
#--------------------------------------

#------------------ load packages
library(data.table)
library(magrittr)
library(rgdal)
library(tidyr)
library(readxl)
#------------------ Set environment
D2018 <- "D:/Chun/Analysis/RRR/Dataset"

#Done
{
GBIF <- "D:/Chun/Analysis/RRR/Dataset/GBIF"
TBIF <- "D:/Chun/Analysis/RRR/Dataset/TaiBIF"
D2018 <- "D:/Chun/Analysis/RRR/Dataset"
ebd <- file.path( "D:/Chun/Analysis/RRR/Dataset/ebd_TW_200001_201712_prv_relMay-2017")
CWBF <- file.path( "D:/Chun/Analysis/RRR/Dataset/eBird historic data")
#------------------ for TaiBIF and GBIF data

 #get RData for each dataset
GBIF.L <- 
  list.files(GBIF, full.names = T) %>%
  .[c(2, 5:7)]
TBIF.L <- 
  list.files(TBIF, full.names = T) %>%
  .[c(1:2,4,6)]



#讀入所有資料集+增加資料來源(Project)&調查時間欄位(Date)##
All.data_ori <- 
  lapply(c(GBIF.L, TBIF.L, ebd, CWBF, D2018), function(x)
  get(load(file.path(x,"/data.RData")))) %>%
  do.call("rbind", .) %>%  
  .[, eventID1 := eventID] %>%
  separate(eventID1, c("Project", "Date", "T","ID"), "_") %>%  
  .[,c( "T", "ID"):= NULL ]
#Bad way#
{
L<-c(GBIF.L, TBIF.L, ebd, CWBF)
for(i in 1:10){
get(load(file.path(L[i],"/data.RData")))
}

All.data <- rbind(Augu101.new, 
                  Augu102.new,
                  BBS.new,
                  CWBF.new,
                  CY.ML.new,
                  ebd.new,
                  F4th.new,
                  Frog.new,
                  HLC.new,
                  Inv.new )
}
}
#*************************************************##
All.data_ori<- get(load(file.path(D2018, "data_20180417.RData")))

#變量轉為數值
All.data_ori$Year %<>% as.numeric
All.data_ori$Month %<>% as.numeric
All.data_ori$Latitude %<>% as.numeric
All.data_ori$Longitude %<>% as.numeric
All.data_ori$individual_count %<>% as.numeric
summary(All.data_ori)

#-- 物種數=0 視為NA (n=4)
All.data_ori %<>% .[individual_count==0, individual_count:= "NA"]
summary(All.data_ori)

#####
#-- 物種清單資訊 (class欄位) [list.s]
list.s <- 
  read_xlsx(file.path(D2018,"data coverage_species list.xlsx"),sheet = 1) %>%
  setDT %>%
  .[,c("accepted_name_code", "common_name_T",
       "class_c", "order_c", "family_c", "genus_c", "scientific_name")] %>%
  unique


#-- 將list.s的物種資訊欄位合併到ALL_data_ori
All.data <- All.data_ori %>% 
            .[,c("common_name_T", "scientific_name"):= NULL] %>%
            list.s[. , on = "accepted_name_code"] %>%
             .[!is.na(scientific_name)] 


summary(All.data)

All.data<-All.data_ori
#----------------------------------------------------------------------
datacount <- All.data %>%
  .[ individual_count != "NA" ] %>% 
  .[, individual_count := as.numeric(individual_count)] %>%
  .[ , .(individual_count = sum(individual_count )), 
     by = list(accepted_name_code, 
               class,scientific_name, 
               is_endemic, is_alien,
               Year, Month,
               Longitude, Latitude, 
               eventID, Project, Date)]

dataNA <- All.data[ is.na(individual_count) ] %>% 
  unique 


All.data <-  rbind(dataNA,datacount) 



#------------------ export as .RData
save(All.data, 
     file= file.path(D2018,"data_coverage_20180417.RData"))
#================= End

get(load(file.path(D2018,"data_coverage_20180417.RData")))



