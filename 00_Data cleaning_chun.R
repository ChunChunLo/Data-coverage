#-----------------------
#title: "Data cleaning"
#author: "Chun"
#date: "25 Dec. 2018"
#-----------------------
#------------------ load packages
library(data.table)
library(magrittr)
library(readxl)
library(tidyr)

#### Ū�J���####
#------------------ TBN and RK data set#####
#---- set environment
D2018 <- file.path( "D:/Chun/Analysis/RRR/Dataset")
#---- import data
TBN <- 
  fread(file.path(D2018, "TBN_keepinTW_20170619_TWD97.csv"),
        sep = ",")  %>%
  .[, which(unlist(lapply(., function(x)!all(is.na(x))))),with=F] %>%          
  .[, month := sprintf("%02d", as.numeric(month))] %>%
  .[, date := sprintf("%02d", as.numeric(date))] %>%  
  .[, eventDate:= paste(year, month, date, sep="-")] %>%
  .[, Project := paste("TBN")] %>% 
  .[, eventID := paste("TBN", eventDate, "NA", "NA", sep = "_")] %>%
  .[, individual_count := NA ] %>%
  .[, list(common_name_T_ori = Cname,
           scientific_name_ori = species,
           accepted_name_code = acc_nameco,
           Year = year, 
           Month = month,
           Longitude = lon,
           Latitude = lat,
           Project,
           eventID, 
           individual_count )]  %>% 
 .[, list(common_name_T_ori, 
          scientific_name_ori,
          accepted_name_code, 
          Year, 
          Month, 
          Longitude,
          Latitude, 
          Project, 
          eventID,
          individual_count)]

RK <- 
  fread(file.path(D2018, "RK_keepinTW_20170619_TWD97.csv"),
        sep = ",") %>%
  .[, which(unlist(lapply(., function(x)!all(is.na(x))))),with=F] %>%          
  .[, month := sprintf("%02d", as.numeric(month))] %>%
  .[, date := sprintf("%02d", as.numeric(date))] %>%  
  .[, eventDate := paste(year, month, date, sep="-")] %>%
  .[, Project := paste("RK") ] %>%
  .[, eventID := paste("RK", eventDate, "NA", "NA", sep = "_")] %>%
  .[, individual_count := NA ] %>%
  .[, list(common_name_T_ori = Cname,
           scientific_name_ori = species,
           accepted_name_code = acc_nameco,
           Year = year, 
           Month = month,
           Longitude = lon,
           Latitude = lat,
           Project,
           eventID, 
           individual_count )] %>%
  .[, list(common_name_T_ori, 
           scientific_name_ori,
           accepted_name_code, 
           Year, 
           Month, 
           Longitude,
           Latitude, 
           Project,
           eventID,
           individual_count)]


#------------------ ebd data set####
#---- set environment
ebddir <- file.path( "D:/Chun/Analysis/RRR/Dataset/ebd_TW_200001_201712_prv_relMay-2017")
#---- import data
ebd <- 
  fread(file.path(ebddir, 
                  "ebd_TW_200001_201712_prv_relMay-2017.txt"),
        sep = "\t", encoding = "UTF-8") %>%
  .[, which(unlist(lapply(., function(x)!all(is.na(x))))),with=F] %>%
  .[, list(category = CATEGORY,
           common_name = `COMMON NAME`,
           common_name_T_ori = NA,
           scientific_name_ori = `SCIENTIFIC NAME`,
           accepted_name_code = NA, 
           subspecies_scientific_name_ori = `SUBSPECIES SCIENTIFIC NAME`,
           eventDate = `OBSERVATION DATE`,
           eventTime = `TIME OBSERVATIONS STARTED`,
           Sampling_ID = `SAMPLING EVENT IDENTIFIER`,
           Group_ID = `GROUP IDENTIFIER`,
           Longitude = LONGITUDE,
           Latitude = LATITUDE,
           individual_count = `OBSERVATION COUNT`,
           Duration_minutes = `DURATION MINUTES`,
           Effect_distance = `EFFORT DISTANCE KM`,
           Effect_area_ha = `EFFORT AREA HA`,
           No._observers = `NUMBER OBSERVERS`,
           All_sp_reported = `ALL SPECIES REPORTED`)]  %>%
  .[, Project := paste("ebd")] %>%
  .[, eventID := paste("ebd", eventDate, eventTime, Sampling_ID, sep = "_")] %>%
  .[, eventDate1 := eventDate] %>%
  separate(eventDate1, c("Year", "Month", "Day"), "-") %>%
  .[, individual_count := sub("X", NA, individual_count)]

colnames(ebd)

# remove all na column :  Filter(function(x)!all(is.na(x)), df) or .[,which(unlist(lapply(., function(x)!all(is.na(x))))),with=F]###

#---- remove repeat sampling event


ebd.du <-
  ebd[Group_ID != "",
      .(`Sampling_ID` = 
          min(`Sampling_ID`)),
      by = `Group_ID`]

ebd.unique <- 
  rbind(ebd[`Group_ID` == ""],
        ebd[`Sampling_ID` %in% ebd.du$Sampling_ID]) %>%
  .[, `Group_ID` := NULL] 

#---- count N
ebd %>% dim %>% .[1]
ebd.unique %>% dim %>% .[1]

ebd <- ebd.unique
ebd %<>%  .[, list(common_name_T_ori, 
                   scientific_name_ori,
                   accepted_name_code, 
                   Year, 
                   Month, 
                   Longitude,
                   Latitude, 
                   Project, 
                   eventID,
                   individual_count)]

rm(ebd.unique, ebd.du)


#------------------ CWBF data set#####
#---- set environment
CWBFdir <- file.path( "D:/Chun/Analysis/RRR/Dataset/eBird historic data")

#---- import data
CWBF <- 
  fread(file.path(CWBFdir, 
                  "eBird�榡���ج���_�ɥR��T_0115�ץ�.csv"),
        sep = "|", encoding = "UTF-8") %>%
  .[, which(unlist(lapply(., function(x)!all(is.na(x))))),with=F] %>%
  .[, list(common_name_T_ori = `Common Name`,
           scientific_name_ori = paste(Genus, Species, sep= " "),
           accepted_name_code = NA,
           eventDate = `Date`,
           eventTime = `Start Time`,
           Sampling_ID = `e_id`,
           Longitude ,
           Latitude ,
           individual_count = Number,
           Duration_minutes = `Duration`,
           Effect_distance = `Effort Distance Miles`,
           Effect_area_ha = `Effort area acres`)]  %>%
  separate(eventDate, c("Month", "Day",  "Year"), "/") %>%
  .[, Month := sprintf("%02d", as.numeric(Month))] %>%
  .[, Day := sprintf("%02d", as.numeric(Day))] %>%
  .[, eventDate := paste(Year, Month, Day, sep = "-")] %>%
  .[, Project := paste("CWBF")] %>%
  .[, eventID := paste("CWBF", eventDate, eventTime, Sampling_ID, sep = "_")] %>%
  .[, individual_count := as.numeric(individual_count)] %>% 
  .[, list(common_name_T_ori, 
           scientific_name_ori,
           accepted_name_code, 
           Year, 
           Month, 
           Longitude,
           Latitude,
           Project,
           eventID,
           individual_count)]

CWBF %>% .[ Year == 2101 ] # 36�� 2101/12/21�����

colnames(CWBF)


#------------------ F4th data set####
#----Set environment
GBIF <- "D:/Chun/Analysis/RRR/Dataset/GBIF"
F4thdir <- file.path(GBIF, 
                     "�ĥ|���˪L�귽�լd���Ͱʪ��լd�����ɮ׺�ť���Ѥθ�Ƥ��R")

#---- import data
###remove all na column :  Filter(function(x)!all(is.na(x)), df) or .[,which(unlist(lapply(., function(x)!all(is.na(x))))),with=F]###
F4th <- 
  fread(file.path(F4thdir, "occurrence.txt"),
        sep = "\t", encoding = "UTF-8") %>%
  Filter(function(x)!all(is.na(x)), .) %>%
  .[, eventTime := sprintf("%04d", as.numeric(eventTime))]%>%
  .[, eventTime := paste(substring(eventTime, 1, 2), substring(eventTime, -1, 2), sep= ":")] %>%
  setDT %>%
  .[, Project := paste( "Forest.4th")] %>%
  .[, eventID := paste( "Forest.4th" , eventDate ,eventTime, sep="_")] %>%
  separate(eventDate, c("Year", "Month", "Date"), "-") %>%
  .[(!is.na(acceptedNameUsageID) & acceptedNameUsageID != "") |
      (!is.na(scientificName) & scientificName != "") |
      (!is.na(acceptedNameUsage) & acceptedNameUsage!= "") |
      (!is.na(vernacularName) & vernacularName != "")] %>%
  .[, list(common_name_T_ori = vernacularName,
           scientific_name_ori = scientificName,
           accepted_name_code = acceptedNameUsageID,
           Year,
           Month,
           Longitude = decimalLongitude,
           Latitude = decimalLatitude,
           Project, 
           eventID = eventID) ]  %>%
  .[,individual_count := NA] %>% 
  .[, list(common_name_T_ori, 
           scientific_name_ori,
           accepted_name_code, 
           Year, 
           Month, 
           Longitude,
           Latitude, 
           Project, 
           eventID,
           individual_count)]

#------------------ CY.ML data set####
#---- Set environment
GBIF <- "D:/Chun/Analysis/RRR/Dataset/GBIF"
CY.MLdir <- file.path(GBIF, 
                      "�Ÿq�������s�m���j�����Űʪ��۹��׫׻P�����լd�[�U�����ǲΤ�Ʋ��������Ͱʪ����Q�Τη��N���y�d�򤧱��Q")

#---- import data
###remove all na column :  Filter(function(x)!all(is.na(x)), df) or .[,which(unlist(lapply(., function(x)!all(is.na(x))))),with=F]###


CY.ML <- 
  fread(file.path(CY.MLdir, "occurrence.txt"),
        sep = "\t", encoding = "UTF-8") %>%
  setDT %>%
  .[, which(unlist(lapply(., function(x)!all(is.na(x))))),with=F] %>%
  .[, Project := paste( "CY.ML")] %>%
  .[, eventID := paste( "CY.ML",  eventDate, sep="_")] %>%
  separate(eventDate, c("Year", "Month", "Date"), "-") %>%
  .[, list(common_name_T_ori = vernacularName,
           scientific_name_ori = scientificName,
           accepted_name_code = acceptedNameUsageID,
           Year,
           Month,
           Longitude = decimalLongitude,
           Latitude = decimalLatitude,
           Project, 
           eventID)] %>%
  .[, individual_count := NA] %>%
  .[, list(common_name_T_ori, 
           scientific_name_ori,
           accepted_name_code, 
           Year, 
           Month, 
           Longitude,
           Latitude, 
           Project, 
           eventID,
           individual_count)]

#------------------ Frog data set####
#---- Set environment
GBIF <- "D:/Chun/Analysis/RRR/Dataset/GBIF"
Frogdir <- file.path(GBIF, 
                     "�O�W������귽�լd�P�Ш|�žɱ��s�p�e")

#---- import data
###remove all na column :  Filter(function(x)!all(is.na(x)), df) or .[,which(unlist(lapply(., function(x)!all(is.na(x))))),with=F]###

Frog <- 
  fread(file.path(Frogdir, "occurrence.txt"),
        sep = "\t", encoding = "UTF-8") %>%
  .[, which(unlist(lapply(., function(x)!all(is.na(x))))),with=F] %>%
  separate(eventDate, c("eventDate", "eventDate1"), "T")%>%
  setDT %>%
  .[, Project := paste( "Frog")] %>%
  .[, eventID := paste( "Frog", eventDate, sep="_")]%>%
  .[, list(common_name_T_ori = vernacularName,
           scientific_name_ori = scientificName,
           accepted_name_code = NA, 
           Year = year,
           Month = month,
           Longitude = decimalLongitude,
           Latitude = decimalLatitude,
           Project, 
           eventID,
           individual_count = individualCount)] %>%
  .[, list(common_name_T_ori, 
           scientific_name_ori,
           accepted_name_code, 
           Year, 
           Month, 
           Longitude,
           Latitude, 
           Project, 
           eventID,
           individual_count)]


#------------------ BBS data set####
#---- set environment
GBIF <- "D:/Chun/Analysis/RRR/Dataset/GBIF"
BBSdir <- file.path(GBIF, "Breeding Bird Survey2009-2015")

#---- import data
BBS <- 
  fread(file.path(BBSdir, 
                  "occurrence.txt"),
        sep = "\t", encoding = "UTF-8") %>% 
  .[, which(unlist(lapply(., function(x)!all(is.na(x))))),with=F] %>%
  separate(eventDate, c("eventDate", "eventDate1"), "T")%>%
  .[, eventTime := gsub("+0800", "", eventTime , fixed = TRUE)] %>%
  .[, Project := paste( "BBS")] %>%
  .[, eventID := paste( "BBS", eventDate, eventTime,sep="_")] %>%
  .[, list(common_name_T_ori = vernacularName,
           scientific_name_ori = scientificName,
           accepted_name_code = NA,
           Year = year,
           Month = month,
           Longitude = decimalLongitude,
           Latitude = decimalLatitude,
           Project, 
           eventID = eventID,
           individual_count = individualCount)] %>%
  .[, list(common_name_T_ori, 
           scientific_name_ori, 
           accepted_name_code, 
           Year, 
           Month, 
           Longitude,
           Latitude, 
           Project, 
           eventID,
           individual_count)]

colnames(BBS)

#------------------ Augu101 data set####
#---- Set environment
TBIF <- "D:/Chun/Analysis/RRR/Dataset/TaiBIF"
Augu101dir <- file.path(TBIF,
                        "101�~��������a�˪L��ϳ����ʴ��Ϋإߺʴ��Ҧ�")

#---- import data
Augu101 <- 
  fread(file.path(Augu101dir, "occurrence.txt"),
        sep = "\t", encoding = "UTF-8") %>%
  .[, month := sprintf("%02d", as.numeric(month))] %>%
  .[, day := sprintf("%02d", as.numeric(day))] %>%
  .[, eventDate := paste(year, month, day, sep="-")] %>%
  .[, Project := paste("Augu101")] %>%
  .[, eventID := paste("Augu101", eventDate, sep="_")] %>%
  .[, list(common_name_T_ori = vernacularName,
           scientific_name_ori = scientificName,
           accepted_name_code = NA, 
           Year = year,
           Month = month,
           Longitude = decimalLongitude,
           Latitude = decimalLatitude,
           Project, 
           eventID,
           individual_count = individualCount)] %>%
  .[, list(common_name_T_ori, 
           scientific_name_ori,
           accepted_name_code, 
           Year, 
           Month, 
           Longitude,
           Latitude, 
           Project, 
           eventID,
           individual_count)]


#------------------ Augu102 data set####
#---- Set environment
TBIF <- "D:/Chun/Analysis/RRR/Dataset/TaiBIF"
Augu102dir <- file.path(TBIF,
                        "102�~��������a�˪L��ϳ����ʴ��Ϋإߺʴ��Ҧ�")

#---- import data
Augu102 <- 
  fread(file.path(Augu102dir, "occurrence.txt"),
        sep = "\t", encoding = "UTF-8") %>%
  separate(eventDate, c("year", "month", "day"), "/")%>%
  .[,month := sprintf("%02d", as.numeric(month))] %>%
  .[,day := sprintf("%02d", as.numeric(day))] %>%
  setDT %>%
  .[, eventDate := paste(year, month, day, sep="-")] %>%
  .[, Project := paste("Augu102")] %>%
  .[, eventID := paste("Augu102", eventDate, sep="_")] %>%
  .[, list(common_name_T_ori = vernacularName,
           scientific_name_ori = scientificName,
           accepted_name_code = NA,
           Year = year,
           Month = month,
           Longitude = decimalLongitude,
           Latitude = decimalLatitude,
           Project, 
           eventID,
           individual_count = individualCount)] %>%
  .[, list(common_name_T_ori, 
           scientific_name_ori,
           accepted_name_code,
           Year, 
           Month, 
           Longitude,
           Latitude, 
           Project, 
           eventID,
           individual_count)]

#------------------ Inv data set####
#---- Set environment
TBIF <- "D:/Chun/Analysis/RRR/Dataset/TaiBIF"
Invdir <- file.path(TBIF, 
                    "�~�Ӻش��L��챱��P�ʴ��p�e")

#---- import data
Inv <- 
  fread(file.path(Invdir, "occurrence.txt"),
        sep = "\t", encoding = "UTF-8") %>%
  .[, which(unlist(lapply(., function(x)!all(is.na(x))))),with=F] %>%
  separate(eventDate, c("Year", "Month", "Date"), "/") %>%
  .[, Month := sprintf("%02d", as.numeric(Month))] %>%
  .[, Date := sprintf("%02d", as.numeric(Date))] %>%
  setDT %>%
  .[, eventDate := paste(Year, Month, Date, sep="-")] %>%
  .[, Project := paste("Inv")] %>%
  .[, eventID := paste("Inv",eventDate,sep="_")] %>%
  .[, list(common_name_T_ori = vernacularName,
           scientific_name_ori = paste0(genus, " ", specificEpithet),
           accepted_name_code = acceptedNameUsageID,
           Year,
           Month,
           Longitude = decimalLongitude,
           Latitude = decimalLatitude,
           Project, 
           eventID,
           individual_count = individualCount)] %>%
  .[, list(common_name_T_ori, 
           scientific_name_ori,
           accepted_name_code, 
           Year, 
           Month, 
           Longitude,
           Latitude, 
           Project, 
           eventID,
           individual_count)]


#------------------ HLC data set####
#---- Set environment
TBIF <- "D:/Chun/Analysis/RRR/Dataset/TaiBIF"
HLCdir <- file.path(TBIF,
                    "�Ὤ�����a�y�L�ϴ˪L�ʰʪ��ʴ��p�e")

#---- import data
HLC <- 
  fread(file.path(HLCdir, "occurrence.txt"),
        sep = "\t", encoding = "UTF-8") %>%
  separate(eventDate, c("Year", "Month", "Date"), "/") %>%
  .[, Month := sprintf("%02d", as.numeric(Month))] %>%
  .[, Date := sprintf("%02d", as.numeric(Date))] %>%
  setDT %>%
  .[, eventDate := paste(Year, Month, Date, sep="-")] %>%
  .[, Project := paste("HLC")] %>%
  .[, eventID := paste("HLC",eventDate,sep="_")] %>%
  .[, list(common_name_T_ori = vernacularName,
           scientific_name_ori = scientificName,
           accepted_name_code = NA,
           Year,
           Month,
           Longitude = decimalLongitude,
           Latitude = decimalLatitude,
           Project, 
           eventID,
           individual_count = individualCount)] %>%
  .[, list(common_name_T_ori, 
           scientific_name_ori,
           accepted_name_code,
           Year, 
           Month, 
           Longitude,
           Latitude, 
           Project, 
           eventID,
           individual_count)]

#### �J���� ####
dfs = sapply(.GlobalEnv, is.data.frame) # �C�X�{���ܼ�: objects() or ls()
Data <- do.call(rbind, mget(names(dfs)[dfs])) %>%
  .[, Year := as.numeric(Year)] %>%
  .[, Month := as.numeric(Month)] %>% 
  .[, Longitude := as.numeric(Longitude)] %>%
  .[, Latitude := as.numeric(Latitude)] %>%
  .[, individual_count := as.numeric(individual_count)]

#### ��X��� ####
# #---- export as RDS
# saveRDS(Data, file.path(D2018, "Ori_Dataset.rds"))
# Data <- readRDS(file.path(D2018, "Ori_Dataset.rds"))
# #---- ��X���ظ�T
# Data_sp.list <-
#   Data[, list(common_name_T_ori,
#             scientific_name_ori,
#             accepted_name_code)] %>%
#   .[, .(Count = .N),
#     by = list(common_name_T_ori, scientific_name_ori, accepted_name_code)]
# 
# write.csv(Data_sp.list,
#        file.path(D2018, "Dataset_sp.list.csv"))

#### ��� Summary ####



#---- load packages
library(data.table)
library(magrittr)
library(rgdal)
library(tidyr)
library(readxl)
#---- Set environment
D2018 <- "D:/Chun/Analysis/RRR/Dataset"

Data <- readRDS(file.path(D2018, "Ori_Dataset.rds"))

#�ܶq�ର�ƭ� (as.numeric�w��vector)
Data$Year %<>% as.numeric
Data$Month %<>% as.numeric
Data$Latitude %<>% as.numeric
Data$Longitude %<>% as.numeric
Data$individual_count %<>% as.numeric

#----Summary
Data_sp.list <- 
  Data[, list(common_name_T_ori, 
              scientific_name_ori,
              accepted_name_code)] %>%
  .[, .(Count = .N),
    by = list(common_name_T_ori, scientific_name_ori, accepted_name_code)]

Data_Point.list <- 
  Data[, list(Longitude, Latitude)] %>%
  .[, .(Count = .N),
    by = list(Longitude, Latitude)]

Data_Time.list <- 
  Data[, list(Year, Month)] %>%
  .[, .(Count = .N),
    by = list(Year, Month)]


summary(Data_ori)

#----�[�` �ۦP���ظ�T�B�a�I�B�ɶ��B�ƥ󤧬���
Data %<>%
  .[ !is.na(individual_count) ] %>% 
  .[, individual_count := as.numeric(individual_count)] %>%
  .[ , .(individual_count = sum(individual_count )), 
     by = list(common_name_T_ori, scientific_name_ori, accepted_name_code, 
               Year, Month, 
               Longitude, Latitude,
               eventID, Project) ] %>%
  rbind(., Data[ is.na(individual_count) ]) %>% 
  unique 

#----Summary
Data_sp.list <- 
  Data[, list(common_name_T_ori, 
              scientific_name_ori,
              accepted_name_code)] %>%
  .[, .(Count = .N),
    by = list(common_name_T_ori, scientific_name_ori, accepted_name_code)]

Data_Point.list <- 
  Data[, list(Longitude, Latitude)] %>%
  .[, .(Count = .N),
    by = list(Longitude, Latitude)]

Data_Time.list <- 
  Data[, list(Year, Month)] %>%
  .[, .(Count = .N),
    by = list(Year, Month)]



# 
# Data %>%
#   .[, list(Project, accepted_name_code, individual_count)] %>%
#   .[, .(nRecord = .N,
#         nSpecies = ), by = list(Project)]


#### �R������~������
#---- load packages
library(data.table)
library(magrittr)
library(readxl)
library(ggplot2)
library(rgdal)
library(plyr)
library(dplyr)
library(raster)
library(rgeos)
library(RColorBrewer) #�t���brewer.pal( 9 , "Reds" )
library(ggpubr)
library(scales)


#---- Set environment
#--from
D2018 <- "D:/Chun/Analysis/RRR/Dataset"
#--Grid
Grid.dir <- "D:/Chun/Analysis/GIS/Taiwan/grid_system_tw_terrestrial_v20180511"
Grid.fname <- "g1km_epsg4326"
#--wd
wd <- file.path(paste0(D2018,"/analysis/Result"))
setwd(wd) 
##############################################��Ʒǳ�
#--��ƶפJ
All.data <- Data

####�R�������I��####

#--turn DF to SpatialPointDF
#epsg:4326>>WGS84
#epsg:3828>>TWD67
#epsg:3826>>TWD97

All.data %<>% .[!is.na(Longitude)] %>%
  .[!is.na(Latitude)]

coordinates(All.data) <- ~ Longitude + Latitude
proj4string(All.data) <- CRS("+init=epsg:4326") #epsg:4326>>WGS84

#--load polygon of Taiwan
Grid <- readOGR(Grid.dir, 
                Grid.fname,
                encoding="UTF-8", use_iconv=TRUE)
Grid %<>% spTransform(CRS("+init=epsg:4326"))
Grid@data %<>% setnames(.,"Id","GID") # change column name

#--�R������ select data in Taiwan only 
All.data.s <- 
  All.data[subset(Grid), ]

#--����g�n���
All.data.s@data <- 
  cbind(All.data.s@data, All.data.s@coords) %>%
  setDT 


Data_Grid <- All.data.s@data
# saveRDS(Data_Grid, file.path("Dataset_Grid range.rds"))
# fwrite(Data_Grid, 
#        file= file.path("Dataset_Grid range.txt"))

#----Summary
Data_sp.list <- 
  Data_Grid[, list(common_name_T_ori, 
              scientific_name_ori,
              accepted_name_code)] %>%
  .[, .(Count = .N),
    by = list(common_name_T_ori, scientific_name_ori, accepted_name_code)]

Data_Point.list <- 
  Data_Grid[, list(Longitude, Latitude)] %>%
  .[, .(Count = .N),
    by = list(Longitude, Latitude)]

Data_Time.list <- 
  Data_Grid[, list(Year, Month)] %>%
  .[, .(Count = .N),
    by = list(Year, Month)]

# ####�M�J����s�� spatial join ����ID��All.data####
# #--import data �פJ���� [Grid]
# Grid <- readOGR(Grid.dir, 
#                 Grid.fname,
#                 encoding="UTF-8", use_iconv=TRUE)
# Grid %<>% spTransform(CRS("+init=epsg:4326"))
# Grid@data %<>% setnames(.,"ID","GID")  # change column name
# 
# #--spatial join
# All.data.s$GID <- over(All.data.s, Grid[, "GID"])$GID
# All.data.s_geo <- All.data.s
# 
# #--�NAll.data.sSPDF��^DF
# All.data.s <- All.data.s@data %>%
#   setDT