library(data.table)
library(magrittr)
library(readxl)
library(ggplot2)
library(rgdal)
library(plyr)
library(dplyr)
library(raster)
library(rgeos)
library(RColorBrewer) #配色用brewer.pal( 9 , "Reds" )
library(ggpubr)
library(scales)



####常用字體####
windowsFonts(
       # 中文字体
       lishu = windowsFont(family = "LiSu"),            # 隸書
       yahei = windowsFont(family = "Microsoft YaHei"), # 微軟雅黑
       jhengHei = windowsFont(family = "Microsoft JhengHei"), # 微軟正黑
       xinwei = windowsFont(family = "STXingwei"),      # 華文新魏
       kaiti = windowsFont(family = "KaiTi"),           # 楷体
       heiti = windowsFont(family = "SimHei"),          # 黑体
       # 英文字体
       aTypeial = windowsFont(family = "ATypeial"),             # ATypeial字体
       calibTypei = windowsFont(family = "CalibTypei"),         # CalibTypei字体
       newman = windowsFont(family = "Times New Roman"),  #Times New Roman字体   
       hand = windowsFont(family = "Lucida Calligraphy"), # Lucida手寫体
       Helvetica = windowsFont(family = "Helvetica")      # 印刷体
       )

####環境設訂####
#--from
D2018 <- "D:/Chun/Analysis/RRR/Dataset"
#--Grid
Grid.dir <- "D:/Chun/Analysis/GIS/Taiwan/grid_system_tw_terrestrial_v20180511/g_3826_0_2200000_s"
Grid.fname <- "g1km_3826_0_2200000_s"
#--Dem
Dem.dir <- "D:/Chun/Analysis/GIS/Taiwan/DEM_20m"
Dem.fname <- "dem_20m.tif"
#--wd
wd <- file.path(paste0(D2018,"/analysis/Result"))
setwd(wd) 
##############################################資料準備
####檔案準備####
#--資料匯入
All.data <-
  get(load(file.path(D2018,"data_coverage_20180417.RData"))) #raw data

##--native species
#All.data %<>%
#  .[is_alien == "0"] #native species
#save(All.data, 
#     file= file.path(D2018,"data_coverage_native.RData"))

All.data <-
  get(load(file.path(D2018,"data_coverage_native.RData")))  #native species data



####分割類群 each class####
All.data_Aves <- All.data %>%
  .[class == "Aves"] 
All.data_Mammalia <- All.data %>%
  .[class == "Mammalia"]
All.data_Reptilia <- All.data %>%
  .[class == "Reptilia"]
All.data_Amphibia <- All.data %>%
  .[class == "Amphibia"]
All.data %$% dim(.)[1]
All.data_Amphibia %$% dim(.)[1]
All.data_Reptilia %$% dim(.)[1]
All.data_Aves %$% dim(.)[1]
All.data_Mammalia %$% dim(.)[1]

All.data$scientific_name %>% uniqueN
All.data_Amphibia$scientific_name %>% uniqueN
All.data_Reptilia$scientific_name %>% uniqueN
All.data_Aves$scientific_name %>% uniqueN
All.data_Mammalia$scientific_name %>% uniqueN

fwrite(All.data_Aves, 
     file= file.path(D2018,"All.data_Aves.csv"))
fwrite(All.data_Mammalia, 
     file= file.path(D2018,"All.data_Mammalia.csv"))
fwrite(All.data_Reptilia, 
     file= file.path(D2018,"All.data_Reptilia.csv"))
fwrite(All.data_Amphibia, 
     file= file.path(D2018,"All.data_Amphibia.csv"))



####刪除跳海點位####

#--turn DF to SpatialPointDF
  #epsg:4326>>WGS84
  #epsg:3828>>TWD67
  #epsg:3826>>TWD97

coordinates(All.data) <- ~ Longitude + Latitude
proj4string(All.data) <- CRS("+init=epsg:4326") #epsg:4326>>WGS84

#--load polygon of Taiwan
Grid <- readOGR(Grid.dir, 
                Grid.fname,
                encoding="UTF-8", use_iconv=TRUE)
Grid %<>% spTransform(CRS("+init=epsg:4326"))
Grid@data %<>% setnames(.,"Id","GID") # change column name

#--刪除跳海 select data in Taiwan only 
All.data.s <- 
  All.data[subset(Grid), ]

#--抓取經緯資料
All.data.s@data <- 
  cbind(All.data.s@data, All.data.s@coords) %>%
  setDT 

####套入網格編號 spatial join 網格ID到All.data####
#--import data 匯入網格 [Grid]
Grid <- readOGR(Grid.dir, 
                Grid.fname,
                encoding="UTF-8", use_iconv=TRUE)
Grid %<>% spTransform(CRS("+init=epsg:4326"))
Grid@data %<>% setnames(.,"ID","GID")  # change column name

#--spatial join
All.data.s$GID <- over(All.data.s, Grid[, "GID"])$GID
All.data.s_geo <- All.data.s

#--將All.data.sSPDF轉回DF
All.data.s <- All.data.s@data %>%
  setDT

##############################################時空分布
######################################################
####4class 紀錄物種數/筆數的時間分布####
#--製表，4class 物種紀錄筆數、物種數量隨時間分布
data_yr <- All.data.s %>%  
  .[, list(accepted_name_code, class, scientific_name, Year)] %>%
  .[, .(nRecord = .N, nSpecies = uniqueN(accepted_name_code)), by = list(Year, class)] %>%
  dcast(Year ~ class, value.var = c("nRecord", "nSpecies")) %>%
  .[, nRecord_Total:= rowSums(.[ ,2:5], na.rm = TRUE)] %>%
  .[, nSpecies_Total:= rowSums(.[ ,6:9], na.rm = TRUE)] %>%
  .[, c("Year",
        "nRecord_Total", "nRecord_Amphibia", "nRecord_Reptilia", "nRecord_Aves", "nRecord_Mammalia",
        "nSpecies_Total", "nSpecies_Amphibia", "nSpecies_Reptilia", "nSpecies_Aves", "nSpecies_Mammalia")]
  
fwrite(data_yr,file = "data_year.csv")

# data_yr<- fread("data_year.csv")
#--製圖，4class 物種紀錄筆數、物種數量隨時間分布

class.list <- cbind(class=c("Total", "Amphibia", "Reptilia", "Aves", "Mammalia"),
                    c(872, 35, 90, 659, 88),
                    c(79, 10, 22, 46, 1)) %>%
  data.table %>%
  setnames(c("class", "native", "invasive"))
class.list$native %<>% as.numeric
class.list$invasive %<>% as.numeric

i=1

##--製圖function
class.plot <- 
  function(i, data_yr){
    C <- class.list[i, class]
    N <- as.numeric(class.list[i,"native"])
    
    data_yr_class <- data_yr %$%
      cbind(.[,"Year"], 
            .[, colnames(data_yr) %like% C, with = FALSE]) %>%
      setnames(gsub(paste0("_",C), "", colnames(.)))
    
    # for hline
    scale_y <- 
      (max(data_yr_class[,"nRecord"], na.rm = TRUE)/ max(data_yr_class[,"nSpecies"], na.rm = TRUE))
    # disable scientific notation
    nsci <- format_format(big.mark = ",", scientific = FALSE)
    # plot 
    plot.list <- 
      ggplot() +
      geom_bar(data = data_yr_class,
               aes(x=Year, y=nRecord*1/scale_y),
               stat = "identity", fill = "#7491A6") +
      geom_line(data = data_yr_class, 
                aes(x=Year, y=nSpecies), size = 0.6) +
      geom_point(data = data_yr_class, 
                 aes(x=Year, y=nSpecies), size=1.5) +
      geom_hline(aes(yintercept = N), 
                 colour= "#CC0000" , linetype="dashed", size = 1) + #添加水平???
      theme_bw() +
      scale_x_continuous(limits = c(1970,2020)) +
      scale_y_continuous(name = "Richness", 
                         sec.axis = sec_axis(~.*scale_y, name = "Data amount", labels = nsci),
                         labels = nsci) +
      ggtitle(paste(C)) +
      theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 12),
            axis.title = element_text(face = "bold", #字體("plain", "italic", "bold", "bold.italic")
                                      #colour = "red", #字體顏色
                                      size = 14),
            axis.text.y.right = element_text(size=12, face="plain", color = "#7491A6"),
            axis.title.y.right = element_text(size=14, face="bold", color = "#7491A6"),
            plot.title = element_text(colour = "black", face = "bold", size = 22, vjust = 1),
            plot.margin = unit(c(0.2,0.5,0.1,0.5), "cm")) 
    return(plot.list)
  }

#--plot
p <- lapply(1:dim(class.list)[1], function(x)
  class.plot(x, data_yr))
# align multiple ggplot2 graphs
main <- ggarrange(plotlist = p,
                  nrow = length(p), align = "v") #align = c("none", "h", "v", "hv")

ggsave(paste("Class","year.png",sep="_"),
       plot= main,
       width = 11, height = 12, dpi = 500)

ggsave(paste("Total","year.png",sep="_"),
       plot= p[[1]],
       width = 11, height = 4, dpi = 500)


####4class 物種紀錄筆數/物種數量於空間上的分布####
#--Prepare data
##-- 資料合計，紀錄筆數、物種數，網格ID ~ 分類群
data_GID <- All.data.s %>%  
  .[, list(accepted_name_code, class, scientific_name, GID)] %>%
  .[, .(nRecord = .N, nSpecies = uniqueN(accepted_name_code)), by = list(GID, class)] %>%
  dcast(GID ~ class, value.var = c("nRecord", "nSpecies")) %>%
  .[, nRecord_Total:= rowSums(.[ ,2:5], na.rm = TRUE)] %>%
  .[, nSpecies_Total:= rowSums(.[ ,6:9], na.rm = TRUE)] %>%
  .[, c("GID",
        "nRecord_Total", "nRecord_Amphibia", "nRecord_Reptilia", "nRecord_Aves", "nRecord_Mammalia",
        "nSpecies_Total", "nSpecies_Amphibia", "nSpecies_Reptilia", "nSpecies_Aves", "nSpecies_Mammalia")]

fwrite(data_GID,file = "data_GID.csv") 
# data_GID <- fread(file = "data_GID.csv") 

#--將每個網格各分類群的紀錄總數資料加入網格圖層
Grid@data %<>% left_join(., data_GID)

#--將新的網格圖層存成shapefile
writeOGR(Grid, 
         wd,
         "nPoint_class",
         overwrite_layer=TRUE, 
         driver = "ESRI Shapefile")


#--製圖，4class 紀錄物種數/筆數的空間分布
Group <- cbind(class=c("nRecord_Total", "nRecord_Amphibia", "nRecord_Reptilia", "nRecord_Aves", "nRecord_Mammalia",
                       "nSpecies_Total", "nSpecies_Amphibia", "nSpecies_Reptilia", "nSpecies_Aves", "nSpecies_Mammalia"),
               Gname= c("Number of total records", "Number of amphibian records", "Number of reptilia records", "Number of aves records", "Number of mammalia records", 
                        "Number of total species", "Number of amphibian species", "Number of reptilia species", "Number of aves species", "Number of mammalia species")) %>%
         data.table

proj4string(Grid) <- CRS("+init=epsg:4326")

  i=1



for(i in 1: dim(Group)[1])
{
  G <- Group[i,class]
  Gname <- Group[i,Gname]
  
  Gmap <- Grid
  #Gmap@data <- Grid@data %>%
  # as.data.table %>%
  # .[, c("GID", G), with = FALSE]
  
  fmap<-fortify(Grid, region = "GID") %>%
    setnames(.,"id","GID") %>%
    merge(., Grid@data, all.x = TRUE)
  
  map<- ggplot() +
        geom_polygon(data = fmap, 
                     aes(long, lat, group, fill = fmap[G])) +
        scale_fill_gradient(low="#bebebe", high="#000000", 
                            guide="colorbar",na.value="transparent",
                            limits = c(NA,NA))+
        labs(fill = NULL ) +
        theme(text = element_text(size = 50)) +
        coord_fixed() +
        theme_void()
  map
  
  ggsave(paste0(Gname,".png"),
         plot = map,
         width = 4.5, height = 6, dpi = 500,
         bg = "transparent")
}
######################################################
####XX製圖Doughnut chart####
#----- 製圖function: Doughnut chart
#' x      numeric vector for each slice
#' group  vector identifying the group for each slice
#' labels vector of labels for individual slices
#' col    colors for each group
#' radius radius for inner and outer pie (usually in [0,1])

#donuts <- function(x, group = 1, labels = NA, col = NULL, radius = c(.7, 1), 
#                   init.angle = if(clockwise) 90 else 0, main = NULL) {
#  group <- rep_len(group, length(x))
#  ug  <- unique(group)
#  tbl <- table(group)[order(ug)]
#  
#  col <- if (is.null(col))
#    seq_along(ug) else rep_len(col, length(ug))
#  col.main <- Map(rep, col[seq_along(tbl)], tbl)
#  col.sub  <- lapply(col.main, function(x) {
#    al <- head(seq(0, 1, length.out = length(x) + 2L)[-1L], -1L)
#    VectoTypeize(adjustcolor)(x, alpha.f = al)
#  })
#  
#  plot.new()
#  
#  par(new = TRUE)
#  pie(x, border = NA, radius = radius[2L],
#      col = unlist(col.sub), labels = labels, 
#      init.angle = init.angle, main = main, cex.main =2.5)
#  
#  par(new = TRUE)
#  pie(x, border = NA, radius = radius[1L],
#      col = unlist(col.main), labels = NA, 
#      init.angle = init.angle, main = NULL)
#}
 ###---plot donuts
#  for(f in 1:dim(class.list)[1])
#  {
#    C<- class.list[f,1]
#    i<- class.list[f,3] %>% as.numeric
#    Class <- list[[i]] 
#    nGrid_sum <- cbind(list[,c(1:3)],Class)
#    
#    
#    for (z in 1:dim(County)[1])
#    {
#      Co <- County[z,1]
#      nGrid_sum_E <- nGrid_sum[which(COUNTYNAME %in% Co[[1]]),2:4] %>%
#        .[,per:= round(as.numeric(Class)/sum(as.numeric(Class))*100,2)] %>%
#        arrange(., Survey) %>%
#        arrange(., road)
#      
#      png(filename = paste0(D2018, "/analysis/pie_", C,"_", Co[[1]], ".png"),
#          width = 600, height = 480) 
#      
#      par()
#      with(nGrid_sum_E,
#           donuts(Class, road, spTypeintf('%s%%', per),
#                  col = c("#68d4b0","#a6a8a8"), radius = c(.65, .95),
#                  init.angle = 90 ))
#      title(Co[[1]], line = -12, cex.main=2.5)
#      
#      dev.off()
#    }
#  } 

#######依各保護區劃分 製表&製圖，4class 有無紀錄網格數
######################################################
  
########################################依各保護區劃分
####依各保護區計算網格比 export barplot####
#--import data 準備...
nGrid <- Grid@data  %>%
    setDT %>%
         .[,c("GID",
              "COUNTY", "natpark", "natreserve", "natprotect", "wlrefuge", "wlhabitat", "wetland", "wetllevel", "iba",
              "nRecord_Total", "nRecord_Amphibia", "nRecord_Reptilia", "nRecord_Aves", "nRecord_Mammalia",
              "nSpecies_Total", "nSpecies_Amphibia", "nSpecies_Reptilia", "nSpecies_Aves", "nSpecies_Mammalia")] %>%
         setnames(., c("COUNTYNAME"),c("County")) %>%
         setDT

save(nGrid, 
     file= file.path(D2018,"nGrid.RData"))
# get(load(file.path(D2018, "nGrid.RData")))
  
#--each reserve
nGrid_N<-nGrid %>%
  .[,c("GID", 
       "County", "natpark", "natreserve", "natprotect", "wlrefuge", "wlhabitat", "wetland", "wetllevel",  "iba",
       "nSpecies_Total", "nSpecies_Amphibia", "nSpecies_Reptilia", "nSpecies_Aves", "nSpecies_Mammalia")] %>%
  setnames(gsub(paste0("nSpecies_"), "", colnames(.)))

#--easy way :
x<-nGrid_N %>%
  .[,c("Total", "Amphibia", "Reptilia", "Aves", "Mammalia")]
x[is.na(x)] <- "no survey"
x[x != "no survey" ] <- "survey"
nGrid_N[,c("Total", "Amphibia", "Reptilia", "Aves", "Mammalia")] <- x
remove(x)

#--class
class.list <- c("Total", "Amphibia", "Reptilia", "Aves", "Mammalia")
type <- cbind(c("County", "natpark", "natreserve", "natprotect", "wlrefuge", "wlhabitat", "wetland", "wetllevel",  "iba"),
              c("縣市", "國家公園", "自然保留區", "自然保護區", "野生動物保護區", "野生動物重要棲息環境", "重要濕地", "濕地等級", "重要野鳥棲息地"))

i=1
f=1
z=1

#--export barplot
for(i in 1:dim(type)[1])
{ 
  Typei <- type[i,1]
  Type.name <- type[i,2]
  
  T <- nGrid_N %$% 
       .[,c(Typei, "Total", "Amphibia", "Reptilia", "Aves", "Mammalia"), with = FALSE] %>%
       na.omit
    
  
  for(f in 1:length(class.list))
  {
    C<- class.list[f] #class
      
    TClass <- T %$%　
                .[, c(Typei, C ), with = FALSE]
      
    nGrid_sum <- TClass %>% 
                 .[, .(Count = .N), by = c(colnames(TClass))] %>%
                 setnames(.,c("Typei", "Survey", "Ci")) %>%
                 .[, sum:= sum(Ci), by = Typei] %>%
                 .[, per:= as.numeric(Ci)/as.numeric(sum)*100] %>% 
                 setDT %>%
                 .[order(per, decreasing = TRUE)] %>%
                 .[order(Survey, decreasing = TRUE)] %>%
                 .[order(Typei, decreasing = TRUE)] %>%
                 #.[, label_y:= cumsum(per), by=list(Typei)] %>%
                 .[, sum:= NULL] %>%
                 .[Survey=="survey", label_y:= as.numeric(19)] %>% #標籤位置
                 .[Survey!="survey", label_y:= as.numeric(99)] #標籤位置
                   
    nGrid_sum <-nGrid_sum[Survey=="survey"] %$%
                .[order(per, decreasing = TRUE)] %$%
                .[, list(Typei, TypeiOrder=per)] %$%
                left_join(nGrid_sum, .) %>% 
                setDT %>% 
                .[is.na(TypeiOrder), TypeiOrder:= as.numeric(0)] %>%
                .[order(Survey, decreasing = TRUE)] %>%
                .[order(TypeiOrder, decreasing = TRUE)]
  
    H <- nGrid_sum$Typei %>% unique %>% length #估算圖表高
    W <- nGrid_sum$Typei %>% as.vector %>% nchar() %>% max #估算圖表長
         
    #plot summary barplot
    
    p_Typei_C<-
        ggplot(data=nGrid_sum, aes(x=reorder(Typei, TypeiOrder) , y=per, fill=Survey), position = "fill")+
        geom_bar(stat = "identity",width=0.8)+
        geom_text(aes(y = label_y, label = paste(sprintf("%0.1f", round(per, digits = 1)),"%")), hjust = 1, vjust=0.5,  colour = "White",size=2.5)+
        labs(title = NULL, x = "", y = "Number of Grids (%)", fill="Survey")+
        coord_flip()+
        theme_classic()+
        #scale_fill_grey()+ #灰階
        theme(
              #plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
              legend.key.size = unit(0.4, "cm"),
              #legend.position="none", #remove legend
              text = element_text(color="black", size=10, face="plain", family =  "jhengHei"),
              axis.text = element_text(color="black", size=10, face="plain"),
              axis.title = element_text(color="black", size=12, face="bold", vjust=1),
              plot.title = element_text(color="black", size=14, face="bold", hjust=0.5, vjust=1.5)
              )
    p_Typei_C
    
    ggsave(paste0(Type.name,"_",C,"_Barplot.png"),
           plot = p_Typei_C,
           width = 10+W*0.271, 
           height = 2.25+H*0.5, 
           dpi = 500, 
           units = "cm",
           bg = "transparent")
    
    remove(TClass,nGrid_sum)
    }
    remove(T)
}

####依各區域畫出調查網格(有/無) export map & 依各區域輸出調查網格(有/無) export table####
# Grid <- readOGR(Grid.dir, 
#                 Grid.fname,
#                encoding="UTF-8", use_iconv=TRUE)
# Grid %<>% spTransform(CRS("+init=epsg:4326"))
# Grid@data %<>% setnames(.,"ID","GID") # change column name
# data_GID <- fread(file = "data_GID.csv") 
# Grid@data %<>% left_join(., data_GID)
# get(load(file.path(D2018, "nGrid.RData")))

####---each reserve
##Survey or not##
nGrid_N<-nGrid %>%
  .[,c("GID", 
       "County", "natpark", "natreserve", "natprotect", "wlrefuge", "wlhabitat", "wetland", "wetllevel",  "iba",
       "nSpecies_Total", "nSpecies_Amphibia", "nSpecies_Reptilia", "nSpecies_Aves", "nSpecies_Mammalia")] %>%
  setnames(gsub(paste0("nSpecies_"), "", colnames(.)))

#--easy way :
x<-nGrid_N %>%
  .[,c("Total", "Amphibia", "Reptilia", "Aves", "Mammalia")]
x[is.na(x)] <- as.numeric(0)
x[x != "0" ] <- as.numeric(1)
nGrid_N[,c("Total", "Amphibia", "Reptilia", "Aves", "Mammalia")] <- x
remove(x)

Grid_map<-Grid
Grid_map@data<-nGrid_N

i=1
f=1

#--list class
class.list <- c("Total", "Amphibia", "Reptilia", "Aves", "Mammalia")
type <- cbind(c("County", "natpark", "natreserve", "natprotect", "wlrefuge", "wlhabitat", "wetland", "wetllevel",  "iba"),
              c("縣市", "國家公園", "自然保留區", "自然保護區", "野生動物保護區", "野生動物重要棲息環境", "重要濕地", "濕地等級", "重要野鳥棲息地"))

sd.cols = c("Total", "Amphibia", "Reptilia", "Aves", "Mammalia")

#--Survey or not map
fmap <- fortify(Grid_map, region="GID") %>%
  merge(., Grid_map@data, by.x = "id", by.y = "GID", all.x = TRUE)

#--dissolve map to create background map
Bmap<- gUnaryUnion(Grid_map, id=Grid_map@data[,County]) %>%
  gUnaryUnion %>% 
  fortify

Typei_sum_table<- data.frame(NULL)
###--export table
for(i in 1:dim(type)[1])
{ 
  Typei <- type[i,1]
  Type.name <- type[i,2]

  Typeimap <- fmap %>% as.data.table %>%
    .[, c("id", "long", "lat", "order", "hole", "piece", "group",
          Typei ,
          "Total", "Amphibia", "Reptilia", "Aves", "Mammalia"), with = FALSE] %>%
    setnames(.,Typei ,"Class") %>%
    .[!is.na(Class)]

  #--計算各類別網格數
  Typei_count <-  nGrid_N %$% 
    .[, Typei, with = FALSE] %>%
    setnames(.,"Type") %>% 
    na.omit %>%
    .[, .(Count = .N), by = Type]  
  #--加總各類別調查網格數
  Typei_sum <-  nGrid_N %$% 
    .[, c(Typei, "Total", "Amphibia", "Reptilia", "Aves", "Mammalia"), with = FALSE] %>%
    setnames(.,Typei, "Type") %>% 
    na.omit %>%
    .[, lapply(.SD, function(x)sum(x)), by = Type, .SDcols = sd.cols] %>% 
    setnames(c("Type", "Total_nGrid_sum", "Amphibia_nGrid_sum", "Reptilia_nGrid_sum", "Aves_nGrid_sum", "Mammalia_nGrid_sum"))
  #--計算各類別調查網格比
  Typei_per <- nGrid_N %$% 
    .[,c(Typei, "Total", "Amphibia", "Reptilia", "Aves", "Mammalia"), with = FALSE] %>%
    setnames(.,Typei, "Type") %>%
    na.omit %>% 
    .[, lapply(.SD, function(x)sum(x)/length(x)*100), by = Type, .SDcols = sd.cols] %>% 
    setnames(c("Type", "Total_nGrid_per", "Amphibia_nGrid_per", "Reptilia_nGrid_per", "Aves_nGrid_per", "Mammalia_nGrid_per"))
  #--export table
  Typei_sum_all<- left_join(Typei_count, Typei_sum) %>%
    left_join(.,Typei_per) %>%
    setDT %>%
    .[, Class:= Type.name] %>%
    .[, c("Class", "Type", "Count", 
          "Total_nGrid_sum", "Total_nGrid_per", 
          "Amphibia_nGrid_sum", "Amphibia_nGrid_per", 
          "Reptilia_nGrid_sum", "Reptilia_nGrid_per",
          "Aves_nGrid_sum", "Aves_nGrid_per", 
          "Mammalia_nGrid_sum", "Mammalia_nGrid_per")]
  Typei_sum_table <- rbind(Typei_sum_table, Typei_sum_all)
  
  Lmap<- gUnaryUnion(Grid_map, id=Grid_map@data[[Typei]]) ##dissolve map to create label position
  row.names(Lmap) <- as.character(1:length(Lmap))  # make sure row names match
    
  #--And add the data back in
  ##--for label text
    
  Lmap <- SpatialPolygonsDataFrame(Lmap, Typei_sum) 
  Typei_sum <- Lmap@data %>% 
    as.data.table %>%  
    .[,long.label := c(coordinates(Lmap)[,1])] %>% # "coordinates" extracts centroids of the polygons, in the order listed at @data
    .[,lat.label := c(coordinates(Lmap)[,2])] 
   
  ##for label polygon
  
  Lmap <- fortify(Lmap, region="Type") %>%
    merge(., Typei_sum, by.x = "id", by.y = "Type", all.x = TRUE)
  

  
  for(f in 1:length(class.list))
  {
    C<- class.list[f] #class
    
    map<- ggplot() +
      geom_polygon(data = Bmap, 
                   aes(long, lat,  group), fill = "black", alpha=0.15) +
      geom_polygon(data = Typeimap, 
                         aes(long, lat,  group , fill = as.factor(Typeimap[[8+f]]))) + 
      scale_fill_manual(labels = NULL,values=c("#FF7484", "#878787"))+
      geom_polygon(data = Lmap, 
                   aes(long, lat,  group), fill = NA, color = "black", size= 0.3) +      
      #geom_text(data= Typei_sum, aes(label = paste(round(Typei_sum[[C]], digits = 0),"%"),
      #          x = long.label, y = lat.label), size= 3, colour="#474747") +
      coord_fixed() +
      theme_void()+ 
      labs(title = C, fill = NULL ) +
      theme(text = element_text(size = 14), 
            title = element_text(color="black", size=20, face="bold", hjust = 1, vjust = 0.5 ),
            #legend.key.size = unit(0, "cm")
            legend.position="none"
            )
     map
     
    ggsave(paste0(Type.name,"_",C,"_Map.png"),
           plot = map,
           width = 4.5, height = 6, dpi = 500,
           bg = "transparent"
           )
    } 
  }
fwrite(Typei_sum_table,"extract range_sum_table.csv")



######################################################


##########################################沿海拔的分布
####4class 紀錄物種數/筆數的海拔分布####
#--import data
#--匯入海拔資訊20公尺DEM [dem]
dem <- raster(file.path(Dem.dir, Dem.fname))
#--將Final的作標系統轉換成與dem相同
dem %<>% spTransform(CRS(proj4string(Grid)))
crs(dem) %<>% spTransform(CRS(proj4string(Grid)))

#--將Final的作標系統轉換成與dem相同
All.data.s_geo %<>% spTransform(CRS(proj4string(dem)))
#--截取每個紀錄點位的海拔資訊 [Final.elev]
data_elev <- data.table(All.data.s_geo@data, 
             elevation = raster::extract(dem, All.data.s_geo@coords)) %>%
  .[, elev.c := cut(elevation, seq(-100, 4000, 100),
                    right=TRUE, labels=seq(-50, 3950, 100))] %>% #cut elevation by 50m
  .[, elev.c := as.numeric(as.character(elev.c))] %>%
  .[, list(accepted_name_code, class, scientific_name, elev.c)] %>%
  .[, .(nRecord = .N, nSpecies = uniqueN(accepted_name_code)), by = list(elev.c, class)] %>%
  dcast(elev.c ~ class, value.var = c("nRecord", "nSpecies")) %>%
  .[, nRecord_Total:= rowSums(.[ ,2:5], na.rm = TRUE)] %>%
  .[, nSpecies_Total:= rowSums(.[ ,6:9], na.rm = TRUE)] %>%
  .[, c("elev.c",
        "nRecord_Total", "nRecord_Amphibia", "nRecord_Reptilia", "nRecord_Aves", "nRecord_Mammalia",
        "nSpecies_Total", "nSpecies_Amphibia", "nSpecies_Reptilia", "nSpecies_Aves", "nSpecies_Mammalia")]

fwrite(data_elev,file = "data_elev.csv")

# data_elev<- fread("data_elev.csv")

##--製圖，4class 物種紀錄筆數/物種數量沿海拔的分布
i=1

class.list <- c("Total", "Amphibia", "Reptilia", "Aves", "Mammalia")

##--製圖function
class.plot <- 
  function(i, data_elev){
    C<- class.list[i] #class
    
    data_elev_class <- data_elev %$%
      cbind(.[,Elevation := elev.c], 
            .[, colnames(data_elev) %like% C, with = FALSE]) %>%
      
      setnames(gsub(paste0("_",C), "", colnames(.)))
    
    # for second y
    scale_y <- 
      (max(data_elev_class[,"nRecord"], na.rm = TRUE)/ max(data_elev_class[,"nSpecies"], na.rm = TRUE))
    
    # disable scientific notation
    nsci <- format_format(big.mark = ",", scientific = FALSE)
    # plot 
    plot.list <- 
      ggplot() +
      geom_bar(data = data_elev_class,
               aes(x=Elevation, y=nRecord*1/scale_y),
               stat = "identity", fill = "#7491A6") +
      geom_line(data = data_elev_class, 
                aes(x=Elevation, y=nSpecies), size = 0.6) +
      geom_point(data = data_elev_class, 
                 aes(x=Elevation, y=nSpecies), size=1.5) +
      theme_bw() +
      #scale_x_continuous(limits = c(1970,2020)) +
      scale_y_continuous(name = "Richness", 
                         sec.axis = sec_axis(~.*scale_y, name = "Data amount", labels = nsci),
                         labels = nsci) +
      ggtitle(paste(C)) +
      theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 12),
            axis.title = element_text(face = "bold", #字體("plain", "italic", "bold", "bold.italic")
                                      #colour = "red", #字體顏色
                                      size = 14),
            axis.text.y.right = element_text(size=12, face="plain", color = "#7491A6"),
            axis.title.y.right = element_text(size=14, face="bold", color = "#7491A6"),
            plot.title = element_text(colour = "black", face = "bold", size = 22, vjust = 1),
            plot.margin = unit(c(0.2,0.5,0.1,0.5), "cm")) 
    return(plot.list)
  }

#--plot
p <- lapply(1:length(class.list), function(x)
  class.plot(x, data_elev))
# align multiple ggplot2 graphs
main <- ggarrange(plotlist = p,
                  nrow = length(p), align = "v") #align = c("none", "h", "v", "hv")

ggsave(paste("Class","Elevation.png",sep="_"),
       plot= main,
       width = 11, height = 12, dpi = 500)

ggsave(paste("Total","Elevation.png",sep="_"),
       plot= p[[1]],
       width = 11, height = 4, dpi = 500)

####4class 紀錄物種數/筆數的海拔分布(面積校正)####
#--import data
#--匯入海拔資訊20公尺DEM [dem]
dem <- raster(file.path(Dem.dir, Dem.fname))

#--將Final的作標系統轉換成與dem相同
Grid %<>% spTransform(CRS(proj4string(dem)))

#--截取每個紀錄點位的海拔資訊 [Final.elev]
sd.cols = c( "nRecord_Total", "nRecord_Amphibia", "nRecord_Reptilia", "nRecord_Aves", "nRecord_Mammalia",
             "nSpecies_Total", "nSpecies_Amphibia", "nSpecies_Reptilia", "nSpecies_Aves", "nSpecies_Mammalia")

data_elev <- data.table(Grid@data, 
                        elevation = raster::extract(dem, Grid)) %>%
  .[, elev.c := cut(elevation, seq(-100, 4000, 100),
                    right=TRUE, labels=seq(-50, 3950, 100))] %>% #cut elevation by 50m
  .[, elev.c := as.numeric(as.character(elev.c))] %>%
  .[,  c("elev.c",
         "nRecord_Total", "nRecord_Amphibia", "nRecord_Reptilia", "nRecord_Aves", "nRecord_Mammalia",
         "nSpecies_Total", "nSpecies_Amphibia", "nSpecies_Reptilia", "nSpecies_Aves", "nSpecies_Mammalia")] %>%
  .[, lapply(.SD, function(x)sum(x)/length(x)/25), by = elev.c, .SDcols = sd.cols] %>%
  .[, Count := .N, by = Type]

.[, .(nRecord = .N, nSpecies = uniqueN(accepted_name_code)), by = list(elev.c, class)] %>%
  dcast(elev.c ~ class, value.var = c("nRecord", "nSpecies")) %>%
  .[, nRecord_Total:= rowSums(.[ ,2:5], na.rm = TRUE)] %>%
  .[, nSpecies_Total:= rowSums(.[ ,6:9], na.rm = TRUE)] %>%
  .[, c("elev.c",
        "nRecord_Total", "nRecord_Amphibia", "nRecord_Reptilia", "nRecord_Aves", "nRecord_Mammalia",
        "nSpecies_Total", "nSpecies_Amphibia", "nSpecies_Reptilia", "nSpecies_Aves", "nSpecies_Mammalia")]

fwrite(data_elev,file = "data_elev.csv")

# data_elev<- fread("data_elev.csv")
####製圖，4class 物種紀錄筆數/物種數量沿海拔的分布####
i=1

class.list <- c("Total", "Amphibia", "Reptilia", "Aves", "Mammalia")

##--製圖function
class.plot <- 
  function(i, data_elev){
    C<- class.list[i] #class
    
    data_elev_class <- data_elev %$%
      cbind(.[,Elevation := elev.c], 
            .[, colnames(data_elev) %like% C, with = FALSE]) %>%
      
      setnames(gsub(paste0("_",C), "", colnames(.)))
    
    # for second y
    scale_y <- 
      (max(data_elev_class[,"nRecord"], na.rm = TRUE)/ max(data_elev_class[,"nSpecies"], na.rm = TRUE))
    
    # disable scientific notation
    nsci <- format_format(big.mark = ",", scientific = FALSE)
    # plot 
    plot.list <- 
      ggplot() +
      geom_bar(data = data_elev_class,
               aes(x=Elevation, y=nRecord*1/scale_y),
               stat = "identity", fill = "#7491A6") +
      geom_line(data = data_elev_class, 
                aes(x=Elevation, y=nSpecies), size = 0.6) +
      geom_point(data = data_elev_class, 
                 aes(x=Elevation, y=nSpecies), size=1.5) +
      theme_bw() +
      #scale_x_continuous(limits = c(1970,2020)) +
      scale_y_continuous(name = "Richness", 
                         sec.axis = sec_axis(~.*scale_y, name = "Data amount", labels = nsci),
                         labels = nsci) +
      ggtitle(paste(C)) +
      theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 12),
            axis.title = element_text(face = "bold", #字體("plain", "italic", "bold", "bold.italic")
                                      #colour = "red", #字體顏色
                                      size = 14),
            axis.text.y.right = element_text(size=12, face="plain", color = "#7491A6"),
            axis.title.y.right = element_text(size=14, face="bold", color = "#7491A6"),
            plot.title = element_text(colour = "black", face = "bold", size = 22, vjust = 1),
            plot.margin = unit(c(0.2,0.5,0.1,0.5), "cm")) 
    return(plot.list)
  }

#--plot
p <- lapply(1:length(class.list), function(x)
  class.plot(x, data_elev))
# align multiple ggplot2 graphs
main <- ggarrange(plotlist = p,
                  nrow = length(p), align = "v") #align = c("none", "h", "v", "hv")

ggsave(paste("Class","Elevation.png",sep="_"),
       plot= main,
       width = 11, height = 12, dpi = 500)

ggsave(paste("Total","Elevation.png",sep="_"),
       plot= p[[1]],
       width = 11, height = 4, dpi = 500)



######################################################

##--製圖function
p <- ggplot() +
  geom_histogram(data = data_elev[elevation != -999],
                 aes(elevation),
                 binwidth = 50) +
  facet_wrap(~class, ncol = 1, scale = "free_y") +
  labs(x = "Elevation", y = "Number of records") +
  theme_bw()
#-- 將圖片存成png
ggsave("nRecord_elevation.png",
       plot = p,
       path = file.path(D2018, "analysis"),
       width = 8, height = 6, dpi = 200)



sp.data.elev <- data.elev %>% 
                .[, list(accepted_name_code, common_name_T,
                         class_c, order_c, family_c, genus_c, 
                         scientific_name,
                         elev, 
                         elevation)] %>%
                unique
#-- ggplot製圖
p <- ggplot() +
  geom_histogram(data = sp.data.elev[elevation != -999],
                 aes(elevation),
                 binwidth = 50) +
  facet_wrap(~class_c, ncol = 1, scale = "free_y") +
  labs(x = "Elevation", y = "Number of records") +
  theme_bw()
#-- 將圖片存成png
ggsave("nSpecies_elevation.png",
       plot = p,
       path = file.path(D2018, "analysis"),
       width = 8, height = 6, dpi = 200)




#-- 截取網格中心點 [Grid.c]
Grid %<>% spTransform(CRS(proj4stTypeing(dem)))
Grid.c <- data.table(gCentroid(Grid, byid = TRUE)@coords,
                     Grid = Grid@data$Grid)
#-- 截取網格中心點的海拔，網格海拔為NA者填入0
Grid.c[, elev := raster::extract(dem, Grid.c[, c("x", "y")])] %>%
  .[is.na(elev), elev := 0]
#-- 分海拔段，L = ~1000，M = 1001~2500，H = 2501~
Grid.c[elev <= 1000, E.zone := "L"] %>%
  .[elev > 1000 & elev <= 2500, E.zone := "M"] %>%
  .[elev > 2500, E.zone := "H"]
#-- 合併物種調查資訊[Final.agg]跟網格資訊[Grid.c]
Grid.c %<>% Final.agg[., on = "Grid"]
#--
table(Grid.c[, E.zone]) # 全台網格數
table(Grid.c[!is.na(Amphibia), E.zone]) # 兩生綱網格數
table(Grid.c[!is.na(Reptilia), E.zone]) # 爬蟲綱網格數
table(Grid.c[!is.na(Mammalia), E.zone]) # 哺乳綱網格數
table(Grid.c[!is.na(Aves), E.zone]) # 鳥綱網格數
table(Grid.c[!is.na(Amphibia) | 
               !is.na(Reptilia) | 
               !is.na(Mammalia) |
               !is.na(Aves), E.zone]) # 所有有紀錄物種的網格數
#   H     L     M 
#1831 26079  9642 全台 
#  11  2040   204 兩生
#  23  4630   469 爬蟲
# 171  2084   797 哺乳
# 108  4577   550 鳥
# 218  8801  1359 所有物種

#C <- readOGR(C.dir, "COUNTY_MOI_1060525")
#All.data$COUNTYNAME <- over(All.data, C[, "COUNTYNAME"])$COUNTYNAME
