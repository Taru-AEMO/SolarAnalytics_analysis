##### find how many systems we have 5 min and 5 second duration data for


#Read in PV Script 

setwd("~/GitHub/DER_Event_analysis/SolarAnalytics_analysis/output/duration counts")

##Find and Read in PV File
pv.file.name <- list.files(pattern="_PVData.csv")
# pv.file.name <- "4551_2017_12_31_PVData.csv"
# pv.file.name <- c("4551_2016_12_21_PVData.csv", "4551_2017_01_12_PVData.csv",
#                   "4551_2017_02_15_PVData.csv", "4551_2017_02_28_PVData.csv", "4551_2017_03_12_PVData.csv",
#                   "4551_2017_04_01_PVData.csv", "4551_2017_12_31_PVData.csv", "4551_2018_01_01_PVData.csv",
#                   "4551_2018_01_19_PVData.csv", "4701_2018_08_13_PVData.csv")


for (f in pv.file.name){
  
  data.date <- paste0(substr(f, 1, 15))
  
  ####

  pv_data_set <- read.csv(f, header=TRUE, stringsAsFactors = FALSE) %>% 
    mutate(ts = ymd_hms(ts), tz="Australia/Brisbane") 
  

pv_data_set <- mutate(pv_data_set, ts= ymd_hms(ts, tz="Australia/Brisbane"))

#######



temp.duration <- pv_data_set %>% 
  arrange(.,c_id, ts)%>%
  mutate(key=paste(c_id,ts,sep="_"))
  
temp.unique <- unique(temp.duration$key)

####
print(paste("for ",data.date,"there are"))
print(paste("total nrows:",nrow(pv_data_set),""))
print(paste("unique nrows:",length(temp.unique),""))
print("----------------------------")


# #####Filter PV File based on Duration Data (NOTE there are some cases where this has already been calculated/provided  - working below assumes it does not)
# 
# #### 5 minute
# temp.duration.5m <- pv_data_set %>% 
#   arrange(.,c_id, ts) %>% 
#   mutate(duration = lead(ts)- ts) %>% 
#   na.omit(duration) %>% 
#   filter(duration<=310 & duration>=290) %>% 
#   group_by(c_id) %>% 
#   summarise(Avg_duration=median(duration))
# 
# 
# #### 5 second
# temp.duration.5s <- pv_data_set %>% 
#   arrange(.,c_id, ts) %>% 
#   mutate(duration = lead(ts)- ts) %>% 
#   na.omit(duration) %>% 
#   filter(duration<=6 & duration>=4) %>% 
#   group_by(c_id) %>% 
#   summarise(Avg_duration=median(duration))  
# 
# #### 30 second
# temp.duration.30s <- pv_data_set %>% 
#   arrange(.,c_id, ts) %>% 
#   mutate(duration = lead(ts)- ts) %>% 
#   na.omit(duration) %>% 
#   filter(duration<=35 & duration>=25) %>% 
#   group_by(c_id) %>% 
#   summarise(Avg_duration=median(duration))  
# 
# 
# #### 1 minute
# temp.duration.60s <- pv_data_set %>% 
#   arrange(.,c_id, ts) %>% 
#   mutate(duration = lead(ts)- ts) %>% 
#   na.omit(duration) %>% 
#   filter(duration<=68 & duration>=52) %>% 
#   group_by(c_id) %>% 
#   summarise(Avg_duration=median(duration))  
# 
# 
# #### prints
# print(paste("for ",data.date,"there are"))
# print(paste("", nrow(temp.duration.5s), "c_ids with 5 second duration"))
# print(paste("", nrow(temp.duration.5m), "c_ids with 5 minute duration"))
# print(paste("", nrow(temp.duration.30s), "c_ids with 30 second duration"))
# print(paste("", nrow(temp.duration.60s), "c_ids with 60 second duration"))
# print("----------------------------")

####
}

####
