##### find how many systems we have 5 min and 5 second duration data for and clean it


#Read in PV Script 

setwd("~/GitHub/DER_Event_analysis/SolarAnalytics_analysis/output/PV_data_csv(uncleaned)")

##Find and Read in PV File
pv.file.name <- list.files(pattern="_PVData.csv")
# pv.file.name <- "4500_2017_12_09_PVData.csv"



for (f in pv.file.name){
  
  setwd("~/GitHub/DER_Event_analysis/SolarAnalytics_analysis/output/PV_data_csv(uncleaned)")
  
  data.date <- paste0(substr(f, 1, 15))
  
  ####

  pv_data_set <- read.csv(f, header=TRUE, stringsAsFactors = FALSE) %>% 
    mutate(ts = ymd_hms(ts), tz="Australia/Brisbane") 
  

pv_data_set <- mutate(pv_data_set, ts= ymd_hms(ts, tz="Australia/Brisbane"))

#######


temp.duration <- pv_data_set %>% 
  arrange(.,c_id, ts)%>%
  mutate(key=paste(c_id,ts,sep="_"))


########################################################
dir.create(file.path(paste0("~/GitHub/DER_Event_analysis/SolarAnalytics_analysis/output/Alternative_duration_analysis/",substr(f,1,15))))
setwd(paste0("~/GitHub/DER_Event_analysis/SolarAnalytics_analysis/output/Alternative_duration_analysis/",substr(f,1,15)))


temp.5m <- pv_data_set %>%
  arrange(.,c_id, ts) %>%
  mutate(duration = lead(ts)- ts) %>%
  na.omit(duration) %>%
  filter(duration<=310 & duration>=290)%>%
  arrange(c_id)

###
# 
# c_ids <- unique(temp.5m$c_id)
# 
# for (c in c_ids){
#   temp.plot <- temp.5m%>%
#     filter(c_id==c)
# 
#   ggplot(temp.plot, aes(ts, power_kW_30sec))+
#     geom_point()+
#     facet_wrap(~c_id)+
#     ggtitle("List of systems with 5 minute duration available")
# 
#   ggsave(paste("5_minute_systems_",c,".jpeg"))
# }


##### 5 second plots

temp.5s <- pv_data_set %>%
  arrange(.,c_id, ts) %>%
  mutate(duration = lead(ts)- ts) %>%
  na.omit(duration) %>%
  filter(duration<=6 & duration>=4) %>%
  arrange(c_id)
# 
# ###
# c_ids <- unique(temp.5s$c_id)
# 
# for (c in c_ids){
#   temp.plot <- temp.5s%>%
#     filter(c_id==c)
# 
#   
#   ggplot(temp.plot, aes(ts, power_kW_30sec))+
#     geom_point()+
#     facet_wrap(~c_id)+
#     ggtitle("List of systems with 5 second duration available (not cleaned)")
#   
#   ggsave(paste("5_second_systems_",c,".jpeg"))
# }
}

### manually remove systems with bad data
# 
# temp.5s.cleaned <- temp.5s%>%
#   filter(! c_id %in% c("158852331","1133700502"))
# 
# #
# #
# save(temp.5s.cleaned,file=paste0("cleaned_",substr(f,1,15),".R"))


### script end
