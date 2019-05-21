####CLEAN_SCRIPT.R

#README: This script is designed to Roughly Clean the Solar Analytics PV Data Set.
##Please note this will need to be undertaken with manual checking and should not be undertaken alone. 

#Read in PV Script or suggest re-running Join Script

setwd(paste0("~/GitHub/DER_Event_analysis/SolarAnalytics_analysis/output/",folder, "/",data.date))

##Find and Read in PV File
pv.file.name <- list.files(pattern="_PVData.csv")

if(exists("pv_data_set")){
  pv_data_set <- as.data.frame(pv_data_set)
  
  print("Using current PV Data Frame")
  
} else if(!is.empty(pv.file.name)){
  pv_data_set <- read.csv(pv.file.name, header=TRUE, stringsAsFactors = FALSE) %>% 
    mutate(ts = ymd_hms(ts), tz="Australia/Brisbane") 
  
  print("Opening PV CSV file in folder")
} else("Please run Join_Script.R there does not seem to be a PV File")


pv_data_set <- mutate(pv_data_set, ts= ymd_hms(ts, tz="Australia/Brisbane"))


#####Filter PV File based on Duration Data (NOTE there are some cases where this has already been calculated/provided  - working below assumes it does not)
temp.duration <- pv_data_set %>% 
  arrange(.,c_id, ts) %>% 
  mutate(duration = lead(ts)- ts) %>% 
  na.omit(duration) %>% 
  filter(duration>0) %>% 
  group_by(c_id) %>% 
  summarise(Avg_duration=median(duration))  

temp.file.duration <- temp.duration %>% 
  mutate(Avg_duration=round(Avg_duration, digits=0)) %>% 
  group_by(Avg_duration) %>% 
  summarise(count=n()) %>% 
  filter(count==max(count)) %>% 
  .[,c(1)] 

temp.duration.keep <- filter(temp.duration, Avg_duration<= (as.numeric(temp.file.duration$Avg_duration, units="secs") + seconds(20)) & Avg_duration>=(as.numeric(temp.file.duration$Avg_duration, units="secs") - seconds(20)))


temp.clean_duration <- mutate(pv_data_set, durn=temp.file.duration$Avg_duration)

if (as.numeric(temp.file.duration$Avg_duration, units="secs")==60){
  colnames(temp.clean_duration)[colnames(temp.clean_duration)=="power_kW_min"] <- "power_kW"
  colnames(temp.duration)[colnames(temp.duration)=="power_kW_min"] <- "power_kW"
  colnames(temp.duration.keep)[colnames(temp.duration.keep)=="power_kW_min"] <- "power_kW"
  colnames(pv_data_set)[colnames(pv_data_set)=="power_kW_min"] <- "power_kW"
  print("Using 1 minute duration Data")
} else if (as.numeric(temp.file.duration$Avg_duration, units="secs")==30){
  colnames(temp.clean_duration)[colnames(temp.clean_duration)=="power_kW_30sec"] <- "power_kW"
  colnames(temp.duration)[colnames(temp.duration)=="power_kW_30sec"] <- "power_kW"
  colnames(temp.duration.keep)[colnames(temp.duration.keep)=="power_kW_30sec"] <- "power_kW"
  colnames(pv_data_set)[colnames(pv_data_set)=="power_kW_30sec"] <- "power_kW"
  print("Using 30 second duration data")
} else (print("Insufficient Data to determine duration"))




temp.clean_duration <- left_join(temp.duration.keep, pv_data_set, by="c_id")

temp.duration.lost <- anti_join(temp.duration, temp.duration.keep,  by = c("c_id", "Avg_duration")) %>% 
  left_join(pv_data_set, by="c_id")

# 
# if (length(unique(temp.clean_3$c_id))>25){
#   
#   numberOutputs = ceiling(length(unique(temp.clean_3$c_id))/25)
#   
#   temp.unique.cids <- as.data.frame(unique(temp.clean_3$c_id))
#   names(temp.unique.cids) <- c("c_id")
#   
#   
#   for (i in (1:numberOutputs)){
#     temp.chart <- filter(temp.unique.cids, row_number()>=(i*25)-24 & row_number()<=i*25)
#     
#     temp.filtered.sites <- left_join(temp.chart, temp.clean_3, by = "c_id")
#     
#     P5b = ggplot(temp.filtered.sites, aes(ts, power_kW))+
#       geom_line()+
#       facet_wrap(~c_id, scales = "free_y")+
#       ggtitle(paste("List of all systems after data has been cleaned", i, "of", numberOutputs))
#     
#     ggsave(paste0(substr(pv.file.name, 1,15),"_Cleaned_Datapoints",i,"of",numberOutputs,".jpeg"), plot=P5b, device="jpeg")
#     
#     
#     temp.filter.time = temp.filtered.sites %>% 
#       filter(ts>=(EventTime-minutes(5)) & ts<=(EventTime+minutes(5)))
#     
#     P5c = ggplot(temp.filter.time, aes(ts, power_kW))+
#       geom_line()+
#       facet_wrap(~c_id, scales = "free_y")+
#       ggtitle(paste("List of all systems after data has been cleaned", i, "of", numberOutputs, "Zoom"))+
#       geom_vline(xintercept = EventTime, linetype="dashed")
#     # geom_vline(xintercept = ymd_hms(EventTime2, tz="Australia/Brisbane"), linetype="dashed")+
#     # geom_vline(xintercept = ymd_hms(EventTime3, tz="Australia/Brisbane"), linetype="dashed")
#     
#     ggsave(paste0(substr(pv.file.name, 1,15),"_Cleaned_Datapoints",i,"of",numberOutputs,"Zoom",".jpeg"), plot=P5c, device="jpeg")
#     
#   }




if(length(unique(temp.duration.lost$c_id))>0) {
  
  numberOutputs1 = ceiling(length(unique(temp.duration.lost$c_id))/25)
  
  temp.unique.cids1 <- as.data.frame(unique(temp.duration.lost$c_id))
  names(temp.unique.cids1) <- c("c_id")
  
  print(paste("Removed", length(unique(temp.duration.lost$c_id)), "systems due to Sampling duration"))
  
  for (iS in (1:numberOutputs1)){
     temp.chart1 <- filter(temp.unique.cids1, row_number()>=(iS*25)-24 & row_number()<=iS*25)
       
     temp.filtered.sites1 <- left_join(temp.chart1, temp.duration.lost, by = "c_id")
  
  P1 = ggplot(temp.filtered.sites1, aes(ts, power_kW))+
        geom_point()+
        facet_wrap(~c_id)+
        ggtitle(paste0("List of Systems that have been removed due to Sampling Duration",iS,"of",numberOutputs1))

  ggplot2::ggsave(paste0(substr(pv.file.name, 1,15),"_Removed_DataPoints",iS,"of",numberOutputs1,".jpeg"), plot=P1, device="jpeg", scale=1)
  
  
  }
  }else(print("No systems removed due to Sampling Duration"))
  



###Clean based on aggregate Power
temp.aggregate_p0 <- aggregate(temp.clean_duration$power_kW, 
                               by = list(c_id = temp.clean_duration$c_id), 
                               FUN = sum)
colnames(temp.aggregate_p0)[colnames(temp.aggregate_p0)=="x"] <- "power_aggregated"

#List out sites where their total power generated is low or less than or equal to zero
temp.unclean_p0 <- filter(temp.aggregate_p0, power_aggregated <=20)

temp.unclean <- left_join(temp.unclean_p0, temp.clean_duration, by="c_id")


if(length(unique(temp.unclean$c_id))>1){
  
  numberOutputs2 = ceiling(length(unique(temp.unclean$c_id))/25)
  
  temp.unique.cids2 <- as.data.frame(unique(temp.unclean$c_id))
  names(temp.unique.cids2) <- c("c_id")
  
  print(paste("Removed", length(unique(temp.unclean$c_id)), "systems due to negative or low data"))
  
  for (iN in (1:numberOutputs2)){
    temp.chart2 <- filter(temp.unique.cids2, row_number()>=(iN*25)-24 & row_number()<=iN*25)
    
    temp.filtered.sites2 <- left_join(temp.chart2, temp.unclean, by = "c_id")
  
 P2a = ggplot(temp.filtered.sites2, aes(ts, power_kW))+
    geom_line()+
    facet_wrap(~c_id, scales = "free_y")+
    ggtitle("List of Systems that have been removed from the data set as there is negative or low data")
 
  ggsave(paste0(substr(pv.file.name, 1,15),"_Removed_Negative",iN,"of",numberOutputs2,"jpeg"), plot=P2a, device="jpeg")
  }
    
  } else if(length(unique(temp.unclean$c_id))==1){
    P2b = ggplot(temp.unclean, aes(ts, power_kW))+
        geom_line()+
        ggtitle("One System has been removed from the data set as there is negative or low  data")
    ggsave(paste0(substr(pv.file.name, 1,15),"_Removed_Negative",".jpeg"), P2b, device="jpeg")
    
    print(paste("Removed one system due to negative or low data"))
    
} else(print("No systems removed from the data set for negative or no data"))

#Remove these sites from the data set
temp.clean_1 <- anti_join(temp.clean_duration, temp.unclean_p0, by = "c_id")

###Clean out if readings at night or jumps
maxdate <- max(temp.clean_1$ts)
mindate <- min(temp.clean_1$ts)

print(paste("Looking at Data from", mindate, "to", maxdate))

temp.clean_1_time<- temp.clean_1 %>% 
  mutate(Time = strftime(ts, format ="%T", usetz=TRUE))

temp.unclean_2a <- temp.clean_1_time %>% 
  filter(Time>='20:00:00' & Time<='23:59:00' & power_kW>10) 

temp.unclean_2b <- temp.clean_1_time %>% 
  filter(Time>'00:00:00' & Time<'04:00:00' & power_kW>10) 

temp.unclean_2 <- rbind(temp.unclean_2a, temp.unclean_2b)

temp.unclean_2_list <- as.data.frame(unique(temp.unclean_2$c_id))
names(temp.unclean_2_list) <- "c_id"

temp.unclean_2 <- left_join(temp.unclean_2_list, temp.clean_duration, by="c_id")

if(nrow(temp.unclean_2)>0) {
 P3 = ggplot(temp.unclean_2, aes(ts, power_kW))+
      geom_point()+
      facet_wrap(~c_id)+
      ggtitle("List of Systems that have been removed from the data set as there is data outside of daylight hours")

ggsave(paste0(substr(pv.file.name, 1,15),"_Removed_NightTime",".jpeg"), plot=P3, scale=1)
} else("No systems removed from the data set for data outside of daylight hours")

temp.clean_2 <- anti_join(temp.clean_1, temp.unclean_2_list, by="c_id")

###Clean out if missing data during the event
EventTime <- ymd_hms(EventTime, tz="Australia/Brisbane")


if (as.numeric(temp.file.duration$Avg_duration, units="secs")==60){
  temp.unclean_rows <- filter(temp.clean_2, ts>=(EventTime-minutes(5)) & ts<=(EventTime+minutes(5))) %>% 
    group_by(c_id) %>% 
    summarise(rows=n()) %>% 
    filter(rows<8)
  
  print(paste("Removed", length(unique(temp.unclean_rows$c_id)), "systems due to datapoints during the event"))
  
} else if (as.numeric(temp.file.duration$Avg_duration, units="secs")==30){
  temp.unclean_rows <- filter(temp.clean_2, ts>=(EventTime-minutes(5)) & ts<=(EventTime+minutes(5))) %>% 
    group_by(c_id) %>% 
    summarise(rows=n()) %>% 
    filter(rows>25 |rows<15)
  
  print(paste("Removed", length(unique(temp.unclean_rows$c_id)), "systems due to datapoints during the event"))
  
} else (print("Insufficient Data to determine if there are sufficent data points during the event"))



temp.unclean_3 <- left_join(temp.unclean_rows, temp.clean_duration, by="c_id")

if(nrow(temp.unclean_3)>0) {
P4 = ggplot(filter(temp.unclean_3,ts>=(EventTime-minutes(1)) & ts<=(EventTime+minutes(2))) , aes(ts, power_kW))+
  geom_point()+
  facet_wrap(~c_id)+
  ggtitle("List of Systems that have been removed from the data set as there too many or not enough data points during the event")

ggsave(paste0(substr(pv.file.name, 1,15),"_Removed_DataPoints",".jpeg"), plot=P4, device="jpeg")
} else("No systems removed from the data set for too many or not enough data points during the event")

temp.clean_3 <- anti_join(temp.clean_2, temp.unclean_3, by="c_id")


# ###NEEED TO MANUALLY ENTER THESEE C_IDs. 
# temp.visualcheck.remove <- c("117737")
# 
# temp.unclean_4 <- temp.clean_3 %>%
#   filter(c_id %in% temp.visualcheck.remove)
# 
# P6 = ggplot(temp.unclean_4, aes(ts, power_kW))+
#   geom_line()+
#   facet_wrap(~c_id, scales = "free_y")+
#   ggtitle("List of systems that have been removed following a visual check")
# 
# ggsave(paste0(substr(pv.file.name, 1,15),"_Cleaned_VisualCheck",".jpeg"), plot=P6, device="jpeg")
# 
# temp.clean_3 <- temp.clean_3 %>%
#   filter(!c_id %in% temp.visualcheck.remove)
# 
# 
# Final_clean <- temp.clean_3


if (length(unique(temp.clean_3$c_id))>25){
  
  numberOutputs = ceiling(length(unique(temp.clean_3$c_id))/25)
  
  temp.unique.cids <- as.data.frame(unique(temp.clean_3$c_id))
  names(temp.unique.cids) <- c("c_id")
  
  
  for (i in (1:numberOutputs)){
    temp.chart <- filter(temp.unique.cids, row_number()>=(i*25)-24 & row_number()<=i*25)
    
    temp.filtered.sites <- left_join(temp.chart, temp.clean_3, by = "c_id")
    
    P5b = ggplot(temp.filtered.sites, aes(ts, power_kW))+
          geom_line()+
          facet_wrap(~c_id, scales = "free_y")+
          ggtitle(paste("List of all systems after data has been cleaned", i, "of", numberOutputs))
    
    ggsave(paste0(substr(pv.file.name, 1,15),"_Cleaned_Datapoints",i,"of",numberOutputs,".jpeg"), plot=P5b, device="jpeg")
    
    
    temp.filter.time = temp.filtered.sites %>% 
      filter(ts>=(EventTime-minutes(5)) & ts<=(EventTime+minutes(5)))
    
    P5c = ggplot(temp.filter.time, aes(ts, power_kW))+
      geom_line()+
      facet_wrap(~c_id, scales = "free_y")+
      ggtitle(paste("List of all systems after data has been cleaned", i, "of", numberOutputs, "Zoom"))+
      geom_vline(xintercept = EventTime, linetype="dashed")
      # geom_vline(xintercept = ymd_hms(EventTime2, tz="Australia/Brisbane"), linetype="dashed")+
      # geom_vline(xintercept = ymd_hms(EventTime3, tz="Australia/Brisbane"), linetype="dashed")
      
    ggsave(paste0(substr(pv.file.name, 1,15),"_Cleaned_Datapoints",i,"of",numberOutputs,"Zoom",".jpeg"), plot=P5c, device="jpeg")
    
  }
  
} else if(length(unique(temp.clean_3$c_id))>0 & length(unique(temp.clean_3$c_id))<=25){
  P5a = ggplot(temp.clean_3, aes(ts, power_kW))+
    geom_line()+
    facet_wrap(~c_id, scales = "free")+
    ggtitle("List of all systems after data has been cleaned")
  
  ggsave(paste0(substr(pv.file.name, 1,15),"_Cleaned_Datapoints",".jpeg"), plot=P5a, device="jpeg")
  
  temp.filter.time = temp.clean_3 %>% 
    filter(ts>=(EventTime-minutes(5)) & ts<=(EventTime+minutes(5)))
  
  P5d = ggplot(temp.filter.time, aes(ts, power_kW))+
    geom_point()+
    facet_wrap(~c_id, scales = "free_y")+
    geom_vline(xintercept = EventTime, linetype="dashed")+
    ggtitle(paste("List of all systems after data has been cleaned", i, "of", numberOutputs, "Zoom"))
  
  ggsave(paste0(substr(pv.file.name, 1,15),"_Cleaned_Datapoints",i,"of",numberOutputs,"Zoom",".jpeg"), plot=P5d, device="jpeg")
  
} else (print("No clean files to plot"))


Final_clean <- temp.clean_3

file.name <- substr(max(unlist(strsplit(Actual_Data_file, "/"))),6,20)
write.csv(Final_clean, paste0(file.name, "_cleaned.csv"))

rm(list=ls(pattern="temp."))
