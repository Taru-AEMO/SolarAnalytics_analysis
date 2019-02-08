####CLEAN_SCRIPT.R

#README: This script is designed to Roughly Clean the Solar Analytics PV Data Set.
##Please note this will need to be undertaken with manual checking and should not be undertaken alone. 

#Read in PV Script or suggest re-running Join Script

setwd(paste0("~/GitHub/DER_Event_analysis/SolarAnalytics_analysis/input/",folder))

##Find and Read in PV File
pv.file.name <- list.files(pattern="_PVData.csv")
pv_data_set <- read.csv(pv.file.name, header=TRUE, stringsAsFactors = FALSE) %>% 
  mutate(ts = ymd_hms(ts)) %>% 
  mutate(ts = with_tz(ts,"Australia/Brisbane")) 

## set to output directory
setwd(paste0("~/GitHub/DER_Event_analysis/SolarAnalytics_analysis/output/",folder))

###Clean based on aggregate Power
temp.aggregate_p0 <- aggregate(pv_data_set$power_kW_30sec, 
                               by = list(c_id = pv_data_set$c_id), 
                               FUN = sum)
colnames(temp.aggregate_p0)[colnames(temp.aggregate_p0)=="x"] <- "power_aggregated"

#List out sites where their total power generated is low or less than or equal to zero
temp.unclean_p0 <- filter(temp.aggregate_p0, power_aggregated <=500)

temp.unclean <- left_join(temp.unclean_p0, pv_data_set, by="c_id")

if(nrow(temp.unclean)>0) {
ggplot(temp.unclean, aes(ts, power_kW_30sec))+
  geom_line()+
  facet_wrap(~c_id)+
  ggtitle("List of Systems that have been removed from the data set as there is negative or no data")

ggsave(paste0(substr(pv.file.name, 1,15),"_Removed_Negative",".jpeg"), plot=last_plot(), scale=1)
} else(print("No systems removed from the data set for negative or no data"))

#Remove these sites from the data set
temp.clean_1 <- anti_join(pv_data_set, temp.unclean_p0, by = "c_id")

###Clean out if readings at night or jumps

temp.unclean_2 <- temp.clean_1 %>% 
  mutate(Time = strftime(ts, format ="%H:%M:%S")) %>% 
  filter(Time> '19:00:00' | Time< '04:00:00') %>% 
  filter(power_kW_30sec>10)

temp.unclean_2_list <- as.data.frame(unique(temp.unclean_2$c_id))
names(temp.unclean_2_list) <- "c_id"

temp.unclean_2 <- left_join(temp.unclean_2_list, pv_data_set, by="c_id")

if(nrow(temp.unclean_2)>0) {
  ggplot(temp.unclean_2, aes(ts, power_kW_30sec))+
  geom_line()+
  facet_wrap(~c_id)+
  ggtitle("List of Systems that have been removed from the data set as there is data outside of daylight hours")

ggsave(paste0(substr(pv.file.name, 1,15),"_Removed_NightTime",".jpeg"), plot=last_plot(), scale=1)
} else("No systems removed from the data set for data outside of daylight hours")

temp.clean_2 <- anti_join(temp.clean_1, temp.unclean_2_list, by="c_id")

###Clean out if missing data during the event
EventTime <- ymd_hms(EventTime, tz="Australia/Brisbane")

temp.unclean_rows <- filter(temp.clean_2, ts>=(EventTime-minutes(5)) & ts<=(EventTime+minutes(5))) %>% 
  group_by(c_id) %>% 
  summarise(rows=n()) %>% 
  filter(rows!=20)

temp.unclean_3 <- left_join(temp.unclean_rows, pv_data_set, by="c_id")

if(nrow(temp.unclean_3)>0) {
ggplot(filter(temp.unclean_3,ts>=(EventTime-minutes(5)) & ts<=(EventTime+minutes(5))) , aes(ts, power_kW_30sec))+
  geom_line()+
  facet_wrap(~c_id)+
  ggtitle("List of Systems that have been removed from the data set as there too many or not enough data points during the event")

ggsave(paste0(substr(pv.file.name, 1,15),"_Removed_DataPoints",".jpeg"), plot=last_plot(), scale=1)
} else("No systems removed from the data set for too many or not enough data points during the event")

temp.clean_3 <- anti_join(temp.clean_2, temp.unclean_3, by="c_id")


ggplot(temp.clean_3, aes(ts, power_kW_30sec))+
  geom_line()+
  facet_wrap(~c_id)+
  ggtitle("List of all systems after data has been cleaned")

ggsave(paste0(subst(pv.file.name, 1,15),"_Cleaned_Datapoints",".jpeg"), plot=last_plot(), scale=1)

Final_clean <- temp.clean_3

file.name <- substr(max(unlist(strsplit(Actual_Data_file, "/"))),6,20)
write.csv(Final_clean, paste0(file.name, "_cleaned.csv"))

rm(list=ls(pattern="temp."))



