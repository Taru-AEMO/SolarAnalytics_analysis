####JOIN_SCRIPT.R

#README: This script is designed to join the Solar Analytics Data Sets together.
setwd(paste0("~/GitHub/DER_Event_analysis/SolarAnalytics_analysis/input/",folder))


##Read in the file, format the parameters appropriately 
actual_data1 <- read.csv(Actual_Data_file, header=TRUE, stringsAsFactors = FALSE) 

actual_data1 <- actual_data1[,colSums(is.na(actual_data1))!=nrow(actual_data1)] %>% 
  na.omit()

if(any(names(actual_data1) == "t_stamp")){
  colnames(actual_data1)[colnames(actual_data1)=="t_stamp"] <- "tstamp"
} else if(any(names(actual_data1) == "utc_tstamp")){
  colnames(actual_data1)[colnames(actual_data1)=="utc_tstamp"] <- "tstamp"
} else if(any(names(actual_data1) == "ts")){
  colnames(actual_data1)[colnames(actual_data1)=="ts"] <- "tstamp"
} else ("Cannot find time stamp column name in csv")


if(any(names(actual_data1) == "duration")){
  colnames(actual_data1)[colnames(actual_data1)=="duration"] <- "d"
} else if(any(names(actual_data1) == "d")){
  colnames(actual_data1)[colnames(actual_data1)=="d"] <- "d"
} else ("Cannot find Duration column name in csv")


if(any(names(actual_data1) == "energy")){
  colnames(actual_data1)[colnames(actual_data1)=="energy"] <- "e"
} else if(any(names(actual_data1) == "e")){
  colnames(actual_data1)[colnames(actual_data1)=="e"] <- "e"
} else ("Cannot find Energy column name in csv")


if(any(names(actual_data1) == "power")){
  colnames(actual_data1)[colnames(actual_data1)=="power"] <- "p"
} else if(any(names(actual_data1) == "p")){
  colnames(actual_data1)[colnames(actual_data1)=="p"] <- "p"
} else ("Cannot find Power column name in csv")


if(any(names(actual_data1) == "voltage")){
  colnames(actual_data1)[colnames(actual_data1)=="voltage"] <- "v"
} else if(any(names(actual_data1) == "v")){
  colnames(actual_data1)[colnames(actual_data1)=="v"] <- "v"
} else ("Cannot find Voltage column name in csv")


if(any(names(actual_data1) == "frequency")){
  colnames(actual_data1)[colnames(actual_data1)=="frequency"] <- "f"
} else if(any(names(actual_data1) == "f")){
  colnames(actual_data1)[colnames(actual_data1)=="f"] <- "f"
} else ("Cannot find Frequency column name in csv")

actual_data1 <- actual_data1%>%
  mutate(ts = ymd_hms(tstamp))%>%
  mutate(ts = with_tz(ts,"Australia/Brisbane")) %>% 
  na.omit(ts) %>% 
  mutate(c_id = as.integer(c_id),
         energy = as.numeric(e),
         power=as.numeric(p),
         voltage= as.numeric(v),
         frequency = as.numeric(f))
  

actual_data1 <- actual_data1 %>% 
  mutate(c_id = as.integer(c_id))


site_details <-  read.csv(Site_details_file, header=TRUE, stringsAsFactors = FALSE) %>% 
  mutate(site_id=as.numeric(site_id))

## test edit 20190502
if(any(names(site_details) == "manufacturer")){
  colnames(site_details)[colnames(site_details)=="manufacturer"] <- "inverter_manufacturer"
}
if(any(names(site_details) == "model")){
  colnames(site_details)[colnames(site_details)=="model"] <- "inverter_model"
}
##


circuit_details <- read.csv(Circuit_details_file, header=TRUE, stringsAsFactors = FALSE)

##Join all the data sets together, evaluate polarity, remove duplicates and calculate 
actual_data_join <- left_join(actual_data1, circuit_details, by="c_id") %>% 
  mutate(energy_polarity= energy*polarity,
         power_polarity = power*polarity)


if(any(names(site_details) == "manufacturer")){
  colnames(site_details)[colnames(site_details)=="manufacturer"] <- "inverter_manufacturer"
} else if(any(names(site_details) == "inverter_manufacturer")){
  colnames(site_details)[colnames(site_details)=="ts"] <- "inverter_manufacturer"
} else ("Cannot find Manufacturer column name in csv")


if(any(names(site_details) == "model")){
  colnames(site_details)[colnames(site_details)=="model"] <- "inverter_model"
} else if(any(names(site_details) == "inverter_model")){
  colnames(site_details)[colnames(site_details)=="inverter_model"] <- "inverter_model"
} else ("Cannot find Model column name in csv")



inverter_details_unique <- site_details[,c('site_id', 'inverter_manufacturer', 'inverter_model')] %>% 
  distinct(site_id, .keep_all=TRUE) %>% 
  plyr::join(site_details, by=c("site_id", 'inverter_manufacturer', 'inverter_model'), type="left", match="first") 

#Join Unique Site details with the Actual Circuit details and add an extra row for Power in kW value
full_data_set <- left_join(actual_data_join, inverter_details_unique, by="site_id") %>% 
  mutate(power_kW_min = 0.06*energy_polarity/3600) %>% 
  mutate(power_kW_30sec = 0.12*energy_polarity/3600)

## create and set to output directory
file.name <- substr(max(unlist(strsplit(Actual_Data_file, "/"))),6,20)

data.date <- as.Date(mean(full_data_set$ts))

setwd(paste0("~/GitHub/DER_Event_analysis/SolarAnalytics_analysis/output/",folder))

dir.create(file.path(paste0("~/GitHub/DER_Event_analysis/SolarAnalytics_analysis/output/",folder, "/",data.date)))

setwd(paste0("~/GitHub/DER_Event_analysis/SolarAnalytics_analysis/output/",folder, "/", data.date))

##Write CSV for All data types
###MANUAL CHECK NEEDS TO BE DONE TO MAKE SURE ALL UNIQUE Connection Types are selected. 

count.data <- full_data_set %>% 
  group_by(con_type, c_id) %>% 
  summarise(count=n()) %>% 
  group_by(con_type) %>% 
  summarise(count=n())

print("Data set contains")
print(count.data)


print("Please check that the above connection types have been appropriately accounted for in the lists below:")


load.list <- c("load_air_conditioner", "ac_load_net", "ac_load","load_other", 
               "load_pool","load_hot_water","load_stove","load_lighting",
               "load_office", "", "load_hot_water_solar", "load_powerpoint",
               "load_refrigerator", "load_shed", "load_ev_charger", "load_machine",
               "load_common_area","load_garage","load_kitchen","load_tenant")



load.list.df <- as.data.frame(load.list) 
colnames(load.list.df) <- "con_type"
load.list.df <- mutate(load.list.df, con_type = as.character(con_type))

load.list.file <- semi_join(load.list.df, count.data,by=c("con_type"))

print("Loads currently includes:")
print(load.list.file)
load_data_set <- filter(full_data_set, con_type %in% load.list)

write.csv(load_data_set, paste0(file.name, "_LoadData.csv"))

pv.list <- c("pv_site_net", "pv_site","pv_inverter_net")
pv.list.df <- as.data.frame(pv.list) 
colnames(pv.list.df) <- "con_type"
pv.list.df <- mutate(pv.list.df, con_type = as.character(con_type))
pv.list.file <- semi_join(pv.list.df, count.data,by=c("con_type"))

print("PV sites currently includes:")
print(pv.list.file)
pv_data_set <- filter(full_data_set, con_type %in% pv.list)

write.csv(pv_data_set, paste0(file.name, "_PVData.csv"))


battery.list <- c("battery_storage")
battery.list.df <- as.data.frame(battery.list) 
colnames(battery.list.df) <- "con_type"
battery.list.df <- mutate(battery.list.df, con_type = as.character(con_type))
battery.list.file <- semi_join(battery.list.df, count.data,by=c("con_type"))


if(nrow(battery.list.file)>0) {
  print("Battery sites currently includes:")
  print(battery.list.file)
  battery_data_set <- filter(full_data_set, con_type %in% battery.list)
  
  write.csv(battery_data_set, paste0(file.name, "_BatteryData.csv"))
  
}else (print("No Battery data in dataset"))



full.list <- c("load_air_conditioner", "ac_load_net", "ac_load","load_other", 
               "load_pool","load_hot_water","load_stove","load_lighting",
               "load_office", "", "load_hot_water_solar", "load_powerpoint",
               "load_refrigerator", "load_shed", "load_ev_charger", 
               "pv_site_net", "pv_site", "battery_storage", "load_machine",
               "load_common_area","load_garage","load_kitchen","load_tenant")

full.list.df <- as.data.frame(full.list)
colnames(full.list.df) <- "con_type"
full.list.df <- mutate(full.list.df, con_type = as.character(con_type))

unaccounted.list <- anti_join(count.data, full.list.df, by="con_type")

if(nrow(unaccounted.list)>0) {
print("Accounted for Connection types:")
print(unaccounted.list)
}else (print("No Connection types unaccounted for"))

if(nrow(battery.list.file)>0) {
rm(list=c("actual_data_join", "actual_data1", "circuit_details", "inverter_details_unique", "site_details",
          "load_data_set", "full_data_set", "battery_data_set"))
}else (rm(list=c("actual_data_join", "actual_data1", "circuit_details", "inverter_details_unique", "site_details",
                 "load_data_set", "full_data_set")))