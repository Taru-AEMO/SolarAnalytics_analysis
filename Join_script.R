####JOIN_SCRIPT.R

#README: This script is designed to join the Solar Analytics Data Sets together.
setwd(paste0("~/GitHub/DER_Event_analysis/SolarAnalytics_analysis/input/",folder))


##Read in the file, format the parameters appropriately 
actual_data1 <- read.csv(Actual_Data_file, header=TRUE, stringsAsFactors = FALSE) %>% 
  na.omit()

if(any(names(actual_data1) == "t_stamp")){
  colnames(actual_data1)[colnames(actual_data1)=="t_stamp"] <- "tstamp"
} else if(any(names(actual_data1) == "utc_tstamp")){
  colnames(actual_data1)[colnames(actual_data1)=="utc_tstamp"] <- "tstamp"
} else ("Cannot find time stamp column name in csv")

actual_data1 <- actual_data1%>%
  mutate(ts = ymd_hms(tstamp))%>%
  mutate(ts = with_tz(ts,"Australia/Brisbane")) %>% 
  na.omit(ts) %>% 
  mutate(c_id = as.integer(c_id),
         energy = as.numeric(energy),
         power=as.numeric(power),
         voltage= as.numeric(voltage),
         frequency = as.numeric(frequency))
  
         

site_details <-  read.csv(Site_details_file, header=TRUE, stringsAsFactors = FALSE) %>% 
  mutate(site_id=as.numeric(site_id))

circuit_details <- read.csv(Circuit_details_file, header=TRUE, stringsAsFactors = FALSE)


##Join all the data sets together, evaluate polarity, remove duplicates and calculate 
actual_data_join <- left_join(actual_data1, circuit_details, by="c_id") %>% 
  mutate(energy_polarity= energy*polarity,
         power_polarity = power*polarity)


inverter_details_unique <- site_details[,c('site_id', 'inverter_manufacturer', 'inverter_model')] %>% 
  distinct(site_id, .keep_all=TRUE) %>% 
  plyr::join(site_details, by=c("site_id", 'inverter_manufacturer', 'inverter_model'), type="left", match="first") 

#Join Unique Site details with the Actual Circuit details and add an extra row for Power in kW value
full_data_set <- left_join(actual_data_join, inverter_details_unique, by="site_id") %>% 
  mutate(power_kW_min = 0.06*energy_polarity/3600) %>% 
  mutate(power_kW_30sec = 0.12*energy_polarity/3600)

## create and set to output directory
file.name <- substr(max(unlist(strsplit(Actual_Data_file, "/"))),6,20)

setwd(paste0("~/GitHub/DER_Event_analysis/SolarAnalytics_analysis/output/",folder))

##Write CSV for Load Data
###MANUAL CHECK NEEDS TO BE DONE TO MAKE SURE ALL UNIQUE Connection Types are selected. 

print("Please check that all of these connection types have been appropriately accounted for:")
print(unique(full_data_set$con_type))

load.list <- c("load_air_conditioner", "ac_load_net", "ac_load","load_other", 
               "load_pool","load_hot_water","load_stove","load_lighting",
               "load_office", "", "load_hot_water", "load_hot_water_solar", "load_powerpoint",
               "load_refrigerator", "load_shed", "load_ev_charger")
print("Loads currently includes:")
print(paste(load.list))
load_data_set <- filter(full_data_set, con_type %in% load.list)

write.csv(load_data_set, paste0(file.name, "_LoadData.csv"))

pv.list <- c("pv_site_net", "pv_site")
print("PV sites currently includes:")
print(paste(pv.list))
pv_data_set <- filter(full_data_set, con_type %in% pv.list)

write.csv(pv_data_set, paste0(file.name, "_PVData.csv"))


battery.list <- c("battery_storage")
print("Battery sites currently includes:")
print(paste(battery.list))
battery_data_set <- filter(full_data_set, con_type %in% battery.list)

write.csv(battery_data_set, paste0(file.name, "_BatteryData.csv"))

rm(list=c("actual_data_join", "actual_data1", "circuit_details", "inverter_details_unique", "site_details"))