####JOIN_SCRIPT.R

#README: This script is designed to join the Solar Analytics Data Sets together.
setwd(paste0("~/GitHub/DER_Event_analysis/SolarAnalytics/",folder))


##Read in the file, format the parameters appropriately 
actual_data1 <- read.csv(Actual_Data_file, header=TRUE, stringsAsFactors = FALSE) %>% 
  mutate(ts = ymd_hms(t_stamp)) %>% 
  mutate(ts = with_tz(ts,"Australia/Brisbane")) 

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


##Write CSV for Load Data
###MANUAL CHECK NEEDS TO BE DONE TO MAKE SURE ALL UNIQUE Connection Types are selected. 
print("Please check that all of these connection types have been appropriately accounted for:")
print(unique(full_data_set$con_type))

load.list <- c("load_air_conditioner", "ac_load_net", "load_other", "battery_storage")
print("Loads currently includes:")
print(paste(load.list))
load_data_set <- filter(full_data_set, con_type %in% load.list)

file.name <- substr(max(unlist(strsplit(Actual_Data_file, "/"))),6,20)

write.csv(load_data_set, paste0(file.name, "_LoadData.csv"))

pv.list <- c("pv_site_net")
print("PV sites currently includes:")
print(paste(pv.list))
pv_data_set <- filter(full_data_set, con_type %in% pv.list)

write.csv(pv_data_set, paste0(file.name, "_PVData.csv"))

rm(list=c("actual_data_join", "actual_data1", "circuit_details", "inverter_details_unique", "site_details"))