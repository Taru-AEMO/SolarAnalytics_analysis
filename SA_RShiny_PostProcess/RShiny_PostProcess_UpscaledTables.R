
## 1 read in csv and process / reformat ####


setwd(paste0("",directory,""))

up_ar <- read.csv(paste0(upscaled_aggregated_file))

raw_ud <- read.csv(paste0(underlying_data_file))


####
## check for duplicate rows

temp.distinct <- dplyr::distinct(up_ar)

print(paste0("Original csv has ",nrow(up_ar)," rows, and ",nrow(temp.distinct)," rows after duplicates removed."))
print("please check and investigate if these numbers don't match. Analysis continued on filtered data set.")


#### post processing upscaled

pp_ar <- temp.distinct %>% 
  dplyr::mutate(Time=ymd_hms(Time,tz="Australia/Brisbane"))

rm(up_ar)


## post processing response categories ## underlying data
## check for duplicates

temp.distinct <- dplyr::distinct(raw_ud)

print(paste0("Underlying data has ",nrow(raw_ud)," rows, and ",nrow(temp.distinct)," rows after duplicates removed."))
print("please check and investigate if these numbers don't match. Analysis continued on filtered data set.")


## fix response categories

pp_ud <- temp.distinct %>% 
  dplyr::filter(!response_category %in% c("5 Off at t0", "NA", "Undefined", "6 Not enough data")) %>% 
  dplyr::mutate(response_category=gsub("3 Drop to Zero", "4 Disconnect",response_category),
                response_category=paste(substr(response_category,3,length(response_category))),
                ts=ymd_hms(ts,tz="Australia/Brisbane"),
                response_category=gsub(" ","-",response_category))

print("Merged drop to zero with disconnect category, response categories now read: ")
print(unique(pp_ud$response_category))

rm(raw_ud)
rm(list=ls(pattern="temp"))


#### create time stamps that will be searched (pre event interval and event time/s)

t0 <- lubridate::ymd_hms(paste0(substr(event_date,1,4),"-",
                                substr(event_date,5,6),"-",
                                substr(event_date,7,8)," ",
                                pre_event_interval),tz="Australia/Brisbane")

date <- lubridate::ymd(paste0(substr(event_date,1,4),"-",substr(event_date,5,6),"-",substr(event_date,7,8)))

tx <- sapply(event_time,function(x) {
  lubridate::ymd_hms(paste(date,x),tz="Australia/Brisbane")
},simplify = FALSE)




#### 2. calculate upscaled data ####
## find % response of each standard
temp.pp_ud <- pp_ud %>% 
  select(ts,power_kW,Standard_Version,response_category) %>% 
  mutate(key=paste0(Standard_Version,"_",response_category))

temp.aggregate <- aggregate(power_kW ~ ts + Standard_Version + response_category, temp.pp_ud, sum) %>% 
  mutate(key=paste0(ts,"_",Standard_Version))

temp.aggregate2 <- aggregate(power_kW ~ ts + Standard_Version, temp.pp_ud, sum) %>% 
  mutate(key=paste0(ts,"_",Standard_Version))

temp.join <- left_join(temp.aggregate,temp.aggregate2,by="key") %>% 
  select(ts=ts.x,key,response_category,Standard_Version=Standard_Version.y,
         res.power=power_kW.x,total.power=power_kW.y) %>% 
  mutate(perc_response=res.power/total.power)

## times % response by upscaled standard output
temp.pp_ar <- pp_ar %>%
  select(Time,Power_kW,Standard_Version)

temp.aggregate <- aggregate(Power_kW ~ Time + Standard_Version, temp.pp_ar, sum) %>% 
  mutate(key=paste0(Time,"_",Standard_Version))

temp.join2 <- left_join(temp.join,temp.aggregate,by="key") %>% 
  filter(!is.na(Power_kW)) %>% 
  select(ts,response_category,Standard_Version=Standard_Version.y,
         kW_sample=res.power,perc_response,Power_kW) %>% 
  mutate(kW_upscaled=perc_response*Power_kW,
         MW_upscaled=kW_upscaled*0.001)

temp.results <- aggregate(MW_upscaled ~ ts + response_category, temp.join2, sum)

## save results clear flames
upscaled_ts <- temp.results

rm(list=ls(pattern="temp"))


# ### temp plot
# temp.results$response_category <- ordered(temp.results$response_category, levels=c("Disconnect", "Curtail", "Ride-Through"))
# 
# ggplot(temp.results, aes(ts,MW_upscaled,colour=response_category))+
#   geom_area(position="stack",aes(fill=response_category))


##### 3. create tables







