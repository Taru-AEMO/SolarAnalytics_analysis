
setwd(paste0("",directory,""))

up_ar <- read.csv(paste0(upscaled_aggregated_file))


####
## check for duplicate rows

temp.distinct <- dplyr::distinct(up_ar)

print(paste0("Original csv has ",nrow(up_ar)," rows, and ",nrow(temp.distinct)," rows after duplicates removed."))
print("please check and investigate if these numbers don't match. Analysis continued on filtered data set.")


#### post processing

pp_ar <- temp.distinct %>% 
  dplyr::mutate(Time=ymd_hms(Time,tz="Australia/Brisbane"))


#### create time stamps that will be searched (pre event interval and event time/s)

t0 <- lubridate::ymd_hms(paste0(substr(event_date,1,4),"-",substr(event_date,5,6),"-",substr(event_date,7,8)," ",pre_event_interval),tz="Australia/Brisbane")

date <- lubridate::ymd(paste0(substr(event_date,1,4),"-",substr(event_date,5,6),"-",substr(event_date,7,8)))

tx <- sapply(event_time,function(x) {
  lubridate::ymd_hms(paste(date,x),tz="Australia/Brisbane")
},simplify = FALSE)

############################################################ 






