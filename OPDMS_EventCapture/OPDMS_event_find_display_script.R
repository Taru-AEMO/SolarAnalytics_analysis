


### this script will rearrange the results from the OPDMS_HSMcsv_event_find_script.R to be more intuitive to read
###


setwd("C:/Users/RTandy/Documents/OPDMS Data")

load("Event_search_2019-05-29_101606.R")


###
output.events <- output.events %>% 
  mutate(min.volt=round(min.volt,digits=4),
         max.volt=round(max.volt,digits=4)) %>% 
  mutate(sync=ifelse(grepl("sync",key), 1,
                     ifelse(grepl("SYNC",key), 1,
                            ifelse(grepl("Sync",key), 1,
                                   0)))) %>% 
  filter(sync==0,
         min.freq<56 | is.na(min.freq),
         min.freq>40 | is.na(min.freq)) %>% 
  select(-sync)


events <- unique(output.events$date.time)



##### source PV script to find capacity factor at event time

PV_search <- data.frame(date=NA,
                        time=NA,
                        postcode=NA,
                        region=NA,
                        time_date=NA,
                        ts=events)


setwd("~/GitHub/DER_Event_analysis/SolarAnalytics_analysis/EQ_DataProcessing")
source("PV_Capacity_at_d0.R")

# load("PV.20190529.R")

# save(PV_search_results,file="PV.20190529.R")

rm(results)
rm(output)
rm(con)
rm(EstEventTime)
rm(EventTime)
rm(i)
rm(input.events)
rm(Post_EventTime)
rm(Pre_EventTime)
rm(Region)

#####

temp.ids <- as.data.frame(events) %>% 
  mutate(event.no.=1:length(events)) %>% 
  select(event.no.,date.time=events)

temp.output.events.id <- left_join(output.events,temp.ids,by="date.time")


input.events <- unique(temp.output.events.id$event.no.)


results <- NULL

# for(t in input.events){
sapply_results <- sapply(input.events,function(x){
  
  temp.output.events <- temp.output.events.id %>% 
    filter(event.no.==x)
  
  # ## too hard
  # ### find number of phases affected
  # 
  # temp.voltage.events <- temp.output.events %>% 
  #   filter(!is.na(min.volt))
  # 
  # temp <- temp.voltage.events %>% 
  #   

  
  ## min and max frequency observations
  if (FALSE %in% is.na(temp.output.events$min.volt)){
  min_pu_obs <- min(filter(select(temp.output.events,min.volt),!is.na(temp.output.events$min.volt)))
  max_pu_obs <- max(filter(select(temp.output.events,max.volt),!is.na(temp.output.events$max.volt)))
  }else {
    min_pu_obs <- NA
    max_pu_obs <- NA
  }
  
  if (FALSE %in% is.na(temp.output.events$min.freq)){
  min_f_obs <- min(filter(select(temp.output.events,min.freq),!is.na(temp.output.events$min.freq)))
  max_f_obs <- max(filter(select(temp.output.events,max.freq),!is.na(temp.output.events$max.freq)))
  }else {
    min_f_obs <- NA
    max_f_obs <- NA
  }
  
  
  ### how long was the disturbance
  average_duration <- mean(temp.output.events$duration)
  longest_duration <- max(temp.output.events$duration)
  
  ### make new data frame with max min and duration values
  temp.summary <- data.frame(Date_Time=temp.output.events$date.time[1],
                             Minimum_pu=min_pu_obs,
                             Maximum_pu=max_pu_obs,
                             Minimum_Hz=min_f_obs,
                             Maximum_Hz=max_f_obs,
                             Event.Time=min(temp.output.events$event.time),
                             Duration_mean=average_duration,
                             Duration_max=longest_duration,
                             # SA=ifelse("SA" %in% unique(temp.output.events$region),"yes","NA"),
                             # VIC=ifelse("VIC" %in% unique(temp.output.events$region),"yes","NA"),
                             # NSW=ifelse("NSW" %in% unique(temp.output.events$region),"yes","NA"),
                             # QLD=ifelse("QLD" %in% unique(temp.output.events$region),"yes","NA"),
                             # TAS=ifelse("TAS" %in% unique(temp.output.events$region),"yes","NA"),
                             region=paste0(unique(temp.output.events$region,collapse=",")))

  
  ### summarise stations for each region then join to the temp.summary frame --> temp.join
  temp.stations <- dplyr::distinct(select(temp.output.events,region,station)) %>% 
    mutate(region=as.factor(region),
           station=as.factor(station)) %>% 
    group_by(region) %>% 
    summarise(stations=paste(station,collapse=", "))
  
  if (length(temp.stations)==1){
    
    temp.region <- dplyr::distinct(select(temp.output.events,region,station)) %>% 
      mutate(region=as.factor(region),
             station=as.factor(station)) %>% 
      group_by(region)
  
    temp.stations <- temp.stations %>% 
      mutate(region=unique(temp.region$region))

  }
  
  temp.join <- left_join(temp.summary,temp.stations,by="region") %>% 
    spread(region,stations)
  
  
  
  
  ### summarise HSM source files and join to temp.join
  temp.sources <- dplyr::distinct(select(temp.output.events,csv)) %>% 
    mutate(csv=as.factor(csv)) %>% 
    # group_by(region) %>% 
    summarise(HSM_source_files=paste(csv,collapse=", "))
  
  
  
  temp.bind <- bind_cols(temp.join,temp.sources)
 

  
  # results <- bind_rows(results,temp.bind)
  },simplify=FALSE)

#########################

   
results <- bind_rows(sapply_results) %>% 
  select(Date_Time,Minimum_pu,Maximum_pu,Minimum_Hz,Maximum_Hz,Event.Time,Duration_mean,Duration_max,NSW,QLD,VIC,SA,TAS,HSM_source_files)

###
rm(list=ls(pattern="temp."))

### Join PV on

temp.PV_cap <- PV_search_results %>% 
  mutate(region=paste0("PV_Capacity_Factor_",gsub("1","",region_sql)),
         Capacity_generating=round(Capacity_generating,digits=4)) %>% 
  select(Date_Time=ts,region,Capacity_generating) %>% 
  spread(region,Capacity_generating)


Final_Results <- left_join(results,temp.PV_cap,by="Date_Time") %>% 
  mutate(Light=ifelse((PV_Capacity_Factor_NSW+PV_Capacity_Factor_QLDCENTRAL+
           PV_Capacity_Factor_QLDNORTH+PV_Capacity_Factor_QLDNORTH+
           PV_Capacity_Factor_QLDSOUTH+PV_Capacity_Factor_SA+PV_Capacity_Factor_TASNORTH+
           PV_Capacity_Factor_TASSOUTH+PV_Capacity_Factor_VIC)>0,"Day","Night"))


#########


setwd("C:/Users/RTandy/Documents/OPDMS Data")

write.csv(Final_Results,"OPDMS_Disturbance_search_20190529.csv")





