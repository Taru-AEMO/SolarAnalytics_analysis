
### run this after voltage prep script. takes dataframes as inputs
### you don't have to run voltage process first before this one

### this script finds instances where a PV system saw a voltage jump in the top X percentile of the day, 
#######  and x% of systems saw a similar jump at the same interval. these intervals are output into a table
#######  as times of potential disturbance.
#######  the purpose is to see if we can identify a disturbance by looking at multiple systems / how many false positives are there?



#####
setwd("~/GitHub/DER_Event_analysis/SolarAnalytics_analysis/output/ToAnalyse")

file.name <- list.files()
# file.name <- c("4500_2017_12_09_101547", "4500_2017_12_09_101622", "4500_2017_12_09_101644", "4500_2017_12_31_142222",
#                "4500_2018_01_14_131712", "4551_2016_12_21_085007", "4551_2017_01_12_222045", "4551_2017_02_15_103436",
#                "4551_2017_02_28_152014", "4551_2017_03_12_132732")
# file.name <- c("4551_2017_04_01_155401", "4551_2017_04_01_155401", "4551_2017_12_31_142257", "4551_2018_01_01_151307",
#                "4551_2018_01_19_093102", "4555_2016_12_25_102726", "4555_2017_01_28_063457", "4555_2017_02_15_103639",
#                "4555_2017_03_12_133022", "4555_2017_12_31_142256", "4555_2018_01_01_151306", "4555_2018_01_14_152712",
#                "4555_2018_02_11_162833", "4701_2018_10_13_122400", "4701_2018_10_13_123900")


# summary.all <- NULL

#### start loop
for (i in file.name){
  
  
  #### set wd and load data frames
  
  setwd(paste0("~/GitHub/DER_Event_analysis/SolarAnalytics_analysis/output/ToAnalyse/",i,""))
  
  
  EventTime <- ymd_hms(paste0(substr(i, 6, 15)," ",substr(i, 17, 18),":",substr(i, 19, 20),":",substr(i, 21, 22)), tz="Australia/Brisbane")
  
  print(EventTime)
  
  ##load
  
  load(file=paste0("volt_d0_",i,".R"))
  load(file=paste0("volt_t0_",i,".R"))
  
  t_0 <- EventTime
  
  ######################################
  #### mutate to get required columns
  
  c_ids <- unique(volt_t0$c_id)
  
  d0 <- NULL
  
  for (d in c_ids){
  
    temp.volt_d0 <- volt_d0%>%
      select(c_id,ts,voltage,p.u.)%>%
      filter(c_id==d)%>%
      arrange(ts)%>%
      mutate(lag_voltage=lag(voltage),
             lag_pu=lag(p.u.),
             delta_voltage=voltage-lag_voltage,
             delta_pu=p.u.-lag_pu,
             delta_pu = ifelse(is.na(delta_pu), 0, delta_pu),
             delta_voltage = ifelse(is.na(delta_voltage), 0, delta_voltage),
             abs_delta=abs(delta_pu))%>%
      select(-lag_pu,-lag_voltage)

    
    d0 <- bind_rows(d0,temp.volt_d0)
     
    }
    
  
  ##### print descriptions:
  print("#############")
  print(paste0("Unique systems ids in this data set: ",length(c_ids),""))
  print("####")
    
  #################################################################################################


  ### this loop finds the p for all systems at the time of the event
  ### does this by filtering for delta at time of event and then determines how many delta above that in a day (for all systems)
  ### output ---->   temp.event.p
    
   temp.volt_t0 <- d0%>%
     filter(ts> EventTime & ts<= (EventTime + minutes(1)))
    
  c_ids <- unique(temp.volt_t0$c_id)
  
  
   temp.event.p <- NULL
   
   for (c in c_ids){
   
    #####
     temp.t0 <- temp.volt_t0%>%
       filter(c_id==c)
       
      temp.d0.plot <- d0%>%
        filter(abs_delta>(temp.t0$abs_delta))
      
      ## intervals > ed
      x <- nrow(temp.d0.plot)
      ## all intervals in day
      y <- nrow(d0)
      
      cid_p <- 1-(x/y)
      
      temp.t0 <- temp.t0%>%
        mutate(p=cid_p)
  
      
      temp.event.p <- bind_rows(temp.event.p,temp.t0)
      
     }
    
    
  ###################################### where put this
  system.threshold <- c(2:20)
  summary <- NULL
  
  
  for (st in system.threshold){
    
    ##### variables
    # number of systems
    s <- st
    

  ######### define thresholds (d [delta] or p [percentile])
  
  temp.event.p <- temp.event.p%>%
    arrange(desc(p))
    
  d <- temp.event.p[s,7]
  p <- temp.event.p[s,8]
  
  
  ######
  print(paste0("for S = ",s,""))
  print(paste0("Change defined as delta >",p," percentile"))
  print("###")
  
  
  ##### new loop for the rest of the day
  #####
  temp.p_count <- NULL
  
  for (c in c_ids){
    
    temp.volt_d0 <- d0%>%
      filter(c_id==c)

      ### 2: q method
    q <- quantile(temp.volt_d0$abs_delta,p)
    
    ###### puts a 1 in the row of every intervals where the delta > p percentile 
    temp.volt_d0.plot <- temp.volt_d0%>%
      mutate(plot=as.numeric((ifelse(abs_delta >= q,1,0))))
    
        # ### 1 : delta method
    # temp.volt_d0.plot <- temp.volt_d0%>%
    #   mutate(plot=as.numeric(ifelse(abs_delta>=d,1,0)))
    
    
    temp.p_count <- bind_rows(temp.p_count, temp.volt_d0.plot)
    
    ##### close loop for (c in c_id)
  }
  
  
  ### aggregtes each interval, plot column shows number of systems >x percentile
  temp.temp <- aggregate(plot ~ ts, temp.p_count, sum)
  
  temp.p_filter <- temp.temp%>%
    filter(plot>=s)

  
  temp.summary <- data.frame(threshold=s,
                             percentile=p,
                             delta=d,
                             positives=nrow(temp.p_filter),
                             actual.event=paste0(EventTime))
  
  summary <- bind_rows(summary,temp.summary)
  
  ### close loop for (st in system.threshold)
  
  }
  
  save(summary,file="Event.find.summary_v3.R")
  
  #   ### close final loop for (i in file.name)
  
  summary.all <- bind_rows(summary.all,summary)
  
  
  }


save(summary.all,file="Event.find.summary_all_v4.R")

  # temp.agg.summary <- aggregate(positives ~  threshold, summary.all, sum)
  # 
  # temp.short.sum <- summary.all%>%
  #   melt(,-threshold)
  

  #################### END
  
  # rm(list=ls(pattern="temp."))
  
  ####################################################### PART 2

setwd("~/GitHub/DER_Event_analysis/SolarAnalytics_analysis/output")
load(file="Event.find.summary_all_P_based.R")
# load(file="Event.find.summary_all_delta_based.R")


P_based <- summary.all
# delta_based <- summary.all


events <- unique(delta_based$actual.event)
temp.bind <- NULL

for (e in events){
  
  temp.filter <- delta_based%>%
    filter(actual.event==e)%>%
    filter(positives==min(positives))
  
  temp.bind <- bind_rows(temp.bind,temp.filter)
  
}

  


    
  