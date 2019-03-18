
### run this after voltage prep script. takes dataframes as inputs
### you don't have to run voltage process first before this one

### this script finds instances where a PV system saw a voltage jump in the top X percentile of the day, 
#######  and x% of systems saw a similar jump at the same interval. these intervals are output into a table
#######  as times of potential disturbance.
#######  the purpose is to see if we can identify a disturbance by looking at multiple systems / how many false positives are there?



#####
setwd("~/GitHub/DER_Event_analysis/SolarAnalytics_analysis/output/ToAnalyse")

file.name <- list.files()
# file.name <- "4500_2017_12_09_101622"


summary.all <- NULL

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
  
  #################################################################################################
  
  c_ids <- unique(volt_t0$c_id)
  
  ##### print descriptions:
  print("#############")
  print(paste0("Unique systems ids in this data set: ",length(c_ids),""))
  print("####")
  
  
  system.threshold <- c(2:20)
  summary <- NULL
  
  
  for (st in system.threshold){
  
  ##### variables
  # number of systems
  s <- st
  
  #####
  temp.find_p <- NULL
  
  ### this loop finds the p for 6 systems at the time of the event
  
  for (c in c_ids){
    
    temp.volt_d0 <- volt_d0%>%
      select(c_id,ts,voltage,p.u.)%>%
      filter(c_id==c)%>%
      arrange(ts)%>%
      mutate(lag_voltage=lag(voltage),
             lag_pu=lag(p.u.),
             delta_voltage=voltage-lag_voltage,
             delta_pu=p.u.-lag_pu,
             delta_pu = ifelse(is.na(delta_pu), 0, delta_pu),
             delta_voltage = ifelse(is.na(delta_voltage), 0, delta_voltage),
             abs_delta=abs(delta_pu))%>%
      select(-lag_pu,-lag_voltage)
  
    #### find event time
    
    temp.volt_t0 <- temp.volt_d0%>%
      filter(ts> EventTime & ts<= (EventTime + minutes(1)))
    
    temp.volt_t0 <- temp.volt_t0[1,]
    
    tb_0 <- temp.volt_t0$ts
    ed <- temp.volt_t0$abs_delta
  
    #####
    temp.volt_d0.plot <- temp.volt_d0%>%
      filter(abs_delta>ed)

    x <- nrow(temp.volt_d0.plot)
    y <- nrow(temp.volt_d0)
    
    cid_p <- 1-(x/y)
    
    ###
    temp.temp <- data.frame(c_id=c,
                            ts=tb_0,
                            p=cid_p,
                            delta=ed)

    temp.find_p <- bind_rows(temp.find_p,temp.temp)
    
    ## close loop for (c in c_ids)
  }
  
  temp.find_p <- temp.find_p%>%
    arrange(desc(p))
    
  d <- temp.find_p[s,4]
  p <- temp.find_p[s,3]
  
  
  ######
  print(paste0("for S = ",s,""))
  print(paste0("Change defined as delta >",p," percentile"))
  print("###")
  
  
  
  ##### new loop for the rest of the day
  #####
  temp.p_count <- NULL
  
  for (c in c_ids){
    
    temp.volt_d0 <- volt_d0%>%
      select(c_id,ts,voltage,p.u.)%>%
      filter(c_id==c)%>%
      arrange(ts)%>%
      mutate(lag_voltage=lag(voltage),
             lag_pu=lag(p.u.),
             delta_voltage=voltage-lag_voltage,
             delta_pu=p.u.-lag_pu,
             delta_pu = ifelse(is.na(delta_pu), 0, delta_pu),
             delta_voltage = ifelse(is.na(delta_voltage), 0, delta_voltage),
             abs_delta=abs(delta_pu))%>%
      select(-lag_pu,-lag_voltage)
  
  
    ### 2: q method
    q <- quantile(temp.volt_d0$abs_delta,p)
    
    temp.volt_d0.plot <- temp.volt_d0%>%
      mutate(plot=as.numeric((ifelse(abs_delta > q,1,0))))
    
    
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
  
  save(summary,file="Event.find.summary_P.R")
  
  #   ### close final loop for (i in file.name)
  
  summary.all <- bind_rows(summary.all,summary)
  
  
  }


save(summary.all,file="Event.find.summary_all_P_based.R")

  # temp.agg.summary <- aggregate(positives ~  threshold, summary.all, sum)
  # 
  # temp.short.sum <- summary.all%>%
  #   melt(,-threshold)
  
  
  
  #################### END
  
  # rm(list=ls(pattern="temp."))
  
  #######################################################
  
    
  