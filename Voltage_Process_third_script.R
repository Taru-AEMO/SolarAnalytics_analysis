
### run this after voltage prep script. takes dataframes as inputs
### you don't have to run voltage process first before this one

### this script finds instances where a PV system saw a voltage jump in the top X percentile of the day, 
#######  and x% of systems saw a similar jump at the same interval. these intervals are output into a table
#######  as times of potential disturbance.
####### the purpose is to see if we can identify a disturbance by looking at multiple systems / how many false positives are there?

##### variables
# percentile
p <- .998
# % of systems
s <- .05

#### produce plots for this run? 0 = no, 1 = yes
# plots <- 0


#####
setwd("~/GitHub/DER_Event_analysis/SolarAnalytics_analysis/output/ToAnalyse")

# file.name <- list.files()
file.name <- "4500_2017_12_09_101622"

all.systems.count <- NULL

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
  t_end_estimate_nadir <- EventTime + minutes(4)
  t_10 <- EventTime - minutes(10)
  t_after <- EventTime + minutes(1)
  t_prior <- EventTime - minutes(1)
  t_hourprior <- EventTime - hours(1)
  t_hourafter <- EventTime + hours(1)

  
  #################################################################################################
  
  c_ids <- unique(volt_t0$c_id)
  
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
  
  ### isolate time of disturbance
  
  # temp.volt_t <- NULL
  # 
  # temp.volt_t <- temp.volt_d0%>%
  #   filter(ts>t_prior & ts<t_after)
  
  ##### put a 1 in the plot column for all intervals >99.8 percentile of deltas in the day
  
  q <- quantile(temp.volt_d0$abs_delta,p)
  
  temp.volt_d0.plot <- temp.volt_d0%>%
    mutate(plot=(ifelse(abs_delta > q,"1","0")))
  
  ### bind
  
  temp.p_count <- bind_rows(temp.p_count,temp.volt_d0.plot)
  
  ## close loop for (c in c_ids)
  }
  ###

  temp.p_count$plot <- as.numeric(temp.p_count$plot)
  
  ###
  temp.temp <- aggregate(plot ~ ts, temp.p_count, sum)
  
  # ggplot(temp.temp,aes(ts,plot))+
  #   geom_point()+
  #   geom_vline(xintercept=EventTime)
  
  ### filter for interval where more than s% of systems saw change greater than p
  temp.p_filter <- temp.temp%>%
    filter(plot>=(length(c_ids)*s))
  
  
  if (length(temp.p_filter)>=1){
  
  ### number columns.
  temp.events <- rownames(temp.p_filter)
  temp.p_filter <- cbind(event.id=temp.events, temp.p_filter)
  
  ### loop
  
  for (e in temp.events){
    
    temp.temp <- temp.p_filter%>%
      filter(event.id==e)
    
    time <- temp.temp$ts
    
    temp.plot_ids <- temp.p_count%>%
      filter(ts == time)%>%
      filter(plot == 1)
    
    temp.plot <- temp.p_count%>%
      filter(c_id %in% temp.plot_ids$c_id)
    
    #### find quartile for each timestamps
    temp.timestamps <- unique(temp.plot$ts)
    
    temp.all.intervals.quant <- NULL
    
    for ( t in temp.timestamps){
      
      temp.aggregate<- NULL
      temp.temp <- temp.plot%>%
        filter(ts==t)
  
      ##
      # temp.temp <- within(temp.temp, quantile <- as.integer(cut(p.u., quantile(p.u., probs=0:4/4), include.lowest=TRUE)))   
      
      temp.min <- aggregate(p.u. ~ ts, temp.temp, min)
      temp.mean <- aggregate(p.u. ~ ts, temp.temp, mean)
      temp.max <- aggregate(p.u. ~ ts, temp.temp, max)
      
      temp.min <- temp.min%>%
        mutate(Legend="min")
      
      temp.mean <- temp.mean%>%
        mutate(Legend="mean")
      
      temp.max <- temp.max%>%
        mutate(Legend="max")
      
      # temp.min <- data.frame(ts=(temp.temp[1,2]),
      #                        quantile=as.integer(0),
      #                        p.u.=min(temp.temp$p.u.))
      
      temp.aggregate <- bind_rows(temp.min,temp.mean)
      
      temp.aggregate <- bind_rows(temp.aggregate,temp.max)
      
      temp.all.intervals.quant <- bind_rows(temp.all.intervals.quant,temp.aggregate)
      
    ## close loop for (t in temp.timestamps) 
    }
      
    ######
    ### plot of day 
  #   
  # temp.plot <- temp.all.intervals.quant
  #   
  # ggplot(temp.plot,aes(ts,p.u.,colour=Legend))+
  #   geom_point()+
  #   geom_vline(aes(xintercept = EventTime),colour="black")+
  #   geom_vline(aes(xintercept = time),colour="red",linetype="dashed")+
  #   labs(title="Min, max and average of each interval, for all systems where possible disturbance identified",
  #        subtitle=sprintf("Time of actual event = %s,   Identified time = %s",EventTime,time))
  # 
  # ggsave(filename=paste0("Possible_dist_",substr(time,12,13),substr(time,15,16),substr(time,18,19),".png"))
  #  
  ##close loop for (e in temp.events)
   
  } }
  
  ### close loop for (i in file.name)
}
  

  #################### END
  
  # rm(list=ls(pattern="temp."))
  
  #######################################################
  
  
  