####VOLTAGE_PROCESS_SCRIPT.R
####run process script first to get "Categorised" df

setwd("~/GitHub/DER_Event_analysis/SolarAnalytics_analysis/output/ToAnalyse/")


# input.file.name <- list.files(pattern="_cleaned.csv")

input.file.name <- c("4555_2017_02_15_103639_cleaned.csv")


for (i in input.file.name){
  
########################################################## 1. FILE READ INS
  ###### creat new output folder
  file.name <- paste(substr(i,1,22))
  
  dir.create(file.path(paste0("~/GitHub/DER_Event_analysis/SolarAnalytics_analysis/output/ToAnalyse/",substr(i,1,22))))
  EventTime <- ymd_hms(paste0(substr(i, 6, 15)," ",substr(i, 17, 18),":",substr(i, 19, 20),":",substr(i, 21, 22)), tz="Australia/Brisbane")
  
  print(EventTime)
  
  ##### read in clean file

  Final_clean <- read.csv(i, header=TRUE, stringsAsFactors = FALSE) %>%
    mutate(ts = ymd_hms(ts, tz="Australia/Brisbane"))
  
  ##### read in file with output response
  setwd(paste0("~/GitHub/DER_Event_analysis/SolarAnalytics_analysis/output/ToAnalyse/",file.name,""))
  
  Categorised <- read.csv(paste0("Response_type_",substr(i, 1,15),".csv"),header=TRUE, stringsAsFactors = FALSE)
  
  setwd("~/GitHub/DER_Event_analysis/SolarAnalytics_analysis/output/ToAnalyse/")
  
  
########################################################## 2. process clean data set, match with disconnection categories

  # setwd(paste0("~/GitHub/DER_Event_analysis/SolarAnalytics_analysis/output/ToAnalyse/",file.name,""))
  
  Final_clean <- Final_clean %>% 
    mutate(pv_install_date_day=ifelse(nchar(pv_install_date)==10,pv_install_date, paste0(pv_install_date, "-28"))) %>%
    mutate(pv_install_date_day=ymd(pv_install_date_day)) %>%
    mutate(Standard_Version = ifelse(pv_install_date_day<"2015-10-09", "AS4777.3:2005", ifelse(pv_install_date_day>="2016-10-09", "AS4777.2:2015", "Transition"))) %>% 
    mutate(DC.Rating.kW.= DC.Rating.kW./1000) %>% 
    mutate(Size = ifelse(DC.Rating.kW.<30, "<30 kW", ifelse(DC.Rating.kW.<=100, "30-100kW", ">100kW"))) %>% 
    mutate(Duration = durn)
  
  #check nominal voltage and create p.u.
  
  temp.med.volt <- median(Final_clean$voltage)
  
  temp.nominal.volt <- ifelse(temp.med.volt> 235 & temp.med.volt <=247,240,
                              ifelse(temp.med.volt>=225 & temp.med.volt <235,230,
                                     print(paste0("median voltage is ",temp.med.volt,", unable to determine nominal voltage"))))
  
  Final_clean <- Final_clean%>%
    mutate(p.u.=voltage/temp.nominal.volt)
  
  ## join final clean with temp category
  
  Final_clean <- left_join(Final_clean,select(Categorised,c_id,Category,Category_basic), by="c_id")

  #Filter for Voltage value at T_0
  
  t_0 <- EventTime
  t_end_estimate_nadir <- EventTime + minutes(4)
  t_end_nadir <- EventTime + minutes(2)
  t_prior <- EventTime - minutes(1)
  t_hourprior <- EventTime - hours(1)
  t_hourafter <- EventTime + hours(1)
  
  temp.volt_t0 <- filter(Final_clean, ts>=(t_0 - seconds(14)) & ts<= (t_0 + seconds(14))) %>%
    .[,c("c_id", "ts","voltage","p.u.","Category","Category_basic")]
  
  
  ##Print confirmation
  print(paste("Number of unique ids in data set", length(unique(Final_clean$c_id))))
  
  print(paste("Number of rows selected", nrow(temp.volt_t0), "this is made up of", length(unique(temp.volt_t0$c_id)), "unique ids"))
  
  print("Please confirm these values match")
  
  ##Change Column Names
  colnames(temp.volt_t0)[colnames(temp.volt_t0)=="voltage"] <- "v_0"
  colnames(temp.volt_t0)[colnames(temp.volt_t0)=="ts"] <- "t0"
  colnames(temp.volt_t0)[colnames(temp.volt_t0)=="p.u."] <- "pu_0"

  ##Filter for possible slightly prior and slightly after disturbance and identify voltage at this time
  temp.nadir.df <- filter(Final_clean, ts>t_prior & ts<=t_end_estimate_nadir)
  temp.nadir_t0 <- temp.nadir.df%>%
    filter(ts>t_0 & ts<=t_end_estimate_nadir)%>%
    group_by(c_id) %>%
    summarise(v_nadir=min(voltage)) %>%
    # summarise(pu_nadir=min(p.u.))%>%
    left_join(temp.nadir.df[,c("c_id","ts","voltage","p.u.")], by=c("c_id", "v_nadir"="voltage")) %>%
    .[,c(1,3,2,4)]
  colnames(temp.nadir_t0)[colnames(temp.nadir_t0)=="ts"] <- "t_nadir"
  colnames(temp.nadir_t0)[colnames(temp.nadir_t0)=="p.u."] <- "pu_nadir"
  
  #Filter for Power Value at T_1 and T_2
  temp.volt_t1 <- filter(Final_clean, ts>=(t_0 + seconds(16)) & ts<=(t_0 + seconds(45))) %>%
    .[,c("c_id","ts","voltage", "p.u.")]
  colnames(temp.volt_t1)[colnames(temp.volt_t1)=="voltage"] <- "v_0plus1"
  colnames(temp.volt_t1)[colnames(temp.volt_t1)=="ts"] <- "t_0plus1"
  colnames(temp.volt_t1)[colnames(temp.volt_t1)=="p.u."] <- "pu_0plus1"
  
  temp.volt_t2 <- filter(Final_clean, ts>=(t_0 + seconds(46)) & ts<=(t_0 + seconds(75))) %>%
    .[,c("c_id","ts","voltage", "p.u.")]
  colnames(temp.volt_t2)[colnames(temp.volt_t2)=="voltage"] <- "v_0plus2"
  colnames(temp.volt_t2)[colnames(temp.volt_t2)=="ts"] <- "t_0plus2"
  colnames(temp.volt_t2)[colnames(temp.volt_t2)=="p.u."] <- "pu_0plus2"
  
  #Join Tables Together and clean
  temp.v0_vnadir_v1_v2 <- plyr::join(temp.volt_t0, temp.nadir_t0, by="c_id", type="left", match="first") %>%
    left_join(temp.volt_t1, by="c_id") %>%
    left_join(temp.volt_t2, by="c_id") 
  
  
######################################### 3. FINDING SYSTEMS WITH SIGNIFICANT RAMP AT TIME OF EVENT ##################
######################## everything inside the following two loops is looking at individual systems
#### loop to find daily ramps

c_ids <- unique(temp.volt_t0$c_id)

temp.all_ids <- NULL

for (c in c_ids) {

  ### find the changes in pu throughout the day for each system
  temp.single_id <- Final_clean%>%
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
    
  ### if the change in voltage at time of event is bigger than q, then plot that system
  q <- quantile(temp.single_id$abs_delta,.998)
  
  temp.single_id.event <- NULL
  
  temp.single_id.event <- temp.single_id%>%
    filter(ts>t_prior & ts<t_end_estimate_nadir)%>%
    mutate(plot=(ifelse(abs_delta > q,"1","0")))%>%
    filter(plot==1)
 
  ## what was the max change in pu seen during event -> d
  temp.d <- temp.single_id.event%>%
    filter(abs_delta==max(abs_delta))
   
  d <- temp.d$delta_pu 

  ### plotting systems
  p_ids <- unique(temp.single_id.event$c_id)

  for (p in p_ids){
    
    #####
    #### plot pu for entire day
    ggplot(temp.single_id,aes(ts,p.u.))+
      labs(title=sprintf("24Hours  %s",EventTime),subtitle = paste0("ID = ",c,"   delta: ",d,""))+
      geom_vline(aes(xintercept=EventTime),colour="green",size=6,alpha=0.3)+
      geom_vline(aes(xintercept=EventTime),colour="black",linetype="dashed")+
      scale_y_continuous(limits=c(0.90,1.1))+
      # geom_vline(aes(xintercept=EventTime+minutes(22)),colour="black",linetype="dashed")+
      geom_line()
    ggsave(filename=sprintf("%s/PlotA_c_id_%s.png",file.name,c))
  
  temp.single_id.hoursnear <- NULL
  
  temp.single_id.hoursnear <- temp.single_id%>%
    filter(t_hourprior<ts & ts<t_hourafter)
   

  #### plot pu for 2 hours either side of event
    ggplot(temp.single_id.hoursnear,aes(ts,p.u.))+
    labs(title=sprintf("2Hours  %s",EventTime),subtitle = paste0("ID = ",c,"  delta: ",d,""))+
    # geom_area(aes(fill=fill))+
    geom_vline(aes(xintercept=EventTime),colour="blue",size=6,alpha=0.3)+
    geom_vline(aes(xintercept=EventTime),colour="black",linetype="dashed")+
    scale_y_continuous(limits=c(0.90,1.1))+
    # geom_vline(aes(xintercept=EventTime+minutes(2)),colour="black",linetype="dashed")+
    geom_line()
  ggsave(filename=sprintf("%s/PlotB_c_id_%s.png",file.name,c))


  #### plot pu for minutes either side of event
  
  temp.single_id.minsnear <- temp.single_id%>%
    filter(t_prior<ts & ts<t_end_estimate_nadir)
  
  ggplot(temp.single_id.minsnear,aes(ts,p.u.))+
    labs(title=sprintf("~6Minutess  %s",EventTime),subtitle = paste0("ID = ",c,"  delta: ",d,""))+
    # facet_wrap(~c_id, scales = "free_y")+
    # geom_vline(aes(xintercept=EventTime),colour="blue",size=6,alpha=0.3)+
    geom_vline(aes(xintercept=EventTime),colour="black",linetype="dashed")+
    scale_y_continuous(limits=c(0.90,1.1))+
    # geom_vline(aes(xintercept=EventTime+minutes(2)),colour="black",linetype="dashed")+
    geom_line()
  ggsave(filename=sprintf("%s/PlotC_c_id_%s.png",file.name,c))
  
  
  ####make list of all systems that were plotted with relevant pu / voltage info
  
  temp.all_ids <- bind_rows(temp.all_ids,temp.d)
  
  }}


##### save list of all the systems that were plotted
systems.identified <- temp.all_ids

##Print confirmation
print(paste("Number of IDs saw voltage change near event time:", length(unique(temp.all_ids$c_id))))

# print(paste("These IDs are:", (systems.identified), ""))

write.csv(systems.identified,file=sprintf("%s/systems_ids.csv",file.name),row.names=TRUE)



################################################################## PLOTS  
################### voltage plots by power category ###### these plots draw off the categorisations that is done in the process script
################### i.e. the systems response to the disturbance
  ###### graphs for the minutes near the event
    temp.nadir.cat <- temp.nadir.df%>%
      group_by(Category,c_id) %>% 
      summarise(count =n())%>%
      group_by(Category) %>% 
      summarise(count = n())%>%
      mutate(Legend=paste0(Category," (n=",count,")"))
    
    temp.nadir.df.cat <- left_join(temp.nadir.df,temp.nadir.cat,by="Category")
  
    temp.nadir.agg <- aggregate(p.u. ~ ts + Legend, temp.nadir.df.cat, mean)
    
      ggplot(temp.nadir.agg,aes(x=ts,y=p.u.,colour=Legend))+
      geom_line(size=1)+
      labs(title=sprintf("%s",EventTime),subtitle = "mean p.u. by disconnection category")+
      geom_hline(aes(yintercept=1.00),colour="black",linetype="dashed")+
      geom_vline(aes(xintercept=EventTime),colour="red",linetype="dashed")
    
    ggsave(filename=sprintf("%s/PU_by_disc_category.png",file.name))
    
    ###### same as above but for voltage
    
    # temp.nadir.agg <- aggregate(voltage ~ ts + Legend, temp.nadir.df.cat, mean)
    # 
    # ggplot(temp.nadir.agg,aes(x=ts,y=voltage,colour=Legend))+
    #   geom_line(size=1)+
    #   labs(title = paste("mean voltage for the time of event by category"))+
    #   geom_hline(aes(yintercept=temp.nominal.volt),colour="black",linetype="dashed")+
    #   geom_vline(aes(xintercept=EventTime),colour="red",linetype="dashed")
    # 
    # ggsave(filename=sprintf("%s/Voltage_time_of_event_by_category.png",file.name))
    
    ################ basic category
    ###### pu
    
    temp.nadir.catb <- temp.nadir.df%>%
      group_by(Category_basic,c_id) %>% 
      summarise(count =n())%>%
      group_by(Category_basic) %>% 
      summarise(count = n())%>%
      mutate(Legend=paste0(Category_basic," (n=",count,")"))
    
    temp.nadir.df.catb <- left_join(temp.nadir.df,temp.nadir.catb,by="Category_basic")
    
    temp.nadir.agg <- aggregate(p.u. ~ ts + Legend, temp.nadir.df.catb, mean)
    
    ggplot(temp.nadir.agg,aes(x=ts,y=p.u.,colour=Legend))+
      geom_line(size=1)+
      labs(title=sprintf("%s",EventTime),subtitle = "mean p.u. by disconnection category (basic)")+
      geom_hline(aes(yintercept=1.00),colour="black",linetype="dashed")+
      geom_vline(aes(xintercept=EventTime),colour="red",linetype="dashed")
    
    ggsave(filename=sprintf("%s/PU_by_category(basic).png",file.name))
    
    ###### same as above but for voltage
    
    # temp.nadir.agg <- aggregate(voltage ~ ts + Legend, temp.nadir.df.catb, mean)
    # 
    # ggplot(temp.nadir.agg,aes(x=ts,y=voltage,colour=Legend))+
    #   geom_line(size=1)+
    #   labs(title = paste("mean voltage for the time of event by category (basic)"))+
    #   geom_hline(aes(yintercept=temp.nominal.volt),colour="black",linetype="dashed")+
    #   geom_vline(aes(xintercept=EventTime),colour="red",linetype="dashed")
    # 
    # ggsave(filename=sprintf("%s/Voltage_time_of_event_by_category(basic).png",file.name))

 
##################################### graphs for the hours and days near the event
  ######################################### 
  ## #graph p.u. voltage for the day

  temp.final.clean <- left_join(Final_clean,temp.nadir.cat,by="Category")
    
  temp.volt_d0 <- aggregate(p.u. ~ ts + Legend, temp.final.clean, mean)

  ggplot(temp.volt_d0,aes(ts,p.u.,colour=Legend))+
    geom_line()+
    labs(title=sprintf("Day of %s",EventTime),subtitle = "mean p.u. by disconnection category")+
    geom_hline(aes(yintercept=1.00),colour="black",linetype="dashed")+
    geom_vline(aes(xintercept=EventTime),colour="red",linetype="dashed")

  ggsave(filename=sprintf("%s/PU_24H.png",file.name))
  
  #graph p.u. for the 2 hours either side of event
  temp.t.hours <- filter(temp.final.clean, ts>t_hourprior & ts<=t_hourafter)

  temp.volt_h0 <- aggregate(p.u. ~ ts + Legend, temp.t.hours, mean)
  
  
  ggplot(temp.volt_h0,aes(ts,p.u.,colour=Legend))+
    geom_line()+
    labs(title=sprintf("Hours of %s",EventTime),subtitle = "mean p.u. by disconnection category")+
    geom_hline(aes(yintercept=1.00),colour="black",linetype="dashed")+
    geom_vline(aes(xintercept=EventTime),colour="red",linetype="dashed")
  
  ggsave(filename=sprintf("%s/PU_4H.png",file.name))
  
  
  }
  


  
  