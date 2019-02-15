####VOLTAGE_PROCESS_SCRIPT.R
####run process script first to get "Categorised" df

setwd("~/GitHub/DER_Event_analysis/SolarAnalytics_analysis/output/ToAnalyse/")


# input.file.name <- list.files(pattern="_cleaned.csv")
input.file.name <- "4500_2017_12_31_142222_cleaned.csv"

# Categorised <- temp.category


# for (i in input.file.name){
  setwd("~/GitHub/DER_Event_analysis/SolarAnalytics_analysis/output/ToAnalyse/")
  dir.create(file.path(paste0("~/GitHub/DER_Event_analysis/SolarAnalytics_analysis/output/ToAnalyse/",substr(i,1,22))))
  
  file.name <- paste(substr(i,1,22))
  
  EventTime <- ymd_hms(paste0(substr(i, 6, 15)," ",substr(i, 17, 18),":",substr(i, 19, 20),":",substr(i, 21, 22)), tz="Australia/Brisbane")
  
  print(EventTime)
  
  
  Final_clean <- read.csv(i, header=TRUE, stringsAsFactors = FALSE) %>%
    mutate(ts = ymd_hms(ts, tz="Australia/Brisbane"))
  
  
  Final_clean <- Final_clean %>% 
    mutate(pv_install_date_day=ifelse(nchar(pv_install_date)==10,pv_install_date, paste0(pv_install_date, "-28"))) %>%
    mutate(pv_install_date_day=ymd(pv_install_date_day)) %>%
    mutate(Standard_Version = ifelse(pv_install_date_day<"2015-10-09", "AS4777.3:2005", ifelse(pv_install_date_day>="2016-10-09", "AS4777.2:2015", "Transition"))) %>% 
    mutate(DC.Rating.kW.= DC.Rating.kW./1000) %>% 
    mutate(Size = ifelse(DC.Rating.kW.<30, "<30 kW", ifelse(DC.Rating.kW.<=100, "30-100kW", ">100kW"))) %>% 
    mutate(Duration = durn)
  
  #check nominal voltage and create p.u.
  
  temp.med.volt <- median(Final_clean$voltage)
  
  temp.nominal.volt <- ifelse(temp.med.volt> 235 & temp.med.volt <=245,240,
                              ifelse(temp.med.volt>=225 & temp.med.volt <235,230,
                                     print(paste0("median voltage is ",temp.med.volt,", unable to determine nominal voltage"))))
  
  Final_clean <- Final_clean%>%
    mutate(p.u.=voltage/temp.nominal.volt)
  
  ## join final clean with temp category
  
  Final_clean <- left_join(Final_clean,select(categorised,c_id,Category,Category_basic), by="c_id")

  #Filter for Voltage value at T_0
  
  t_0 <- EventTime
  t_end_estimate_nadir <- EventTime + minutes(4)
  t_end_nadir <- EventTime + minutes(2)
  t_prior_nadir <- EventTime - minutes(2)
  
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

  #Filter for possible slightly prior and slightly after disturbance and identify voltage at this time
  temp.nadir.df <- filter(Final_clean, ts>t_prior_nadir & ts<=t_end_estimate_nadir)
  temp.nadir_t0 <- temp.nadir.df%>%
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
  
  ########### voltage plots by category
  
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
      labs(title = paste("mean p.u. for the time of event by category"))+
      geom_hline(aes(yintercept=1.00),colour="black",linetype="dashed")+
      geom_vline(aes(xintercept=EventTime),colour="red",linetype="dashed")
    
    ggsave(filename=sprintf("%s/PU_time_of_event_by_category.png",file.name))
    
    # #### basic category
    
    temp.nadir.cat <- temp.nadir.df%>%
      group_by(Category_basic,c_id) %>% 
      summarise(count =n())%>%
      group_by(Category_basic) %>% 
      summarise(count = n())%>%
      mutate(Legend=paste0(Category_basic," (n=",count,")"))
    
    temp.nadir.df.catb <- left_join(temp.nadir.df,temp.nadir.cat,by="Category_basic")
    
    temp.nadir.agg <- aggregate(p.u. ~ ts + Legend, temp.nadir.df.catb, mean)
    
    ggplot(temp.nadir.agg,aes(x=ts,y=p.u.,colour=Legend))+
      geom_line(size=1)+
      labs(title = paste("mean p.u. for the time of event by category (basic)"))+
      geom_hline(aes(yintercept=1.00),colour="black",linetype="dashed")+
      geom_vline(aes(xintercept=EventTime),colour="red",linetype="dashed")
    
    ggsave(filename=sprintf("%s/PU_time_of_event_by_category(basic).png",file.name))
    
    
    # #### standard version
    
    temp.nadir.stan <- temp.nadir.df%>%
      group_by(Standard_Version,c_id) %>% 
      summarise(count =n())%>%
      group_by(Standard_Version) %>% 
      summarise(count = n())%>%
      mutate(Legend=paste0(Standard_Version," (n=",count,")"))
    
    temp.nadir.stan <- left_join(temp.nadir.df,temp.nadir.stan,by="Standard_Version")
    
    temp.nadir.agg <- aggregate(p.u. ~ ts + Legend, temp.nadir.stan, mean)
    
    ggplot(temp.nadir.agg,aes(x=ts,y=p.u.,colour=Legend))+
      geom_line(size=1)+
      labs(title = paste("mean p.u. for the time of event by standard version"))+
      geom_hline(aes(yintercept=1.00),colour="black",linetype="dashed")+
      geom_vline(aes(xintercept=EventTime),colour="red",linetype="dashed")
    
    ggsave(filename=sprintf("%s/PU_time_of_event_by_standard_version.png",file.name))
    
  
    # which systems saw the greatest change in voltage
    
    temp.volt.ramps <- temp.v0_vnadir_v1_v2%>%
      select(c_id,t0,v_0,pu_0,t_0plus1,v_0plus1,pu_0plus1,t_0plus2,v_0plus2,pu_0plus2,t_nadir,v_nadir,pu_nadir,Category,Category_basic)%>%
      mutate(pu_01_delta=pu_0-pu_0plus1,pu_02_delta=pu_0-pu_0plus2,pu_nadir_delta=pu_0-pu_nadir)
    
    temp <- aggregate(pu_01_delta ~ Category,temp.volt.ramps, mean)
    temp.temp <- aggregate(pu_02_delta ~ Category,temp.volt.ramps, min)
    # temp <- aggregate(pu_nadir_delta ~ Category, temp.volt.ramps, min)


  

#   #graph p.u. voltage for the day
#   
#   temp.volt_d0 <- aggregate(p.u. ~ ts, Final_clean, mean)
# 
#   ggplot(temp.volt_d0,aes(ts,p.u.))+
#     geom_line()+
#     labs(title = "mean p.u. for the day of event, all systems")+
#     geom_hline(aes(yintercept=1.00),colour="black",linetype="dashed")+
#     geom_vline(aes(xintercept=EventTime),colour="red",linetype="dashed")
#   
#   #graph p.u for nadir
#   
#   temp.nadir.agg <- aggregate(p.u. ~ ts, temp.nadir.df, mean)
#   
#   ggplot(temp.nadir.agg,aes(ts,p.u.))+
#     geom_line()+
#     labs(title = "mean p.u. for the time of event, all systems")+
#     # geom_hline(aes(yintercept=1.00),colour="black",linetype="dashed")
#     geom_vline(aes(xintercept=EventTime),colour="red",linetype="dashed")
#   
  
  
  
  
  
  