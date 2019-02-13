####VOLTAGE_PROCESS_SCRIPT.R

setwd("~/GitHub/DER_Event_analysis/SolarAnalytics_analysis/output/ToAnalyse/")


# input.file.name <- list.files(pattern="_cleaned.csv")
input.file.name <- "4500_2017_12_31_142222_cleaned.csv"

Voltage_output <- NULL

# for (i in input.file.name){
  setwd("~/GitHub/DER_Event_analysis/SolarAnalytics_analysis/output/ToAnalyse/")
  dir.create(file.path(paste0("~/GitHub/DER_Event_analysis/SolarAnalytics_analysis/output/ToAnalyse/",substr(i,1,22))))
  
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
  
  
  #Filter for Voltage value at T_0
  
  t_0 <- EventTime
  t_end_estimate_nadir <- EventTime + minutes(4)
  t_end_nadir <- EventTime + minutes(2)
  t_prior_nadir <- EventTime - minutes(4)
  
  temp.volt_t0 <- filter(Final_clean, ts>=(t_0 - seconds(14)) & ts<= (t_0 + seconds(14))) %>%
    .[,c("c_id", "ts","voltage","p.u.")]
  
  
  ##Print confirmation
  print(paste("Number of unique ids in data set", length(unique(Final_clean$c_id))))
  
  print(paste("Number of rows selected", nrow(temp.volt_t0), "this is made up of", length(unique(temp.volt_t0$c_id)), "unique ids"))
  
  print("Please confirm these values match")
  
  # #Change Column Names
  colnames(temp.volt_t0)[colnames(temp.volt_t0)=="voltage"] <- "v_0"
  colnames(temp.volt_t0)[colnames(temp.volt_t0)=="ts"] <- "t0"
  colnames(temp.volt_t0)[colnames(temp.volt_t0)=="p.u."] <- "pu_0"

  #Filter for possible Nadir time then identify the T_Nadir and identify power at this time
  temp.nadir.df <- filter(Final_clean, ts>t_prior_nadir & ts<=t_end_estimate_nadir)
  temp.nadir_t0 <- temp.nadir.df%>%
    group_by(c_id) %>%
    summarise(v_nadir=min(voltage)) %>%
    left_join(temp.nadir.df[,c("c_id","ts","voltage")], by=c("c_id", "v_nadir"="voltage")) %>%
    .[,c(1,3,2)]
  colnames(temp.nadir_t0)[colnames(temp.nadir_t0)=="ts"] <- "t_nadir"
  
  #Filter for Power Value at T_1 and T_2
  temp.volt_t1 <- filter(Final_clean, ts>=(t_0 + seconds(16)) & ts<=(t_0 + seconds(45))) %>%
    .[,c("c_id","ts","voltage", "p.u.")]
  colnames(temp.volt_t1)[colnames(temp.volt_t1)=="voltage"] <- "v_0plus1"
  colnames(temp.volt_t1)[colnames(temp.volt_t1)=="ts"] <- "t_0plus1"
  
  temp.volt_t2 <- filter(Final_clean, ts>=(t_0 + seconds(46)) & ts<=(t_0 + seconds(75))) %>%
    .[,c("c_id","ts","voltage", "p.u.")]
  colnames(temp.volt_t2)[colnames(temp.volt_t2)=="voltage"] <- "v_0plus2"
  colnames(temp.volt_t2)[colnames(temp.volt_t2)=="ts"] <- "t_0plus2"
  
  #Join Tables Together and clean
  temp.v0_vnadir_v1_v2 <- plyr::join(temp.volt_t0, temp.nadir_t0, by="c_id", type="left", match="first") %>%
    left_join(temp.volt_t1, by="c_id") %>%
    left_join(temp.volt_t2, by="c_id") 
  
  
  ###########
  
  
  
  

  #graph p.u. voltage for the day
  
  # temp.volt_d0 <- aggregate(p.u. ~ ts, Final_clean, mean)
  # 
  # ggplot(temp.volt_d0,aes(ts,p.u.))+
  #   geom_line()+
  #   labs(title = "mean p.u. for the day of event, all systems")+
  #   geom_hline(aes(yintercept=1.00),colour="black",linetype="dashed")
  
  #graph p.u for nadir
  
  temp.nadir.agg <- aggregate(p.u. ~ ts, temp.nadir.df, mean)
  
  ggplot(temp.nadir.agg,aes(ts,p.u.))+
    geom_line()+
    labs(title = "mean p.u. for the time of event")+
    # geom_hline(aes(yintercept=1.00),colour="black",linetype="dashed")
    geom_vline(aes(xintercept=EventTime),colour="red",linetype="dashed")
  
  
  
  
  
  
  