####VOLTAGE_PREP_SCRIPT.R
#### this script provides 2x data frames used for the voltage process scripts. 
### 1. volt_d0 (this is the data for entire day)
### 2. volt_t0 (this is the data for the minutes near the event time)
#### requires manual input for DNSP coltage change data
#### run process script first to get "Categorised" df

setwd("~/GitHub/DER_Event_analysis/SolarAnalytics_analysis/input/ToAnalyse/")


input.file.name <- list.files(pattern="_cleaned.csv")

# input.file.name <- c("4555_2017_01_28_063457_cleaned.csv","4555_2017_02_15_103639_cleaned.csv",
#                      "4555_2017_03_12_133022_cleaned.csv","4555_2017_12_31_142256_cleaned.csv","4555_2018_01_01_151306_cleaned.csv",
#                      "4555_2018_01_14_152712_cleaned.csv","4555_2018_02_11_162833_cleaned.csv","4701_2018_10_13_122400_cleaned.csv",
#                      "4701_2018_10_13_123900_cleaned.csv")


for (i in input.file.name){
  
  volt_d0 <- NULL
  volt_t0 <- NULL
  
  ########################################################## 1. FILE READ INS
  ###### creat new output folder
  file.name <- paste(substr(i,1,22))
  
  EventTime <- ymd_hms(paste0(substr(i, 6, 15)," ",substr(i, 17, 18),":",substr(i, 19, 20),":",substr(i, 21, 22)), tz="Australia/Brisbane")
  
  print(EventTime)
  
  ##### read in clean file
  setwd("~/GitHub/DER_Event_analysis/SolarAnalytics_analysis/input/ToAnalyse/")
  
  Final_clean <- read.csv(i, header=TRUE, stringsAsFactors = FALSE) %>%
    mutate(ts = ymd_hms(ts, tz="Australia/Brisbane"))
  
  ##### read in file with output response
  setwd(paste0("~/GitHub/DER_Event_analysis/SolarAnalytics_analysis/input/ToAnalyse/",file.name,""))
  
  Categorised <- read.csv(paste0("Response_type_",substr(i, 1,15),".csv"),header=TRUE, stringsAsFactors = FALSE)
  
  ##### read in file with DNSP voltage records for each event and isolate DNSP volt change = EventTime (-> temp.dv)
  ##### MANUAL INPUT
  
  setwd("~/GitHub/DER_Event_analysis/SolarAnalytics_analysis/input/ToAnalyse/")
  DNSP.data_csv <- read.csv("events.csv")
  
  temp.DNSP.data <- DNSP.data_csv %>%
    mutate(ts=ymd_hms(paste(DATE,TIME,sep=" "), tz="Australia/Brisbane"))%>%
    select(ts,VOLTAGE_DIP)%>%
    filter(ts==EventTime)
  
  temp.dv <- abs(max(temp.DNSP.data$VOLTAGE_DIP))
  
  
  ########################################################## 2. process clean data set, match with disconnection categories
  
  # setwd(paste0("~/GitHub/DER_Event_analysis/SolarAnalytics_analysis/output/ToAnalyse/",file.name,""))
  
  volt_d0 <- Final_clean %>% 
    mutate(pv_install_date_day=ifelse(nchar(pv_install_date)==10,pv_install_date, paste0(pv_install_date, "-28"))) %>%
    mutate(pv_install_date_day=ymd(pv_install_date_day)) %>%
    mutate(Standard_Version = ifelse(pv_install_date_day<"2015-10-09", "AS4777.3:2005", ifelse(pv_install_date_day>="2016-10-09", "AS4777.2:2015", "Transition"))) %>% 
    mutate(DC.Rating.kW.= DC.Rating.kW./1000) %>% 
    mutate(Size = ifelse(DC.Rating.kW.<30, "<30 kW", ifelse(DC.Rating.kW.<=100, "30-100kW", ">100kW"))) %>% 
    mutate(Duration = durn)%>%
    mutate(DNSP_record=temp.dv)
  
  #check nominal voltage and create p.u.
  
  temp.med.volt <- median(volt_d0$voltage)
  
  temp.nominal.volt <- ifelse(temp.med.volt> 235 & temp.med.volt <=247,240,
                              ifelse(temp.med.volt>=225 & temp.med.volt <235,230,
                                     print(paste0("median voltage is ",temp.med.volt,", unable to determine nominal voltage"))))
  
  volt_d0 <- volt_d0%>%
    mutate(p.u.=voltage/temp.nominal.volt)%>%
    mutate(nominal_voltage=temp.nominal.volt)
  
  ## join final clean with temp category
  
  volt_d0 <- left_join(volt_d0,select(Categorised,c_id,Category,Category_basic), by="c_id")
  
  #Filter for Voltage value at time of the event -> temp.volt_t0
  
  t_0 <- EventTime
  t_end_estimate_nadir <- EventTime + minutes(4)
  t_end_nadir <- EventTime + minutes(2)
  t_prior <- EventTime - minutes(1)
  t_hourprior <- EventTime - hours(1)
  t_hourafter <- EventTime + hours(1)
  
  temp.volt_t0 <- filter(volt_d0, ts>=(t_0 - seconds(14)) & ts<= (t_0 + seconds(14))) %>%
    .[,c("c_id", "ts","voltage","p.u.","Category","Category_basic")]
  
  ##Print confirmation
  print(paste("Number of unique ids in data set", length(unique(volt_d0$c_id))))
  
  print(paste("Number of rows selected", nrow(temp.volt_t0), "this is made up of", length(unique(temp.volt_t0$c_id)), "unique ids"))
  
  print("Please confirm these values match")
  
  ##Change Column Names
  colnames(temp.volt_t0)[colnames(temp.volt_t0)=="voltage"] <- "v_0"
  colnames(temp.volt_t0)[colnames(temp.volt_t0)=="ts"] <- "t0"
  colnames(temp.volt_t0)[colnames(temp.volt_t0)=="p.u."] <- "pu_0"
  
  volt_t0 <- temp.volt_t0
    
  ############################################################## 
  ################################ uncomment this section for -> temp.v0_vnadir_v1_v2 
  #
  # ##Filter for possible slightly prior and slightly after disturbance and identify voltage at this time
  # temp.nadir.df <- filter(Final_clean, ts>t_prior & ts<=t_end_estimate_nadir)
  # temp.nadir_t0 <- temp.nadir.df%>%
  #   filter(ts>t_0 & ts<=t_end_estimate_nadir)%>%
  #   group_by(c_id) %>%
  #   summarise(v_nadir=min(voltage)) %>%
  #   # summarise(pu_nadir=min(p.u.))%>%
  #   left_join(temp.nadir.df[,c("c_id","ts","voltage","p.u.")], by=c("c_id", "v_nadir"="voltage")) %>%
  #   .[,c(1,3,2,4)]
  # colnames(temp.nadir_t0)[colnames(temp.nadir_t0)=="ts"] <- "t_nadir"
  # colnames(temp.nadir_t0)[colnames(temp.nadir_t0)=="p.u."] <- "pu_nadir"
  # 
  # #Filter for Power Value at T_1 and T_2
  # temp.volt_t1 <- filter(Final_clean, ts>=(t_0 + seconds(16)) & ts<=(t_0 + seconds(45))) %>%
  #   .[,c("c_id","ts","voltage", "p.u.")]
  # colnames(temp.volt_t1)[colnames(temp.volt_t1)=="voltage"] <- "v_0plus1"
  # colnames(temp.volt_t1)[colnames(temp.volt_t1)=="ts"] <- "t_0plus1"
  # colnames(temp.volt_t1)[colnames(temp.volt_t1)=="p.u."] <- "pu_0plus1"
  # 
  # temp.volt_t2 <- filter(Final_clean, ts>=(t_0 + seconds(46)) & ts<=(t_0 + seconds(75))) %>%
  #   .[,c("c_id","ts","voltage", "p.u.")]
  # colnames(temp.volt_t2)[colnames(temp.volt_t2)=="voltage"] <- "v_0plus2"
  # colnames(temp.volt_t2)[colnames(temp.volt_t2)=="ts"] <- "t_0plus2"
  # colnames(temp.volt_t2)[colnames(temp.volt_t2)=="p.u."] <- "pu_0plus2"
  # 
  # #Join Tables Together and clean
  # temp.v0_vnadir_v1_v2 <- plyr::join(temp.volt_t0, temp.nadir_t0, by="c_id", type="left", match="first") %>%
  #   left_join(temp.volt_t1, by="c_id") %>%
  #   left_join(temp.volt_t2, by="c_id") 
  ##############################################################
  ## outputs
  
  dir.create(file.path(paste0("~/GitHub/DER_Event_analysis/SolarAnalytics_analysis/output/ToAnalyse/",substr(i,1,22))))
  setwd(paste0("~/GitHub/DER_Event_analysis/SolarAnalytics_analysis/output/ToAnalyse/",file.name,""))
  
  ### saving 2x frames. 1: volt_d0 and 2: volt_t0
  
  save(volt_d0,file=paste0("volt_d0_",file.name,".R"))
  save(volt_t0,file=paste0("volt_t0_",file.name,".R"))
  
  
  ##
  }
  
  ##
  rm(list=ls(pattern="temp."))
  
  #### script end
