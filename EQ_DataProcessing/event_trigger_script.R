library("openxlsx")
library("ggplot2")
library("dplyr")
library("tidyr")
library("lubridate")
library("seewave")
library("XLConnect")
library("reshape2")
library("gtable")
library("grid") # low-level grid functions are required


setwd("~/GitHub/EventTriggers")

temp.xcel <- list.files(pattern=".xlsx")

op <- options(digits.secs=6)

temp.file <- NULL
Output.Table <- NULL

for (i in temp.xcel){
  
  # b <- loadWorkbook(system.file(i, package = "openxlsx"))
  # 
  wb <- read.xlsx(i, sheet="Waveform") %>% 
    mutate(DateTime=dmy_hms(Date.and.Time,tz = "Australia/Brisbane"))
  
  names(wb) <- c("DateandTime", "PhaseI1", "PhaseI2", "PhaseI3", "PhaseV1", "PhaseV2", "PhaseV3", "DateTime")
    
  wb1 <- wb[,c("DateTime", "PhaseV1", "PhaseV2", "PhaseV3")] %>% 
    melt(.,id.vars="DateTime")
  
  wb2 <- read.xlsx(i, sheet="High_resolution_Half_cycle") %>% 
    mutate(DateTime=dmy_hms(Date.and.Time,tz = "Australia/Brisbane"))
  
  names(wb2) <- c("DateandTime", "PowerP1", "PowerP2", "PowerP3", "Frequency", "ReactivePowerP1", "ReactivePowerP2", "ReactivePowerP3", "RMSI1", "RMSI2", "RMSI3", "RMSV1", "RMSV2", "RMSV3", "DateTime")
  
  wb4 <- wb2 %>% 
    .[,c("DateTime", "PowerP1", "PowerP2", "PowerP3", "Frequency")]
  
  names(wb4) <- c("DateTime", "PhaseV1", "PhaseV2", "PhaseV3", "Frequency") 
  
  wb4 <- wb4 %>% 
    melt(.,id.vars="DateTime")
  
  
  wb5 <- wb2 %>% 
    .[,c("DateTime", "RMSV1", "RMSV2", "RMSV3")]
  
  names(wb5) <- c("DateTime", "PhaseV1", "PhaseV2", "PhaseV3") 
  
  wb5 <- wb5%>% 
    melt(.,id.vars="DateTime")
  
  temp.phase <- NULL
  temp.file <- NULL
  
  
  for (ii in c("PhaseV1", "PhaseV2", "PhaseV3")){
  # temp.c <- filter(wb1,variable==ii)  %>% 
  #   mutate(PosNeg = ifelse((value>=0 & (lag(value)<0)|(value<0 & (lead(value)>=0))),1,0),
  #          NegPos = ifelse((value<=0 & (lag(value)>0)|(value>0 & (lead(value)<=0))),1,0)) %>% 
  #   filter(PosNeg==1|NegPos==1) %>% 
  #   mutate(ZeroCross = DateTime-(((value-0)/(value-lead(value)))*(DateTime-lead(DateTime)))) %>% 
  #   filter(row_number()%% 2==1) %>% 
  #   .[,c("ZeroCross", "PosNeg","NegPos")]
  
  estimate.event.time <- ymd_hms(paste0("20",substr(i,104,105),"_",substr(i,101,102),"_",substr(i,98,99)," ",substr(i,107,108),":",substr(i,110,111),";",substr(i,113,114)), tz="Australia/Brisbane")
  
  
  if (i=="Parkhurst Dip & Freq_COMP_Parkhurst 66.11kV Sub Norman Rd Fdr 11 kV Norman Rd Fdr_e917_EV_549996_02_12_18_12_26_25.xlsx"){
    estimate.event.time <- estimate.event.time+seconds(1)
  }
  
  temp.c <- wb1[order(wb1$DateTime),] %>% 
    filter(variable==ii)  %>%
    filter(DateTime>=estimate.event.time) %>% 
    mutate(PosNeg = ifelse((value>=0 & (lag(value)<0)|(value<0 & (lead(value)>=0))),1,0),
           NegPos = ifelse((value<=0 & (lag(value)>0)|(value>0 & (lead(value)<=0))),1,0)) %>% 
    filter(PosNeg==1|NegPos==1) %>% 
    mutate(ZeroCross = dplyr::if_else(((value-lead(value))!=0),
                                      (DateTime-(((value-0)/(value-lead(value)))*(DateTime-lead(DateTime)))),
                                      (DateTime + ((lead(DateTime)-DateTime)/2)))) %>% 
    mutate(Keep = ifelse((PosNeg==lead(PosNeg) & PosNeg==1), 
                         1, 
                         ifelse((NegPos==lead(NegPos) & NegPos==1),
                                1, 0))) %>% 
    filter(Keep==1) %>% 
    .[,c("ZeroCross", "PosNeg","NegPos")]%>% 
    na.omit()
  

  # odd_indexes <- seq(1,nrow(temp.c),2)
  
  temp.d <- filter(temp.c, PosNeg==1) %>% 
    mutate(difference = ZeroCross - lag(ZeroCross)) %>% 
    na.omit() %>% 
    slice(which.max(difference)) %>% 
    mutate(maxPhaseAngle = ((difference -0.02)/0.02)*360) %>% 
    mutate(Phase=paste0(ii)) %>% 
    mutate(OriginalFile=paste0(i))
    
  temp.e <- filter(temp.c, NegPos==1) %>% 
    mutate(difference = ZeroCross - lag(ZeroCross)) %>% 
    na.omit() %>% 
    slice(which.max(difference)) %>% 
    mutate(maxPhaseAngle = ((difference -0.02)/0.02)*360) %>% 
    mutate(Phase=paste0(ii)) %>% 
    mutate(OriginalFile=paste0(i))
  
  temp.f <- rbind(temp.d, temp.e) %>%
    group_by(OriginalFile) %>% 
    slice(which.max(maxPhaseAngle))
  
  temp.phase <- rbind(temp.phase, temp.f) 
  
  temp.g <- filter(wb4, variable==ii)
  
  
  ##POWER CALCULATION
# temp.preevent <- filter(temp.g, DateTime<=(temp.f$ZeroCross- seconds(0.5)) & DateTime>=(temp.f$ZeroCross- seconds(0.51)))
  
  # if (min(temp.g$DateTime)>(temp.f$ZeroCross- seconds(0.8))){
  #   temp.preevent_mag <- filter(temp.g, DateTime==min(temp.g$DateTime)) %>% 
  #     group_by(variable) %>% 
  #     summarise(AverageValue=mean(value))
  #   
  # }else{
  temp.preevent_mag <- filter(temp.g, DateTime<=(temp.f$ZeroCross- seconds(0.5)) & DateTime>=(temp.f$ZeroCross- seconds(0.8))) %>% 
    group_by(variable) %>% 
    summarise(AverageValue=mean(value))
# }
  
 # temp.postevent <- filter(temp.g, DateTime>=(temp.f$ZeroCross+ seconds(0.5)) & DateTime<=(temp.f$ZeroCross+ seconds(0.51)))
  
  temp.postevent_mag <- filter(temp.g, DateTime>=(temp.f$ZeroCross+ seconds(0.5)) & DateTime<=(temp.f$ZeroCross+ seconds(0.8))) %>% 
    group_by(variable) %>% 
    summarise(AverageValue=mean(value))
  
  # temp.event_max <- filter(temp.g, DateTime>=temp.preevent$DateTime & DateTime<=temp.postevent$DateTime) %>% 
  #   group_by(variable) %>% 
  #   filter(value==max(value)) %>% 
  #   filter(row_number()%% 2==1) %>% 
  #   mutate(preevent = temp.preevent$value) %>% 
  #   mutate(diff = abs(preevent-value))
  # 
  # temp.event_min <- filter(temp.g, DateTime>=temp.preevent$DateTime & DateTime<=temp.postevent$DateTime) %>% 
  #   group_by(variable) %>% 
  #   filter(value==min(value)) %>% 
  #   filter(row_number()%% 2==1) %>% 
  #   mutate(preevent = temp.preevent$value) %>% 
  #   mutate(diff = abs(preevent-value))
  # 
  # temp.event_value <- rbind(temp.event_max, temp.event_min) %>% 
  #   group_by(variable) %>% 
  #   slice(which.max(diff)) %>% 
  #   .[,c("DateTime", "variable", "value")]
  
  temp.g <- temp.f %>% 
    mutate(PreEvent=temp.preevent_mag$AverageValue/1000,
           PostEvent = temp.postevent_mag$AverageValue/1000)
  
  temp.h <- filter(wb5, variable==ii)
  
  temp.preevent_volts <- filter(temp.h, DateTime<=(temp.f$ZeroCross- seconds(0.5)) & DateTime>=(temp.f$ZeroCross- seconds(0.8))) %>% 
    group_by(variable) %>% 
    summarise(AverageValue=mean(value))
  
  # temp.postevent <- filter(temp.g, DateTime>=(temp.f$ZeroCross+ seconds(0.5)) & DateTime<=(temp.f$ZeroCross+ seconds(0.51)))
  
  temp.postevent_volts <- filter(temp.h, DateTime>=(temp.f$ZeroCross+ seconds(0.5)) & DateTime<=(temp.f$ZeroCross+ seconds(0.8))) %>% 
    group_by(variable) %>% 
    summarise(AverageValue=mean(value))
  
  # temp.preevent_volts <- filter(temp.h, DateTime<=(temp.f$ZeroCross- seconds(0.5)) & DateTime>=(temp.f$ZeroCross- seconds(0.51)))
  # 
  # temp.postevent_volts <- filter(temp.h, DateTime>=(temp.f$ZeroCross+ seconds(0.5)) & DateTime<=(temp.f$ZeroCross+ seconds(0.51)))
  
  temp.event_max_volts <- filter(temp.h, DateTime>=(temp.f$ZeroCross- seconds(0.5)) & DateTime<=temp.f$ZeroCross+ seconds(0.5)) %>% 
    group_by(variable) %>% 
    filter(value==max(value)) %>% 
    filter(row_number()%% 2==1) %>% 
    mutate(preevent = temp.preevent_volts$AverageValue) %>% 
    mutate(diff =value-preevent)
  
  temp.event_min_volts <- filter(temp.h, DateTime>=(temp.f$ZeroCross- seconds(0.5)) & DateTime<=temp.f$ZeroCross+ seconds(0.5)) %>% 
    group_by(variable) %>% 
    filter(value==min(value)) %>% 
    filter(row_number()%% 2==1) %>% 
    mutate(preevent = temp.preevent_volts$AverageValue) %>% 
    mutate(diff =value-preevent)
  
  temp.event_value_volts <- rbind(temp.event_max_volts, temp.event_min_volts) %>% 
    group_by(variable) %>% 
    slice(which.max(abs(diff))) %>% 
    .[,c("DateTime", "variable", "value")]
  
  names(temp.event_value_volts) <- c("DateTime", "variable", "value")
  
  temp.i <- temp.g %>% 
    mutate(Volts_t0_kv=temp.preevent_volts$AverageValue/1000,
           Volts_min_kV = temp.event_value_volts$value/1000,
           # PostEvent_volts = temp.postevent_volts$AverageValue/1000,
           Feeder = substr(i, 1, 9))
  
   temp.file <- rbind(temp.file, temp.i)
    

  }
  
temp.phase <- temp.phase %>%
  group_by(OriginalFile) %>%
  slice(which.max(maxPhaseAngle))


  ggplot(filter(wb1, DateTime>=(temp.phase$ZeroCross - seconds(0.2)) &DateTime<=(temp.phase$ZeroCross + seconds(1.5))), aes(DateTime, value, colour =variable))+
    geom_line()+
    geom_vline(xintercept = temp.phase$ZeroCross)

  ggsave(paste0(substr(i, 91, 114),temp.phase$Phase,".jpeg"), plot=last_plot(), scale=1)

  wb3 <- wb2 %>%
    filter(DateTime>=(temp.phase$ZeroCross - seconds(0.2)) &DateTime<=(temp.phase$ZeroCross + seconds(1.5))) %>%
    mutate(PowerP1 = PowerP1/1000,
           PowerP2 = PowerP2/1000,
           PowerP3 = PowerP3/1000,
           Frequency = Frequency *6) %>%
    .[,c("DateTime", "PowerP1", "PowerP2", "PowerP3", "Frequency")] %>%
    melt(.,id.vars="DateTime")

  ggplot(wb3, aes(DateTime, value, colour =variable))+
    geom_line()+
    scale_y_continuous(
      "Power (kW)",
      sec.axis = sec_axis(~ . /6, name = "Frequency"))+
    geom_vline(xintercept = temp.phase$ZeroCross, linetype="dashed")

  ggsave(paste0(substr(i, 91, 114),"_PowerCurve",".jpeg"), plot=last_plot(), scale=1)
  
  
  ggplot(wb5, aes(DateTime, value, colour =variable))+
    geom_line()+
    ylab("Volts (Vrms)")+
    geom_vline(xintercept = temp.phase$ZeroCross, linetype="dashed")
  
  ggsave(paste0(substr(i, 91, 114),"_Volts",".jpeg"), plot=last_plot(), scale=1)
  
  temp.output1 <- temp.file[,c("Feeder", "Phase","Volts_t0_kv")] %>% 
    spread(Phase, Volts_t0_kv)
  
  colnames(temp.output1) <- c("Feeder", "V_t0_UL1_kV","V_t0_UL2_kV","V_t0_UL3_kV")
  
  temp.output2 <- temp.file[,c("Feeder", "Phase","Volts_min_kV")] %>% 
    spread(Phase, Volts_min_kV)
  
  colnames(temp.output2) <- c("Feeder", "V_min_UL1_kV","V_min_UL2_kV","V_min_UL3_kV")
  
  temp.output3 <- temp.file[,c("Feeder", "Phase","PreEvent")] %>% 
    spread(Phase, PreEvent)
  
  colnames(temp.output3) <- c("Feeder", "P_t0_UL1","P_t0_UL2_kV","P_t0_UL3_kV")
  
  temp.output4 <- temp.file[,c("Feeder", "Phase","PostEvent")] %>% 
    spread(Phase, PostEvent)
  
  colnames(temp.output4) <- c("Feeder", "P_post_UL1","P_post_UL2_kV","P_post_UL3_kV")
  
  
  temp.output5 <- temp.file[,c("Feeder", "Phase","maxPhaseAngle")] %>% 
    spread(Phase, maxPhaseAngle)
  
  colnames(temp.output5) <- c("Feeder", "VPhaseAngle_Phase1","VPhaseAngle_Phase2","VPhaseAngle_Phase3")
  
  temp.output <- left_join(temp.output1, temp.output2, by="Feeder") %>% 
    left_join(.,temp.output3, by="Feeder") %>% 
    left_join(.,temp.output4, by="Feeder") %>% 
    left_join(.,temp.output5, by="Feeder") %>% 
    mutate(File = unique(temp.file$OriginalFile),
           Time=min(temp.file$ZeroCross))
  
  Output.Table <- rbind(Output.Table, temp.output)
}


temp.file2 <- temp.output %>% 
  mutate(Power_Diff_perc=(abs(EventMagnitude-PreEvent)/PreEvent)*100) %>% 
  mutate(Demand_Before_Event_kW = PreEvent) %>% 
  mutate(Demand_After_Event_kW = PostEvent) %>% 
  mutate(Magnitude_of_Fault_kW = EventMagnitude-PreEvent) %>% 
  mutate(Net_Demand_Increase_kW = PostEvent-PreEvent) %>%
  mutate(Voltage_Phase_angle_deg = maxPhaseAngle) %>% 
  mutate(Voltage_Change_pu = EventMagnitude_volts/(11/sqrt(3))) %>% 
  mutate(DateTime = round_date(ZeroCross, "sec")) %>% 
  .[,c("Feeder","DateTime","Power_Diff_perc", "Demand_Before_Event_kW", "Demand_After_Event_kW", "Magnitude_of_Fault_kW", "Net_Demand_Increase_kW",
       "Voltage_Phase_angle_deg", "Voltage_Change_pu")]
  
  


temp.summary <- temp.file %>% 
  group_by(OriginalFile) %>% 
  summarise(DateTime=mean(ZeroCross),
            Voltage_Phase_Angle_Deg = max(abs(maxPhaseAngle)),
            Demand_Before_Event_kW = mean(PreEvent),
            Demand_After_Event_kW = mean(PostEvent),
            Magnitude_of_Fault_kW = max(abs(EventSize)),
            Net_Demand_Increase_kW = mean(ChangeDemand),
            Number_Phases_Impacted =sum(percDiff>10))



write.csv(temp.file2, "tempresults.csv")
write.csv(temp.summary, "tempsummary.csv")



  
  
      
  temp.odd <- data.frame(x=temp.c[odd_indexes,1])
  
  temp.b <- filter(temp.a, DateTime>=min(DateTime) & DateTime<=min(DateTime)+seconds(0.02))
  
  
  Amplitude1 <- max(temp.b$Waveform.Cycle.64..Phase.V1N)
  Amplitude2 <- max(temp.b$Waveform.Cycle.64..Phase.V2N)
  Amplitude3 <- max(temp.b$Waveform.Cycle.64..Phase.V3N)
  
  temp.c <- filter(temp.b, Waveform.Cycle.64..Phase.V1N>=0)[,c(5,8)]
  temp.d <- head(temp.c[order(temp.c$Waveform.Cycle.64..Phase.V1N),],2)
  
  temp.c <- filter(temp.b, Waveform.Cycle.64..Phase.V1N<=0)[,c(5,8)]
  temp.e <- tail(temp.c[order(temp.c$Waveform.Cycle.64..Phase.V1N),],2)
  
  temp.c <- rbind(temp.d,temp.e)
  temp.c <- temp.c[order(temp.c$DateTime),]
  
  temp.d <- temp.c[1,2]- (((temp.c[1,1]-0)/(temp.c[1,1]-temp.c[2,1]))*(temp.c[1,2]-temp.c[2,2]))
  
  temp.e <- temp.c[3,2]- (((temp.c[3,1]-0)/(temp.c[3,1]-temp.c[4,1]))*(temp.c[3,2]-temp.c[4,2]))
  
  start.value <- ifelse(temp.c[2,1]>temp.c[1,1], temp.d, temp.e)
  end.value <- max(temp.a$DateTime)
  
  newdf <- data.frame(Time = c(start.value:end.value))
                        
  
  
  
  )
  
  temp.b <- temp.b[,c(8,5)] %>% 
    mutate(PosNeg=ifelse(Waveform.Cycle.64..Phase.V1N))
  
  
  
}

ggplot(temp.a, aes(x=Time))+
  geom_line(aes(y=Waveform.Cycle.64..Phase.V1N, colour="V1"))


##identify 0 zero crossing
