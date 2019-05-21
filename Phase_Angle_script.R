library("openxlsx")
library("ggplot2")
library("dplyr")
library("lubridate")
 library("tidyr")
# library("XLConnect")
library("reshape2")
# library("gtable")
# library("grid") # low-level grid functions are required
# library("raster")
# library("SDMTools")
library("grid")
library("gridExtra")


setwd("~/GitHub/DER_Event_analysis/SolarAnalytics_analysis/input/EQ_data/CSV data from EQL/BRD11A/Oscilloscope data/")

temp.xcel <- list.files(pattern=".asc")

# temp.xcel <- c("BRD11A_date_11.11.18_time_10.36.44.asc", "BRD11A_date_16.11.18_time_08.34.20.asc")

op <- options(digits.secs=6)

Output.file <- NULL

for (i in temp.xcel){
  
  print(paste("Processing", i))
  
  wb <- read.table(i, header=TRUE, skip = 10) %>% 
    mutate(DateTime=paste(Date, Time)) %>% 
    mutate(DateTime=ydm_hms(DateTime, tz="Australia/Brisbane"))

  
  
  wb_plot <- wb[,c("DateTime", "U12", "U23", "U31")] %>% 
        melt(.,id.vars="DateTime")
        
  
  
  ###REmove due to not enough readings
  temp.df <- wb_plot[order(wb_plot$DateTime),] %>%  
    mutate(TimeDiff=DateTime-lag(DateTime)) %>% 
    na.omit()
  
  temp.remove.readings<- temp.df %>% 
    filter(TimeDiff>0.05) %>% 
    slice(which.min(DateTime)) %>% 
    mutate(mintime=DateTime-TimeDiff-seconds(0.01))
    
  
  ###Remove due to error in Magnitude values
  temp.max <- tail(head(sort(wb_plot$value, decreasing=TRUE), 200),1)
  
  temp.remove.magnitude <- filter(wb_plot, value>=temp.max-1000) %>% 
    slice(which.max(DateTime))
  # temp.remove.magnitude <- max(temp.filter$DateTime)
  
  min.time1 <-  temp.remove.readings$mintime
  
  min.time2 <- temp.remove.magnitude$DateTime
  
  if(length(min.time1)==0){
    min.time1 <- max(wb$DateTime)
  }else  min.time1 <- temp.remove.readings$mintime
    
    
  DateTime_Remove <- pmin(min.time1, min.time2)
  
  wb_new1 <- filter(wb, DateTime<DateTime_Remove-seconds(0.001))
  
  
  
  wb_new2 <- filter(wb_new1, DateTime>min(DateTime)+seconds(0.02))  %>% 
    mutate(Time_round=format(round(as.numeric(substr(Time, 7,15)), digits=2), nsmall = 2)) %>% 
    .[,c("Time_round","U12", "U23", "U31")] %>% 
    melt(.,id.vars="Time_round") %>% 
    group_by(Time_round) %>% 
    summarise(max_value=max(value)) %>% 
    filter(., max_value>=-0.1) %>% 
    filter(.,max_value<=1) %>% 
    slice(which.min(Time_round))

  if(nrow(wb_new2)==0){
    wb_new <- wb_new1
  }else if(nrow(wb_new2)==1){
    wb_new <- filter(wb_new1, DateTime<as.POSIXct(paste0(substr(min(DateTime),1,17), wb2$Time_round))-seconds(0.03))
}else {print("Check what's wrong")
  wb_new <- wb_new1}
  # ASSESSING REMOVED TIME SERIES DATA
  # ggplot(wb_plot, aes(DateTime, value, colour =variable))+
  #   geom_line()+
  #   geom_vline(xintercept = temp.remove.magnitude$DateTime, color="red", linetype = "dashed", size=1, alpha=0.2)+
  #   geom_vline(xintercept = temp.remove.readings$mintime, color="yellow", size=5, alpha=0.2)
  # 
  # 
  # ggsave(paste0(i,"_WholeChart.jpeg"), plot=last_plot(), scale=1)
  # 
  #   
  # 
  # temp.output <- temp.remove.readings %>% 
  #   mutate(Event = i) %>% 
  #   mutate(EventMaxTime = max(wb_plot$DateTime)) %>% 
  #   mutate(TimeRemoved_Readings = (EventMaxTime -mintime)) %>% 
  #   mutate(TimeRemoved_Magnitude = EventMaxTime - temp.remove.magnitude$DateTime) %>% 
  #   .[,c("Event", "EventMaxTime", "TimeRemoved_Readings", "TimeRemoved_Magnitude")]
  # 
  # Output.file <- rbind(Output.file, temp.output)
#   
# }

    # wb_new <-  filter(wb_new, DateTime<=max(temp.filter$DateTime))
    # mutate(DateTime=ymd_hms(paste(Date, Time),tz = "Australia/Brisbane"))
  # 
  # names(wb) <- c("DateandTime", "PhaseI1", "PhaseI2", "PhaseI3", "PhaseV1", "PhaseV2", "PhaseV3", "DateTime")
    
  # wb1 <- wb_new[,c("DateTime", "UL1", "UL2", "UL3")] %>% 
  #   melt(.,id.vars="DateTime")
  
  # temp.max <- max(wb1$value)
  # temp.filter <- filter(wb1, value>=temp.max-95)
  
  # wb_new <-  filter(wb_new, DateTime<=max(temp.filter$DateTime))
  
  wb1 <- wb_new[,c("DateTime", "U12", "U23", "U31")] %>% 
    melt(.,id.vars="DateTime")
  
  # wb2 <- read.xlsx(i, sheet="High_resolution_Half_cycle") %>% 
  #   mutate(DateTime=dmy_hms(Date.and.Time,tz = "Australia/Brisbane"))
  # 
  # names(wb2) <- c("DateandTime", "PowerP1", "PowerP2", "PowerP3", "Frequency", "ReactivePowerP1", "ReactivePowerP2", "ReactivePowerP3", "RMSI1", "RMSI2", "RMSI3", "RMSV1", "RMSV2", "RMSV3", "DateTime")
  # 
  wb4 <- wb_new %>%
    .[,c("DateTime", "I1", "I2", "I3")]

  names(wb4) <- c("DateTime", "U12", "U23", "U31")

  wb4 <- wb4 %>%
    melt(.,id.vars="DateTime")
  # 
  # 
  # wb5 <- wb2 %>% 
  #   .[,c("DateTime", "RMSV1", "RMSV2", "RMSV3")]
  # 
  # names(wb5) <- c("DateTime", "PhaseV1", "PhaseV2", "PhaseV3") 
  # 
  # wb5 <- wb5%>% 
  #   melt(.,id.vars="DateTime")
  
  # rms_filename <- paste0(substr(i,1,7),"RMS",substr(i,20,51))
  # 
  # setwd("~/GitHub/DER_Event_analysis/SolarAnalytics_analysis/input/EQ_data/CSV data from EQL/BRD11A/RMS data")
  # 
  # RMS_file <- read.table(rms_filename, header=TRUE, skip = 10) %>% 
  #   mutate(DateTime=paste(Date, Time)) %>% 
  #   mutate(DateTime=ydm_hms(DateTime, tz="Australia/Brisbane"))
  # 
  # setwd("~/GitHub/DER_Event_analysis/SolarAnalytics_analysis/input/EQ_data/CSV data from EQL/BRD11A/Oscilloscope data")
  # 
  
  # 
  temp.phase <- NULL
  
  for (ii in c("U12", "U23", "U31")){
    print(paste("Processing", ii))
  
  
  
    temp.c <- wb1[order(wb1$DateTime),] %>% 
      filter(variable==ii)  %>%
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
  
  temp.c <-  temp.c[order(temp.c$ZeroCross),] 

  maxtime <- max(temp.c$ZeroCross)
  # odd_indexes <- seq(1,nrow(temp.c),2)
  
  temp.d <- filter(temp.c, PosNeg==1) %>% 
    mutate(difference = ZeroCross - lag(ZeroCross)) %>% 
    na.omit() %>% 
    filter(.,is.finite(difference)) %>% 
    filter(ZeroCross<=(max(temp.c$ZeroCross)-seconds(0.02))) %>%
    slice(which.max(difference)) %>% 
    mutate(maxPhaseAngle = ((difference -0.02)/0.02)*360) %>% 
    mutate(Phase=paste0(ii)) %>% 
    mutate(OriginalFile=paste0(i))
    
  temp.e <- filter(temp.c, NegPos==1) %>% 
    mutate(difference = ZeroCross - lag(ZeroCross)) %>% 
    na.omit() %>% 
    filter(.,is.finite(difference)) %>% 
    filter(ZeroCross<=(max(temp.c$ZeroCross)-seconds(0.02))) %>%
    slice(which.max(difference)) %>% 
    mutate(maxPhaseAngle = ((difference -0.02)/0.02)*360) %>% 
    mutate(Phase=paste0(ii)) %>% 
    mutate(OriginalFile=paste0(i))
  
  
  # temp.dmin <- filter(temp.c, PosNeg==1) %>% 
  #   mutate(difference = ZeroCross - lag(ZeroCross)) %>% 
  #   na.omit() %>% 
  #   slice(which.min(difference)) %>% 
  #   mutate(maxPhaseAngle = 350+((difference -0.02)/0.02)*360) %>% 
  #   mutate(Phase=paste0(ii)) %>% 
  #   mutate(OriginalFile=paste0(i))
  # 
  # temp.emin <- filter(temp.c, NegPos==1) %>% 
  #   mutate(difference = ZeroCross - lag(ZeroCross)) %>% 
  #   na.omit() %>% 
  #   slice(which.min(difference)) %>% 
  #   mutate(maxPhaseAngle = 360+((difference -0.02)/0.02)*360) %>% 
  #   mutate(Phase=paste0(ii)) %>% 
  #   mutate(OriginalFile=paste0(i))
  
  temp.f <- rbind(temp.d, temp.e) %>% 
    # rbind(.,temp.dmin) %>% 
    # rbind(.,temp.emin) %>%
    group_by(OriginalFile) %>% 
    slice(which.max(maxPhaseAngle))
  
  temp.phase <- rbind(temp.phase, temp.f) 
  
  temp.g <- filter(wb4, variable==ii) %>% 
    mutate(variable=paste0("I", substr(ii, 3,3)))
    
  temp.preevent <- filter(temp.g, DateTime<=(temp.f$ZeroCross)) %>% 
    slice(which.max(DateTime))

  temp.postevent <- filter(temp.g, DateTime>=(temp.f$ZeroCross)) %>% 
    slice(which.min(DateTime))
  
  temp.event_max <- filter(temp.g, DateTime>=temp.preevent$DateTime& DateTime<= max(temp.g$DateTime)) %>% 
    group_by(variable) %>% 
    filter(value==max(value)) %>% 
    filter(row_number()%% 2==1) %>% 
    mutate(preevent = temp.preevent$value) %>% 
    mutate(diff = abs(preevent-value))
  
  temp.event_min <- filter(temp.g, DateTime>=temp.preevent$DateTime & DateTime<=max(temp.g$DateTime)) %>% 
    group_by(variable) %>% 
    filter(value==min(value)) %>% 
    filter(row_number()%% 2==1) %>% 
    mutate(preevent = temp.preevent$value) %>% 
    mutate(diff = abs(preevent-value))
  
  temp.event_value <- rbind(temp.event_max, temp.event_min) %>% 
    group_by(variable) %>% 
    slice(which.max(diff)) %>% 
    .[,c("DateTime", "variable", "value")]
  
  temp.h <- temp.f %>% 
    mutate(PreEvent=temp.preevent$value,
           EventMagnitude = temp.event_value$value,
           PostEvent = ifelse(length(unique(temp.postevent$value))>0, temp.postevent$value, 0))
  
  #EventDetails <- paste0(substr(i, 1,3),"_",substr(i, 23,24),substr(i, 26,27), substr(i, 29,30), "_",substr(i, 37,38), substr(i, 40,41),substr(i, 43,44))
  EventDetails <- paste0(substr(i, 1,6),"_",substr(i, 26,27),substr(i, 29,30), substr(i, 32,33), "_",substr(i, 40,41), substr(i, 43,44),substr(i, 46,47))

  # minplot <- temp.f$ZeroCross-seconds(0.3)
  # maxplot <- temp.f$ZeroCross+seconds(0.7)
  # 
  # 
  # 
  # temp.plot.values1 <- filter(wb1,variable==ii) %>% 
  #   filter(DateTime>= minplot & DateTime<= maxplot)
  # 
  # temp.plot.values2 <- temp.g %>% 
  #   filter(DateTime>= minplot & DateTime<= maxplot)
  
  # p1 <- ggplot(temp.plot.values1, aes(x = DateTime, y=value))+
  #   geom_line()+
  #   ylab("Voltage (Volts)")+
  #   # scale_x_date()+
  #   geom_vline(xintercept = temp.f$ZeroCross, color="yellow", size=5, alpha=0.1)
  # # ggtitle(paste(ii))
  # 
  # p2 <- ggplot(temp.plot.values2, aes(x = DateTime, y=value))+
  #   geom_line()+
  #   ylab("Current (Amps)")+
  #   # geom_line(stat="identity")+
  #   # facet_wrap(~c_id, scales = "free_y")+
  #   geom_vline(xintercept = temp.f$ZeroCross, color="yellow", size=5, alpha=0.1)
  # ggtitle(paste(ii))
  
  # p3 <- ggplot(temp.disconnect.values2, aes(x = ts, y=voltage/240))+
  #   geom_line()+
  #   geom_vline(xintercept = EventTime, linetype="dashed")
  # 
  # g1 <- ggplotGrob(p1)
  # #g1 <- gtable_add_cols(g1, unit(0,"mm")) # add a column for missing legend
  # g2 <- ggplotGrob(p2)
  # 
  # g <- rbind(g1, g2, size="first") # stack the two plots
  # g$widths <- unit.pmax(g1$widths, g2$widths) # use the largest widths
  # # center the legend vertically
  # #g$layout[grepl("guide", g3$layout$name),c("t","b")] <- c(1,nrow(g))
  # grid.newpage()
  # grid.draw(g)
  # 
  # ggsave(paste0(EventDetails,"_",ii,"_Chart.jpeg"), plot=g, device="jpeg")

  
  # 
  # ggplot(filter(temp.plot, DateTime>=(temp.f$ZeroCross - seconds(0.2)) &DateTime<=(temp.f$ZeroCross + seconds(0.3))), aes(DateTime, value, colour =variable))+
  #   geom_line()+
  #   geom_vline(xintercept = temp.f$ZeroCross, color="yellow", size=5, alpha=0.2)
  # 
  # ggsave(paste0(EventDetails,"_",ii,"_PhaseJump.jpeg"), plot=last_plot(), scale=1)
  # 
  # 
  # temp.h <- filter(wb5, variable==ii)
  # 
  # temp.preevent_volts <- filter(temp.h, DateTime<=(temp.f$ZeroCross- seconds(0.3)) & DateTime>=(temp.f$ZeroCross- seconds(0.31)))
  # 
  # temp.postevent_volts <- filter(temp.h, DateTime>=(temp.f$ZeroCross+ seconds(0.5)) & DateTime<=(temp.f$ZeroCross+ seconds(0.51)))
  # 
  # temp.event_max_volts <- filter(temp.h, DateTime>=temp.preevent_volts$DateTime & DateTime<=temp.postevent_volts$DateTime) %>% 
  #   group_by(variable) %>% 
  #   filter(value==max(value)) %>% 
  #   filter(row_number()%% 2==1) %>% 
  #   mutate(preevent = temp.preevent_volts$value) %>% 
  #   mutate(diff = abs(preevent-value))
  # 
  # temp.event_min_volts <- filter(temp.h, DateTime>=temp.preevent_volts$DateTime & DateTime<=temp.postevent_volts$DateTime) %>% 
  #   group_by(variable) %>% 
  #   filter(value==min(value)) %>% 
  #   filter(row_number()%% 2==1) %>% 
  #   mutate(preevent = temp.preevent_volts$value) %>% 
  #   mutate(diff = abs(preevent-value))
  # 
  # temp.event_value_volts <- rbind(temp.event_max_volts, temp.event_min_volts) %>% 
  #   group_by(variable) %>% 
  #   slice(which.max(diff)) %>% 
  #   .[,c("DateTime", "variable", "value")]
  # 
  # names(temp.event_value_volts) <- c("DateTime", "variable", "value")
  # 
  # temp.i <- temp.g %>% 
  #   mutate(PreEvent_volts=temp.preevent_volts$value/1000,
  #          EventMagnitude_volts = temp.event_value_volts$value/1000,
  #          PostEvent_volts = temp.postevent_volts$value/1000,
  #          Feeder = substr(i, 1, 9))
  # 
   Output.file <- rbind(Output.file, temp.h)
    

  }
  
  temp.phase <- temp.phase %>%
    group_by(OriginalFile) %>%
    slice(which.max(maxPhaseAngle))
  
  # temp.phase <- tail(sort(temp.phase$ZeroCross),1)


  #EventDetails <- paste0(substr(i, 1,3),"_",substr(i, 23,24),substr(i, 26,27), substr(i, 29,30), "_",substr(i, 37,38), substr(i, 40,41),substr(i, 43,44))
  EventDetails <- paste0(substr(i, 1,6),"_",substr(i, 26,27),substr(i, 29,30), substr(i, 32,33), "_",substr(i, 40,41), substr(i, 43,44),substr(i, 46,47))
  
  
  
  ggplot(filter(wb1, DateTime>=(temp.phase[1,]$ZeroCross - seconds(0.2)) &DateTime<=(temp.phase[1,]$ZeroCross + seconds(0.3))), aes(DateTime, value, colour =variable))+
    geom_line()+
    geom_vline(xintercept = temp.phase[1,]$ZeroCross, color="yellow", size=5, alpha=0.2)

  ggsave(paste0(EventDetails,"_AllPhases.jpeg"), plot=last_plot(), scale=1)

  # wb3 <- wb4 %>%
  #   filter(DateTime>=(temp.phase$ZeroCross - seconds(0.2)) &DateTime<=(temp.phase$ZeroCross + seconds(1.5))) %>%
  #   .[,c("DateTime", "Current1", "Current2", "Current3", "Frequency")] %>%
  #   melt(.,id.vars="DateTime")

  ggplot(filter(wb4,DateTime>=(temp.phase[1,]$ZeroCross - seconds(0.2)) &DateTime<=(temp.phase[1,]$ZeroCross + seconds(0.3))), aes(DateTime, value, colour =variable))+
    geom_line()+
    scale_y_continuous(
      "Current (Amps)")+
    geom_vline(xintercept = temp.phase[1,]$ZeroCross, color="yellow", size=5, alpha=0.2)

  ggsave(paste0(EventDetails,"_AllCurrents.jpeg"), plot=last_plot(), scale=1)
  
  
  # ggplot(wb5, aes(DateTime, value, colour =variable))+
  #   geom_line()+
  #   ylab("Volts (Vrms)")+
  #   geom_vline(xintercept = temp.phase$ZeroCross, linetype="dashed")
  # 
  # ggsave(paste0(substr(i, 91, 114),"_Volts",".jpeg"), plot=last_plot(), scale=1)

}


temp.output5 <- Output.file[,c("OriginalFile", "Phase","maxPhaseAngle")] %>% 
  spread(Phase, maxPhaseAngle)

colnames(temp.output5) <- c("Feeder", "VPhaseAngle_Phase1","VPhaseAngle_Phase2","VPhaseAngle_Phase3")


write.csv(temp.output5, "PhaseAngle_output_updated.csv")
