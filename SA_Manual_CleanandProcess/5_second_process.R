#####

setwd("~/GitHub/DER_Event_analysis/SolarAnalytics_analysis/output/Alternative_duration_analysis")

file.name <- list.files()
# file.name <- "4500_2017_12_09_101547"

#Read in cleaned Script 

for (f in file.name){

setwd(paste0("~/GitHub/DER_Event_analysis/SolarAnalytics_analysis/output/Alternative_duration_analysis/",f,""))

load(paste0("cleaned_",substr(f, 1, 15),".R"))

data.date <- paste0(substr(f, 1, 15))
EventTime <- ymd_hms(paste0(substr(f, 6, 15)," ",substr(f, 17, 18),":",substr(f, 19, 20),":",substr(f, 21, 22)), tz="Australia/Brisbane")

print(EventTime)



temp.5s <- temp.5s.cleaned%>%
  mutate(power_kW_5sec = 0.72*energy_polarity/3600)

#check nominal voltage and create p.u.

temp.med.volt <- median(temp.5s$voltage)

temp.nominal.volt <- ifelse(temp.med.volt> 235 & temp.med.volt <=247,240,
                            ifelse(temp.med.volt>=225 & temp.med.volt <235,230,
                                   print(paste0("median voltage is ",temp.med.volt,", unable to determine nominal voltage"))))

temp.5s <- temp.5s%>%
  mutate(p.u.=voltage/temp.nominal.volt)

#Filter for Voltage value at time of the event -> temp.volt_t0

t_0 <- EventTime
t_end_estimate_nadir <- EventTime + minutes(4)
t_end_nadir <- EventTime + minutes(2)
t_prior <- EventTime - minutes(1)
t_hourprior <- EventTime - hours(1)
t_hourafter <- EventTime + hours(1)


temp.t0 <- filter(temp.5s, ts>=(t_0 - seconds(14)) & ts<= (t_0 + seconds(14))) %>%
  .[,c("c_id", "ts","voltage","p.u.","power_kW_5sec")] 


temp.tnear <- filter(temp.5s, ts>=t_hourprior & ts<= t_hourafter) %>%
  .[,c("c_id", "ts","voltage","p.u.","power_kW_5sec")] 


### plot time immediately at event
c_ids <- unique(temp.t0$c_id)

for (c in c_ids){
  temp.plot <- temp.t0%>%
    filter(c_id==c)


 P1 <- ggplot(temp.plot, aes(ts, power_kW_5sec), colour=c_id)+
    geom_line()+
    facet_wrap(~c_id)+
    geom_vline(xintercept=EventTime,colour="black",linetype="dashed")+
    ggtitle("power at t0")

 
 P2 <- ggplot(temp.plot, aes(ts, p.u.), colour=c_id)+
   geom_line()+
   facet_wrap(~c_id)+
   geom_vline(xintercept=EventTime,colour="black",linetype="dashed")+
   ggtitle("voltage at t0")
 
 G1 <- ggplotGrob(P1)
 G2 <- ggplotGrob(P2)
 G3 <- rbind(G1,G2,size="first")
 
 grid.newpage()
 # grid.draw(G3)
 ggsave(paste("5s_data_event_time",c,".jpeg"),plot=G3)
}


### plot time hours before/after at event
c_ids <- unique(temp.tnear$c_id)

for (c in c_ids){
  temp.plot <- temp.tnear%>%
    filter(c_id==c)
  
  
  P1 <- ggplot(temp.plot, aes(ts, power_kW_5sec), colour=c_id)+
    geom_line()+
    facet_wrap(~c_id)+
    geom_vline(xintercept=EventTime,colour="black",linetype="dashed")+
    ggtitle("power at t0")
  
  
  P2 <- ggplot(temp.plot, aes(ts, p.u.), colour=c_id)+
    geom_line()+
    facet_wrap(~c_id)+
    geom_vline(xintercept=EventTime,colour="black",linetype="dashed")+
    ggtitle("voltage at t0")
  
  G1 <- ggplotGrob(P1)
  G2 <- ggplotGrob(P2)
  G3 <- rbind(G1,G2,size="first")
  
  grid.newpage()
  # grid.draw(G3)
  ggsave(paste("5s_data_hours_near_event",c,".jpeg"),plot=G3)
}

### plot whole day

c_ids <- unique(temp.5s$c_id)

for (c in c_ids){
  temp.plot <- temp.5s%>%
    filter(c_id==c)
  
  
  P1 <- ggplot(temp.plot, aes(ts, power_kW_5sec), colour=c_id)+
    geom_point()+
    facet_wrap(~c_id)+
    geom_vline(xintercept=EventTime,colour="black",linetype="dashed")+
    ggtitle("power at t0")
  
  
  P2 <- ggplot(temp.plot, aes(ts, p.u.), colour=c_id)+
    geom_point()+
    facet_wrap(~c_id)+
    geom_vline(xintercept=EventTime,colour="black",linetype="dashed")+
    ggtitle("voltage at t0")
  
  G1 <- ggplotGrob(P1)
  G2 <- ggplotGrob(P2)
  G3 <- rbind(G1,G2,size="first")
  
  grid.newpage()
  # grid.draw(G3)
  ggsave(paste("5s_data_day0",c,".jpeg"),plot=G3)
}

#### final loop

}
####
