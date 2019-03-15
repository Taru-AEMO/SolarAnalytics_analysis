### run this after voltage prep script. takes dataframes as inputs


setwd(paste0("~/GitHub/DER_Event_analysis/SolarAnalytics_analysis/output/ToAnalyse"))

file.name <- list.files()


for (i in file.name){

setwd(paste0("~/GitHub/DER_Event_analysis/SolarAnalytics_analysis/output/ToAnalyse/",file.name,""))


EventTime <- ymd_hms(paste0(substr(i, 6, 15)," ",substr(i, 17, 18),":",substr(i, 19, 20),":",substr(i, 21, 22)), tz="Australia/Brisbane")

print(EventTime)

#### load data frames

load(file=paste0("volt_d0_",file.name,".R"))
load(file=paste0("volt_t0_",file.name,".R"))

temp.volt_t0 <- volt_t0
Final_clean <- volt_d0


t_0 <- EventTime
t_end_estimate_nadir <- EventTime + minutes(4)
t_end_nadir <- EventTime + minutes(2)
t_prior <- EventTime - minutes(1)
t_hourprior <- EventTime - hours(1)
t_hourafter <- EventTime + hours(1)



######################################### FINDING SYSTEMS WITH SIGNIFICANT RAMP AT TIME OF EVENT ##################
######################## everything inside this loop is looking at individual systems
#### loop to find daily ramps

c_ids <- unique(temp.volt_t0$c_id)

temp.all.systems <- NULL
temp.all.systems.events <- NULL


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
           abs_delta=abs(delta_pu),
           pc=paste0(substr(file.name,1,4)))%>%
    select(-lag_pu,-lag_voltage)
  
  
  ##### isolate change in pu for minutes near event
  
  ### this must be NULL because not all systems have data at time of event - make sure not using previous loop's data?
  temp.single_id.event <- NULL
  
  temp.single_id.event <- temp.single_id%>%
    filter(ts>t_prior & ts<t_end_estimate_nadir)
  
  
  # ########################################################### Comment out to cancel plot outputs  
  ####################################################################################
  ############################### PLOT OPTION 1    -> temp.single_id.event.plot
    #####  uncomment this section to only plot systems where PV voltage dip is within 0.05pu of DNSP recorded dip
    #
    #  temp.single_id.event.plot <- temp.single_id.event%>%
    #    mutate(plot=(ifelse(abs_delta >= dv,
    #                         "1",
    #                        ifelse(abs_delta > dv-0.05,
    #                               "1",
    #                               "0"))))%>%
    #    filter(plot==1)


    ############################### PLOT OPTION 2     -> temp.single_id.event.plot
    ############### un comment this section to only plot systems where voltage dip in top 99.8th percentile of the day
    ### if the change in voltage at time of event is bigger than q, then plot that system

    q <- quantile(temp.single_id$abs_delta,.998)

    temp.single_id.event.plot <- temp.single_id.event%>%
      mutate(plot=(ifelse(abs_delta > q,"1","0")))%>%
      filter(plot==1)

    ## what was the max change in pu seen during event -> d
    temp.d <- temp.single_id.event.plot%>%
      filter(abs_delta==max(abs_delta))

    d <- temp.d$delta_pu


    ################## a loop for plotting systems based on Plot option 1 or plot option 2
    p_ids <- unique(temp.single_id.event.plot$c_id)

    for (p in p_ids){

      ######################################################## PLOT A
      #### plot pu for entire day
      ggplot(temp.single_id,aes(ts,p.u.))+
        labs(title=sprintf("24Hours  %s",EventTime),subtitle = paste0("ID = ",c,"   delta: ",d,""))+
        geom_vline(aes(xintercept=EventTime),colour="green",size=6,alpha=0.3)+
        geom_vline(aes(xintercept=EventTime),colour="black",linetype="dashed")+
        scale_y_continuous(limits=c(0.90,1.1))+
        # geom_vline(aes(xintercept=EventTime+minutes(22)),colour="black",linetype="dashed")+
        geom_line()
      ggsave(filename=sprintf("%s/PlotA_c_id_%s.png",file.name,c))


      ########################################################## PLOT B

      temp.single_id.hoursnear <- NULL

      temp.single_id.hoursnear <- temp.single_id%>%
        filter(t_hourprior<ts & ts<t_hourafter)

      #### plot pu for 2 hours either side of event
        ggplot(temp.single_id.hoursnear,aes(ts,p.u.))+
        labs(title=sprintf("2Hours  %s",EventTime),subtitle = paste0("ID = ",c,"  delta: ",d,""))+
        geom_vline(aes(xintercept=EventTime),colour="blue",size=6,alpha=0.3)+
        geom_vline(aes(xintercept=EventTime),colour="black",linetype="dashed")+
        scale_y_continuous(limits=c(0.90,1.1))+
        # geom_vline(aes(xintercept=EventTime+minutes(2)),colour="black",linetype="dashed")+
        geom_line()
      ggsave(filename=sprintf("%s/PlotB_c_id_%s.png",file.name,c))


      ########################################################## PLOT C

      temp.single_id.minsnear <- temp.single_id%>%
        filter(t_prior<ts & ts<t_end_estimate_nadir)

      #### plot pu for minutes either side of event

      ggplot(temp.single_id.minsnear,aes(ts,p.u.))+
        labs(title=sprintf("~6Minutess  %s",EventTime),subtitle = paste0("ID = ",c,"  delta: ",d,""))+
        # geom_vline(aes(xintercept=EventTime),colour="blue",size=6,alpha=0.3)+
        geom_vline(aes(xintercept=EventTime),colour="black",linetype="dashed")+
        scale_y_continuous(limits=c(0.90,1.1))+
        # geom_vline(aes(xintercept=EventTime+minutes(2)),colour="black",linetype="dashed")+
        geom_line()
      ggsave(filename=sprintf("%s/PlotC_c_id_%s.png",file.name,c))


      ####make list of all systems that were plotted with relevant pu / voltage info

      temp.all_ids <- bind_rows(temp.all_ids,temp.d)

      ## close plot loop (for p in p_id)
      }

   ##### save list of all the systems that were plotted
   systems.identified <- temp.all_ids

   ### only uncomment this if you have the temp.v0_vnadir_v1_v2 df
   # systems.identified <- left_join(select(temp.all_ids,-plot),select(temp.v0_vnadir_v1_v2,c_id,v_0,pu_0,pu_nadir),by="c_id")
   #
   #   systems.identified <- systems.identified%>%
   #    mutate(nadir_delta=pu_nadir-pu_0)

   ##Print confirmation
   print(paste("Number of IDs saw voltage change near event time:", length(unique(temp.all_ids$c_id))))

   print(paste("These IDs are:", (systems.identified), ""))

   write.csv(systems.identified,file=sprintf("%s/%s_systems_ids.csv",file.name,file.name),row.names=TRUE)
  
             
####################################################################################
############################################################# END of single system plots #########         
             
############################################################# CATEGORISATION PLOTS    
# ################################################################## 
# ########## voltage plots by power category ###### these plots draw off the categorisations that is done in the process script
# ################### i.e. the systems response to the disturbance

#   ###### graphs for the minutes near the event
#     temp.nadir.cat <- temp.nadir.df%>%
#       group_by(Category,c_id) %>% 
#       summarise(count =n())%>%
#       group_by(Category) %>% 
#       summarise(count = n())%>%
#       mutate(Legend=paste0(Category," (n=",count,")"))
#     
#     temp.nadir.df.cat <- left_join(temp.nadir.df,temp.nadir.cat,by="Category")
#   
#     temp.nadir.agg <- aggregate(p.u. ~ ts + Legend, temp.nadir.df.cat, mean)
#     
#       ggplot(temp.nadir.agg,aes(x=ts,y=p.u.,colour=Legend))+
#       geom_line(size=1)+
#       labs(title=sprintf("%s",EventTime),subtitle = "mean p.u. by disconnection category")+
#       geom_hline(aes(yintercept=1.00),colour="black",linetype="dashed")+
#       geom_vline(aes(xintercept=EventTime),colour="red",linetype="dashed")
#     
#     ggsave(filename=sprintf("%s/PU_by_disc_category.png",file.name))
#     
#     ###### same as above but for voltage
#     
#     # temp.nadir.agg <- aggregate(voltage ~ ts + Legend, temp.nadir.df.cat, mean)
#     # 
#     # ggplot(temp.nadir.agg,aes(x=ts,y=voltage,colour=Legend))+
#     #   geom_line(size=1)+
#     #   labs(title = paste("mean voltage for the time of event by category"))+
#     #   geom_hline(aes(yintercept=temp.nominal.volt),colour="black",linetype="dashed")+
#     #   geom_vline(aes(xintercept=EventTime),colour="red",linetype="dashed")
#     # 
#     # ggsave(filename=sprintf("%s/Voltage_time_of_event_by_category.png",file.name))
#     
#     ################ basic category
#     ###### pu
#     
#     temp.nadir.catb <- temp.nadir.df%>%
#       group_by(Category_basic,c_id) %>% 
#       summarise(count =n())%>%
#       group_by(Category_basic) %>% 
#       summarise(count = n())%>%
#       mutate(Legend=paste0(Category_basic," (n=",count,")"))
#     
#     temp.nadir.df.catb <- left_join(temp.nadir.df,temp.nadir.catb,by="Category_basic")
#     
#     temp.nadir.agg <- aggregate(p.u. ~ ts + Legend, temp.nadir.df.catb, mean)
#     
#     ggplot(temp.nadir.agg,aes(x=ts,y=p.u.,colour=Legend))+
#       geom_line(size=1)+
#       labs(title=sprintf("%s",EventTime),subtitle = "mean p.u. by disconnection category (basic)")+
#       geom_hline(aes(yintercept=1.00),colour="black",linetype="dashed")+
#       geom_vline(aes(xintercept=EventTime),colour="red",linetype="dashed")
#     
#     ggsave(filename=sprintf("%s/PU_by_category(basic).png",file.name))
#     
#     ###### same as above but for voltage
#     
#     # temp.nadir.agg <- aggregate(voltage ~ ts + Legend, temp.nadir.df.catb, mean)
#     # 
#     # ggplot(temp.nadir.agg,aes(x=ts,y=voltage,colour=Legend))+
#     #   geom_line(size=1)+
#     #   labs(title = paste("mean voltage for the time of event by category (basic)"))+
#     #   geom_hline(aes(yintercept=temp.nominal.volt),colour="black",linetype="dashed")+
#     #   geom_vline(aes(xintercept=EventTime),colour="red",linetype="dashed")
#     # 
#     # ggsave(filename=sprintf("%s/Voltage_time_of_event_by_category(basic).png",file.name))
# 
#  
# ##################################### graphs for the hours and days near the event
#   ## #graph p.u. voltage for the day
# 
#   temp.final.clean <- left_join(Final_clean,temp.nadir.cat,by="Category")
#     
#   temp.volt_d0 <- aggregate(p.u. ~ ts + Legend, temp.final.clean, mean)
# 
#   ggplot(temp.volt_d0,aes(ts,p.u.,colour=Legend))+
#     geom_line()+
#     labs(title=sprintf("Day of %s",EventTime),subtitle = "mean p.u. by disconnection category")+
#     geom_hline(aes(yintercept=1.00),colour="black",linetype="dashed")+
#     geom_vline(aes(xintercept=EventTime),colour="red",linetype="dashed")
# 
#   ggsave(filename=sprintf("%s/PU_24H.png",file.name))
#   
#   #graph p.u. for the 2 hours either side of event
#   temp.t.hours <- filter(temp.final.clean, ts>t_hourprior & ts<=t_hourafter)
# 
#   temp.volt_h0 <- aggregate(p.u. ~ ts + Legend, temp.t.hours, mean)
#   
#   
#   ggplot(temp.volt_h0,aes(ts,p.u.,colour=Legend))+
#     geom_line()+
#     labs(title=sprintf("Hours of %s",EventTime),subtitle = "mean p.u. by disconnection category")+
#     geom_hline(aes(yintercept=1.00),colour="black",linetype="dashed")+
#     geom_vline(aes(xintercept=EventTime),colour="red",linetype="dashed")
#   
#   ggsave(filename=sprintf("%s/PU_4H.png",file.name))
# 
# ##################################################################  
# ################################################################## END OF CATEGORISATION PLOTS  



################ postcode summaries #### bind together every system in this disturbance event 

## pu. for all time

temp.all.systems <- bind_rows(temp.all.systems,temp.single_id)

## p.u. for events

temp.all.systems.events <- bind_rows(temp.all.systems.events,temp.single_id.event)


#### close loop for (c in c_ids)

}

############### BOXPLOTS
#################
temp.all.systems <- temp.all.systems%>%
  mutate(Legend="All_Times")

temp.all.systems.events <- temp.all.systems.events%>%
  mutate(Legend="Disturbance")

##
temp.plot <- bind_rows(temp.all.systems,temp.all.systems.events)

ggplot(temp.plot,aes(Legend,delta_pu,fill=Legend))+
  geom_boxplot(varwidth = FALSE, alpha=0.2) +
  ggtitle(sprintf("change in p.u. for event: %s ",EventTime))+
  scale_fill_brewer(palette="Set3")

ggsave(sprintf("%s_boxplot_dist.png",file.name))

###

ggplot(temp.plot,aes(Legend,p.u.,fill=Legend))+
  geom_boxplot(varwidth = FALSE, alpha=0.2) +
  ggtitle(sprintf("p.u. for event: %s ",EventTime))+
  scale_fill_brewer(palette="Set3")

ggsave(sprintf("%s_boxplot_dist_pu.png",file.name))


################## 
################## output a csv file which as the average and max pu_delta seen at each event -> DNSP.comp.events
## data for all systems for all events

if(temp.dv<0){

  max.delta.all.systems <- min(temp.all.systems.events$delta_pu)

  temp.filter <- temp.all.systems.events%>%
    filter(delta_pu<0)

  avg.delta.all.systems <- mean(temp.filter$delta_pu)
} else {

  max.delta.all.systems <- max(temp.all.systems.events$delta_pu)

  temp.filter <- temp.all.systems.events%>%
    filter(delta_pu>0)

  avg.delta.all.systems <- mean(temp.filter$delta_pu)
}

temp.DNSP.comp <- data.frame(event=EventTime,
                             average.dip=avg.delta.all.systems,
                             max.dip=max.delta.all.systems)

DNSP.comp.events <- bind_rows(DNSP.comp.events,temp.DNSP.comp)
ggplot(agg.delta.all.systems,aes(abs_delta))+geom_density()
ggsave(filename=sprintf("%s/max_delta_density.png",file.name))


#### all systems for all events ### one is entire day data, one is event time data ### this bit doesn't run because of RAM

all.systems <- bind_rows(all.systems,temp.all.systems)

all.systems <- all.systems%>%
  mutate(Legend="No_Disturbance")


all.systems.events <- bind_rows(all.systems.events,temp.all.systems.events)

all.systems.events <- all.systems.events%>%
  mutate(Legend="Disturbance")
################## 
############# close loop for (i in input.files)

}

#################### BOX PLOTS based on postcode #### this bit doesn't run because of RAM
# ##################   
#
#   postcodes <- c("4701","4555","4551","4550")
#   
#   for (p in postcodes){
#   temp.plot <- temp.filter%>%
#     filter(pc==postcodes)%>%
#     filter(Legend=="Disturbance")
# 
#   ggplot(temp.plot,aes(pc,delta_pu,colour=pc))+
#     geom_boxplot(varwidth = TRUE, alpha=0.2) +
#     scale_fill_brewer(palette="Set3")
# 
#   ggsave("boxplot_dist.png")
#   }
# 
#   temp.plot <- temp.filter%>%
#     filter(pc==postcodes)%>%
#     filter(Legend=="No_Disturbance")
#   
#   ggplot(temp.plot,aes(pc,delta_pu,colour=pc))+
#     geom_boxplot(varwidth = TRUE, alpha=0.2) +
#     scale_fill_brewer(palette="Set3")
#   
#   ggsave("boxplot_nodist.png")

################## 
########## write csv outputs

write.csv(DNSP.comp.events,file="DNSP_comp_events.csv")


###

rm(list=ls(pattern="temp."))


  