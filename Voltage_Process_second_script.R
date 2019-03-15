
### run this after voltage prep script. takes dataframes as inputs
### you don't have to run voltage process first before this one

setwd("~/GitHub/DER_Event_analysis/SolarAnalytics_analysis/output/ToAnalyse")

file.name <- list.files()
# file.name <- "4500_2017_12_09_101622"

plot <- 0
all.systems.count <- NULL

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
  t_1 <- EventTime - minutes(1)
  # t_prior <- EventTime - minutes(1)
  t_hourprior <- EventTime - hours(1)
  t_hourafter <- EventTime + hours(1)
  

######################################### MULTUPLE SYSTEMS, TREND ANALYSIS
#### this section... 

# temp.aggregate <- volt_d0%>%
#   select(ts,c_id,voltage)%>%
#   mutate(time=substr(ts,12,19))
# 
# write.csv(temp.aggregate,file="volt_d0.csv")

###### find average value for last 10 minutes and categorise systems based on after event value vs last 10 minutes value
  
  temp.t10 <- volt_d0%>%
    select(ts,c_id,voltage,p.u.)%>%
    filter(ts>t_10 & ts<EventTime)%>%
    arrange(c_id,ts)
    
  c_ids <- unique(volt_t0$c_id)
  
  temp.bind <- NULL
  
   for (c in c_ids){
  
  temp.single.t10 <- temp.t10%>%
    filter(c_id==c)
  
  temp.norm <- aggregate(voltage ~ c_id, temp.single.t10, mean)
  
  
  temp.volt_near <- volt_d0%>%
    select(ts,c_id,voltage,p.u.)%>%
    filter(ts>t_1 & ts<t_end_estimate_nadir, c_id==c)
  
  temp.temp <- left_join(temp.volt_near,select(temp.norm,c_id,voltage.norm=voltage),by="c_id")
  
  temp.bind <- bind_rows(temp.bind,temp.temp)
   }
  ##### categories : applies category to system ID based on the largest change from ~voltage~ before event VS after event.

  temp.bind <- temp.bind%>%
    mutate(relative=voltage/voltage.norm,
           dif=round(abs(1-relative),digits=7))
  
  #####  assign quartiles  
  
  # temp.max.dif <- aggregate(dif ~ c_id, temp.bind, max)
  
  # temp.max.dif <- within(temp.max.dif, quartile <- as.integer(cut(dif, quantile(dif, probs=0:4/4), include.lowest=TRUE)))
  ##### 
  
  # temp.temp <- left_join(temp.bind,select(temp.max.dif,c_id,max.dif=dif),by="c_id")

  ##### sense check for categories plot
 # if (plot = '1'){
  
   ggplot(temp.bind,aes(ts,relative,colour=c_id))+
    geom_point()+
    labs(title="relative voltage immediately after event")
  
  ggsave(filename="PlotA.png",device="png")
  
  ##### density plot
  ggplot(temp.bind,aes(relative,colour=c_id))+
    geom_density()+
    labs(title="voltage immediately after event, relative to voltage ~10 minutes~ prior")
  
  ggsave(filename="PlotB.png",device="png")
 # }
  
  #### 
  
  temp.bind.levels <- temp.bind%>%
    mutate(lev=ifelse(relative>0.98 & relative<1.02,"nil",
                      ifelse(relative>=1.02,"moderate_inc",
                             ifelse(relative<=0.98,"moderate_dec","na"))))
  temp.cat <- NULL
  
  for (c in c_ids){
    
    temp.max.dif <- temp.bind.levels%>%
      filter(c_id==c)%>%
      filter(dif==(max(dif)))%>%
      select(c_id,lev,voltage.norm)
    
    temp.cat <- bind_rows(temp.cat,temp.max.dif)
  }

  temp.cat <- unique(temp.cat)
  
  ####
  
  temp.event.near <- volt_d0%>%
    select(ts,c_id,voltage,p.u.,power_kW)%>%
    filter(ts>(t_0 - minutes(4)) & ts <=t_end_estimate_nadir)%>%
    arrange(c_id,ts)
   
  ### sample size
      temp.temp <- temp.cat%>%
        group_by(lev,c_id) %>%
        summarise(count =n())%>%
        group_by(lev) %>%
        summarise(count = n())%>%
        mutate(Legend=paste0(lev," (n=",count,")"))
  
      temp.cat <- left_join(select(temp.temp,lev,Legend),temp.cat,by="lev")
      ###
  temp.event.near <- left_join(temp.event.near,temp.cat,by="c_id")

  ###
  temp.event.near <- temp.event.near%>%
    mutate(relative.voltage=voltage/voltage.norm)
  
##### group by categories and plot
  # if (plot = 1){
  
  temp.plot <- aggregate(relative.voltage ~ ts + Legend,temp.event.near,mean)
  
  ggplot(temp.plot,aes(ts,relative.voltage,colour=Legend))+
    geom_line()+
    labs(title="voltage before an after event, relative to (10min) average voltage",
         subtitle=paste0(EventTime))+
    geom_vline(xintercept=EventTime)
  
  ggsave(filename="PlotC.png",device="png")
  # }
######

temp.all <- temp.cat%>%
    select(c_id,lev)%>%
    mutate(event=EventTime)
  
  temp.all <- unique(temp.all)
  
all.systems.count <- bind_rows(all.systems.count,temp.all)
  
  
##### close loop for (i in file.name)
}

temp.all.systems.count <- all.systems.count%>%
  group_by(event,lev) %>%
  summarise(count =n())
  # mutate(Legend=paste0(lev," (n=",count,")"))

ggplot(temp.all.systems.count,aes(x = event, y = count, fill = lev))+
  geom_bar(stat = "identity")

# check this: 
# <180 , >260, must disconnect after 1 second. >265 must disconnect

#################### END
rm(list=ls(pattern="temp."))


