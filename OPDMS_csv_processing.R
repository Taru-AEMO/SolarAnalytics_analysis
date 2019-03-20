
### this script is for finding frequency / voltage events in the previously saved High Speed Monitoring data on OPDMS
### in will read in csvs and search for periods where voltage / frequency dropped below x threshold
###
require("dplyr")
require("tidyr")
require("lubridate")
require("ggplot2")
require("reshape2")
require("installr")

######## set thresholds for voltage and frequency pick ups

v_min <- 0.8
v_max <- 1.2

### NOFB
f_min <- 49.85
f_max <- 50.15

########
output.events <- NULL
setwd("C:/Users/RTandy/Documents/OPDMS Data/HSM")

date.file <- list.files()
# date.file <- "201802"

#### start loop for date files
for (d in date.file){
  
setwd(paste0("C:/Users/RTandy/Documents/OPDMS Data/HSM/",d,""))

monitor.file <- list.files()
# monitor.file <- "H13_ROSS"

#### start loop for monitor files
for (m in monitor.file){

setwd(paste0("C:/Users/RTandy/Documents/OPDMS Data/HSM/",d,"/",m,""))

input.csv.name <- list.files()

#### start loop for input csvs
for (i in input.csv.name){
  
  #### read details and time date format fix
  temp.csv <- read.csv(file=i, header = FALSE)
  
  temp.time <- paste(temp.csv[5,2])
  temp.date <- paste(temp.csv[4,2])
  
  temp.time.date <- dmy_hms(paste(temp.date,temp.time,sep=" "),tz="Australia/Brisbane")
  
  temp.time <- strftime(temp.time.date, format ="%T", usetz=TRUE, tz="Australia/Brisbane")
  
  print(paste0("",temp.time.date,""))
  
    ##### if occured during daytime then read data
  if (temp.time<="19:00:00" & temp.time >="08:00:00"){
  
    print("daytime condition satisfied")
    
    temp.region <- paste(temp.csv[1,2])
    temp.station <- paste(temp.csv[2,2])
    temp.monitor <- paste(temp.csv[3,2])
    temp.date <- paste(temp.csv[4,2])
    temp.start.time <- temp.time
  
  #### read data
  temp.csv <- read.table(file=i,skip=9,sep=",",header=TRUE)

  
  ##################### look for voltage or frequency event
  ###### nominal voltage, mutate to find p.u.
  print("column names in this csv:")
  print(colnames(temp.csv))
  print("column names (freq/voltage) that can be retrieved by this script are:  'kV' 'Hz' " )
  print("please check for relevant col names in your csv")
  
  if ("kV" %in% colnames(temp.csv) | "Hz" %in% colnames(temp.csv)){
  
  temp.dataframe <- temp.csv%>%
    select(No.,s,kV,Hz)
  
  temp.med.volt <- median(temp.dataframe$kV)
  
  temp.nominal.volt <- round(temp.med.volt)
  # temp.nominal.volt <- ifelse(temp.med.volt> 235 & temp.med.volt <=247,240,
  #                             ifelse(temp.med.volt>=225 & temp.med.volt <235,230,
  #                                    ifelse(temp.med.volt> 56 & temp.med.volt <=75,66,
  #                                           ifelse(temp.med.volt> 235 & temp.med.volt <=247,240,
  #                                                  ifelse(temp.med.volt> 235 & temp.med.volt <=247,240,
  #                                                         ifelse(temp.med.volt> 235 & temp.med.volt <=247,240,))
  #                                    print(paste0("median voltage is ",temp.med.volt,", unable to determine nominal voltage"))))
  
  ## maybe delete this
  # temp.dataframe <- temp.dataframe%>%
  #   mutate(pu=kV/temp.nominal.volt,
  #          lag.Hz=lag(Hz),
  #          lag.kV=lag(kV),
  #          lag.pu=lag(pu),
  #          delta.kV=kV-lag.kV,
  #          delta.Hz=Hz-lag.Hz,
  #          delta.pu=pu-lag.pu)%>%
  #   select(-lag.Hz,-lag.kV,-lag.pu)%>%
  #   filter(is.na(temp.dataframe$delta.pu)==FALSE)
  
  temp.dataframe <- temp.dataframe%>%
    mutate(pu=kV/temp.nominal.volt,
           plot_v=ifelse(pu>v_max,1,
                       ifelse(pu<v_min,1,0)),
           plot_f=ifelse(Hz>f_max,1,
                         ifelse(Hz<f_min,1,0)))
  
  
  if ((sum(temp.dataframe$plot_f) + (sum(temp.dataframe$plot_v))) >0) {
    
    temp.event <- data.frame(date=temp.date,
                             time=temp.time,
                             date.time=temp.time.date,
                             region=temp.region,
                             station=temp.station,
                             f.duration=((sum(temp.dataframe$plot_f))/50),
                             v.duration=((sum(temp.dataframe$plot_v))/50),
                             min.freq=ifelse((sum(temp.dataframe$plot_f))>0,min(temp.dataframe$Hz),NA),
                             max.freq=ifelse((sum(temp.dataframe$plot_f))>0,max(temp.dataframe$Hz),NA),
                             min.volt=ifelse((sum(temp.dataframe$plot_v))>0,min(temp.dataframe$pu),NA),
                             max.volt=ifelse((sum(temp.dataframe$plot_v))>0,max(temp.dataframe$pu),NA))
    
    
    output.events <- bind_rows(output.events,temp.event)
    
  }

  #####
  }else {print("column names condition not satisfied, frequency or voltage data not found")
        print("##############")}
  #### close daytime if statement
  }
  #### close (i for input.csv.name)
  } } }
#####

rm(list=ls(pattern="temp."))
  
##### end
  








