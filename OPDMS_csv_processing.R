
### this script is for finding frequency / voltage events in the previously saved High Speed Monitoring data on OPDMS
### in will read in csvs and search for periods where voltage / frequency dropped below x threshold
###
require("dplyr")
require("tidyr")
require("lubridate")
require("ggplot2")
require("reshape2")
require("installr")
library("Hmisc")

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
  
    # print("daytime condition satisfied")
    
    temp.region <- paste(temp.csv[1,2])
    temp.station <- paste(temp.csv[2,2])
    temp.monitor <- paste(temp.csv[3,2])
    temp.date <- paste(temp.csv[4,2])
    temp.start.time <- temp.time
  
  print(paste0("",temp.station,""))
  #### searches by key terms for relevant columns in csv
  temp.length.check <- read.csv(file=i)
  ### if statement to avoid blank csvs
  if (nrow(temp.length.check)>8){
    
  temp.headers.csv <- read.table(file=i,nrows=1,skip=8,sep=",",header=TRUE)
  
  temp.headers <- temp.headers.csv%>%
    gather(head1,head2)%>%
    mutate(key=paste(head1,head2,sep="_"),
           pull=ifelse(grepl("PPS",key) & grepl("kV",key), 1,
                       ifelse(grepl("Hz",key), 1, 0)))
  
  
  ### specify columns to read
  temp.col.collect <- as.integer(c(1,2,paste0(which(grepl(1,temp.headers$pull)))))
  
  ### store the key
  temp.key.save <- temp.headers[temp.col.collect,3]
  
  
  #### read data
  temp.csv <- read.table(file=i, skip=9, sep=",", header=TRUE)
  
  temp.dataframe <- temp.csv%>%
    select(temp.col.collect)
  
  ##################### look for voltage or frequency event | only if voltage or frequency data retrieved
  ###### nominal voltage, mutate to find p.u.
  print("column names in csv:")
  print(temp.headers$key)
  print("retrieved key words:")
  print("PPS + kV  |  Hz")
  print("########################################")
  
  #### if there is voltage or frequency data, then process
  if (ncol(temp.dataframe)>2){
  
 
    ####process for multiple kV columns
    
    temp.cols <- which(grepl("kV",colnames(temp.dataframe)))
    temp.plot.count <- NULL
    
    ### if there are voltage cols then:
    if (length(temp.cols)>0){
    
    for (k in temp.cols){
      
      temp.select <- temp.dataframe%>%
        select(kV=k)
      
      temp.med.volt <- median(temp.select$kV)
      
      temp.nominal.volt <- round(temp.med.volt)
  
      temp.bind <- temp.select%>%
        mutate(pu=kV/temp.nominal.volt,
               plot_v=ifelse(is.nan(pu),0,
                             ifelse(pu>v_max,1,
                                    ifelse(pu<v_min,1,0))))

      temp.key <- temp.key.save[k]
      
      # temp.plot.count <- bind_cols(temp.plot.count,temp.bind)
    
      
      #####
      if (sum(temp.bind$plot_v) >0) {
        
        temp.event <- data.frame(date=temp.date,
                                 time=temp.time,
                                 date.time=temp.time.date,
                                 region=temp.region,
                                 station=temp.station,
                                 duration=((sum(temp.dataframe$plot_v))/50),
                                 min.volt=min(temp.bind$pu),
                                 max.volt=max(temp.bind$pu),
                                 min.freq=NA,
                                 max.freq=NA,
                                 key=temp.key,
                                 csv=i)
        
        
        output.events <- bind_rows(output.events,temp.event)

      
      }}

    ### if there is a frequency column then:
    
    if ("Hz" %in% colnames(temp.dataframe)){
    
    temp.bind <- temp.dataframe%>%
      mutate(plot_f=ifelse(Hz>f_max,1,
                           ifelse(Hz<f_min,1,0)))
    
    
    # temp.plot.count <- bind_cols(temp.plot.count,temp.bind)
    
    if (sum(temp.bind$plot_f) >0) {
      
      temp.key <- temp.key.save[which(grepl("Hz",temp.key.save))]
      
      temp.event <- data.frame(date=temp.date,
                               time=temp.time,
                               date.time=temp.time.date,
                               region=temp.region,
                               station=temp.station,
                               duration=((sum(temp.bind$plot_f))/50),
                               min.volt=NA,
                               max.volt=NA,
                               min.freq=min(temp.bind$Hz),
                               max.freq=max(temp.bind$Hz),
                               key=temp.key,
                               csv=i)

      
      output.events <- bind_rows(output.events,temp.event)
      
      
    }}
    
    
    
    
    # temp.long <- temp.dataframe%>%
    #   gather(unit,value,-No.,-s)
    
  # if ("kV" %in% colnames(temp.csv) & "Hz" %in% colnames(temp.csv)){
  # 
  # temp.dataframe <- temp.csv%>%
  #   select(No.,s,kV,Hz)
  # }
  # 
  # else if ("kV" %in% colnames(temp.csv) & "Hz" %nin% colnames(temp.csv)){
  #   
  #   temp.dataframe <- temp.csv%>%
  #     select(No.,s,kV)%>%
  #     mutate(Hz=NA)
  #   }
  # 
  # else if ("Hz" %in% colnames(temp.csv) & "kV" %nin% colnames(temp.csv)){
  #   
  #   temp.dataframe <- temp.csv%>%
  #     select(No.,s,Hz)%>%
  #     mutate(kV=NA)
  # } else {
  #   temp.dataframe <- NULL
  #   
  #   # print("column names condition not satisfied")
  #   print("#####")
  #   }
  # 
  #### if there is voltage or frequency data, then process
  # (grepl("kV",colnames(temp.dataframe)) | "Hz" %in% colnames(temp.dataframe))
  
  #####
    # 
    # temp.cols <- which(grepl("plot",colnames(temp.plot.count)))
    # temp.plot.sum <- NULL
    # 
    # for (p in temp.cols){
    # 
    #   temp.select <- temp.plot.count%>%
    #     select(plot=p)
    # 
    #  p <- sum(temp.select$p)}

    
  
  # if ((sum(temp.dataframe$plot_f) | (sum(temp.dataframe$plot_v))) >0) {
  #   
  #   temp.event <- data.frame(date=temp.date,
  #                            time=temp.time,
  #                            date.time=temp.time.date,
  #                            region=temp.region,
  #                            station=temp.station,
  #                            f.duration=((sum(temp.dataframe$plot_f))/50),
  #                            v.duration=((sum(temp.dataframe$plot_v))/50),
  #                            min.freq=ifelse((sum(temp.dataframe$plot_f))>0,min(temp.dataframe$Hz),NA),
  #                            max.freq=ifelse((sum(temp.dataframe$plot_f))>0,max(temp.dataframe$Hz),NA),
  #                            min.volt=ifelse((sum(temp.dataframe$plot_v))>0,min(temp.dataframe$pu),NA),
  #                            max.volt=ifelse((sum(temp.dataframe$plot_v))>0,max(temp.dataframe$pu),NA))
  #   
  #   
  #   output.events <- bind_rows(output.events,temp.event)

    #### close for "if there is data"
      }
  #### close for plot condition >0
  }
  #### close for "if there is voltage/ frequency data" statement
  }
  #### close daytime if statement
  }
  #### close loops 
  } } }
#####

# rm(list=ls(pattern="temp."))
  
##### end
  





