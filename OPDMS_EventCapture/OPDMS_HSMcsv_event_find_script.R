
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

v_min <- 0.90
v_max <- 1.1

### NOFB
f_min <- 49.85
f_max <- 50.15

########
output.events <- NULL
output.med.voltage <- NULL
setwd("C:/Users/RTandy/Documents/OPDMS Data/HSM")

date.file <- list.files()
# date.file <- "201903"

#### start loop for date files
for (d in date.file){
  
  setwd(paste0("C:/Users/RTandy/Documents/OPDMS Data/HSM/",d,""))
  
  monitor.file <- list.files()
  # monitor.file <- "BLTS"
  
  
  # find specific monitors
  # find.monitors <- c("H2_SPINE","H13_ROSS","RCTS","BETS","CBTS","TSTS","FBTS","BLTS")
  # monitor.file <- monitor.file[monitor.file %in% find.monitors]
  
  
  #### start loop for monitor files
  for (m in monitor.file){
    
    setwd(paste0("C:/Users/RTandy/Documents/OPDMS Data/HSM/",d,"/",m,""))
    
    input.csv.name <- list.files()
    # input.csv.name <- "20190303-1512-VIC-BLTS-BLTS_2_2.csv"
    # input.csv.name <- input.csv.name[1]
    
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
      # if (temp.time<="19:00:00" & temp.time >="08:00:00"){
      
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

        
        ##### average of 3 phases
        # temp.headers <- temp.headers.csv%>%
        #   gather(head1,head2)%>%
        #   mutate(key=paste(head1,head2,sep="_"),
        #          pull=ifelse(grepl("PPS",key) & grepl("kV",key), 1,
        #                      ifelse(grepl("Hz",key), 1, 0)))
        
        
        #### multiple phases
        temp.headers <- temp.headers.csv%>%
          gather(head1,head2)%>%
    mutate(key=paste0(head1,"_",head2),
           unit=paste0(".",head2,"."),
           pull=ifelse(grepl(".kV.",unit,fixed=TRUE), 1,
                       ifelse(grepl(".Hz.",unit,fixed=TRUE), 1,
                              ifelse(grepl(".V.",unit,fixed=TRUE), 1,0))))
  
  
  
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
  # print("column names in csv:")
  # print(temp.headers$key)
  # print("retrieved key words:")
  # print("V | kV  |  Hz")
  print("########################################")
  
  #### if there is voltage or frequency data, then process
  if (ncol(temp.dataframe)>2){
  
 
    ####process for multiple kV columns
    
    temp.cols <- which(grepl("V",colnames(temp.dataframe)) | grepl("kV",colnames(temp.dataframe)))
    temp.plot.count <- NULL
    
    ### if there are voltage cols then:
    if (length(temp.cols)>0){
    
    for (temp.k in temp.cols){
      
      temp.select <- temp.dataframe%>%
        select(temp.k)
      
      if("V.1" %in% colnames(temp.select) | "V.2" %in% colnames(temp.select) |
         "V.3" %in% colnames(temp.select) | "V.4" %in% colnames(temp.select) |
         "V.5" %in% colnames(temp.select) | "V.6" %in% colnames(temp.select) |
         "V.7" %in% colnames(temp.select) | "V.8" %in% colnames(temp.select) |
         "V.9" %in% colnames(temp.select) | "V.10" %in% colnames(temp.select) |
         "V.11" %in% colnames(temp.select) | "V.12" %in% colnames(temp.select) |
         "V.13" %in% colnames(temp.select) | "V.14" %in% colnames(temp.select)){names(temp.select)[1]<-paste("V")
      }
      
      if("kV.1" %in% colnames(temp.select) | "kV.2" %in% colnames(temp.select) |
         "kV.3" %in% colnames(temp.select) | "kV.4" %in% colnames(temp.select) |
         "kV.5" %in% colnames(temp.select) | "kV.6" %in% colnames(temp.select) |
         "kV.7" %in% colnames(temp.select) | "kV.8" %in% colnames(temp.select) |
         "kV.9" %in% colnames(temp.select) | "kV.10" %in% colnames(temp.select) |
         "kV.11" %in% colnames(temp.select) | "kV.12" %in% colnames(temp.select) |
         "kV.13" %in% colnames(temp.select) | "kV.14" %in% colnames(temp.select)){names(temp.select)[1]<-paste("kV")
      }
      
      
      ## if volts, change to kV
      if("V" %in% colnames(temp.select)){
        temp.select <- temp.select %>% 
          mutate(kV=V*0.001) %>% 
          select(-V)
      }
      
      ### if for nominal voltage
      temp.rows <- temp.select[1:10,]

      
      temp.med.volt <- ((median(temp.rows))*sqrt(3))
      
      
      temp.nominal.volt <- ifelse(temp.med.volt>=450 & temp.med.volt<=550,500,
                                  ifelse(temp.med.volt>300 & temp.med.volt<=365,330,
                                         ifelse(temp.med.volt>=247.5 & temp.med.volt<=300,275,
                                                ifelse(temp.med.volt>=198 & temp.med.volt<=242,220,
                                                  ifelse(temp.med.volt>120 & temp.med.volt<=146,132,
                                                         ifelse(temp.med.volt>=99 & temp.med.volt<=120,110,
                                                                ifelse(temp.med.volt>=59 & temp.med.volt<=75,66,
                                                                       ifelse(temp.med.volt>=19 & temp.med.volt<=26,22,
                                                                              ifelse(temp.med.volt>=9 & temp.med.volt<=15,11,
                                                                                     ifelse(temp.med.volt<2,0,
                                                                                            temp.med.volt))))))))))
      
      

      ###
      temp.bind <- temp.select%>%
        mutate(kV_sq=kV*sqrt(3),
               pu=ifelse(is.nan(kV_sq/temp.nominal.volt),NA,
                         ifelse(is.infinite(kV_sq/temp.nominal.volt),NA,
                                kV_sq/temp.nominal.volt)),
               plot_v=ifelse(is.na(pu),0,
                             ifelse(pu>v_max,1,
                                    ifelse(pu<v_min,1,0))))
      
      median(temp.bind$pu)
      
      
      ## comment in this to change for delta pu search instead of min max search
               # lag_pu=lag(pu),
               # d_pu=pu-lag_pu,
               # plot_v=ifelse(is.na(d_pu),0,
               #               ifelse(d_pu>0.1,1,0)))



      temp.key <- temp.key.save[temp.k]

      # temp.plot.count <- bind_cols(temp.plot.count,temp.bind)

      
      ## un comment if you want to plot & savethe monitors with ~0 median voltage
      # if (temp.nominal.volt==0){
      # 
      #   temp.zero.nom <- data.frame(key=temp.key,
      #                           csv=i,
      #                           station=temp.station,
      #                           date.time=temp.time.date,
      #                           med.voltage=temp.med.volt)
      # 
      #   output.med.voltage <- bind_rows(output.med.voltage,temp.zero.nom)
      # 
      # 
      # 
      #   temp.plot <- bind_cols(select(temp.dataframe,s),temp.bind)
      # 
      #   ggplot(temp.plot,aes(s,kV_sq))+
      #     geom_line()
      # 
      #   ggsave(file=paste0("C:/Users/RTandy/Documents/OPDMS Data/zero_",substr(i,1,29),"_",temp.key,".png"))
      # 
      # }
      
      
      

      # #####
      if (sum(temp.bind$plot_v) >0) {
        
        # temp.plot <- bind_cols(select(temp.dataframe,s),temp.bind)
        # 
        # ggplot(temp.plot,aes(s,pu))+
        #   geom_line()
        # 
        # dir.create(paste0("C:/Users/RTandy/Documents/OPDMS Data/",substr(temp.date,7,10),substr(temp.date,4,5),substr(temp.date,1,2)))
        # ggsave(file=paste0("C:/Users/RTandy/Documents/OPDMS Data/",
        #                  substr(temp.date,7,10),substr(temp.date,4,5),substr(temp.date,1,2),
        #                  "/plots_",substr(i,1,29),"_",temp.key,".png"))
        
        
        ### find the event time
        temp.mutate <- temp.bind %>% 
          mutate(deviation=abs(1-pu),
                 max.dev=max(deviation))
        
        temp.time.find <- bind_cols(temp.mutate,select(temp.dataframe,No.,s)) %>% 
          filter(deviation==max.dev)
        
        temp.time.offset <- as.numeric(temp.time.find$s[1])
        
        temp.t1 <- temp.time.date + seconds(temp.time.offset)
        
        ###
        
        temp.event <- data.frame(date=temp.date,
                                 time=temp.time,
                                 date.time=temp.time.date,
                                 region=temp.region,
                                 station=temp.station,
                                 event.time=temp.t1,
                                 duration=((sum(temp.bind$plot_v))/50),
                                 min.volt=min(temp.bind$pu),
                                 max.volt=max(temp.bind$pu),
                                 min.freq=NA,
                                 max.freq=NA,
                                 key=temp.key,
                                 csv=i)


        output.events <- bind_rows(output.events,temp.event)


      }
      }

    ## if there is a frequency column then:

    if ("Hz" %in% colnames(temp.dataframe)){

    temp.bind <- temp.dataframe%>%
      mutate(plot_f=ifelse(Hz>f_max,1,
                           ifelse(Hz<f_min,1,0)))


    # temp.plot.count <- bind_cols(temp.plot.count,temp.bind)

    if (sum(temp.bind$plot_f) >0) {

      temp.key <- temp.key.save[which(grepl("Hz",temp.key.save))]

      ###
      # temp.plot <- bind_cols(select(temp.dataframe,s),temp.bind)
      # 
      # ggplot(temp.plot,aes(s,Hz))+
      #   geom_line()+
      #   ylim(49,51)
      #   
      # 
      # dir.create(paste0("C:/Users/RTandy/Documents/OPDMS Data/",substr(temp.date,7,10),substr(temp.date,4,5),substr(temp.date,1,2)))
      # ggsave(file=paste0("C:/Users/RTandy/Documents/OPDMS Data/",
      #                    substr(temp.date,7,10),substr(temp.date,4,5),substr(temp.date,1,2),
      #                    "/plots_",substr(i,1,29),"_",temp.key,".png"))


      ### find the event time
      temp.mutate <- temp.bind %>% 
        mutate(deviation=abs(50-Hz),
               max.dev=max(deviation))
      
      temp.time.find <- bind_cols(temp.mutate,select(temp.dataframe,No.,s)) %>% 
        filter(deviation==max.dev)
      
      temp.time.offset <- as.numeric(temp.time.find$s[1])
      
      temp.t1 <- temp.time.date + seconds(temp.time.offset)
      ###
      
      temp.event <- data.frame(date=temp.date,
                               time=temp.time,
                               date.time=temp.time.date,
                               region=temp.region,
                               station=temp.station,
                               event.time=temp.t1,
                               duration=((sum(temp.bind$plot_f))/50),
                               min.volt=NA,
                               max.volt=NA,
                               min.freq=min(temp.bind$Hz),
                               max.freq=max(temp.bind$Hz),
                               key=temp.key,
                               csv=i)


      output.events <- bind_rows(output.events,temp.event)


    }}

    #### close for "if there is data"
      }

  #### close for plot condition >0
  }

  #### close for "if there is voltage/ frequency data" statement
  }

  #### close daytime if statement
  # }
  
  rm(list=ls(pattern="temp."))

}

}
rm(list=ls(pattern="temp."))
}
#####

setwd("C:/Users/RTandy/Documents/OPDMS Data")
save(output.events,file=gsub(":","",gsub(" ","_",paste0("Event_search_",Sys.time(),".R"))))
write.csv(output.events,file=gsub(":","",gsub(" ","_",paste0("Event_search_",Sys.time(),".csv"))))
  
##### end


temp.output.events <- filter(output.events,min.volt>0)



