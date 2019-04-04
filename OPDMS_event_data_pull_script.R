####################################################################
################################### this script will pull all the data for the events found in OPDMS_csv_processing.R

setwd("C:/Users/RTandy/Documents/OPDMS Data/manual files")

load("Event_search_2019-03-26_153412.R")


#################################################
 ###### PULL data for events

temp.events.to.plot <- output.events
  
  # filter(date=="03/03/2019")

######

dates <- unique(temp.events.to.plot$date)
# dates <- "20/11/2018"

for (d in dates){

temp.dates <- temp.events.to.plot%>%
  filter(date==d)
  
stations <- unique(temp.dates$station)


temp.bind.station <- NULL

for (s in stations){
  
  setwd(paste0("C:/Users/RTandy/Documents/OPDMS Data/HSM/",substr(d,7,10),"",substr(d,4,5),"/",s,""))
  
  temp.station <- temp.dates%>%
    filter(station==s)
  
  csv <- unique(temp.station$csv)
  
  temp.bind.csv <- NULL
  
  for (c in csv){
  
    #### define col collect 
    
    temp.headers.csv <- read.table(file=c,nrows=1,skip=8,sep=",",header=TRUE)
    
    temp.headers <- temp.headers.csv%>%
      gather(head1,head2)%>%
      mutate(key=paste(head1,head2,sep="_"),
             pull=ifelse(grepl("PPS",key) & grepl("kV",key), 1,
                         ifelse(grepl("Hz",key), 1,
                                ifelse(grepl("MW",key),1,0))))
    
    ### specify columns to read
    temp.col.collect <- as.integer(c(1,2,paste0(which(grepl(1,temp.headers$pull)))))
    
    
    #### read data
    temp.csv <- read.table(file=c, skip=8, sep=",", header=TRUE)
    
    temp.dataframe <- temp.csv%>%
      select(temp.col.collect)
    

  # temp.col.collect <- c(substr(temp.csv$key,1,nchar(temp.csv$key)-3))
  
  
  temp.read <- temp.dataframe%>%
    # select(temp.col.collect)%>%
    gather(legend,value,-Sample,-Time.Offset)%>%
    mutate(value=as.numeric(value),
           csv=c,
           station=s,
           date=d)%>%
    filter(is.na(value)==FALSE)

  
  temp.bind.csv <- bind_rows(temp.bind.csv,temp.read)
  
  
  }

  dir.create(paste0("C:/Users/RTandy/Documents/OPDMS Data/Event_data/",substr(d,1,2),"",substr(d,4,5),"",substr(d,7,10),""))

  write.csv(temp.bind.csv, file=paste0("C:/Users/RTandy/Documents/OPDMS Data/Event_data/",substr(d,1,2),"",substr(d,4,5),"",substr(d,7,10),"/",s,"_",substr(d,1,2),"",substr(d,4,5),"",substr(d,7,10),".csv"))
  
  
  
}


}







  
  