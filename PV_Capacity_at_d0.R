
##### this script is for finding the % at which PV is generating as a specific time, i.e. time of disturbance

library("stringr")
library("plyr")
library("RODBC")
library("data.table")
require("dplyr")
require("tidyr")
require("lubridate")
require("ggplot2")
require("reshape2")
require("installr")
library("gtable")
library("grid")
library("gridExtra")

con <- odbcConnect("WARE", uid = "", pwd = "")


###################################### read in events list .csv 
####### the csv must be filled in with the times / location for which you want to find the PV generating capacity
####### if the postcode is not known input the subregion
####### please follow naming conventions: TAS / TASSOUTH / TASNORTH / QLD / QLDSOUTH / QLDNORTH / QLDCENTRAL / VIC / NSW / SA

setwd("~/GitHub/DER_Event_analysis/SolarAnalytics_analysis/input/")

temp.events.csv <- read.csv("events_list_input.csv", header=TRUE)


temp.events <- temp.events.csv%>%
  mutate(time_date=paste(date,time,sep=" "),
         ts=dmy_hms(time_date,tz="Australia/Brisbane"),
         region=as.character(region),
         postcode=as.numeric(postcode))


input.events <- rownames(temp.events)
temp.events <- cbind(event.id=input.events, temp.events)

output <- NULL


for (i in input.events){

#########################################

temp.event <- temp.events%>%
  filter(event.id==i)


#### change region format for the sql

if(is.na(temp.event$postcode)==FALSE){
  
temp.event <- temp.event%>%
  mutate(region_sql=ifelse(postcode<=2999 & postcode>=2000,"NSW1",
         ifelse(postcode<=3999 & postcode>=3000,"VIC1",
                ifelse(postcode<=5999 & postcode>=5000,"SA1",
                       ifelse(postcode<=4999 & postcode>=4703,"QLDNORTH",
                              ifelse(postcode<=4702 & postcode>=4601,"QLDCENTRAL",
                                     ifelse(postcode<=4600 & postcode>=4000,"QLDSOUTH","no valid postcode")))))))
}
# 
# if(is.na(temp.event$region)==FALSE){
# 
# temp.event <- temp.event%>%
#   mutate(region_sql=ifelse(region=="SA","SA1",
#                                  ifelse(region=="VIC","VIC1",
#                                         ifelse(region=="NSW","NSW1",
#                                                ifelse(region=="QLDSOUTH",region,
#                                                       ifelse(region=="QLDCENTRAL",region,
#                                                              ifelse(region=="QLDNORTH",region,
#                                                                     ("no valid region"))))))))
# }



####

Region <- (temp.event$region_sql)
EventTime <- (temp.event$ts)

####

EstEventTime <-   round_date(EventTime, "30 minutes")

#### format for SQL inputs

Pre_EventTime <- gsub("-","/",paste(EstEventTime - days(1)))
Post_EventTime <- gsub("-","/",paste(EstEventTime + days(1)))

###################################### CER installed capacities per region

temp.capacities <- data.frame(region=c("QLDNORTH","QLDCENTRAL","QLDSOUTH","NSW","VIC","SA","TASNORTH","TASSOUTH"),
                              cap_KW=c(398808,220950,1794489,1965772,1553691,1020921,70713,62911))

temp.capacities <- temp.capacities%>%
  mutate(region=as.character(region),
         cap_MW=cap_KW*0.001)

# # region _ postcode _ capacity
# # Queensland North	SGUPV_QLDNORTH	4703 - 4999	          398,808 
# # Queensland Central	SGUPV_QLDCENTRAL	4601 - 4702		    220,950 
# # Queensland South	SGUPV_QLDSOUTH	4000 - 4600		        1,794,489 
# # NSW	SGUPV_NSW1	2000 - 2999		                          1,965,772 
# # Victoria	SGUPV_VIC1	3000 - 3999		                    1,553,691 
# # SA	SGUPV_SA1	5000 - 5999		                            1,020,921 
# # Tasmania North	SGUPV_TASNORTH	7191 - 7999		          70,713 
# # Tasmania South	SGUPV_TASSOUTH	7000 - 7190		          62,911 
# # Queensland total	SGUPV_QLD1	4000 - 4999		            2,414,247 
# # Tasmania total	SGUPV_TAS1	7000 - 7999	  	            133,625 



####################################### "HH Daily Actual"
sql1 = sprintf("select r.site,
               r.vtime,
               r.powermean as \"HH Daily Actual\"
               from anemos.act_sgupv_pred r,
               (select t.site,
               t.fallback,
               t.external,
               t.prodsite,
               t.summated_priority,
               t.atime
               from anemos.act_sgupv_run t
               where t.summated_priority =
               (select max(x.summated_priority)
               from anemos.act_sgupv_run x
               where x.atime = t.atime
               and x.site = t.site)
               -- the date range
               and t.atime >= to_date('%s', 'yyyy/mm/dd hh24:mi:ss') +1
               and t.atime <= to_date('%s', 'yyyy/mm/dd hh24:mi:ss') ) y
               where r.site = y.site
               and r.fallback = y.fallback
               and r.external = y.external
               and r.prodsite = y.prodsite
               and r.atime = y.atime
               and r.site in (select
               (case areaid
               when 'NSW1' then 'SGUPV_NSW1'
               when 'QLDSOUTH' then 'SGUPV_QLDS'
               when 'QLDNORTH' then 'SGUPV_QLDN'
               when 'QLDCENTRAL' then 'SGUPV_QLDC'
               when 'TASNORTH' then 'SGUPV_TASN'
               when 'TASSOUTH' then 'SGUPV_TASS'
               when 'SA1' then 'SGUPV_SA1'
               when 'VIC1' then 'SGUPV_VIC1'
               else 'N/A'
               end ) Area
               from registration.region_area r
               where r.effectivedate = (select max(x.effectivedate)
               from registration.region_area_trk x
               where x.regionid = r.regionid and x.authorisedby is not null
               )
               and r.versionno = (select max(x.versionno)
               from registration.region_area_trk x
               where x.regionid = r.regionid and x.authorisedby is not null
               and x.effectivedate = r.effectivedate
               )
               and r.areaid in ('%s')
               )
               order by r.site, r.vtime",Pre_EventTime,Post_EventTime,Region)


#Run Query # If fails no error is returned

temp.actual <- sqlQuery(channel=con, query = sql1)


temp.actual$ts <- force_tz(temp.actual$VTIME,tz="Australia/Brisbane")


########## find for MW gen at time of event

temp.gen <- temp.actual%>%
  filter(ts==EstEventTime)

mw <- temp.gen$`HH Daily Actual`


########## find PV capacity for that event/region

re <- temp.event$region_sql

temp.cap <- temp.capacities%>%
  filter(region==re)

cap <- as.numeric(temp.cap$cap_MW)

##########create data frame summary

temp.output <- temp.event%>%
  select(date,time,postcode,region,ts)%>%
  mutate(MW_generating=mw,
         MW_installed_region=cap,
         Capacity_generating=MW_generating/MW_installed_region)


output <- bind_rows(output,temp.output)

############# end loop
}


########## csv output

setwd("~/GitHub/DER_Event_analysis/SolarAnalytics_analysis/output/")
write.csv(output,"events_list_PVcap_output.csv")

rm(list=ls(pattern="temp."))

######## SCRIPT END
