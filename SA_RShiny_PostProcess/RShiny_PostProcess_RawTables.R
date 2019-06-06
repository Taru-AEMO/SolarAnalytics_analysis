
#### run "arrange data" script first


######## 1. aggregations of time series data for all P, delta_P and %delta_P tables ####


## aggregate power stats
temp.aggregate <- aggregate(power_kW ~ ts, pp_ud, sum)

temp.total <- temp.aggregate %>% 
  dplyr::filter(ts %in% c(t0 , tx))

temp.total <-  temp.total %>% 
  dplyr::mutate(delta_kW=power_kW-(temp.total$power_kW[1]),
                delta_perc=(delta_kW/(temp.total$power_kW[1])*100),
                legend="Total",
                type="monitored_total")


## aggregate power + response
temp.aggregate <- aggregate(power_kW ~ ts + response_category, pp_ud, sum)

temp.response <- temp.aggregate %>% 
  dplyr::filter(ts %in% c(t0 , tx))

##apply mutates once for each response category
list_responses <- unique(temp.response$response_category)

temp.filter <- sapply(list_responses,function(x) {

  temp.row <- temp.response %>%
    dplyr::filter(response_category==x)

  temp.row <- temp.row %>%
    dplyr::mutate(delta_kW=power_kW-(temp.row$power_kW[1]),
                  delta_perc=(delta_kW/(temp.row$power_kW[1])*100),
                  legend=paste0(x),
                  type="response") %>%
    select(-response_category)

},simplify = FALSE)

## bind to data frame
delta_list <- NULL
delta_list <- bind_rows(temp.filter,temp.total)

## remove temp files
rm(list=ls(pattern="temp"))



## aggregate power + standard
temp.aggregate <- aggregate(power_kW ~ ts + Standard_Version, pp_ud, sum)

temp.standard <- temp.aggregate %>% 
  dplyr::filter(ts %in% c(t0 , tx))

## apply mutates once for each standard category
list_standards <- unique(temp.standard$Standard_Version)

temp.filter <- sapply(list_standards,function(x) {
  
  temp.row <- temp.standard %>%
    dplyr::filter(Standard_Version==x)
  
  temp.row <- temp.row %>%
    dplyr::mutate(delta_kW=power_kW-(temp.row$power_kW[1]),
                  delta_perc=(delta_kW/(temp.row$power_kW[1])*100),
                  legend=paste0(x),
                  type="standard") %>%
    select(-Standard_Version)
  
},simplify = FALSE)

## bind to data frame
delta_list <- bind_rows(temp.filter,delta_list)

## remove temp files
rm(list=ls(pattern="temp"))


## aggregate power + standard + response
temp.aggregate <- aggregate(power_kW ~ ts + response_category + Standard_Version, pp_ud, sum)


temp.standard.response <- temp.aggregate %>% 
  dplyr::filter(ts %in% c(t0 , tx)) %>% 
  dplyr::mutate(key=paste0(response_category,"_",Standard_Version))

## apply mutates once for each standard category + response category
list_responseby_stand <- unique(temp.standard.response$key)

temp.filter <- sapply(list_responseby_stand,function(x) {
  
  temp.row <- temp.standard.response %>%
    dplyr::filter(key==x)
  
  temp.row <- temp.row %>%
    dplyr::mutate(delta_kW=power_kW-(temp.row$power_kW[1]),
                  delta_perc=(delta_kW/(temp.row$power_kW[1])*100),
                  legend=paste0(x),
                  type="response_by_standard") %>%
    select(-Standard_Version,-response_category,-key)
  
},simplify = FALSE)

## bind to final data frame
delta_list <- bind_rows(temp.filter,delta_list)

## remove temp files
rm(list=ls(pattern="temp"))





####### 2 delta_list -> construct tables ####


##### table 1.   change in monitored PV power (delta_P) from t0 by standard ####
## manipulate / rearrange
table_1b <- delta_list %>% 
  dplyr::filter(type=="standard") %>% 
  gather(unit,value,-legend,-type,-ts) %>%
  mutate(header=paste(legend,unit)) %>% 
  select(-type,-legend,-unit) %>%
  spread(header,value) %>% 
  select(ts,
         "AS4777.3:2005 power_kW","AS4777.3:2005 delta_kW","AS4777.3:2005 delta_perc",
         "Transition power_kW","Transition delta_kW","Transition delta_perc",
         "AS4777.2:2015 power_kW","AS4777.2:2015 delta_kW","AS4777.2:2015 delta_perc")


##### table x.   change in monitored PV power (delta_P) from t0 total ####
table_1a <- delta_list %>% 
  dplyr::filter(type=="monitored_total") %>% 
  gather(unit,value,-legend,-type,-ts)  %>% 
  mutate(header=paste(legend,unit)) %>% 
  select(-type,-legend,-unit) %>%
  spread(header,value) %>% 
  select(ts,"Total power_kW","Total delta_kW", "Total delta_perc")



##### table 2.   change in monitored PV power (delta_P) for each response group as a percentage of total delta_P, for each standard ####
## A grab delta kW for each standard
table_2 <- delta_list %>% 
  dplyr::filter(type=="standard" | type=="monitored_total",ts %in% tx) %>% 
  select(ts,legend,delta_kW) %>%
  arrange(ts) %>% 
  dplyr::mutate(key=paste0(ts,"_",legend))

## B grab delta kW for response category by standard
temp.table <- delta_list %>% 
  dplyr::filter(type=="response_by_standard",ts %in% tx) %>% 
  dplyr::select(ts,legend,delta_kW) %>%
  tidyr::separate(legend,c("A","legend"),sep="_") %>% 
  tidyr::spread(A,delta_kW)

## C grab delta kW for response category totals
temp.table2 <- delta_list %>% 
  dplyr::filter(type=="response",ts %in% tx) %>% 
  dplyr::select(ts,legend,delta_kW) %>% 
  tidyr::spread(legend,delta_kW) %>% 
  dplyr::mutate(legend="Total")

## bind together B and C (now have delta kW for all response categories)
temp.bind <- bind_rows(temp.table,temp.table2) %>% 
  arrange(ts) %>% 
  dplyr::mutate(key=paste0(ts,"_",legend))

## join BC with A
temp.join <- left_join(table_2,select(temp.bind,-ts,-legend),by="key") %>% 
  dplyr::select(-key)


## mutate so response category columns (B and C) are percentage of A
## (will require manual changes if response categories are changed)
temp.table <- temp.join %>% 
  select(ts,legend,delta_kW,Curtail,Disconnect,Ride_Through=`Ride-Through`) %>% 
  dplyr::mutate(perc_Ride_Through=(Ride_Through/delta_kW)*100,
                perc_Curtail=(Curtail/delta_kW)*100,
                perc_Disconnect=(Disconnect/delta_kW)*100)

## filter for the ts, standard, and power columns -- then any columsn with "percentage"
# (trying to reduce changes to the script required if we decide to use different response categories in the future)
temp.col_collect <- as.integer(c(1,2,3,paste0(which(grepl("perc",colnames(temp.table))))))


## 
table_2 <- NULL
table_2 <- select(temp.table,temp.col_collect) 

## rearrange rows
table_2$legend <- ordered(table_2$legend, levels=c("AS4777.3:2005", "Transition", "AS4777.2:2015", "Total"))
table_2 <- arrange(table_2,ts,legend)


rm(list=ls(pattern="temp"))


##### table 3.  monitored PV response by inverter standard + size (number of systems) [delta_list not an input] ####
## test that each circuit only has one response category assigned to it
temp.sites <- unique(pp_ud$c_id)

temp.sample <- unique(select(pp_ud,c_id,response_category))

print(paste0("number of unique circuit ids: ",length(temp.sites)))
print(paste0("number of unique circuit - response matches: ",nrow(temp.sample)))
print("Please investigate if these numbers do not match, as multiple responses have been assigned to one circuit")
print("__________")

## if test failed, these are you're duplicated circuits
if (length(temp.sites) != nrow(temp.sample)){
temp.doubles <- temp.sample%>% 
  filter(c_id %in% (temp.sample$c_id[duplicated(temp.sample$c_id)])) %>% 
  arrange(c_id)
}

## filter for standard version, size, and response of each circuit ID
unique_list <- unique(select(pp_ud,c_id,Standard_Version,Grouping,response_category)) %>% 
  mutate(response_category=gsub("-","_",response_category),
         count=1)

## rearrange, count, and create table 3
table_3 <- unique_list %>% 
  group_by(Standard_Version,Grouping,response_category) %>% 
  summarise(n=sum(count)) %>% 
  spread(Standard_Version,n) %>% 
  select(Grouping,response_category,`AS4777.3:2005`,Transition,`AS4777.2:2015`)

## rearrange rows
table_3$response_category <- ordered(table_3$response_category, levels=c("Ride_Through", "Curtail", "Disconnect"))
table_3 <- arrange(table_3,Grouping,response_category)

## remove tmeps
rm(list=ls(pattern="temp"))


##### table 4. number of systems in reach response cat as % of all systems on that standard  ####
temp.table <- unique_list %>% 
  group_by(Standard_Version,response_category) %>% 
  summarise(n=sum(count))

temp.table2 <- unique_list %>% 
  group_by(Standard_Version) %>% 
  summarise(p=sum(count))

temp.join <- left_join(temp.table,temp.table2,by="Standard_Version") %>% 
  mutate(perc_standard=(n/p)*100) %>% 
  select(-n,-p) %>% 
  spread(Standard_Version,perc_standard)

table_4 <- temp.join %>% 
  select(response_category,`AS4777.3:2005`,Transition,`AS4777.2:2015`)

## rearrange rows
table_4$response_category <- ordered(table_4$response_category, levels=c("Ride_Through", "Curtail", "Disconnect"))
table_4 <- arrange(table_4,response_category)

## remove temps
rm(list=ls(pattern="temp"))


#### table.5: Unscaled data: sample count, cleaned data only ####

#Create basis for table 5, grouping by Standard and Size
temp.table1 <- unique_list %>%
  group_by(Standard_Version,Grouping) %>%
  summarise(n=n_distinct(c_id)) %>%
  spread(Grouping,n) 

#Add row totals
temp.table1$Total = rowSums( temp.table1[ sapply(temp.table1, is.numeric)] )

#Add column totals and create final table
table_5 <- rbind( temp.table1, append(c(Standard_Version="Total"),  
                   as.list(colSums( temp.table1[ sapply(temp.table1, is.numeric)] ))))

rm(list=ls(pattern="temp"))






##### 3 save outputs ####
setwd(paste0("",directory,"/PP_output_",event_date,""))



sink(paste0("All_Raw_Tables_",savetime,".csv"))
cat("Total Power Loss")
cat('\n')
write.csv(table_1a)
cat('____________________________')
cat('\n')
cat('\n')
cat("Power Loss By Standard Version")
cat('\n')
write.csv(table_1b)
cat('____________________________')
cat('\n')
cat('\n')
cat("Response as a percentage of Power Lost")
cat('\n')
write.csv(table_2)
cat('____________________________')
cat('\n')
cat('\n')
cat("Count of response categories By Tranch")
cat('\n')
write.csv(table_3)
cat('____________________________')
cat('\n')
cat('\n')
cat("Response category as percentage of systems on each standard")
cat('\n')
write.csv(table_4)
cat('____________________________')
cat('\n')
cat('\n')
cat("Unnamed Table 5")
cat('\n')
write.csv(table_5)
cat('____________________________')
sink()




rm(list=ls(pattern="table"))

##### end  ####