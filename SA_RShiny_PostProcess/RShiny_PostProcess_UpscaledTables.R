
#### run "arrange data" script first


##### 1. create tables ####

power_preevent <- upscaled_ts %>% 
  filter(ts==t0) 
names(power_preevent)[names(power_preevent)=="MW_upscaled"] <- "PreEvent_MW"

#Find Minimum Power following the event
power_event <- upscaled_ts %>% 
  filter(ts%in%tx)

names(power_event)[names(power_event)=="MW_upscaled"] <- "Event_MW"


#Calculate the Power Loss during the event by Tranch
temp.df = left_join(power_preevent, power_event, by=c("response_category", "Standard_Version")) 

df_total <- temp.df %>% 
  group_by(ts.y) %>% 
  summarise(PreEvent_MW = sum(PreEvent_MW),
            Event_MW = sum(Event_MW)) %>% 
  mutate(Tot_Power_Loss_MW = PreEvent_MW-Event_MW) %>% 
  mutate(Tot_ChangeInPower_perc = Tot_Power_Loss_MW/PreEvent_MW) 


temp_df_total <- df_total %>% 
  select(ts.y, Tot_Power_Loss_MW)
  

#Evaluate Power loss by Tanch and response category
df_tranch_response <- temp.df %>%   
  mutate(Power_Loss_MW = PreEvent_MW-Event_MW) %>% 
  mutate(ChangeInPower_perc = Power_Loss_MW/PreEvent_MW) %>% 
  left_join(.,temp_df_total, by="ts.y") %>% 
  mutate(Proportion_DeltaP_perc = Power_Loss_MW/Tot_Power_Loss_MW) %>% 
  select(ts.y, response_category, Standard_Version, PreEvent_MW, Event_MW,Power_Loss_MW, ChangeInPower_perc, Proportion_DeltaP_perc)



#Evaluate Power loss by response category
df_response <- temp.df %>% 
  group_by(ts.y, response_category) %>% 
  summarise(PreEvent_MW = sum(PreEvent_MW),
            Event_MW = sum(Event_MW)) %>% 
  mutate(Power_Loss_MW = PreEvent_MW-Event_MW) %>% 
  mutate(ChangeInPower_perc = Power_Loss_MW/PreEvent_MW) %>% 
  left_join(.,temp_df_total, by="ts.y") %>% 
  mutate(Proportion_DeltaP_perc = Power_Loss_MW/Tot_Power_Loss_MW) %>% 
  select(ts.y, response_category, PreEvent_MW, Event_MW,Power_Loss_MW, ChangeInPower_perc,Proportion_DeltaP_perc)

#Evaluate Power loss by response category
df_tranch <- temp.df %>% 
  group_by(ts.y, Standard_Version) %>% 
  summarise(PreEvent_MW = sum(PreEvent_MW),
            Event_MW = sum(Event_MW)) %>% 
  mutate(Power_Loss_MW = PreEvent_MW-Event_MW) %>% 
  mutate(ChangeInPower_perc = Power_Loss_MW/PreEvent_MW) %>% 
  left_join(.,temp_df_total, by="ts.y") %>% 
  mutate(Proportion_DeltaP_perc = Power_Loss_MW/Tot_Power_Loss_MW) %>% 
  select(ts.y, Standard_Version, PreEvent_MW, Event_MW,Power_Loss_MW, ChangeInPower_perc,Proportion_DeltaP_perc)

#Calculate Power at t0
df_TotPower_t0 <- power_preevent %>% 
  group_by(ts) %>% 
  summarise(PreEvent_MW = sum(PreEvent_MW))

df_Power_t0 <- power_preevent %>% 
  group_by(ts, Standard_Version) %>% 
  summarise(Std_PreEvent_MW = round(sum(PreEvent_MW),2)) %>%
  left_join(.,df_TotPower_t0, by = "ts") %>% 
  mutate(PreEvent_perc = round((100*Std_PreEvent_MW/PreEvent_MW),2)) %>% 
  mutate(Value = paste0(Std_PreEvent_MW, " (",PreEvent_perc,"%)")) %>% 
  select(ts, Standard_Version, PreEvent_MW, Value)

df_Power_t0 <- dcast(df_Power_t0, ts+PreEvent_MW~Standard_Version, value.var="Value")

##### save outputs ####
setwd(paste0("",directory,"/PP_output_",event_date,""))


sink("Power_Loss_Upscale.csv")
cat("Total Power Loss")
cat('\n')
write.csv(df_total)
cat('____________________________')
cat('\n')
cat('\n')
cat("Power Loss By Tranch")
cat('\n')
write.csv(df_tranch)
cat('____________________________')
cat('\n')
cat('\n')
cat("Power Loss By Response")
cat('\n')
write.csv(df_response)
cat('____________________________')
cat('\n')
cat('\n')
cat("Power Loss By Tranch and Response")
cat('\n')
write.csv(df_tranch_response)
cat('____________________________')
cat('\n')
cat('\n')
cat("Proportion at t0")
cat('\n')
write.csv(df_Power_t0)
sink()





rm(list=ls(pattern="temp"))




