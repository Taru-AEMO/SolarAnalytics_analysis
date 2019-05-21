
#### run "arrange data" script first


##### 1. create tables ####

power_preevent <- upscaled_ts %>% 
  filter(ts==t0) 
names(power_preevent)[names(power_preevent)=="MW_upscaled"] <- "PreEvent_MW"

#Find Minimum Power following the event
power_event <- upscaled_ts %>% 
  filter(ts==tx)
names(power_event)[names(power_event)=="MW_upscaled"] <- "Event_MW"


#Calculate the Power Loss during the event by Tranch
temp.df = left_join(power_preevent, power_event, by=c("response_category", "Standard_Version")) 

df_total <- temp.df %>% 
  group_by(ts.x) %>% 
  summarise(PreEvent_MW = sum(PreEvent_MW),
            Event_MW = sum(Event_MW)) %>% 
  mutate(Tot_Power_Loss_MW = PreEvent_MW-Event_MW) %>% 
  mutate(Tot_ChangeInPower_perc = Tot_Power_Loss_MW/PreEvent_MW) %>% 

temp_df_total <- df_total %>% 
  select(ts.x, Tot_Power_Loss_MW)
  

#Evaluate Power loss by Tanch and response category
df_tranch_response <- temp.df %>%   
  mutate(Power_Loss_MW = PreEvent_MW-Event_MW) %>% 
  mutate(ChangeInPower_perc = Power_Loss_MW/PreEvent_MW) %>% 
  left_join(.,temp_df_total, by="ts.x") %>% 
  mutate(Proportion_DeltaP_perc = Power_Loss_MW/Tot_Power_Loss_MW) %>% 
  select(ts.x, response_category, Standard_Version, PreEvent_MW, Event_MW,Power_Loss_MW, ChangeInPower_perc, Proportion_DeltaP_perc)



#Evaluate Power loss by response category
df_response <- temp.df %>% 
  group_by(ts.x, response_category) %>% 
  summarise(PreEvent_MW = sum(PreEvent_MW),
            Event_MW = sum(Event_MW)) %>% 
  mutate(Power_Loss_MW = PreEvent_MW-Event_MW) %>% 
  mutate(ChangeInPower_perc = Power_Loss_MW/PreEvent_MW) %>% 
  left_join(.,temp_df_total, by="ts.x") %>% 
  mutate(Proportion_DeltaP_perc = Power_Loss_MW/Tot_Power_Loss_MW) %>% 
  select(ts.x, response_category, PreEvent_MW, Event_MW,Power_Loss_MW, ChangeInPower_perc,Proportion_DeltaP_perc)





#Evaluate Power loss by response category
df_tranch <- temp.df %>% 
  group_by(ts.x, Standard_Version) %>% 
  summarise(PreEvent_MW = sum(PreEvent_MW),
            Event_MW = sum(Event_MW)) %>% 
  mutate(Power_Loss_MW = PreEvent_MW-Event_MW) %>% 
  mutate(ChangeInPower_perc = Power_Loss_MW/PreEvent_MW) %>% 
  left_join(.,temp_df_total, by="ts.x") %>% 
  mutate(Proportion_DeltaP_perc = Power_Loss_MW/Tot_Power_Loss_MW) %>% 
  select(ts.x, Standard_Version, PreEvent_MW, Event_MW,Power_Loss_MW, ChangeInPower_perc,Proportion_DeltaP_perc)


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
sink()





rm(list=ls(pattern="temp"))




