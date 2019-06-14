###UPSCALED PLOTS

##Input files from other R Scripts and Manipulated as needed
#Set order of the chart correct from the beginning - by changing the factor levels
pp_upscaled_ts <- upscaled_ts %>% 
  mutate(Legend=response_category)%>% 
  mutate(StdOrder =ifelse(Standard_Version =="AS4777.2:2015", 3, ifelse(Standard_Version=="Transition", 2, ifelse(Standard_Version=="AS4777.3:2005", 1, 4)))) %>% 
  mutate(ResponseOrder = ifelse(response_category =="Curtail", 2, ifelse(response_category=="Ride-Through", 1, ifelse(response_category=="Disconnect", 3, 4))))

pp_upscaled_ts$response_category <- factor(pp_upscaled_ts$response_category, levels = unique(pp_upscaled_ts$response_category[order(-pp_upscaled_ts$ResponseOrder)]))
pp_upscaled_ts$Standard_Version <- factor(pp_upscaled_ts$Standard_Version, levels = unique(pp_upscaled_ts$Standard_Version[order(pp_upscaled_ts$StdOrder)]))
upscaled_ts <- select(pp_upscaled_ts, ts, response_category, Standard_Version, MW_upscaled)

colnames(upscaled_ts)[2] <- "Legend"


df_total <- df_total
df_response <- df_response
colnames(df_response)[2] <- "Legend"
df_tranch_response <- df_tranch_response
colnames(df_tranch_response)[2] <- "Legend"


##Plot 1 - Stacked Area Chart of Upscaled Response (by response category)
temp.plot1 <- aggregate(MW_upscaled ~ ts + Legend, upscaled_ts, sum) %>% 
  filter(ts>(t0-minutes(5)) & ts <(t0+minutes(15)))

p1 <- ggplot(temp.plot1, aes(ts,MW_upscaled))+
    geom_area(position="stack",aes(fill=Legend))+
  labs(title="[Upscaled data] PV output by response category")+
  theme(legend.position="bottom")+
  xlab("Time")+
  ylab("Power (MW)")+
  geom_vline(aes(xintercept=t0),size=0.9,colour="black",linetype="dashed")+
  sapply(tx,function(tx)geom_vline(aes(xintercept=tx),size=0.9,colour="navy",linetype="dashed"))+
  scale_fill_manual(values=AEMOCpp)

##Plot 2 - Stacked Area Chart of Upscaled Response (by response category and Standard)
temp.plot2 <- aggregate(MW_upscaled ~ ts + Legend + Standard_Version, upscaled_ts, sum) %>% 
  filter(ts>(t0-minutes(5)) & ts <(t0+minutes(15)))

p2 <- ggplot(temp.plot2, aes(ts, MW_upscaled))+
  geom_area(position="stack",aes(fill=Legend))+
  facet_wrap(~Standard_Version)+
  geom_vline(aes(xintercept=t0),size=0.9,linetype="dashed", colour="black")+
  sapply(tx,function(tx)geom_vline(aes(xintercept=tx),size=0.9,colour="red",linetype="dashed"))+
  theme(legend.position="bottom")+
  xlab("Time")+
  ylab("Power (MW)")+
  labs(title="[Upscaled data] PV output by response category and standard")+
  scale_fill_manual(values=AEMOCpp)

##Plot 3 - Basic distribution of power loss by response category chart

temp.response <- select(df_response, ts.y, Legend, Power_Loss_MW) 
temp.total <- df_total %>% 
  select(ts.y, Tot_Power_Loss_MW)

temp.plot3 <- left_join(temp.response, temp.total, by="ts.y") %>% 
  mutate(Standard_Version = "Combined") %>% 
  select(ts.y, Standard_Version, Legend, Power_Loss_MW, Tot_Power_Loss_MW) %>% 
  as.data.frame()
  
p3 <- ggplot(temp.plot3, aes(x=ts.y))+
  geom_bar(aes(y=Power_Loss_MW, fill = Legend),stat= "identity")+
  geom_point(aes(y=Tot_Power_Loss_MW))+
  scale_fill_manual(values=AEMOCpp)+
  xlab(NULL)+
  ylab("Power Loss (MW)")+
  labs(title="[Upscaled data] Power Loss by response category and standard",
       subtitle = "Positive value equates to a reduction in generation exported to the network")+
  theme(legend.position="bottom")

##Plot 4 - Distribution of power loss by response category and standard
temp.total.response <- df_tranch_response %>% 
  group_by(ts.y, Standard_Version) %>% 
  summarise(Tot_Power_Loss_MW = sum(Power_Loss_MW))

temp.plot4 <- select(df_tranch_response,ts.y, Standard_Version, Legend, Power_Loss_MW) %>% 
  left_join(temp.total.response, by=c("ts.y", "Standard_Version")) %>% 
  rbind(temp.plot3)

p4 <- ggplot(temp.plot4, aes(x=Standard_Version))+
  geom_bar(aes(y=Power_Loss_MW, fill = Legend), stat="identity")+
  scale_fill_manual(values=AEMOCpp)+
  geom_point(aes(y=Tot_Power_Loss_MW))+
  labs(title="Power Loss by Response and Standard",
       subtitle = "Positive value equates to a reduction in generation exported to the network")+
  ylab("Power Loss by response (MW)")+
  xlab(NULL)+
  theme(legend.position="bottom")

##### 6. save outputs ####
setwd(paste0("",directory,"/PP_output_",event_date,""))

ggsave(p1,file=paste0("plot_1_upscale_",savetime,".png"),height =7, width =10)
ggsave(p2,file=paste0("plot_2_upscale_",savetime,".png"),height =7, width =10)
ggsave(p3,file=paste0("plot_3_upscale_",savetime,".png"),height =7, width =10)
ggsave(p4,file=paste0("plot_4_upscale",savetime,".png"),height =7, width =10)
# ggsave(p5,file=paste0("plot_5_",savetime,".png"))
# ggsave(p6,file=paste0("plot_6_",savetime,".png"))

rm(p1)
rm(p2)
rm(p3)
rm(p4)
# rm(p5)
# rm(p6)
###

rm(list=ls(pattern="temp"))