###UPSCALED PLOTS

#Set order of the chart correct from the beginning - by changing the factor levels
pp_upscaled_ts <- upscaled_ts %>% 
  mutate(Legend=response_category)%>% 
  mutate(StdOrder =ifelse(Standard_Version =="AS4777.2:2015", 3, ifelse(Standard_Version=="Transition", 2, ifelse(Standard_Version=="AS4777.3:2005", 1, 4)))) %>% 
  mutate(ResponseOrder = ifelse(response_category =="Curtail", 2, ifelse(response_category=="Ride-Through", 1, ifelse(response_category=="Disconnect", 3, 4))))

pp_upscaled_ts$response_category <- factor(pp_upscaled_ts$response_category, levels = unique(pp_upscaled_ts$response_category[order(-pp_upscaled_ts$ResponseOrder)]))
pp_upscaled_ts$Standard_Version <- factor(pp_upscaled_ts$Standard_Version, levels = unique(pp_upscaled_ts$Standard_Version[order(pp_upscaled_ts$StdOrder)]))
upscaled_ts <- select(pp_upscaled_ts, ts, response_category, Standard_Version, MW_upscaled)


##Plot 1 - Stacked Area Chart of Upscaled Response (by response category)
temp.plot1 <- aggregate(MW_upscaled ~ ts + response_category, upscaled_ts, sum) %>% 
  filter(ts>(t0-minutes(5)) & ts <(t0+minutes(15))) 

p1 <- ggplot(temp.plot1, aes(ts,MW_upscaled,colour=response_category))+
  geom_area(position="stack",aes(fill=response_category))+
  labs(title="[Upscaled data] PV output by response category")+
  theme(legend.position="bottom")+
  xlab("Time")+
  ylab("Power (MW)")+
  geom_vline(aes(xintercept=t0),size=0.9,colour="black",linetype="dashed")+
  sapply(tx,function(tx)geom_vline(aes(xintercept=tx),size=0.9,colour="red",linetype="dashed"))




##Plot 2 - Stacked Area Chart of Upscaled Response (by response category and Standard)
temp.plot2 <- aggregate(MW_upscaled ~ ts + response_category + Standard_Version, upscaled_ts, sum) %>% 
  mutate(Legend=response_category)%>% 
  filter(ts>(t0-minutes(5)) & ts <(t0+minutes(15)))
 

p2 <- ggplot(temp.plot2, aes(ts, MW_upscaled, colour=response_category))+
  geom_area(position="stack",aes(fill=response_category))+
  facet_wrap(~Standard_Version)+
  geom_vline(aes(xintercept=t0),size=0.9,linetype="dashed", colour="black")+
  theme(legend.position="bottom")+
  xlab("Time")+
  ylab("Power (MW)")


rm(list=ls(pattern="temp"))




##### 6. save outputs ####
setwd(paste0("",directory,"/PP_output_",event_date,""))

ggsave(p1,file=paste0("plot_1_upscale_",savetime,".png"))
ggsave(p2,file=paste0("plot_2_upscale_",savetime,".png"))
# ggsave(p3,file=paste0("plot_3_",savetime,".png"))
# ggsave(p4,file=paste0("plot_4_",savetime,".png"))
# ggsave(p5,file=paste0("plot_5_",savetime,".png"))
# ggsave(p6,file=paste0("plot_6_",savetime,".png"))

rm(p1)
rm(p2)
# rm(p3)
# rm(p4)
# rm(p5)
# rm(p6)
###