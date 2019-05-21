
### run Arrange Data script first

##################### raw data plots
## plot 1 #### total PV output for the day ####
# temp.plot <- aggregate(power_kW ~ ts, pp_ud, sum)
# 
# p1 <- ggplot(temp.plot, aes(ts,power_kW))+
#   geom_line()
# 
# plot(p1)

## remove temps
rm(list=ls(pattern="temp"))




## plot 2 ## raw data PV output short (for the time near the event) ####
temp.plot <- aggregate(power_kW ~ ts + response_category, pp_ud, sum)

colnames(temp.plot)[2] <- "Legend"
  

p2 <- ggplot(temp.plot, aes(ts,power_kW,colour=Legend))+
  geom_area(position="stack",aes(fill=Legend))+
  labs(title="[Raw data] PV output by response category")+
  theme(legend.position="bottom")+
  xlab("Time")+
  ylab("Power (kW)")+
  geom_vline(aes(xintercept=t0),size=0.9,colour="black",linetype="dashed")+
  sapply(tx,function(tx)geom_vline(aes(xintercept=tx),size=0.9,colour="red",linetype="dashed"))

# plot(p2)

## remove temps
rm(list=ls(pattern="temp"))




## plot 3 ## PV output short (for the time near the event) by respopnse, standard, and size #### 
temp.plot <- aggregate(power_kW ~ ts + response_category + Grouping + Standard_Version, pp_ud, sum) %>% 
  mutate(Legend=paste0(Standard_Version," ",Grouping))

p3 <- ggplot(temp.plot, aes(ts, power_kW, colour=response_category))+
  geom_area(position="stack",aes(fill=response_category))+
  facet_wrap(~Legend)+
  geom_vline(aes(xintercept=t0),size=0.9,linetype="dashed", colour="black")+
  theme(legend.position="bottom")+
  xlab("Time")+
  ylab("Power (kW)")

# plot(p3)

## remove temps
rm(list=ls(pattern="temp"))




## plot 4 ## upscaled PV output short (for the time near the event) ####
temp.plot <- aggregate(MW_upscaled ~ ts + response_category, upscaled_ts, sum)

colnames(temp.plot)[2] <- "Legend"


p4 <- ggplot(temp.plot, aes(ts,MW_upscaled,colour=Legend))+
  geom_area(position="stack",aes(fill=Legend))+
  labs(title="[Upscaled data] PV output by response category")+
  theme(legend.position="bottom")+
  xlab("Time")+
  ylab("Power (MW)")+
  geom_vline(aes(xintercept=t0),size=0.9,colour="black",linetype="dashed")+
  sapply(tx,function(tx)geom_vline(aes(xintercept=tx),size=0.9,colour="red",linetype="dashed"))

# plot(p4)

## remove temps
rm(list=ls(pattern="temp"))




## plot 5 ## upscaled next to raw data. plots 4 and 2 together ####

temp.p <- p4 +
  theme(legend.position = "none")
temp.p2 <- p2 +
  theme(legend.position = "none")

p5 <- grid.arrange(temp.p2,temp.p,nrow=2)

## remove temps
rm(list=ls(pattern="temp"))




## plot 6 ## response by zone bar graph
## collate sample count for standard version, zone, and response of each circuit ID
temp.sample <- unique(select(pp_ud,c_id,Standard_Version,zone,response_category)) %>% 
  mutate(response_category=gsub("-","_",response_category),
         count=1)

temp.group <- temp.sample %>% 
  group_by(Standard_Version,zone,response_category) %>% 
  summarise(d=sum(count)) %>% 
  ungroup() %>% 
  filter(response_category=="Disconnect") %>% 
  mutate(key=paste0(Standard_Version,"_",zone))

temp.group2 <- temp.sample %>% 
  group_by(Standard_Version,zone) %>% 
  summarise(n=sum(count)) %>% 
  ungroup() %>% 
  mutate(key=paste0(Standard_Version,"_",zone))

temp.join <- left_join(temp.group2,select(temp.group,key,response_category,d),by="key") %>% 
  mutate(perc_disc=(d/n)*100) %>% 
  select(-response_category,-d,-key)

temp.join[is.na(temp.join)] <- 0

temp.plot <- temp.join

temp.plot$Standard_Version <- ordered(temp.plot$Standard_Version, levels=c("AS4777.3:2005", "Transition", "AS4777.2:2015"))
temp.plot$lab <- paste0("n=",temp.plot$n)

p6 <- ggplot(temp.plot, aes(zone,perc_disc,colour=Standard_Version))+
  geom_bar(stat="identity",position="dodge",aes(fill=Standard_Version))+
  labs(title="Percentage of PV that disconnected as a percentage of zone/standard groups",
       subtitle="'n' indicates the sample size for each zone/standard group")+
  ylab("Percentage of sites in sample that disconnected (%)")+
  xlab(NULL)+
  theme(legend.position="bottom")+
  geom_text(aes(label=lab,colour=Standard_Version),position=position_dodge(width=1),vjust=-.5,size=3)+
  scale_colour_manual(values=c("black","black","black"))


## remove temps
rm(list=ls(pattern="temp"))




##### 6. save outputs ####
setwd(paste0("",directory,"/PP_output_",event_date,""))

# ggsave(p1,file=paste0("plot_1",savetime,".png"))
ggsave(p2,file=paste0("plot_2_",savetime,".png"))
ggsave(p3,file=paste0("plot_3_",savetime,".png"))
ggsave(p4,file=paste0("plot_4_",savetime,".png"))
ggsave(p5,file=paste0("plot_5_",savetime,".png"))
ggsave(p6,file=paste0("plot_6_",savetime,".png"))

# rm(p1)
rm(p2)
rm(p3)
rm(p4)
rm(p5)
rm(p6)

################### end ####


