
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




##### 6. save outputs ####
setwd(paste0("",directory,"/PP_output_",event_date,""))

ggsave(p1,file="plot_1.png")
ggsave(p2,file="plot_2.png")
ggsave(p3,file="plot_3.png")
ggsave(p4,file="plot_4.png")
ggsave(p5,file="plot_5.png")


################### end ####


