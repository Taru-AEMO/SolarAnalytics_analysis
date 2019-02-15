####PROCESS_SCRIPT.R

setwd("~/GitHub/DER_Event_analysis/SolarAnalytics_analysis/output/ToAnalyse/")


input.file.name <- list.files(pattern="_cleaned.csv")


#Define Category thresholds 
Cat1_PL_perc <- 0.04
#Categories 2-6: Curtailment
Cat2_PL_perc <- 0.1
Cat3_PL_perc <- 0.25
Cat4_PL_perc <- 0.5
Cat5_PL_perc <- 0.75
Cat6_PL_perc <- 0.1
#As a value in kW
#Category 7: Disconnect
Cat7_Disconnect_kW=0.05
Cat8_Disconnect_kW=0


Final_output <- NULL

for (i in input.file.name){
  setwd("~/GitHub/DER_Event_analysis/SolarAnalytics_analysis/output/ToAnalyse/")
dir.create(file.path(paste0("~/GitHub/DER_Event_analysis/SolarAnalytics_analysis/output/ToAnalyse/",substr(i,1,22))))

EventTime <- ymd_hms(paste0(substr(i, 6, 15)," ",substr(i, 17, 18),":",substr(i, 19, 20),":",substr(i, 21, 22)), tz="Australia/Brisbane")
                    
print(EventTime)


Final_clean <- read.csv(i, header=TRUE, stringsAsFactors = FALSE) %>%
  mutate(ts = ymd_hms(ts, tz="Australia/Brisbane"))

Final_clean <- Final_clean %>% 
  mutate(pv_install_date_day=ifelse(nchar(pv_install_date)==10,pv_install_date, paste0(pv_install_date, "-28"))) %>%
  mutate(pv_install_date_day=ymd(pv_install_date_day)) %>%
  mutate(Standard_Version = ifelse(pv_install_date_day<"2015-10-09", "AS4777.3:2005", ifelse(pv_install_date_day>="2016-10-09", "AS4777.2:2015", "Transition"))) %>% 
  mutate(DC.Rating.kW.= DC.Rating.kW./1000) %>% 
  mutate(Size = ifelse(DC.Rating.kW.<30, "<30 kW", ifelse(DC.Rating.kW.<=100, "30-100kW", ">100kW"))) %>% 
  mutate(Duration = durn)
  # subset(select = -duration) %>% 
  # na.omit()

#README: This script is designed to Process a Cleaned Data Set to Evaluate the Number of Disconnections.

#Filter for Power value at T_0

t_0 <- EventTime
t_end_estimate_nadir <- EventTime + minutes(4)
t_end_nadir <- EventTime + minutes(2)

temp.power_t0 <- filter(Final_clean, ts>=(t_0 - seconds(14)) & ts<= (t_0 + seconds(14))) %>%
  .[,c("c_id", "ts","power_kW")]

##Print confirmation
print(paste("Number of unique ids in data set", length(unique(Final_clean$c_id))))

print(paste("Number of rows selected", nrow(temp.power_t0), "this is made up of", length(unique(temp.power_t0$c_id)), "unique ids"))

print("Please confirm these values match")

#Change Column Names
colnames(temp.power_t0)[colnames(temp.power_t0)=="power_kW"] <- "p_0"
colnames(temp.power_t0)[colnames(temp.power_t0)=="ts"] <- "t0"


#Filter for possible Nadir time then identify the T_Nadir and identify power at this time
temp.nadir.df <- filter(Final_clean, ts>t_0 & ts<=t_end_estimate_nadir)
temp.nadir_t0 <- temp.nadir.df%>%
  group_by(c_id) %>%
  summarise(p_nadir=min(power_kW)) %>%
  left_join(temp.nadir.df[,c("c_id","ts","power_kW")], by=c("c_id", "p_nadir"="power_kW")) %>%
  .[,c(1,3,2)]
colnames(temp.nadir_t0)[colnames(temp.nadir_t0)=="ts"] <- "t_nadir"

#Filter for Power Value at T_1 and T_2
temp.power_t1 <- filter(Final_clean, ts>=(t_0 + seconds(16)) & ts<=(t_0 + seconds(45))) %>%
  .[,c("c_id","ts","power_kW")]
colnames(temp.power_t1)[colnames(temp.power_t1)=="power_kW"] <- "p_0plus1"
colnames(temp.power_t1)[colnames(temp.power_t1)=="ts"] <- "t_0plus1"

temp.power_t2 <- filter(Final_clean, ts>=(t_0 + seconds(46)) & ts<=(t_0 + seconds(75))) %>%
  .[,c("c_id","ts","power_kW")]
colnames(temp.power_t2)[colnames(temp.power_t2)=="power_kW"] <- "p_0plus2"
colnames(temp.power_t2)[colnames(temp.power_t2)=="ts"] <- "t_0plus2"

#Join Tables Together and clean
temp.p0_pnadir_p1_p2 <- plyr::join(temp.power_t0, temp.nadir_t0, by="c_id", type="left", match="first") %>%
  left_join(temp.power_t1, by="c_id") %>%
  left_join(temp.power_t2, by="c_id") 


###################4.CALCULATE RAMPING AND DIFFERENCES#############################################
temp.ramp_diff <- temp.p0_pnadir_p1_p2 %>%
  mutate(time_rampdown= t_nadir - t0,
         power_diff_p0nadir = p_nadir-p_0) %>%
  mutate(perc_drop_nadir = power_diff_p0nadir/p_0,
         perc_drop_to_p1 = (p_0plus1-p_0)/p_0,
         perc_drop_to_p2 = (p_0plus2-p_0)/p_0,
         ramp_down_rate = power_diff_p0nadir/as.numeric(time_rampdown))

# #################5.ASSIGNING CATEGORIES######################################################


temp.ramp.diff <- temp.ramp_diff %>% 
  mutate(p_0plus2 = ifelse(is.na(p_0plus2), 0, p_0plus2))  %>% 
  mutate(p_0plus1 = ifelse(is.na(p_0plus1), 0, p_0plus1)) %>% 
  mutate(perc_drop_to_p1 = ifelse(is.na(perc_drop_to_p1), 0, perc_drop_to_p1)) %>% 
  mutate(perc_drop_to_p2 = ifelse(is.na(perc_drop_to_p2), 0, perc_drop_to_p2)) 

temp.category <- temp.ramp.diff %>%
  mutate(Category = ifelse(p_nadir<=Cat7_Disconnect_kW & p_0>Cat7_Disconnect_kW,"Category 7 - Disconnect",
                           ifelse(pmax(abs(perc_drop_to_p1),abs(perc_drop_to_p2))<=Cat1_PL_perc, "Category 1 - Ride Through",
                                  ifelse((abs(p_0plus1)<abs(p_0plus2))&abs(perc_drop_to_p1)<=Cat1_PL_perc, "Category 2 - Dip",
                                         ifelse(abs(perc_drop_nadir)<=Cat3_PL_perc, "Category 3 - Mild Curtailment",
                                                ifelse(abs(perc_drop_nadir)<=Cat4_PL_perc, "Category 4 - Medium Curtailment",
                                                       ifelse(abs(perc_drop_nadir)<=Cat5_PL_perc, "Category 5 - Significant Curtailment",
                                                              ifelse(abs(perc_drop_nadir)>Cat5_PL_perc, "Category 6 - Severe Curtailment",
                                                                     "Not categorised")))))))) %>% 
  mutate(Category_basic = ifelse(Category=="Category 7 - Disconnect", "Category 7 - Disconnect",
                                 ifelse(Category=="Category 1 - Ride Through", "Category 1 - Ride Through",
                                        "Category 2-6 - Curtailment")))


####OUTPUTS######
###Print CSV of Category Types

setwd(paste0("~/GitHub/DER_Event_analysis/SolarAnalytics_analysis/output/ToAnalyse/",substr(i,1,22)))

temp.duration <- Final_clean[1,c("Duration")] 

temp.category.table1 <- temp.category %>% 
  group_by(Category_basic,c_id) %>% 
  summarise(count =n()) %>% 
  group_by(Category_basic) %>% 
  summarise(count = n()) 
  

temp.category.table2 <- temp.category %>% 
  group_by(t0, Category,c_id) %>% 
  summarise(count =n()) %>% 
  group_by(t0, Category) %>% 
  summarise(count = n())

temp.final4 <- temp.category.table2 %>% 
  group_by(t0) %>% 
  summarise(TotalNumberSystems=sum(count)) %>% 
  mutate(Location = ifelse(substr(i,1,4)=="4500", "Brendale - 4500",
                           ifelse(substr(i,1,4)=="4701", "Parkhurst Norman - 4701",
                                  ifelse(substr(i,1,4)=="4551", "Currimundi - 4551",
                                         ifelse(substr(i,1,4)=="4555", "Palmwoods Central - 4555",
                                                ifelse(substr(i,1,4)=="7050", "Kingston - 7050",
                                                "Unknown Postcode")))))) %>% 
  mutate(Duration = temp.duration) %>% 
  .[,c(3,1,2,4)]


temp.final5 <- temp.final4 %>% 
  mutate(Disconnect = ifelse(is.empty(filter(temp.category.table1, Category_basic=="Category 7 - Disconnect")$count),0,filter(temp.category.table1, Category_basic=="Category 7 - Disconnect")$count)) %>% 
  mutate(Curtail = ifelse(is.empty(filter(temp.category.table1, Category_basic=="Category 2-6 - Curtailment")$count),0,filter(temp.category.table1, Category_basic=="Category 2-6 - Curtailment")$count)) %>% 
  mutate(RideThrough = ifelse(is.empty(filter(temp.category.table1, Category_basic=="Category 1 - Ride Through")$count),0,filter(temp.category.table1, Category_basic=="Category 1 - Ride Through")$count))

temp.totalsum <- temp.category.table2 %>% 
  group_by(t0) %>% 
  summarise(Total=sum(count))

temp.category.table2 <- temp.category.table2 %>% 
  mutate(Total= temp.totalsum$Total) %>% 
  mutate(Proportion_Percent = (count/Total)*100)

write.csv(temp.category.table2, paste0("Response_type_", substr(i, 1,15), ".csv"))

print(temp.category.table2)

####Print Charts for all Categories

draft_output <- left_join(Final_clean, temp.category, by="c_id")

temp.output <- left_join(temp.power_t0, draft_output, by = c("c_id","t0"="ts"))


###Category 1-7 
temp.list <-unique(temp.category$Category)

for (ii in temp.list){
  temp.curtail <- filter(temp.output, Category == ii)
  
  temp.curtail.values <- filter(Final_clean, c_id %in% c(unique(temp.curtail$c_id)))
  
  if (length(unique(temp.curtail.values$c_id))>25){
    
    numberOutputs = ceiling(length(unique(temp.curtail.values$c_id))/25)
    
    temp.unique.cids <- as.data.frame(unique(temp.curtail.values$c_id))
    names(temp.unique.cids) <- c("c_id")
    
    for (iii in (1:numberOutputs)){
      
      temp.chart <- filter(temp.unique.cids, row_number()>=(iii*25)-24 & row_number()<=iii*25)
      
      temp.filtered.sites <- left_join(temp.chart, temp.curtail.values, by = "c_id")
      
      ggplot(filter(temp.filtered.sites, ts>= (EventTime-minutes(2)) & ts<=(EventTime+minutes(5))), aes(ts, power_kW))+ 
        geom_line()+
        facet_wrap(~c_id, scales = "free_y")+
        geom_vline(xintercept = EventTime, linetype="dashed")+
        ggtitle(paste(ii,iii, "of", numberOutputs))
      
      ggsave(paste0(ii,"_",substr(input.file.name, 6,15),"_", iii, "of", numberOutputs, ".jpeg"), plot=last_plot(), device="jpeg")
    }
    
  }else if(length(unique(temp.curtail.values$c_id))>0 & length(unique(temp.curtail.values$c_id))<=25){
    
      ggplot(filter(temp.curtail.values, ts>= (EventTime-minutes(2)) & ts<=(EventTime+minutes(5))), aes(ts, power_kW))+ 
      geom_line()+
      facet_wrap(~c_id, scales = "free_y")+
      geom_vline(xintercept = EventTime, linetype="dashed")+
      ggtitle(paste(ii))
    
    ggsave(paste0(ii,"_",substr(input.file.name, 6,15),".jpeg"), plot=last_plot(), device="jpeg")
   
  }else (print("No clean files to plot"))
}  
########################TEST - APPLYING VOLTAGE CHART AS WELL#########################
temp.disconnect <- filter(temp.output, Category == "Category 7 - Disconnect")

temp.disconnect.values <- unique(temp.disconnect$c_id)

for (idisconnect in temp.disconnect.values){
  
  temp.disconnect.values2 <- filter(Final_clean, c_id==idisconnect) %>%
      filter(ts>= (EventTime-minutes(2)) & ts<=(EventTime+minutes(5)))

    p1 <- ggplot(temp.disconnect.values2, aes(x = ts, y=power_kW))+
      geom_line(stat="identity")+
      geom_vline(xintercept = EventTime, linetype="dashed")
    # ggtitle(paste(ii))

    p2 <- ggplot(temp.disconnect.values2, aes(x = ts, y=voltage))+
      geom_line()+
      # geom_line(stat="identity")+
      # facet_wrap(~c_id, scales = "free_y")+
      geom_vline(xintercept = EventTime, linetype="dashed")
    # ggtitle(paste(ii))

    p3 <- ggplot(temp.disconnect.values2, aes(x = ts, y=voltage/240))+
      geom_line()+
      geom_vline(xintercept = EventTime, linetype="dashed")

    g1 <- ggplotGrob(p1)
    #g1 <- gtable_add_cols(g1, unit(0,"mm")) # add a column for missing legend
    g2 <- ggplotGrob(p2)
    g3 <- ggplotGrob(p3)
    g <- rbind(g1, g2, g3, size="first") # stack the two plots
    g$widths <- unit.pmax(g1$widths, g2$widths) # use the largest widths
    # center the legend vertically
    g$layout[grepl("guide", g3$layout$name),c("t","b")] <- c(1,nrow(g))
    grid.newpage()
    grid.draw(g)
    
    ggsave(paste0("_",substr(i, 6,22),"_",idisconnect,".jpeg"), plot=g, device="jpeg")
  
}

####Summarising To Standard Version and Outputting######

temp.b <- temp.output %>%
  group_by(Category_basic, Standard_Version) %>% 
  summarise(Count=n())

ggplot(data = temp.b, aes(x = Standard_Version, y=Count, fill = Category_basic))+
  geom_bar(stat = "identity")
  

temp.final2 <- temp.final5 %>% 
  mutate(DisconnectAS2015=ifelse(is.empty(filter(temp.b, Category_basic =="Category 7 - Disconnect" & Standard_Version =="AS4777.2:2015")$Count),0,filter(temp.b, Category_basic =="Category 7 - Disconnect" & Standard_Version =="AS4777.2:2015")$Count),
         DisconnectAS2005=ifelse(is.empty(filter(temp.b, Category_basic =="Category 7 - Disconnect" & Standard_Version =="AS4777.3:2005")$Count),0,filter(temp.b, Category_basic =="Category 7 - Disconnect" & Standard_Version =="AS4777.3:2005")$Count),
         DisconnectTranstn=ifelse(is.empty(filter(temp.b, Category_basic =="Category 7 - Disconnect" & Standard_Version =="Transition")$Count),0, filter(temp.b, Category_basic =="Category 7 - Disconnect" & Standard_Version =="Transition")$Count),
          CurtailAS2015=ifelse(is.empty(filter(temp.b, Category_basic =="Category 2-6 - Curtailment" & Standard_Version =="AS4777.2:2015")$Count),0, filter(temp.b, Category_basic =="Category 2-6 - Curtailment" & Standard_Version =="AS4777.2:2015")$Count),
          CurtailAS2005=ifelse(is.empty(filter(temp.b, Category_basic =="Category 2-6 - Curtailment" & Standard_Version =="AS4777.3:2005")$Count),0,filter(temp.b, Category_basic =="Category 2-6 - Curtailment" & Standard_Version =="AS4777.3:2005")$Count),
          CurtailTranstn=ifelse(is.empty(filter(temp.b, Category_basic =="Category 2-6 - Curtailment" & Standard_Version =="Transition")$Count),0,filter(temp.b, Category_basic =="Category 2-6 - Curtailment" & Standard_Version =="Transition")$Count)) %>% 
  mutate(Disconnect = paste0(round(Disconnect/TotalNumberSystems*100,2), "% (",Disconnect," systems)"),
         DisconnectAS2015 = paste0(round(DisconnectAS2015/TotalNumberSystems*100,2), "% (",DisconnectAS2015," systems)"),
         DisconnectAS2005 = paste0(round(DisconnectAS2005/TotalNumberSystems*100,2), "% (",DisconnectAS2005," systems)"),
         DisconnectTranstn = paste0(round(DisconnectTranstn/TotalNumberSystems*100,2), "% (",DisconnectTranstn," systems)"),
         Curtail = paste0(round(Curtail/TotalNumberSystems*100,2), "% (",Curtail," systems)"),
         CurtailAS2015 = paste0(round(CurtailAS2015/TotalNumberSystems*100,2), "% (",CurtailAS2015," systems)"),
         CurtailAS2005 = paste0(round(CurtailAS2005/TotalNumberSystems*100,2), "% (",CurtailAS2005," systems)"),
         CurtailTranstn = paste0(round(CurtailTranstn/TotalNumberSystems*100,2), "% (",CurtailTranstn," systems)"),
         RideThrough = paste0(round(RideThrough/TotalNumberSystems*100,2), "% (",RideThrough," systems)")) 

temp.d <- temp.b %>% 
  group_by(Standard_Version) %>% 
  summarise(TotalCount = sum(Count)) %>% 
  right_join(.,temp.b, by="Standard_Version") %>% 
  mutate(Percentage_of_Systems = Count/TotalCount*100)

write.csv(temp.d, paste0("Response_type_by_Standard_", substr(i, 6,22), ".csv"))

ggplot(data = temp.d, aes(x = Standard_Version, y=Percentage_of_Systems, fill = Category_basic))+
  geom_bar(stat = "identity")

ggsave(paste0("BarChart_Rsponse_byStandard_",substr(i, 6,22),".jpeg"), plot=last_plot(), device="jpeg")


write.csv(temp.output, paste0("Output_",substr(i, 6,22),".csv"))

temp.final3 <- temp.final2 %>%
  mutate(MaxVoltage=max(filter(draft_output, ts>=EventTime & ts<=(EventTime+ minutes(2)))$voltage)) %>% 
  mutate(MinVoltage=min(filter(draft_output, ts>=EventTime & ts<=(EventTime+ minutes(2)))$voltage)) %>% 
  mutate(MaxVoltage = paste(round(MaxVoltage,2), "V (", round(MaxVoltage/240,2), "pu)")) %>% 
  mutate(MinVoltage = paste(round(MinVoltage,2), "V (", round(MinVoltage/240,2), "pu)"))

temp.final <- temp.final3

colnames(temp.final) <- c("Location", "Time of Event", "Total Number of Systems", "Duration", "Number of Disconnections", "Number of Curtailments", "Number of Ride Through","AS4777.2:2015 Disconnections", 
                          "AS4777.3:2005 Disconnections","Transition Disconnections","AS4777.2:2015 Curtailment", "AS4777.2:2005 Curtailment", "Transition Curtailment", 
                          "Maximum Voltage Observed", "Minumum Voltage Observed")

Final_output <- rbind(Final_output, temp.final)

# write.csv(draft_output, "TimeSeriesOutput.csv")
# Aggregate_output <- draft_output %>% 
#   group_by(ts) %>% 
#   summarise(Power = sum(power_kW))
# write.csv(Aggregate_output, "Aggregate_TimeSeriesOutput.csv")


rm(list=ls(pattern="temp."))
rm(list="Final_clean", "draft_output", "g","g1","g2", "g3", "p1", "p2", "p3")

}

setwd("~/GitHub/DER_Event_analysis/SolarAnalytics_analysis/output/ToAnalyse/")
write.csv(Final_output, "Final_output.csv")

#   
# temp.c <- temp.output %>%
#   group_by(Category_basic, Standard_Version) %>% 
#   summarise(InstalledCap=sum(DC.Rating.kW.))
# 
# ggplot(data = temp.c, aes(x = Standard_Version, y=InstalledCap, fill = Category_basic))+
#   geom_bar()
# 
# write.csv(temp.b, "impact_standard_count.csv", row.names=TRUE)
# 
# temp.c <- draft_output %>% 
#   group_by(ts, Category_basix, Standard_Version) %>% 
#   summarise(Agg_power= sum(power_kW))

# temp.before.Event <- EventTime - minutes(5)
#   
# temp.after.Event <- EventTime + minutes(10)
# 
# ggplot(filter(temp.c, ts>=temp.before.Event & ts<=temp.after.Event), aes(x=ts, y=Agg_power))+
#   geom_point()+
#   facet_grid(~Standard_Version)+
#   geom_vline(xintercept = EventTime, linetype="dashed")



# chartoutput <- filter(draft_output, Category_basix=="Category 2 - Dip" & Standard_Version=="AS4777.2:2015")
# 
# chartoutput <- chartoutput[,c("c_id","ts", "power_kW_30sec")]

# chartoutput1 <- melt(chartoutput, id_vars=ts, measure.vars=c_id, variable.name=value)

# chartoutput <- filter(chartoutput, ts>=temp.before.Event & ts<=temp.after.Event )
# 
# ggplot(chartoutput, aes(x=ts))+
#   geom_line(aes(y=power_kW_30sec))+
#   facet_wrap(~c_id)
# 
# 
# draft_output2 <- filter(draft_output, s_postcode=="4551") %>% 
#   filter(ts>=temp.before.Event & ts<=temp.after.Event)
# 
# draft_output2 <- filter(draft_output, Standard_Version=="AS4777.2:2015" & Category_basix=="Category 3 - Disconnect") %>% 
#   filter(ts>="2017-02-15 10:30:00" & ts<="2017-02-15 10:45:00")
# 
# 
# ggplot(draft_output2, aes(x=ts))+
#   geom_line(aes(y=power_kW_30sec))+
#   facet_wrap(~c_id)
# 

# 
# 
# colnames(temp.clean_1)[colnames(temp.clean_1)=="State"] <- "s_state"
# 
# colnames(temp.clean_1)[colnames(temp.clean_1)=="duration"] <- "d"
# 


# aggregate_pv_data <- temp.clean_1 %>% 
#   group_by(ts) %>% 
#   summarise(power_kW)
# 
# 
# temp.working <- temp.clean_1 %>%
#   mutate(DC.Rating.kW.= DC.Rating.kW./1000) %>% 
#   mutate(Size = ifelse(DC.Rating.kW.<30, "<30 kW", ifelse(DC.Rating.kW.<=100, "30-100kW", ">100kW"))) %>%
#   na.omit()


# temp.a <- as.data.frame( unique(temp.category$c_id))
# 
# colnames(temp.a)[colnames(temp.a)=="unique(temp.category$c_id)"] <- "c_id"
# 
# temp.b <- as.data.frame( unique(temp.working$c_id))
# colnames(temp.b)[colnames(temp.b)=="unique(temp.working$c_id)"] <- "c_id"
# 
# anti_join(temp.b, temp.a)


# 
# temp.disconnect <- filter(temp.output, Category_basic == "Category 7 - Disconnect")
# 
# temp.disconnect.values <- filter(Final_clean, c_id %in% c(unique(temp.disconnect$c_id)))
# 
# ggplot(filter(temp.disconnect.values, ts>= (EventTime-minutes(2)) & ts<=(EventTime+minutes(5))), aes(ts, power_kW))+ 
#     geom_line()+
#     facet_wrap(~c_id, scales = "free_y")+
#     geom_vline(xintercept = EventTime, linetype="dashed")+
#     ggtitle(paste("Systems that disconnected"))
#   
# ggsave(paste0("Disconnected_",substr(input.file.name, 1,15), ".jpeg"), plot=last_plot(), device="jpeg")
# 



# 
# temp.info <- temp.disconnect %>% 
#   select(c_id, site_id, s_postcode, inverter_manufacturer, inverter_model, pv_install_date, Standard_Version, Size, Category)
# 
# print(paste("Number of rows in output file", nrow(temp.output)))
# print(paste("Unique number of c_ids in data file", length(unique(temp.output$c_id))))
# 
# # 
# # write.csv(draft_output, "output2.csv")

###Plotting power and Voltage as one
# plot_power_voltage <- function(id){
#   temp.curtail.values2 <- filter(Final_clean, c_id==id) %>%
#     filter(ts>= (EventTime-minutes(2)) & ts<=(EventTime+minutes(5)))
#   
#   p1 <- ggplot(temp.curtail.values2, aes(x = ts, y=power_kW))+
#     geom_line(stat="identity")+
#     geom_vline(xintercept = EventTime, linetype="dashed")
#   # ggtitle(paste(ii))
#   
#   p2 <- ggplot(temp.curtail.values2, aes(x = ts, y=voltage))+
#     geom_line()+
#     # geom_line(stat="identity")+
#     # facet_wrap(~c_id, scales = "free_y")+
#     geom_vline(xintercept = EventTime, linetype="dashed")
#   # ggtitle(paste(ii))
#   
#   
#   g1 <- ggplotGrob(p1)
#   #g1 <- gtable_add_cols(g1, unit(0,"mm")) # add a column for missing legend
#   g2 <- ggplotGrob(p2)
#   g3 <- rbind(g1, g2, size="first") # stack the two plots
#   g3$widths <- unit.pmax(g1$widths, g2$widths) # use the largest widths
#   # center the legend vertically
#   g3$layout[grepl("guide", g3$layout$name),c("t","b")] <- c(1,nrow(g3))
#   grid.newpage()
#   grid.draw(g)
#   
# }

###Example of a working version
# set.seed(123)
# pl <- lapply(1:11, function(.x) 
#   qplot(1:10, rnorm(10), main=paste("plot", .x)))
# ml <- marrangeGrob(pl, nrow=2, ncol=2)
# ## non-interactive use, multipage pdf
# ## ggsave("multipage.pdf", ml)
# ## interactive use; calling `dev.new` multiple times
# ml