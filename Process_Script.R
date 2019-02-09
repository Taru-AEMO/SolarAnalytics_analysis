####PROCESS_SCRIPT.R

setwd(paste0("~/GitHub/DER_Event_analysis/SolarAnalytics_analysis/output/",folder,"/",data.date))

#README: This script is designed to Process a Cleaned Data Set to Evaluate the Number of Disconnections.

EventTime <- ymd_hms(EventTime, tz="Australia/Brisbane")

print(EventTime)

#Filter for Power value at T_0

t_0 <- EventTime
t_end_estimate_nadir <- EventTime + minutes(4)
t_end_nadir <- EventTime + minutes(2)

temp.power_t0 <- filter(Final_clean, ts>=(t_0 - seconds(14)) & ts<= (t_0 + seconds(14))) %>%
  .[,c("c_id", "ts","power_kW")]

##Print confirmation
print(paste("Number of unique ids in data set", length(unique(Final_clean$c_id))))

print(paste("Number of rows selected", nrow(temp.power_t0), "this is made up of", length(unique(temp.power_t0$c_id)), "unique ids"))

Print("Please confirm these values match")

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

temp.power_t2 <- filter(Final_clean, ts>=(t_0 + seconds(46)) & ts<=(t_0 + seconds(135))) %>%
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
                                         ifelse(abs(p_nadir)<=Cat3_PL_perc, "Category 3 - Mild Curtailment",
                                                ifelse(abs(p_nadir)<=Cat4_PL_perc, "Category 4 - Medium Curtailment",
                                                       ifelse(abs(p_nadir)<=Cat5_PL_perc, "Category 5 - Significant Curtailment",
                                                              ifelse(abs(p_nadir)>Cat5_PL_perc, "Category 6 - Severe Curtailment",
                                                                     "Not categorised")))))))) %>% 
  # mutate(Category = ifelse(p_nadir<=Cat7_Disconnect_kW & p_0>Cat7_Disconnect_kW,"Category 7 - Disconnect",
  # ifelse(pmax(abs(perc_drop_nadir), abs(perc_drop_to_p1),abs(perc_drop_to_p2))<=Cat1_PL_perc, "Category 1 - Ride Through",
  #        ifelse((abs(p_nadir)<abs(p_0plus1)<abs(p_0plus2))&abs(perc_drop_nadir)<=Cat1_PL_perc, "Category 2 - Dip",
  #               # ifelse(abs(p_nadir)<=Cat3_PL_perc, "Category 3 - Mild Curtailment",
  #               #        ifelse(abs(p_nadir)<=Cat4_PL_perc, "Category 4 - Medium Curtailment",
  #                             ifelse(abs(perc_drop_nadir)<=Cat5_PL_perc, "Category 3-5 - Curtailment",
  #                                    ifelse(abs(perc_drop_nadir)>Cat5_PL_perc, "Category 6 - Severe Curtailment",
  #                                           "Not categorised")))))) %>%
  mutate(Category_basix = ifelse(p_0<=Cat8_Disconnect_kW, "Disconnected prior to event",
                                 ifelse(p_nadir<=Cat7_Disconnect_kW & p_0>Cat8_Disconnect_kW,"Category 3 - Disconnect",
                                        ifelse(pmax(abs(perc_drop_to_p1),abs(perc_drop_to_p2))<=Cat1_PL_perc, "Category 1 - Ride Through",
                                               ifelse((abs(p_0plus1)<abs(p_0plus2))&abs(perc_drop_to_p1)<=Cat1_PL_perc, "Category 2 - Dip",
                                                      "Category 2 - Curtailment")))))


temp.category.table <- temp.category %>% 
  group_by(Category,c_id) %>% 
  summarise(count =n()) %>% 
  group_by(Category) %>% 
  summarise(count = n())


print(temp.category.table)
# 
# 
# colnames(temp.clean_1)[colnames(temp.clean_1)=="State"] <- "s_state"
# 
# colnames(temp.clean_1)[colnames(temp.clean_1)=="duration"] <- "d"
# 
temp.clean_1 <- Final_clean %>%
  mutate(pv_install_date_day=ifelse(nchar(pv_install_date)==10,pv_install_date, paste0(pv_install_date, "-28"))) %>%
  mutate(pv_install_date_day=ymd(pv_install_date_day)) %>%
  mutate(Standard_Version = ifelse(pv_install_date_day<"2015-10-09", "AS4777.3:2005", ifelse(pv_install_date_day>="2016-10-09", "AS4777.2:2015", "Transition")))

# aggregate_pv_data <- temp.clean_1 %>% 
#   group_by(ts) %>% 
#   summarise(power_kW)
# 
# 
temp.working <- temp.clean_1 %>%
  mutate(DC.Rating.kW.= DC.Rating.kW./1000) %>% 
  mutate(Size = ifelse(DC.Rating.kW.<30, "<30 kW", ifelse(DC.Rating.kW.<=100, "30-100kW", ">100kW"))) %>%
  na.omit()


# temp.a <- as.data.frame( unique(temp.category$c_id))
# 
# colnames(temp.a)[colnames(temp.a)=="unique(temp.category$c_id)"] <- "c_id"
# 
# temp.b <- as.data.frame( unique(temp.working$c_id))
# colnames(temp.b)[colnames(temp.b)=="unique(temp.working$c_id)"] <- "c_id"
# 
# anti_join(temp.b, temp.a)

draft_output <- left_join(temp.working, temp.category, by="c_id")

temp.output <- left_join(temp.power_t0, draft_output, by = c("c_id","t0"="ts"))

write.csv(draft_output, "output2.csv")


temp.b <- temp.output %>%
  group_by(Category_basix, Standard_Version) %>% 
  summarise(Count=n())

ggplot(data = temp.b, aes(x = Standard_Version, y=Count, fill = Category_basix))+
  geom_bar(stat = "identity")

temp.d <- temp.b %>% 
  group_by(Standard_Version) %>% 
  summarise(TotalCount = sum(Count)) %>% 
  right_join(.,temp.b, by="Standard_Version") %>% 
  mutate(Percentage_of_Systems = Count/TotalCount*100)


ggplot(data = temp.d, aes(x = Standard_Version, y=Percentage_of_Systems, fill = Category_basix))+
  geom_bar(stat = "identity")


  
temp.c <- temp.output %>%
  group_by(Category_basix, Standard_Version) %>% 
  summarise(InstalledCap=sum(DC.Rating.kW.))

ggplot(data = temp.c, aes(x = Standard_Version, y=InstalledCap, fill = Category_basix))+
  geom_bar()

write.csv(temp.b, "impact_standard_count.csv", row.names=TRUE)

temp.c <- draft_output %>% 
  group_by(ts, Category_basix, Standard_Version) %>% 
  summarise(Agg_power= sum(power_kW))

temp.before.Event <- EventTime - minutes(5)
  
temp.after.Event <- EventTime + minutes(10)

ggplot(filter(temp.c, ts>=temp.before.Event & ts<=temp.after.Event), aes(x=ts, y=Agg_power))+
  geom_point()+
  facet_grid(~Standard_Version)+
  geom_vline(xintercept = EventTime, linetype="dashed")

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