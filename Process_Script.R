####PROCESS_SCRIPT.R

#README: This script is designed to Process a Cleaned Data Set to Evaluate the Number of Disconnections.

EventTime <- ymd_hms(EventTime, tz="Australia/Brisbane")

'2017-02-15 10:34:25 AEST'
#Filter for Power value at T_0

t_0 <- EventTime
t_end_estimate_nadir <- EventTime + minutes(4)
t_end_nadir <- EventTime + minutes(2)

temp.power_t0 <- filter(Final_clean, ts==t_0) %>%
  .[,c("c_id", "ts","power_kW_30sec")]


temp.unmatch <- anti_join(Final_clean,temp.power_t0, by='c_id') %>% 
  filter(ts== (ymd_hms(t_0, tz="Australia/Brisbane") - seconds(30))) %>% 
  .[,c("c_id", "ts","power_kW_30sec")]

temp.power_t0 <- rbind(temp.power_t0, temp.unmatch)

colnames(temp.power_t0)[colnames(temp.power_t0)=="power_kW_30sec"] <- "p_0"
colnames(temp.power_t0)[colnames(temp.power_t0)=="ts"] <- "t0"
#Filter for possible Nadir time then identify the T_Nadir and identify power at this time
temp.nadir.df <- filter(Final_clean, ts>t_0 & ts<=t_end_estimate_nadir)
temp.nadir_t0 <- temp.nadir.df%>%
  group_by(c_id) %>%
  summarise(p_nadir=min(power_kW_30sec)) %>%
  left_join(temp.nadir.df[,c("c_id","ts","power_kW_30sec")], by=c("c_id", "p_nadir"="power_kW_30sec")) %>%
  .[,c(1,3,2)]
colnames(temp.nadir_t0)[colnames(temp.nadir_t0)=="ts"] <- "t_nadir"

#Filter for Power Value at T_1 and T_2
temp.power_t1 <- filter(Final_clean, ts==(ymd_hms(t_0, tz="Australia/Brisbane") + seconds(30))) %>%
  .[,c("c_id","ts","power_kW_30sec")]
colnames(temp.power_t1)[colnames(temp.power_t1)=="power_kW_30sec"] <- "p_0plus1"
colnames(temp.power_t1)[colnames(temp.power_t1)=="ts"] <- "t_0plus1"

temp.power_t2 <- filter(Final_clean, ts==(ymd_hms(t_0, tz="Australia/Brisbane") + minutes(1)+seconds(30))) %>%
  .[,c("c_id","ts","power_kW_30sec")]
colnames(temp.power_t2)[colnames(temp.power_t2)=="power_kW_30sec"] <- "p_0plus2"
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


temp.a <- as.data.frame( unique(temp.category$c_id))

colnames(temp.a)[colnames(temp.a)=="unique(temp.category$c_id)"] <- "c_id"

temp.b <- as.data.frame( unique(temp.working$c_id))
colnames(temp.b)[colnames(temp.b)=="unique(temp.working$c_id)"] <- "c_id"

anti_join(temp.b, temp.a)

draft_output <- left_join(temp.working, temp.category, by="c_id")

write.csv(draft_output, "output2.csv")


temp.b <- filter(draft_output, ts==EventTime) %>%
  group_by(Category_basix, Standard_Version) %>% 
  summarise(Count=n(),
            InstalledCap=sum(DC.Rating.kW.))


write.csv(temp.b, "impact_standard_count.csv", row.names=TRUE)

temp.c <- draft_output %>% 
  group_by(ts, Category_basix, Standard_Version) %>% 
  summarise(Agg_power= sum(power_kW_30sec))

temp.before.Event <- EventTime - minutes(5)
  
temp.after.Event <- EventTime + minutes(10)

ggplot(filter(temp.c, ts>=temp.before.Event & ts<=temp.after.Event), aes(x=ts, y=Agg_power))+
  geom_point()+
  facet_grid(~Standard_Version)

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