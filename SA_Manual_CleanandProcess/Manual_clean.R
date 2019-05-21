# ###NEEED TO MANUALLY ENTER THESEE C_IDs. 
folder <- "qld_4500/2018_02_08/"

Actual_Data_file <- "4500_2018_02_08_cleaned.csv"

setwd(paste0("~/GitHub/DER_Event_analysis/SolarAnalytics_analysis/output/",folder))

temp.clean_3 <- read.csv(Actual_Data_file, header=TRUE, stringsAsFactors = FALSE)

temp.visualcheck.remove <- c("430338455",
                              "974794764",
                             "24708995",
                             "1384574425",
                             "612456721",
                             "2125349252",
                            "908657853",
                            "334805309",
                             "1152442906",
                             "272820342",
                             "1574415849",
                             "1548795788",
                             "204621245")

temp.unclean_4 <- temp.clean_3 %>%
  filter(c_id %in% temp.visualcheck.remove)

P6 = ggplot(temp.unclean_4, aes(ts, power_kW))+
  geom_point()+
  facet_wrap(~c_id, scales = "free_y")+
  ggtitle("List of systems that have been removed following a visual check")

ggsave(paste0(substr(Actual_Data_file, 1,15),"_Cleaned_VisualCheck",".jpeg"), plot=P6, device="jpeg")

temp.clean_4 <- temp.clean_3 %>%
  filter(!c_id %in% temp.visualcheck.remove)


Final_clean <- temp.clean_4

file.name <- substr(max(unlist(strsplit(Actual_Data_file, "/"))),6,15)
write.csv(Final_clean, paste0(file.name, "_cleaned.csv"))