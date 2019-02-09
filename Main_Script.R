require("dplyr")
require("tidyr")
require("lubridate")
require("ggplot2")
require("reshape2")
require("installr")

#Set Working Directory - have pulled this out as it will change with each data set.
folder <- "qld_4555"

dir.create(file.path(paste0("~/GitHub/DER_Event_analysis/SolarAnalytics_analysis/output/",folder)))


setwd(paste0("~/GitHub/DER_Event_analysis/SolarAnalytics_analysis/input/",folder))
#Input the file name of each of the data sets
Actual_Data_file <- "data_4555_2018_02_11.csv"

Site_details_file <- list.files(pattern="site_details")

Circuit_details_file <- "circuit_details_4555_updated.csv"


EventTime <- "2018_02_11 16:28:33"

#Run through each Process
#1. Read, Format and Join Data Files
setwd("~/GitHub/DER_Event_analysis/SolarAnalytics_analysis")

source("Join_script.R")

#2.Clean PV Data Files
setwd("~/GitHub/DER_Event_analysis/SolarAnalytics_analysis")
source("Clean_script_draft.R")

#3. Process Cleaned Files to Evaluate Number of Disconnections

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
Cat7_Disconnect_kW=0.1
Cat8_Disconnect_kW=0

setwd("~/GitHub/DER_Event_analysis/SolarAnalytics_analysis")
source("Process_script.R")

