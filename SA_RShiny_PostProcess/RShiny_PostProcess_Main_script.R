require("dplyr")
require("tidyr")
require("lubridate")
require("ggplot2")
require("reshape2")
require("installr")
library("gtable")
library("grid")
library("gridExtra")
setwd("~/GitHub/DER_Event_analysis/SolarAnalytics_analysis/SA_RShiny_PostProcess")
source("AEMOtemplate_format.R")
################ set directory for all inputs / outputs
directory <- "~/DER_disturbance_analysis/20180906"


#
setwd(paste0("",directory,""))
################# set inputs
## save short underlying data from r shiny ______ this file is used for all plots of minutes/hour near event
underlying_data_file <- "NSW_ud_1243_5.csv"
underlying_data_file <- "QLD_ud_1243_5.csv"
underlying_data_file <- "SA_ud_1243_5.csv"
underlying_data_file <- "VIC_ud_1243_5.csv"


## save long underlying data from r shiny ______ this file is used for one plot and must include time series for entire day
# day_timeseries_file <- ""


## toggle upscaled data and save aggregated results from r shiny
upscaled_aggregated_file <- "NSW_upscaled_1243_5.csv"
upscaled_aggregated_file <- "QLD_upscaled_1243_5.csv"
upscaled_aggregated_file <- "SA_upscaled_1243_5.csv"
upscaled_aggregated_file <- "VIC_upscaled_1243_5.csv"



####### event data, only one t0 can be input per analysis. multiple events can be input.
###### don't deviate from formats
event_date <- "20180906"

### t0 is the interval immediately prior to event <-- must match exact time stamp in data
pre_event_interval <- "12:43:55"

event_time <- c("12:44:55")



######
dir.create(file.path(paste0("",directory,"/PP_output_",event_date,"")))


############# 1. run this before any other ######################  <-- run this first
setwd("~/GitHub/DER_Event_analysis/SolarAnalytics_analysis/SA_RShiny_PostProcess")
source("RShiny_PostProcess_ArrangeData_script.R")


######### these 3 are independent of each other
############# monitored data analysis - tables
setwd("~/GitHub/DER_Event_analysis/SolarAnalytics_analysis/SA_RShiny_PostProcess")
source("RShiny_PostProcess_RawTables.R")

############# upscaled data analysis - tables
setwd("~/GitHub/DER_Event_analysis/SolarAnalytics_analysis/SA_RShiny_PostProcess")
source("RShiny_PostProcess_UpscaledTables.R")

############# data analysis - plots
setwd("~/GitHub/DER_Event_analysis/SolarAnalytics_analysis/SA_RShiny_PostProcess")
source("RShiny_PostProcess_Plots.R")

############ upscaled Plots
setwd("~/GitHub/DER_Event_analysis/SolarAnalytics_analysis/SA_RShiny_PostProcess")
source("RShiny_PostProcess_UpscaledPlots.R")


#######################################################################

