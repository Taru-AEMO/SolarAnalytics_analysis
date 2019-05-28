require("dplyr")
require("tidyr")
require("lubridate")
require("ggplot2")
require("reshape2")
require("installr")
library("gtable")
library("grid")
library("gridExtra")
source("AEMOtemplate_format.R")
################ set directory for all inputs / outputs
directory <- "~/GitHub/DER_Event_analysis/SolarAnalytics_analysis/output/All_Event_Results"

#
setwd(paste0("",directory,""))
################# set inputs
## save short underlying data from r shiny ______ this file is used for all plots of minutes/hour near event
# underlying_data_file <- "E1_6_short_ud.csv"
underlying_data_file <- "20180118_underlying.csv"
# underlying_data_file <- "E3_10_short_ud.csv"
#underlying_data_file <- "E4_5_short_ud.csv"

# underlying_data_file <- "T1_2_short_ud.csv"
# underlying_data_file <- "T2_2_short_ud.csv"


## save long underlying data from r shiny ______ this file is used for one plot and must include time series for entire day
# day_timeseries_file <- ""

## toggle upscaled data and save aggregated results from r shiny
# upscaled_aggregated_file <- "E1_6_short_upscaled_ar.csv"
upscaled_aggregated_file <- "20180118_upscale.csv"
# upscaled_aggregated_file <- "E3_10_short_upscaled_ar.csv"
#upscaled_aggregated_file <- "E4_5_short_upscaled_ar.csv"
# upscaled_aggregated_file <- NULL



####### event data, only one t0 can be input per analysis. multiple events can be input.
###### don't deviate from formats
event_date <- "20180118"

### t0 is the interval immediately prior to event <-- must match exact time stamp in data
# pre_event_interval <- "07:52:25"
pre_event_interval <- "15:18:25"
# pre_event_interval <- "09:43:55"
#pre_event_interval <- "11:29:55"

# pre_event_interval <- "09:29:55"
# pre_event_interval <- "09:32:55"
# pre_event_interval <- "09:35:55"
# pre_event_interval <- "09:36:55"
# pre_event_interval <- "09:38:55"
# pre_event_interval <- "09:43:55"
# pre_event_interval <- "09:49:55"


# event_time <- c("07:52:55","07:53:55")
event_time <- c( "15:19:25")
# event_time <- c("09:44:25","09:45:55","09:50:55")
#event_time <- c("11:30:55")

# event_time <- c("09:31:25")
# event_time <- c("09:33:25","09:33:55")
# event_time <- c("09:36:25")
# event_time <- c("09:37:25")



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

