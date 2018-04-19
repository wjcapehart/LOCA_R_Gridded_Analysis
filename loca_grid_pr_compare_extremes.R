

library("ncdf4")
library("lubridate") # processing dates and time
library("maps")
library("RColorBrewer")
library("colorRamps")
library("grDevices")
library("colorspace")
library("extRemes")
library("beanplot")  # library for using bean plots for comparative plots
library("reshape2")  # library for manipulating data frames


################################################################
#
# Run Control
#
################################################################

PDF_on                  = FALSE
ROW_print_diagnostics   = TRUE
OPENDAP_on              = FALSE
nprocs                  = 8



################################################################
#
# Thredds information
#
################################################################

if (OPENDAP_on){
  thredds_URL_prefix = "http://kyrill.ias.sdsmt.edu:8080/thredds/dodsC/LOCA_SODAK/"
} else {
  thredds_URL_prefix = "/projects/ECEP/LOCA_MACA_Ensembles/LOCA/LOCA_SODAK/"
}




################################################################
#
# Scenario Choices
#
################################################################

variable                = "pr"
variable_human_name     = "Daily Precipitation"
variable_prefered_units = "mm"

control_period        = "1961_to_1990"
control_period        = "1976_to_2005"
working_period        = "2020_to_2049"

control_period_labels = "1961 to 1990"
control_period_labels = "1976 to 2005"
working_period_labels = "2020 to 2049"


prefix = "~/GitHub/LOCA_R_Gridded_Analysis/return_statistics_SODAK_LOCA_"



################################################################
#
# Site Control
#
################################################################

station_name = "Rapid City"
station_lon  = 256.96875 - 360.0
station_lat  =   44.03125

station_name = "Sioux Falls"

station_lat  =  43.57583103 
station_lon  = -96.738497046

################################################################
#
# File Control
#
################################################################


control_file = paste(prefix,
                     "historical",
                     "_",
                     variable,
                     "_",
                     control_period,
                     ".nc",
                     sep="")

rcp85_file   = paste(prefix,
                     "rcp85",
                     "_",
                     variable,
                     "_",
                     working_period,
                     ".nc",
                     sep="")

rcp45_file   = paste(prefix,
                     "rcp45",
                     "_",
                     variable,
                     "_",
                     working_period,
                     ".nc",
                     sep="")



if (PDF_on) {
  pdf(paste("./comparative_return_statistics",
            "_",
            variable,
            "_",
            working_period,
            "__vs__",
            control_period,
            ".pdf",
            sep=""))
}



################################################################
#
# Extracting Data Coordinates
#
################################################################

   nc.hist = nc_open(filename = control_file)

   latitude = ncvar_get(nc      = nc.hist,    # netcdf file ID
                        varid   = "lat",      # variable name from file
                        verbose = FALSE)      # print diagnostic data

   longitude = ncvar_get(nc      = nc.hist,    # netcdf file ID
                         varid   = "lon",      # variable name from file
                         verbose = FALSE)      # print diagnostic data
   
   longitude[longitude>180] = longitude[longitude>180] - 360

   ensemble_member = ncvar_get(nc    = nc.hist,    # netcdf file ID
                             varid   = "ensemble_member", # variable name from file
                             verbose = FALSE)      # print diagnostic data
   
   event_return_interval = ncvar_get(nc      = nc.hist,    # netcdf file ID
                                             varid   = "extreme_event_return_interval",      # variable name from file
                                             verbose = FALSE)      # print diagnostic data
   
   
   nx = length(longitude)
   ny = length(latitude)
   np = length(event_return_interval)
   ne = length(ensemble_member)
   
   
   i_sta = which.min((longitude-station_lon)^2 )
   j_sta = which.min((latitude -station_lat)^2 )
   
################################################################
#
# Extracting Return Peruods
#
################################################################

   nc.rcp45 = nc_open(filename = rcp45_file)
   nc.rcp85 = nc_open(filename = rcp85_file)
   
   
   return_period.hist = ncvar_get(nc      = nc.hist,    # netcdf file ID
                                  varid   = "daily_precip_return",      # variable name from file
                                  verbose = FALSE)      # print diagnostic data
   
   return_period.rcp45 = ncvar_get(nc      = nc.rcp45,    # netcdf file ID
                                   varid   = "daily_precip_return",      # variable name from file
                                   verbose = FALSE)      # print diagnostic data

   return_period.rcp85 = ncvar_get(nc      = nc.rcp85,    # netcdf file ID
                                   varid   = "daily_precip_return",      # variable name from file
                                   verbose = FALSE)      # print diagnostic data
   
   
   dimnames(return_period.hist) = list(longitude,
                                        latitude,
                                        ensemble_member,
                                        event_return_interval)
   
   dimnames(return_period.rcp45) = list(longitude,
                                       latitude,
                                       ensemble_member,
                                       event_return_interval)
   
   dimnames(return_period.rcp85) = list(longitude,
                                       latitude,
                                       ensemble_member,
                                       event_return_interval)
   
   
   nc_close(nc.hist)
   nc_close(nc.rcp45)
   nc_close(nc.rcp85)
   
   remove(nc.hist,
          nc.rcp45,
          nc.rcp85,
          prefix,
          control_file,
          rcp45_file,
          rcp85_file,
          thredds_URL_prefix
          )
  
   ################################################################
   #
   # create mean and medium aggregates by point and returnperiod
   #
   ################################################################
   
   mean.return_period.hist = apply(X      = return_period.hist,
                                   MARGIN = c(1,2,4),
                                   FUN    = "mean")

   
   mean.return_period.rcp45 = apply(X      = return_period.rcp45,
                                    MARGIN = c(1,2,4),
                                    FUN    = "mean")

   
   mean.return_period.rcp85 = apply(X      = return_period.rcp85,
                                    MARGIN = c(1,2,4),
                                    FUN    = "mean")
   
   
   delta.mean.mean.return_period.rcp85 = apply(X      = (return_period.rcp85 -
                                                         return_period.hist),
                                               MARGIN = c(1,2,4),
                                               FUN    = "mean")
     
   delta.mean.mean.return_period.rcp45 = apply(X      = (return_period.rcp45 -
                                                         return_period.hist),
                                               MARGIN = c(1,2,4),
                                               FUN    = "mean")
   
   ################################################################
   #
   # Plot Mean Deltas  RCP 8.5
   #
   ################################################################
  
   
   nlevels = 11
   RColorBrewer.RdBu = colorRampPalette( rev(brewer.pal(nlevels, "RdBu")) )

   for (p in 1:np) {
     
     zlim      = c(-max(abs(delta.mean.mean.return_period.rcp85[,,p])),
                   max(abs(delta.mean.mean.return_period.rcp85[,,p])))
     
     filled.contour(x         = longitude,
                    y         = latitude,
                    z         = delta.mean.mean.return_period.rcp85[,,p],
                    zlim      = zlim,
                    levels    = pretty(zlim, nlevels),
                    color.palette = RColorBrewer.RdBu,
                    main      = paste(event_return_interval[p],
                                      "-Year ",
                                      "Return Period Change for Daily Rainfall (mm)",
                                      "\n",
                                      "RCP 8.5 ",
                                      "(", working_period_labels, ")",
                                      " - Historical ",
                                      "(", control_period_labels, ")",
                                      sep = ""),
                    asp       = 1,                    plot.axes = { map(database = "county",
                                      add      = TRUE,
                                      col      = "black",
                                      lwd      = 1);
                                  map(database = "state",
                                      add      = TRUE,
                                      col      = "black",
                                      lwd      = 2)
                                 }
     )
   }

   
   ################################################################
   #
   # Plot Mean Deltas  RCP 4.5
   #
   ################################################################
   
   
   nlevels = 11
   RColorBrewer.RdBu = colorRampPalette( rev(brewer.pal(nlevels, "RdBu")) )
   
   for (p in 1:np) {
     
     zlim      = c(-max(abs(delta.mean.mean.return_period.rcp45[,,p])),
                    max(abs(delta.mean.mean.return_period.rcp45[,,p])))
     
     filled.contour(x         = longitude,
                    y         = latitude,
                    z         = delta.mean.mean.return_period.rcp45[,,p],
                    zlim      = zlim,
                    levels    = pretty(zlim, nlevels),
                    color.palette = RColorBrewer.RdBu,
                    main      = paste(event_return_interval[p],
                                      "-Year ",
                                      "Return Period Change for Daily Rainfall (mm)",
                                      "\n",
                                      "RCP 8.5 ",
                                      "(", working_period_labels, ")",
                                      " - Historical ",
                                      "(", control_period_labels, ")",
                                      sep = ""),
                    asp       = 1,                    plot.axes = { map(database = "county",
                                                                        add      = TRUE,
                                                                        col      = "black",
                                                                        lwd      = 1);
                      map(database = "state",
                          add      = TRUE,
                          col      = "black",
                          lwd      = 2)
                    }
     )
   }
   
      
  ################################################################
  #
  # Extract and Organize Point Data
  #
  ################################################################

  hist_point_data  = return_period.hist[ i_sta,j_sta,,]
  rcp85_point_data = return_period.rcp85[i_sta,j_sta,,]
  rcp45_point_data = return_period.rcp45[i_sta,j_sta,,]
  
  
  
  
  station_values = melt(data        = hist_point_data,              # your array
                        na.rm       = TRUE,                  # don't use missing values
                        varnames    = c("Ensemble","Return Period"),  # names of your two dimensions
                        value.name  = "HIST") # the final name of your aray value
  
  station_rcp85 = melt(data        = rcp85_point_data,              # your array
                       na.rm       = TRUE,                  # don't use missing values
                       varnames    = c("Ensemble","Return Period"),  # names of your two dimensions
                       value.name  = "RCP85") # the final name of your aray value
  
  
  station_rcp45 = melt(data        = rcp45_point_data,              # your array
                       na.rm       = TRUE,                  # don't use missing values
                       varnames    = c("Ensemble","Return Period"),  # names of your two dimensions
                       value.name  = "RCP45") # the final name of your aray value
  
  
   
  station_values$RCP45 = station_rcp45$RCP45
  station_values$RCP85 = station_rcp85$RCP85
  
  
  base_all     = melt(data       = station_values,
                      varnames   = c("Ensemble","Return Period","HIST", "RCP45", "RCP85"),
                      id.vars    = c("Ensemble","Return Period"),
                      na.rm      = TRUE,
                      value.name = "Return_Period_Values")
  
  colnames(base_all) <- c("Ensembles","Return_Period","Scenario","Return_Period_Values")
  
  base_all$merged_period_scenarios = paste(sprintf("%2d", base_all$Return_Period),
                                           base_all$Scenario,
                                           sep = " ")
  

  remove(station_rcp45,
         station_rcp85,
         hist_point_data,
         rcp85_point_data,
         rcp45_point_data)
  
  
  ################################################################
  #
  # Plot Return Periods for Station for RCP45
  #
  ################################################################
  
  
  hist_rcp45 = subset(x      = base_all,
                      subset = (Scenario != "RCP85"))
  

  beanplot(formula     = Return_Period_Values~merged_period_scenarios,  # formula selection for y axis
           data        = hist_rcp45,                                     # data frame to use
           col         = list(c("skyblue1", "darkblue","darkblue","darkblue"),  # same order as earlier for each series
                              c("yellow",  "yellow4",  "yellow4", "yellow4")), 
           border      = c("darkblue","darkred"),                              
           overallline = "median",                              # can't get rid of this dang thing
           beanlines   = "median",                              # use median for the "central value"
           side        = "both",
           main        = paste(station_name,
                               " LOCA Return Intervals",         # title caption
                                sep=""),
           xlab        = "Return Period (Years)",               # xaxis title
           ylab        = paste("Maximum ",
                               variable_human_name,                   # yaxis title
                               " (",
                               variable_prefered_units,
                               ")",
                               sep=""),
           log         = "",                                    # overide log axis on y (it defaults to log for big datasets)
           xlim        = c(1,np),                   # x axis range
           ylim        = c(min(base_all$Return_Period_Values)-5,
                           max(base_all$Return_Period_Values)+5)
  )
  
  
  legend("topleft", 
         fill   = c("skyblue1", 
                    "yellow"), 
         legend = c(paste(control_period_labels,"Historical"),
                    paste(working_period_labels,"RCP 4.5"))
  )
  
  
  
  ################################################################
  #
  # Plot Return Periods for Station for RCP85
  #
  ################################################################
  
  
  hist_rcp85 = subset(x      = base_all,
                      subset = (Scenario != "RCP45"))
  
  
  beanplot(formula     = Return_Period_Values~merged_period_scenarios,  # formula selection for y axis
           data        = hist_rcp85,                                     # data frame to use
           col         = list(c("skyblue1", "darkblue","darkblue","darkblue"),  # same order as earlier for each series
                              c("pink",  "darkred",  "darkred", "darkred")), 
           border      = c("darkblue","darkred"),                              
           overallline = "median",                              # can't get rid of this dang thing
           beanlines   = "median",                              # use median for the "central value"
           side        = "both",
           main        = paste(station_name,
                               " LOCA Return Intervals",         # title caption
                               sep=""),
           xlab        = "Return Period (Years)",               # xaxis title
           ylab        = paste("Maximum ",
                               variable_human_name,                   # yaxis title
                               " (",
                               variable_prefered_units,
                               ")",
                               sep=""),
           log         = "",                                    # overide log axis on y (it defaults to log for big datasets)
           xlim        = c(1,np),                   # x axis range
           ylim        = c(min(base_all$Return_Period_Values)-5,
                           max(base_all$Return_Period_Values)+5)
  )
  
  
  legend("topleft", 
         fill   = c("skyblue1", 
                    "red"), 
         legend = c(paste(control_period_labels,"Historical"),
                    paste(working_period_labels,"RCP 8.5"))
         )
  
  
  
  ################################################################
  #
  # Plot Return Periods for Station for RCP85
  #
  ################################################################
  
  
  rcp45_rcp85 = subset(x      = base_all,
                      subset = (Scenario != "HIST"))
  
  
  beanplot(formula     = Return_Period_Values~merged_period_scenarios,  # formula selection for y axis
           data        = rcp45_rcp85,                                     # data frame to use
           col         = list( c("yellow",  "yellow4",  "yellow4", "yellow4"),  # same order as earlier for each series
                              c("pink",  "darkred",  "darkred", "darkred")), 
           border      = c("darkblue","darkred"),                              
           overallline = "median",                              # can't get rid of this dang thing
           beanlines   = "median",                              # use median for the "central value"
           side        = "both",
           main        = paste(station_name,
                               " LOCA Return Intervals",         # title caption
                               sep=""),
           xlab        = "Return Period (Years)",               # xaxis title
           ylab        = paste("Maximum ",
                               variable_human_name,                   # yaxis title
                               " (",
                               variable_prefered_units,
                               ")",
                               sep=""),
           log         = "",                                    # overide log axis on y (it defaults to log for big datasets)
           xlim        = c(1,np),                   # x axis range
           ylim        = c(min(base_all$Return_Period_Values)-5,
                           max(base_all$Return_Period_Values)+5)
  )
  
  
  legend("topleft", 
         fill   = c("yellow", 
                    "red"), 
         legend = c(paste(working_period_labels,"RCP 4.5"),
                    paste(working_period_labels,"RCP 8.5"))
  )
  
  
  ################################################################
  #
  # Close Graphics
  #
  ################################################################
  
  if (PDF_on) {
    dev.off()
  }
