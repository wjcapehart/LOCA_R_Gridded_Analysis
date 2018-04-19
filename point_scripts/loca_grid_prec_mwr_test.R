

URL_Root_Directory <- "~/GitHub/LOCA_R_Gridded_Analysis/point_scripts/"

# Here is the variable we want to extract
#  these should match the files names since the variable in the file and file name match

target_variable = "prec"
variable_name   = "Periods Between Wetting Rains"
variable_units  = "days" # after post processing (starts as m/s)

threshold_value =  2.0 # ( 90.-32) * 5/9 

# get the extreme function to use

extreme_func = "GP"


# get URL files
LOCA_File  = paste(location_abrv,
                     "_LOCA.nc",
                     sep="")


# make the final URLs for extracting with the "paste" function

LOCA_URL = paste(URL_Root_Directory,  # string 1 to concatenate
                LOCA_File,         # string 2 to concatenate
                sep=""               # separation character ("" = none)
               )






nc.loca <- nc_open(filename = LOCA_URL)


print(nc.loca)


# get time values

time_hist<-ncvar_get(nc.loca, "time_hist")
tunits<-ncatt_get(nc.loca, "time_hist", attname="units")
tustr<-strsplit(tunits$value, " ")

time_reference_point_hist = unlist(tustr)[3]


time_futr <-ncvar_get(nc.loca, "time_futr")
tunits    <-ncatt_get(nc.loca, "time_futr", attname="units")
tustr     <-strsplit(tunits$value, " ")

# and tidy things up

time_reference_point_futr = unlist(tustr)[3]

# merge into a single vector...

time_vector   = as.Date( x      = abind(time_hist,time_futr), 
                         origin = time_reference_point_hist )

year_vector   = year(time_vector)
decade_vector =  year_vector - year_vector %% 10



# create ensemble dimension

ensemble <- ncvar_get(nc.loca, "ensemble_model")

ensembles_to_use = ensemble[!str_detect(ensemble, "CNRM")]
ensembles_to_use = ensembles_to_use[!str_detect(ensembles_to_use, "GISS")]

n_ensembles = length(ensembles_to_use)

print(ensembles_to_use)

# import spatial coordinates

scenario <- ncvar_get(nc.loca, "RCP_scenario")
print(scenario)

scenario = c("RCP45","RCP85")



varid.hist = paste(target_variable,
                     "_hist",
                     sep="")
  
varid.futr = paste(target_variable,
                     "_futr",
                     sep="")
  
var2d.hist = ncvar_get(nc      = nc.loca,                      # netcdf file ID
                     varid   = varid.hist,            # variable name from file
                     verbose = FALSE                      # print diagnostic data
                     )                 # scaling temperature from K to DegC

var2d.futr = ncvar_get(nc    = nc.loca,                      # netcdf file ID
                     varid   = varid.futr,            # variable name from file
                     verbose = FALSE                      # print diagnostic data
                     )                  # scaling temperature from K to DegC




dimnames(var2d.hist) <- list(ensemble,time_hist)

dimnames(var2d.futr) <- list(scenario,ensemble,time_futr)

size.hist2 = 2*20454*31
var2d.hist2  <- array(1:size.hist2, dim=c(2,31,20454))

var2d.hist2[1,,] = var2d.hist
var2d.hist2[2,,] = var2d.hist


dimnames(var2d.hist2) <- list(scenario,ensemble,time_hist)



var2d.hist = var2d.hist2
remove(var2d.hist2)




var3d = abind(var2d.hist, var2d.futr, along=3)


prec_for_wetting_rain = var3d

dimnames(prec_for_wetting_rain) = list(scenario,ensemble,time_vector)

prec_for_wetting_rain[var3d < wetting_rain_treshold]  = 0
prec_for_wetting_rain[var3d >= wetting_rain_treshold] = 1

prec_for_wetting_rain_returns = prec_for_wetting_rain

prec_for_wetting_rain_returns[,,] = NA
 

median_wetting_rain_returns = prec_for_wetting_rain_returns


ens_target_num = 0

ne = length(ensembles_to_use)

for (ens_target in 1:ne) {

  ######################################
  #
  # Base Period
  #
  
  # create segments for RCP 45 and RCP 85 for your period(s)
  
  decade_values = seq(from = 1950,
                      to   = 2090, 
                      by   =   10)
  
  for (dec in 1 : length(decade_values))  {
    
    t_start = min(which(decade_vector==decade_values[dec]))
    t_end   = max(which(decade_vector==decade_values[dec]))
    
    
    # rcp45 
    
    array_segment = prec_for_wetting_rain[1, ens_target, t_start:t_end]
    
    rle_values = rle(array_segment)

    intervals = rle_values$lengths[rle_values$values==0] 
    
    prec_for_wetting_rain_returns[1, ens_target, t_start:(t_start+length(intervals)-1)] = intervals

    median_wetting_rain_returns[1, ens_target, t_start] = median(intervals) 

    # rcp85 
    
    array_segment = prec_for_wetting_rain[2, ens_target, t_start:t_end]
    
    rle_values = rle(array_segment)

    intervals = rle_values$lengths[rle_values$values==0] 
    
    prec_for_wetting_rain_returns[2, ens_target, t_start:(t_start+length(intervals)-1)] = intervals
    
    median_wetting_rain_returns[2, ens_target, t_start] = median(intervals) 
  }
    
    
}
dimnames(median_wetting_rain_returns) = list(scenario,ensemble,decade_vector)
dimnames(prec_for_wetting_rain_returns) = list(scenario,ensemble,decade_vector)

dimnames(prec_for_wetting_rain_returns) = list(scenario,ensemble,decade_vector)
wetting_rain_returns  = melt(data      = prec_for_wetting_rain_returns,              # your array
                             na.rm       = TRUE,                  # don't use missing values
                             varnames    = c("Scenario","Ensemble","Time"),  # names of your two dimensions
                             value.name  = "Variable")               # the final name of your aray value


median_wetting_rain_returns = melt(data      = median_wetting_rain_returns,              # your array
                             na.rm       = TRUE,                  # don't use missing values
                             varnames    = c("Scenario","Ensemble","Time"),  # names of your two dimensions
                             value.name  = "Variable")               # the final name of your aray value

 wetting_rain_returns$decade = paste(wetting_rain_returns$Time,
                                     "-",
                                     (wetting_rain_returns$Time+9), 
                                     sep="")          
 
  median_wetting_rain_returns$decade = paste(median_wetting_rain_returns$Time,
                                     "-",
                                     (median_wetting_rain_returns$Time+9), 
                                     sep="") 

  
  
  
  

wetting_rain_returns$merged_decade_scenarios = paste(wetting_rain_returns$decade,
                                         wetting_rain_returns$Scenario,
                                         sep = " ")

median_wetting_rain_returns$merged_decade_scenarios = paste(median_wetting_rain_returns$decade,
                                         median_wetting_rain_returns$Scenario,
                                         sep = " ")


  
  beanplot(formula     = Variable~merged_decade_scenarios,  # formula selection for y axis
           data        = median_wetting_rain_returns,                                     # data frame to use
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
           ylab        = paste(variable_name,                   # yaxis title
                               " (",
                               variable_units,
                               ")",
                               sep=""),
           log         = "",                                    # overide log axis on y (it defaults to log for big datasets)
           xlim        = c(1,length(decade_values)),                   # x axis range
           ylim        = c(min(median_wetting_rain_returns$Variable),
                           max(median_wetting_rain_returns$Variable))
  )
  
  
  legend("topleft", 
         fill   = c("yellow", 
                    "red"), 
         legend = c(paste("RCP 4.5"),
                    paste("RCP 8.5"))
  )
  