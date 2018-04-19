
library("ncdf4")
library("lubridate") # processing dates and time
library("climdex.pcic")
library("PCICt")



  ################################################################
  #
  # Machine Optioins
  #
  ################################################################

  PDF_on                  = TRUE
  ROW_print_diagnostics   = TRUE
  OPENDAP_on              = FALSE
  nprocs                  = 8

 
  period_span = 30
  scenario    = "historical"
  start_year  = c(1950)
  period      = 1
  end_year    = c(2005)

  year = start_year : end_year
  period_span = length(year)
  
  archive_name = paste("~/GitHub/LOCA_R_Gridded_Analysis",
                       "/etccdi_cci_indicies_",
                       "SODAK_LOCA_",
                       scenario,
                       "_",
                       "tasmin_tasmax_pr",
                       "_",
                       start_year,
                       "_to_",
                       end_year,
                       ".RData",
                       sep="")
    
  print(archive_name)
  
  load(archive_name)
  
  ###############################################
  ###############################################
  ###############################################
  ###############################################
  ###############################################
  ###############################################
  ##
  ##  NetCDF File Creation
  ##
  ###############################################
  ###############################################
  ###############################################
  ###############################################
  ###############################################
  
  
  bound1 = start_year : end_year
  bound2 = bound1 + 1
  
  bnds = 1:2
  
  time_bounds = array( 0,  dim=c(2,length(year)), dimnames=list(bnds,year))
  
  time_bounds[1,] = start_year : end_year
  time_bounds[2,] = bound1 + 1
  
  delta_lon = (longitude[2]-longitude[1])
  lon_bounds = array( 0,  dim=c(2,length(longitude)), dimnames=list(bnds,longitude))
  lon_bounds[1,] = longitude - delta_lon/2
  lon_bounds[2,] = longitude + delta_lon/2
  
  
  delta_lat = (latitude[2]-latitude[1])
  lat_bounds = array( 0,  dim=c(2,length(latitude)), dimnames=list(bnds,latitude))
  lat_bounds[1,] = latitude - delta_lat/2
  lat_bounds[2,] = latitude + delta_lat/2  
  
  
  netcdf_output_file_name = paste("~/GitHub/LOCA_R_Gridded_Analysis",
                                  "/etccdi_cci_indicies_",
                                  "SODAK_LOCA_",
                                  scenario,
                                  "_",
                                  "tasmin_tasmax_pr",
                                  "_",
                                  start_year,
                                  "_to_",
                                  end_year,
                                  ".nc",
                                  sep="")
  
  print(netcdf_output_file_name)
  
  #
  # dimensions
  #
  
  netcdf_time_dim    = ncdim_def(name = "time",
                                 units = "years since 0000-01-01",
                                 longname = "Time",
                                 calendar = "365_day",
                                 val   = start_year : end_year,
                                 unlim = FALSE)  
  
  netcdf_lon_dim     = ncdim_def(name  = "lon",
                                 units = "degrees_east",
                                 val   = longitude,
                                 longname = "Longitude",
                                 unlim = FALSE)
  
  netcdf_lat_dim     = ncdim_def(name  = "lat",
                                 units = "degrees_north",
                                 val   = latitude,
                                 longname = "Latitude",
                                 unlim = FALSE)
  
  netcdf_enschar_dim  = ncdim_def(name          = "ensemble_member_characters",
                                  units         = "",
                                  val           =  1:max(nchar(ensemble_site)),
                                  create_dimvar = FALSE)
  
  netcdf_bounds_dim  = ncdim_def(name  = "bnds",
                                 units = "",
                                 val   = 1:2,
                                 unlim = FALSE,
                                 create_dimvar = FALSE)
  
  netcdf_ens_dim     = ncdim_def(name          = "ensemble_member",
                                 units         = "",
                                 val           = 1:length(ensemble_site),
                                 create_dimvar = FALSE)
  
  fill_value_short = -32767
  fill_value_float = 9.96921e+36
  fill_value_dble  = 9.969209968386869e+36
  
  
  print("Dimensions Created")
  
  #
  # coordinate variables
  #
  
  netcdf_time_bounds   = ncvar_def(nam      = "time_bnds",
                                   units    = "years since 0000-01-01",
                                   dim      = list(netcdf_bounds_dim,
                                                   netcdf_time_dim),
                                   missval  = fill_value_float,
                                   longname = "Climate Period Bounds",
                                   prec     = "single")
  
  netcdf_lon_bounds   = ncvar_def(nam      = "lon_bnds",
                                  units    = "degrees_east",
                                  dim      = list(netcdf_bounds_dim,
                                                  netcdf_lon_dim),
                                  missval  = fill_value_float,
                                  longname = "Longitude Bounds",
                                  prec     = "single")
  
  netcdf_lat_bounds   = ncvar_def(nam      = "lat_bnds",
                                  units    = "degrees_north",
                                  dim      = list(netcdf_bounds_dim,
                                                  netcdf_lat_dim),
                                  missval  = fill_value_float,
                                  longname = "Latitude Bounds",
                                  prec     = "single")    
  
  netcdf_ensemble = ncvar_def(nam      = "ensemble_member",
                              units    = "",
                              dim      = list(netcdf_enschar_dim,
                                              netcdf_ens_dim),
                              longname = "Ensemble Member",
                              prec     = "char")
  
  print("Miscelaneous Coordinates Created")
  
  #
  # new variables
  #
  
  netcdf_climdex_su =  ncvar_def(nam      = "climdex_su",
                                 units    = "days",
                                 dim      = list(netcdf_lon_dim,
                                                 netcdf_lat_dim,
                                                 netcdf_time_dim,
                                                 netcdf_ens_dim),
                                 missval  = fill_value_short,
                                 longname = "SU Number of Summer Days",
                                 prec     = "short")
  
  
  netcdf_climdex_ic =  ncvar_def(nam      = "climdex_id",
                                 units    = "days",
                                 dim      = list(netcdf_lon_dim,
                                                 netcdf_lat_dim,
                                                 netcdf_time_dim,
                                                 netcdf_ens_dim),
                                 missval  = fill_value_short,
                                 longname = "IC Number of Icing Days",
                                 prec     = "short")
  
  
  netcdf_climdex_fd =  ncvar_def(nam      = "climdex_fd",
                                 units    = "days",
                                 dim      = list(netcdf_lon_dim,
                                                 netcdf_lat_dim,
                                                 netcdf_time_dim,
                                                 netcdf_ens_dim),
                                 missval  = fill_value_short,
                                 longname = "FD Number of Frost Days",
                                 prec     = "short")
  
  netcdf_climdex_tr =  ncvar_def(nam      = "climdex_tr",
                                 units    = "days",
                                 dim      = list(netcdf_lon_dim,
                                                 netcdf_lat_dim,
                                                 netcdf_time_dim,
                                                 netcdf_ens_dim),
                                 missval  = fill_value_short,
                                 longname = "TR Number of Tropical Nights",
                                 prec     = "short")
  
  netcdf_climdex_sdii =  ncvar_def(nam      = "climdex_sdii",
                                   units    = "",
                                   dim      = list(netcdf_lon_dim,
                                                   netcdf_lat_dim,
                                                   netcdf_time_dim,
                                                   netcdf_ens_dim),
                                   missval  = fill_value_float,
                                   longname = "SDII Simple Precpitation Intensity Index",
                                   prec     = "float")
  
  netcdf_climdex_r10mm =  ncvar_def(nam      = "climdex_r10mm",
                                    units    = "days",
                                    dim      = list(netcdf_lon_dim,
                                                    netcdf_lat_dim,
                                                    netcdf_time_dim,
                                                    netcdf_ens_dim),
                                    missval  = fill_value_short,
                                    longname = "R10mm Precipitation Exceeding 10mm Per Day",
                                    prec     = "short")
  
  
  netcdf_climdex_r20mm =  ncvar_def(nam      = "climdex_r20mm",
                                    units    = "days",
                                    dim      = list(netcdf_lon_dim,
                                                    netcdf_lat_dim,
                                                    netcdf_time_dim,
                                                    netcdf_ens_dim),
                                    missval  = fill_value_short,
                                    longname = "R20mm Precipitation Exceeding 20mm Per Day",
                                    prec     = "short")
  
  netcdf_climdex_r03mm =  ncvar_def(nam      = "climdex_r03mm",
                                    units    = "days",
                                    dim      = list(netcdf_lon_dim,
                                                    netcdf_lat_dim,
                                                    netcdf_time_dim,
                                                    netcdf_ens_dim),
                                    missval  = fill_value_short,
                                    longname = "R03mm Precipitation Exceeding 03mm Per Day",
                                    prec     = "short")
  
  netcdf_climdex_r13mm =  ncvar_def(nam      = "climdex_r13mm",
                                    units    = "days",
                                    dim      = list(netcdf_lon_dim,
                                                    netcdf_lat_dim,
                                                    netcdf_time_dim,
                                                    netcdf_ens_dim),
                                    missval  = fill_value_short,
                                    longname = "R13mm Precipitation Exceeding 13mm Per Day",
                                    prec     = "short")   
  
  
  
  netcdf_climdex_r95ptot =  ncvar_def(nam      = "climdex_r95ptot",
                                      units    = "days",
                                      dim      = list(netcdf_lon_dim,
                                                      netcdf_lat_dim,
                                                      netcdf_time_dim,
                                                      netcdf_ens_dim),
                                      missval  = fill_value_short,
                                      longname = "Total Daily Precipitation Exceeding 95%ile Threshold",
                                      prec     = "short")   
  
  
  
  netcdf_climdex_r99ptot =  ncvar_def(nam      = "climdex_r99ptot",
                                      units    = "days",
                                      dim      = list(netcdf_lon_dim,
                                                      netcdf_lat_dim,
                                                      netcdf_time_dim,
                                                      netcdf_ens_dim),
                                      missval  = fill_value_short,
                                      longname = "Total Daily Precipitation Exceeding 99%ile Threshold",
                                      prec     = "short")   
  
  
  netcdf_climdex_prcptot =  ncvar_def(nam      = "climdex_prcptot",
                                      units    = "kg m-2",
                                      dim      = list(netcdf_lon_dim,
                                                      netcdf_lat_dim,
                                                      netcdf_time_dim,
                                                      netcdf_ens_dim),
                                      missval  = fill_value_float,
                                      longname = "Total Sum of Daily Non-Zero Precipitation",
                                      prec     = "float")   
  
  
  netcdf_climdex_cdd =  ncvar_def(nam      = "climdex_cdd",
                                  units    = "days",
                                  dim      = list(netcdf_lon_dim,
                                                  netcdf_lat_dim,
                                                  netcdf_time_dim,
                                                  netcdf_ens_dim),
                                  missval  = fill_value_short,
                                  longname = "Maximum length of dry spell",
                                  prec     = "short")   
  
  
  
  netcdf_climdex_csdi =  ncvar_def(nam      = "climdex_csdi",
                                   units    = "days",
                                   dim      = list(netcdf_lon_dim,
                                                   netcdf_lat_dim,
                                                   netcdf_time_dim,
                                                   netcdf_ens_dim),
                                   missval  = fill_value_short,
                                   longname = "Cold Spell Duration Index",
                                   prec     = "short")    
  
  netcdf_climdex_wsdi =  ncvar_def(nam      = "climdex_wsdi",
                                   units    = "days",
                                   dim      = list(netcdf_lon_dim,
                                                   netcdf_lat_dim,
                                                   netcdf_time_dim,
                                                   netcdf_ens_dim),
                                   missval  = fill_value_short,
                                   longname = "Warm Spell Duration Index",
                                   prec     = "short")    
  
  netcdf_climdex_gsl =  ncvar_def(nam      = "climdex_gsl",
                                  units    = "days",
                                  dim      = list(netcdf_lon_dim,
                                                  netcdf_lat_dim,
                                                  netcdf_time_dim,
                                                  netcdf_ens_dim),
                                  missval  = fill_value_short,
                                  longname = "Growing Season Length",
                                  prec     = "short")    
  
  netcdf_climdex_txx_a =  ncvar_def(nam      = "climdex_txx_a",
                                    units    = "degC",
                                    dim      = list(netcdf_lon_dim,
                                                    netcdf_lat_dim,
                                                    netcdf_time_dim,
                                                    netcdf_ens_dim),
                                    missval  = fill_value_float,
                                    longname = "Annual maximum value of daily maximum temperature",
                                    prec     = "float")     
  
  netcdf_climdex_txn_a =  ncvar_def(nam      = "climdex_txn_a",
                                    units    = "degC",
                                    dim      = list(netcdf_lon_dim,
                                                    netcdf_lat_dim,
                                                    netcdf_time_dim,
                                                    netcdf_ens_dim),
                                    missval  = fill_value_float,
                                    longname = "Annual minimum value of daily maximum temperature",
                                    prec     = "float")          
  
  netcdf_climdex_tnx_a =  ncvar_def(nam      = "climdex_tnx_a",
                                    units    = "degC",
                                    dim      = list(netcdf_lon_dim,
                                                    netcdf_lat_dim,
                                                    netcdf_time_dim,
                                                    netcdf_ens_dim),
                                    missval  = fill_value_float,
                                    longname = "Annual maximum value of daily minimum temperature",
                                    prec     = "float")     
  
  netcdf_climdex_tnn_a =  ncvar_def(nam      = "climdex_tnn_a",
                                    units    = "degC",
                                    dim      = list(netcdf_lon_dim,
                                                    netcdf_lat_dim,
                                                    netcdf_time_dim,
                                                    netcdf_ens_dim),
                                    missval  = fill_value_float,
                                    longname = "Annual minimum value of daily minimum temperature",
                                    prec     = "float")          
  
  netcdf_climdex_dtr_a =  ncvar_def(nam      = "climdex_dtr_a",
                                    units    = "degC",
                                    dim      = list(netcdf_lon_dim,
                                                    netcdf_lat_dim,
                                                    netcdf_time_dim,
                                                    netcdf_ens_dim),
                                    missval  = fill_value_float,
                                    longname = "Annual Mean Diurnal Temperature Range",
                                    prec     = "float")      
  
  netcdf_climdex_rx1day_a =  ncvar_def(nam      = "climdex_rx1day_a",
                                       units    = "kg m-2",
                                       dim      = list(netcdf_lon_dim,
                                                       netcdf_lat_dim,
                                                       netcdf_time_dim,
                                                       netcdf_ens_dim),
                                       missval  = fill_value_float,
                                       longname = "Monthly maximum 1-day precipitation",
                                       prec     = "float")   
  
  netcdf_climdex_rx5day_a =  ncvar_def(nam      = "climdex_rx5day_a",
                                       units    = "kg m-2",
                                       dim      = list(netcdf_lon_dim,
                                                       netcdf_lat_dim,
                                                       netcdf_time_dim,
                                                       netcdf_ens_dim),
                                       missval  = fill_value_float,
                                       longname = "Monthly maximum 5-day precipitation",
                                       prec     = "float")      
  

  netcdf_seas_avg_interv_btn_003mm_a =  ncvar_def(nam = "avg_interv_btn_003mm_a",
                                                  units    = "days",
                                                  dim      = list(netcdf_lon_dim,
                                                                  netcdf_lat_dim,
                                                                  netcdf_time_dim,
                                                                  netcdf_ens_dim),
                                                  missval  = fill_value_short,
                                                  longname = "Mean Wetting Rain Return Period (prec>0.1\")",
                                                  prec     = "short")      
  
  netcdf_seas_med_interv_btn_003mm_a =  ncvar_def(nam = "median_interv_btn_003mm_a",
                                                  units    = "days",
                                                  dim      = list(netcdf_lon_dim,
                                                                  netcdf_lat_dim,
                                                                  netcdf_time_dim,
                                                                  netcdf_ens_dim),
                                                  missval  = fill_value_short,
                                                  longname = "Median Wetting Rain Return Period (prec>0.1\")",
                                                  prec     = "short")      
  
  netcdf_seas_max_interv_btn_003mm_a =  ncvar_def(nam = "max_interv_btn_003mm_a",
                                                  units    = "days",
                                                  dim      = list(netcdf_lon_dim,
                                                                  netcdf_lat_dim,
                                                                  netcdf_time_dim,
                                                                  netcdf_ens_dim),
                                                  missval  = fill_value_short,
                                                  longname = "Max Wetting Rain Return Period (prec>0.1\")",
                                                  prec     = "short")           
  
  
  netcdf_seas_avg_interv_btn_013mm_a =  ncvar_def(nam = "avg_interv_btn_013mm_a",
                                                  units    = "days",
                                                  dim      = list(netcdf_lon_dim,
                                                                  netcdf_lat_dim,
                                                                  netcdf_time_dim,
                                                                  netcdf_ens_dim),
                                                  missval  = fill_value_float,
                                                  longname = "Mean Wetting Rain Return Period (prec>0.5\")",
                                                  prec     = "float")      
  
  netcdf_seas_med_interv_btn_013mm_a =  ncvar_def(nam = "median_interv_btn_013mm_a",
                                                  units    = "days",
                                                  dim      = list(netcdf_lon_dim,
                                                                  netcdf_lat_dim,
                                                                  netcdf_time_dim,
                                                                  netcdf_ens_dim),
                                                  missval  = fill_value_short,
                                                  longname = "Median Wetting Rain Return Period (prec>0.5\")",
                                                  prec     = "short")      
  
  netcdf_seas_max_interv_btn_013mm_a =  ncvar_def(nam = "max_interv_btn_013mm_a",
                                                  units    = "days",
                                                  dim      = list(netcdf_lon_dim,
                                                                  netcdf_lat_dim,
                                                                  netcdf_time_dim,
                                                                  netcdf_ens_dim),
                                                  missval  = fill_value_short,
                                                  longname = "Max Wetting Rain Return Period (prec>0.5\")",
                                                  prec     = "short")           
  
  
  print("Variable Coordinates Created")
  
  
  ##############
  #
  # Drop File
  #    
  ##############
  
  print("creating file")
  
  nc_ghcn = nc_create(filename = netcdf_output_file_name,
                      vars     = list(netcdf_time_bounds,
                                      netcdf_lon_bounds,
                                      netcdf_lat_bounds,
                                      netcdf_ensemble,
                                      netcdf_climdex_su,
                                      netcdf_climdex_ic,
                                      netcdf_climdex_fd,
                                      netcdf_climdex_tr,
                                      netcdf_climdex_sdii,
                                      netcdf_climdex_r10mm,
                                      netcdf_climdex_r20mm,
                                      netcdf_climdex_r03mm,
                                      netcdf_climdex_r13mm,
                                      netcdf_climdex_r95ptot,
                                      netcdf_climdex_r99ptot,
                                      netcdf_climdex_prcptot,
                                      netcdf_climdex_cdd,
                                      netcdf_climdex_csdi,
                                      netcdf_climdex_wsdi,
                                      netcdf_climdex_gsl,
                                      netcdf_climdex_txx_a,
                                      netcdf_climdex_txn_a,
                                      netcdf_climdex_tnx_a,
                                      netcdf_climdex_tnn_a,
                                      netcdf_climdex_dtr_a,
                                      netcdf_climdex_rx1day_a,
                                      netcdf_climdex_rx5day_a,
                                      netcdf_seas_avg_interv_btn_003mm_a,
                                      netcdf_seas_med_interv_btn_003mm_a,
                                      netcdf_seas_max_interv_btn_003mm_a,
                                      netcdf_seas_avg_interv_btn_013mm_a,
                                      netcdf_seas_med_interv_btn_013mm_a,
                                      netcdf_seas_max_interv_btn_013mm_a                        ),
                      force_v4 = FALSE,
                      verbose  = FALSE )
  
  
  ##############
  #
  # Coordinate Attributes
  #    
  ##############   
  
  #
  # Time
  #
  
  ncatt_put(nc         = nc_ghcn,
            varid      = "time",
            attname    = "standard_name",
            attval     = "time",
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE )
  
  
  
  ncatt_put(nc         = nc_ghcn,
            varid      = "time",
            attname    = "long_name",
            attval     = "Year Inside Climate Period",
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE )
  
  ncatt_put(nc         = nc_ghcn,
            varid      = "time",
            attname    = "bounds",
            attval     = "time_bnds",
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE )   
  
  ncatt_put(nc         = nc_ghcn,
            varid      = "time",
            attname    = "calendar",
            attval     = "365-day",
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE )
  
  #
  # Time Bounds
  #
  
  ncatt_put(nc         = nc_ghcn,
            varid      = "time_bnds",
            attname    = "standard_name",
            attval     = "time",
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE )
  
  ncatt_put(nc         = nc_ghcn,
            varid      = "time_bnds",
            attname    = "long_name",
            attval     = "Period Bounds",
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE )
  
  ncatt_put(nc         = nc_ghcn,
            varid      = "time",
            attname    = "bounds",
            attval     = "time_bnds",
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE )       
  
  #
  # Lontitude
  #
  
  ncatt_put(nc         = nc_ghcn,
            varid      = "lon",
            attname    = "standard_name",
            attval     = "longitude",
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE )
  
  ncatt_put(nc         = nc_ghcn,
            varid      = "lon",
            attname    = "axis",
            attval     = "X",
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE )
  
  ncatt_put(nc         = nc_ghcn,
            varid      = "lon",
            attname    = "long_name",
            attval     = "Longtidue",
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE )      
  
  ncatt_put(nc         = nc_ghcn,
            varid      = "lon",
            attname    = "bounds",
            attval     = "lon_bnds",
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE )      
  #
  # Latitude
  #
  
  ncatt_put(nc         = nc_ghcn,
            varid      = "lat",
            attname    = "standard_name",
            attval     = "latitude",
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE )
  
  ncatt_put(nc         = nc_ghcn,
            varid      = "lat",
            attname    = "axis",
            attval     = "Y",
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE )
  
  ncatt_put(nc         = nc_ghcn,
            varid      = "lat",
            attname    = "long_name",
            attval     = "Latitude",
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE )
  
  ncatt_put(nc         = nc_ghcn,
            varid      = "lat",
            attname    = "bounds",
            attval     = "lat_bnds",
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE )           
  
  #
  # climdex.su ; SU, Number of summer days
  #
  
  ncatt_put(nc         = nc_ghcn,
            varid      = netcdf_climdex_su,
            attname    = "description",
            attval     = "Number of Summer Days",
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE )
  
  ncatt_put(nc         = nc_ghcn,
            varid      = netcdf_climdex_su,
            attname    = "long_name",
            attval     = "Number of Summer Days",
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE )
  
  ncatt_put(nc         = nc_ghcn,
            varid      = netcdf_climdex_su,
            attname    = "comment",
            attval     = "SU, Number of Summer Days: Annual count of days when TX (daily maximum temperature) > 25°C",
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE )          
  
  #
  # climdex.id ; ID, Number of ucing days
  #
  
  ncatt_put(nc         = nc_ghcn,
            varid      = netcdf_climdex_ic,
            attname    = "description",
            attval     = "Number of Summer Days",
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE )
  
  ncatt_put(nc         = nc_ghcn,
            varid      = netcdf_climdex_ic,
            attname    = "long_name",
            attval     = "Number of Icing Days",
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE )
  
  ncatt_put(nc         = nc_ghcn,
            varid      = netcdf_climdex_ic,
            attname    = "comment",
            attval     = "ID, Number of icing days: Annual count of days when TX (daily maximum temperature) < 0°C",
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE )             
  
  #
  # climdex.fd ; 
  #
  
  ncatt_put(nc         = nc_ghcn,
            varid      = netcdf_climdex_fd,
            attname    = "description",
            attval     = "Number of Frost Days",
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE )
  
  ncatt_put(nc         = nc_ghcn,
            varid      = netcdf_climdex_fd,
            attname    = "long_name",
            attval     = "Number of Frost Days",
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE )
  
  ncatt_put(nc         = nc_ghcn,
            varid      = netcdf_climdex_fd,
            attname    = "comment",
            attval     = "Number of frost days: Annual count of days when TN (daily minimum temperature) < 0°C",
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE )             
  
  #
  # climdex.tr ; TR
  #
  
  ncatt_put(nc         = nc_ghcn,
            varid      = netcdf_climdex_tr,
            attname    = "description",
            attval     = "Number of Tropical Nights",
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE )
  
  ncatt_put(nc         = nc_ghcn,
            varid      = netcdf_climdex_tr,
            attname    = "long_name",
            attval     = "Number of Tropical Nights",
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE )
  
  ncatt_put(nc         = nc_ghcn,
            varid      = netcdf_climdex_tr,
            attname    = "comment",
            attval     = "TR, Number of tropical nights: Annual count of days when TN (daily minimum temperature) > 20°C",
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE )             
  
  #
  # climdex.sdii ; 
  #
  
  ncatt_put(nc         = nc_ghcn,
            varid      = netcdf_climdex_tr,
            attname    = "description",
            attval     = "Simple Precpitation Intensity Index",
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE )
  
  ncatt_put(nc         = nc_ghcn,
            varid      = netcdf_climdex_tr,
            attname    = "long_name",
            attval     = "Simple Precpitation Intensity Index",
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE )
  
  ncatt_put(nc         = nc_ghcn,
            varid      = netcdf_climdex_tr,
            attname    = "comment",
            attval     = "SDII Simple pricipitation intensity index",
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE )             
  
  #
  # climdex.r10mm ; 
  #
  
  ncatt_put(nc         = nc_ghcn,
            varid      = netcdf_climdex_r10mm,
            attname    = "description",
            attval     = "Precipitation Exceeding 10mm Per Day",
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE )
  
  ncatt_put(nc         = nc_ghcn,
            varid      = netcdf_climdex_r10mm,
            attname    = "long_name",
            attval     = "Precipitation Exceeding 10mm Per Day",
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE )
  
  ncatt_put(nc         = nc_ghcn,
            varid      = netcdf_climdex_r10mm,
            attname    = "comment",
            attval     = "R10mm, Annual count of days when PRCP≥ 10mm",
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE )                    
  
  #
  # climdex.r20mm ; 
  #
  
  ncatt_put(nc         = nc_ghcn,
            varid      = netcdf_climdex_r20mm,
            attname    = "description",
            attval     = "Precipitation Exceeding 20mm Per Day",
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE )
  
  ncatt_put(nc         = nc_ghcn,
            varid      = netcdf_climdex_r20mm,
            attname    = "long_name",
            attval     = "Precipitation Exceeding 20mm Per Day",
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE )
  
  ncatt_put(nc         = nc_ghcn,
            varid      = netcdf_climdex_r20mm,
            attname    = "comment",
            attval     = "R20mm Annual count of days when PRCP≥ 20mm",
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE )                    
  
  #
  # climdex.r03mm ; 
  #
  
  ncatt_put(nc         = nc_ghcn,
            varid      = netcdf_climdex_r03mm,
            attname    = "description",
            attval     = "Precipitation Exceeding 0.1 in Per Day",
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE )
  
  ncatt_put(nc         = nc_ghcn,
            varid      = netcdf_climdex_r03mm,
            attname    = "long_name",
            attval     = "Precipitation Exceeding 0.1 in Per Day",
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE )
  
  ncatt_put(nc         = nc_ghcn,
            varid      = netcdf_climdex_r03mm,
            attname    = "comment",
            attval     = "Rnnmm Annual count of days when PRCP≥ nnmm, nn is a user defined threshold",
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE )                    
  
  #
  # climdex.r13mm ; 
  #
  
  ncatt_put(nc         = nc_ghcn,
            varid      = netcdf_climdex_r13mm,
            attname    = "description",
            attval     = "Precipitation Exceeding 0.5 in Per Day",
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE )
  
  ncatt_put(nc         = nc_ghcn,
            varid      = netcdf_climdex_r13mm,
            attname    = "long_name",
            attval     = "Precipitation Exceeding 0.5 in Per Day",
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE )
  
  ncatt_put(nc         = nc_ghcn,
            varid      = netcdf_climdex_r13mm,
            attname    = "comment",
            attval     = "Rnnmm Annual count of days when PRCP≥ nnmm, nn is a user defined threshold",
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE )                    
  
  #
  # climdex.r95ptot ; 
  #
  
  ncatt_put(nc         = nc_ghcn,
            varid      = netcdf_climdex_r95ptot,
            attname    = "description",
            attval     = "Total Daily Precipitation Exceeding 95%ile Threshold",
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE )
  
  ncatt_put(nc         = nc_ghcn,
            varid      = netcdf_climdex_r95ptot,
            attname    = "long_name",
            attval     = "Total Daily Precipitation Exceeding 95%ile Threshold",
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE )
  
  ncatt_put(nc         = nc_ghcn,
            varid      = netcdf_climdex_r95ptot,
            attname    = "comment",
            attval     = "R95pTOT. Annual total PRCP when RR > 95p",
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE )                    
  
  #
  # climdex.r99ptot ; 
  #
  
  ncatt_put(nc         = nc_ghcn,
            varid      = netcdf_climdex_r99ptot,
            attname    = "description",
            attval     = "Total Daily Precipitation Exceeding 99%ile Threshold",
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE )
  
  ncatt_put(nc         = nc_ghcn,
            varid      = netcdf_climdex_r99ptot,
            attname    = "long_name",
            attval     = "Total Daily Precipitation Exceeding 99%ile Threshold",
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE )
  
  ncatt_put(nc         = nc_ghcn,
            varid      = netcdf_climdex_r99ptot,
            attname    = "comment",
            attval     = "R99pTOT. Annual total PRCP when RR > 99p",
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE )                    
  
  #
  # climdex.prcptot ; 
  #
  
  ncatt_put(nc         = nc_ghcn,
            varid      = netcdf_climdex_prcptot,
            attname    = "description",
            attval     = "Annual total precipitation for wet days",
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE )
  
  ncatt_put(nc         = nc_ghcn,
            varid      = netcdf_climdex_prcptot,
            attname    = "long_name",
            attval     = "Annual total precipitation for wet days",
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE )
  
  ncatt_put(nc         = nc_ghcn,
            varid      = netcdf_climdex_prcptot,
            attname    = "comment",
            attval     = "PRCPTOT. Annual total precipitation in wet days",
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE )                    
  
  #
  # climdex.cdd ; 
  #
  
  ncatt_put(nc         = nc_ghcn,
            varid      = netcdf_climdex_cdd,
            attname    = "description",
            attval     = "Maximum Length of Dry Spell",
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE )
  
  ncatt_put(nc         = nc_ghcn,
            varid      = netcdf_climdex_cdd,
            attname    = "long_name",
            attval     = "Maximum Length of Dry Spell",
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE )
  
  ncatt_put(nc         = nc_ghcn,
            varid      = netcdf_climdex_cdd,
            attname    = "comment",
            attval     = "CDD. Maximum length of dry spell, maximum number of consecutive days with RR < 1mm",
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE )     
  
  #
  # climdex.csdi ; 
  #
  
  ncatt_put(nc         = nc_ghcn,
            varid      = netcdf_climdex_csdi,
            attname    = "description",
            attval     = "Cold Spell Duration Index",
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE )
  
  ncatt_put(nc         = nc_ghcn,
            varid      = netcdf_climdex_csdi,
            attname    = "long_name",
            attval     = "Cold Spell Duration Index",
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE )
  
  ncatt_put(nc         = nc_ghcn,
            varid      = netcdf_climdex_csdi,
            attname    = "comment",
            attval     = "CSDI, Cold speel duration index: Annual count of days with at least 6 consecutive days when TN < 10th percentile",
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE )            
  
  #
  # climdex.wsdi ; 
  #
  
  ncatt_put(nc         = nc_ghcn,
            varid      = netcdf_climdex_wsdi,
            attname    = "description",
            attval     = "Warm Spell Duration Index",
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE )
  
  ncatt_put(nc         = nc_ghcn,
            varid      = netcdf_climdex_wsdi,
            attname    = "long_name",
            attval     = "Warm Spell Duration Index",
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE )
  
  ncatt_put(nc         = nc_ghcn,
            varid      = netcdf_climdex_wsdi,
            attname    = "comment",
            attval     = "Warm speel duration index: Annual count of days with at least 6 consecutive days when TX > 90th percentile",
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE )            
  
  
  #
  # climdex.gsl ; 
  #
  
  ncatt_put(nc         = nc_ghcn,
            varid      = netcdf_climdex_gsl,
            attname    = "description",
            attval     = "Growing Season Length",
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE )
  
  ncatt_put(nc         = nc_ghcn,
            varid      = netcdf_climdex_gsl,
            attname    = "long_name",
            attval     = "Growing Season Length",
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE )
  
  ncatt_put(nc         = nc_ghcn,
            varid      = netcdf_climdex_gsl,
            attname    = "comment",
            attval     = "GSL, Growing season length: Annual (1st Jan to 31st Dec in Northern Hemisphere (NH), 1st July to 30th June in Southern Hemisphere (SH)) count between first span of at least 6 days with daily mean temperature TG>5oC and first span after July 1st (Jan 1st in SH) of 6 days with TG<5°C.",
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE )            
  
  #
  # climdex.txx.a ; 
  #
  
  ncatt_put(nc         = nc_ghcn,
            varid      = netcdf_climdex_txx_a,
            attname    = "description",
            attval     = "Annual Max Daily Max Temperature",
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE )
  
  ncatt_put(nc         = nc_ghcn,
            varid      = netcdf_climdex_txx_a,
            attname    = "long_name",
            attval     = "Annual Max Daily Max Temperature",
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE )
  
  ncatt_put(nc         = nc_ghcn,
            varid      = netcdf_climdex_txx_a,
            attname    = "comment",
            attval     = "TXxA, Annual maximum value of daily maximum temperature",
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE )              
  
  #
  # climdex.txn.a ; 
  #
  
  ncatt_put(nc         = nc_ghcn,
            varid      = netcdf_climdex_txn_a,
            attname    = "description",
            attval     = "Annual Min Daily Max Temperature",
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE )
  
  ncatt_put(nc         = nc_ghcn,
            varid      = netcdf_climdex_txn_a,
            attname    = "long_name",
            attval     = "Annual Min Daily Max Temperature",
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE )
  
  ncatt_put(nc         = nc_ghcn,
            varid      = netcdf_climdex_txn_a,
            attname    = "comment",
            attval     = "TXnA, Annual minimum value of daily maximum temperature",
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE )   
  
  #
  # climdex.tnx.a ; 
  #
  
  ncatt_put(nc         = nc_ghcn,
            varid      = netcdf_climdex_tnx_a,
            attname    = "description",
            attval     = "Annual Max Daily Min Temperature",
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE )
  
  ncatt_put(nc         = nc_ghcn,
            varid      = netcdf_climdex_tnx_a,
            attname    = "long_name",
            attval     = "Annual Max Daily Min Temperature",
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE )
  
  ncatt_put(nc         = nc_ghcn,
            varid      = netcdf_climdex_tnx_a,
            attname    = "comment",
            attval     = "TNxA, Annual maximim value of daily minimum temperature",
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE )     
  
  #
  # climdex.tnn.a ; 
  #
  
  ncatt_put(nc         = nc_ghcn,
            varid      = netcdf_climdex_tnn_a,
            attname    = "description",
            attval     = "Annual Min Daily Min Temperature",
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE )
  
  ncatt_put(nc         = nc_ghcn,
            varid      = netcdf_climdex_tnn_a,
            attname    = "long_name",
            attval     = "Annual Min Daily Min Temperature",
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE )
  
  ncatt_put(nc         = nc_ghcn,
            varid      = netcdf_climdex_tnn_a,
            attname    = "comment",
            attval     = "TNnA, Annual minimum value of daily minimum temperature",
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE )     
  
  #
  # climdex.dtr.a ; 
  #
  
  ncatt_put(nc         = nc_ghcn,
            varid      = netcdf_climdex_dtr_a,
            attname    = "description",
            attval     = "Annual Mean Diurnal Temperature Range",
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE )
  
  ncatt_put(nc         = nc_ghcn,
            varid      = netcdf_climdex_dtr_a,
            attname    = "long_name",
            attval     = "Annual Mean Diurnal Temperature Range",
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE )
  
  ncatt_put(nc         = nc_ghcn,
            varid      = netcdf_climdex_dtr_a,
            attname    = "comment",
            attval     = "DTRA, Daily temperature range: Monthly mean difference between TX and TN",
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE )          
  
  #
  # climdex.rx1day.a ; 
  #
  
  ncatt_put(nc         = nc_ghcn,
            varid      = netcdf_climdex_rx1day_a,
            attname    = "description",
            attval     = "Annual Maximum 1-day Precipitation",
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE )
  
  ncatt_put(nc         = nc_ghcn,
            varid      = netcdf_climdex_rx1day_a,
            attname    = "long_name",
            attval     = "Annual Maximum 1-day Precipitation",
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE )
  
  ncatt_put(nc         = nc_ghcn,
            varid      = netcdf_climdex_rx1day_a,
            attname    = "comment",
            attval     = "Rx1dayA, Annual maximum 1-day precipitation",
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE )    
  #
  # climdex.rx5day.a ; 
  #
  
  ncatt_put(nc         = nc_ghcn,
            varid      = netcdf_climdex_rx5day_a,
            attname    = "description",
            attval     = "Annual Maximum 5-day Precipitation",
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE )
  
  ncatt_put(nc         = nc_ghcn,
            varid      = netcdf_climdex_rx5day_a,
            attname    = "long_name",
            attval     = "Annual Maximum 5-day Precipitation",
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE )
  
  ncatt_put(nc         = nc_ghcn,
            varid      = netcdf_climdex_rx5day_a,
            attname    = "comment",
            attval     = "Rx1dayA, Annual maximum 5-day precipitation",
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE )    
  
  #
  # avg_interv_btn_003mm_a; 
  #
  
  ncatt_put(nc         = nc_ghcn,
            varid      = netcdf_seas_avg_interv_btn_003mm_a,
            attname    = "description",
            attval     = "Mean Wetting Rain Return Period (prec>0.1\")",
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE )
  
  ncatt_put(nc         = nc_ghcn,
            varid      = netcdf_seas_avg_interv_btn_003mm_a,
            attname    = "long_name",
            attval     = "Mean Wetting Rain Return Period (prec>0.1\")",
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE )
  
  #
  # avg_interv_btn_013mm_a; 
  #
  
  ncatt_put(nc         = nc_ghcn,
            varid      = netcdf_seas_avg_interv_btn_013mm_a,
            attname    = "description",
            attval     = "Mean Wetting Rain Return Period (prec>0.5\")",
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE )
  
  ncatt_put(nc         = nc_ghcn,
            varid      = netcdf_seas_avg_interv_btn_013mm_a,
            attname    = "long_name",
            attval     = "Mean Wetting Rain Return Period (prec>0.5\")",
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE )
  
  #
  # med_interv_btn_003mm_a; 
  #
  
  ncatt_put(nc         = nc_ghcn,
            varid      = netcdf_seas_med_interv_btn_003mm_a,
            attname    = "description",
            attval     = "Median Wetting Rain Return Period (prec>0.1\")",
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE )
  
  ncatt_put(nc         = nc_ghcn,
            varid      = netcdf_seas_med_interv_btn_003mm_a,
            attname    = "long_name",
            attval     = "Median Wetting Rain Return Period (prec>0.1\")",
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE )   
  
  #
  # med_interv_btn_013mm_a; 
  #
  
  ncatt_put(nc         = nc_ghcn,
            varid      = netcdf_seas_med_interv_btn_013mm_a,
            attname    = "description",
            attval     = "Median Wetting Rain Return Period (prec>0.5\")",
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE )
  
  ncatt_put(nc         = nc_ghcn,
            varid      = netcdf_seas_med_interv_btn_013mm_a,
            attname    = "long_name",
            attval     = "Median Wetting Rain Return Period (prec>0.5\")",
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE )      
  
  
  
  #
  # max_interv_btn_003mm_a; 
  #
  
  ncatt_put(nc         = nc_ghcn,
            varid      = netcdf_seas_max_interv_btn_003mm_a,
            attname    = "description",
            attval     = "Maximum Wetting Rain Return Period (prec>0.1\")",
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE )
  
  ncatt_put(nc         = nc_ghcn,
            varid      = netcdf_seas_max_interv_btn_003mm_a,
            attname    = "long_name",
            attval     = "Maximum Wetting Rain Return Period (prec>0.1\")",
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE )   
  
  #
  # max_interv_btn_013mm_a; 
  #
  
  ncatt_put(nc         = nc_ghcn,
            varid      = netcdf_seas_max_interv_btn_013mm_a,
            attname    = "description",
            attval     = "Maximum Wetting Rain Return Period (prec>0.5\")",
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE )
  
  ncatt_put(nc         = nc_ghcn,
            varid      = netcdf_seas_max_interv_btn_013mm_a,
            attname    = "long_name",
            attval     = "Maximum Wetting Rain Return Period (prec>0.5\")",
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE )       
  
  ##############
  #
  # Drop Variables
  #    
  ##############
  
  
  ncvar_put(nc      = nc_ghcn,
            varid   = netcdf_time_bounds,
            vals    = time_bounds,
            verbose = FALSE )
  
  ncvar_put(nc      = nc_ghcn,
            varid   = netcdf_lon_bounds,
            vals    = lon_bounds,
            verbose = FALSE )
  
  ncvar_put(nc      = nc_ghcn,
            varid   = netcdf_lat_bounds,
            vals    = lat_bounds,
            verbose = FALSE )
  
  ncvar_put(nc      = nc_ghcn,
            varid   = netcdf_ensemble,
            vals    = ensemble_site,
            verbose = FALSE )
  
  ncvar_put(nc      = nc_ghcn,
            varid   = netcdf_climdex_su,
            vals    = climdex.su,
            verbose = FALSE )
  
  ncvar_put(nc      = nc_ghcn,
            varid   = netcdf_climdex_ic,
            vals    = climdex.id,
            verbose = FALSE )
  
  ncvar_put(nc      = nc_ghcn,
            varid   = netcdf_climdex_fd,
            vals    = climdex.fd,
            verbose = FALSE )
  
  ncvar_put(nc      = nc_ghcn,
            varid   = netcdf_climdex_tr,
            vals    = climdex.tr,
            verbose = FALSE )          
  
  ncvar_put(nc      = nc_ghcn,
            varid   = netcdf_climdex_sdii,
            vals    = climdex.sdii,
            verbose = FALSE )    
  
  ncvar_put(nc      = nc_ghcn,
            varid   = netcdf_climdex_r10mm,
            vals    = climdex.r10mm,
            verbose = FALSE )     
  
  ncvar_put(nc      = nc_ghcn,
            varid   = netcdf_climdex_r20mm,
            vals    = climdex.r20mm,
            verbose = FALSE )           
  
  ncvar_put(nc      = nc_ghcn,
            varid   = netcdf_climdex_r03mm,
            vals    = climdex.r03mm,
            verbose = FALSE )           
  
  ncvar_put(nc      = nc_ghcn,
            varid   = netcdf_climdex_r13mm,
            vals    = climdex.r13mm,
            verbose = FALSE )           
  
  ncvar_put(nc      = nc_ghcn,
            varid   = netcdf_climdex_r95ptot,
            vals    = climdex.r95ptot,
            verbose = FALSE )           
  
  ncvar_put(nc      = nc_ghcn,
            varid   = netcdf_climdex_r99ptot,
            vals    = climdex.r99ptot,
            verbose = FALSE )           
  
  ncvar_put(nc      = nc_ghcn,
            varid   = netcdf_climdex_prcptot,
            vals    = climdex.prcptot,
            verbose = FALSE )           
  
  ncvar_put(nc      = nc_ghcn,
            varid   = netcdf_climdex_cdd,
            vals    = climdex.cdd,
            verbose = FALSE ) 
  
  ncvar_put(nc      = nc_ghcn,
            varid   = netcdf_climdex_wsdi,
            vals    = climdex.wsdi,
            verbose = FALSE )       
  
  ncvar_put(nc      = nc_ghcn,
            varid   = netcdf_climdex_csdi,
            vals    = climdex.csdi,
            verbose = FALSE )     
  
  ncvar_put(nc      = nc_ghcn,
            varid   = netcdf_climdex_gsl,
            vals    = climdex.gsl,
            verbose = FALSE )   
  
  ncvar_put(nc      = nc_ghcn,
            varid   = netcdf_climdex_txx_a,
            vals    = climdex.txx.a,
            verbose = FALSE )  
  
  ncvar_put(nc      = nc_ghcn,
            varid   = netcdf_climdex_txn_a,
            vals    = climdex.txn.a,
            verbose = FALSE )  
  
  ncvar_put(nc      = nc_ghcn,
            varid   = netcdf_climdex_tnx_a,
            vals    = climdex.tnx.a,
            verbose = FALSE )  
  
  ncvar_put(nc      = nc_ghcn,
            varid   = netcdf_climdex_tnn_a,
            vals    = climdex.tnn.a,
            verbose = FALSE )  
  
  ncvar_put(nc      = nc_ghcn,
            varid   = netcdf_climdex_dtr_a,
            vals    = climdex.dtr.a,
            verbose = FALSE )  
  
  ncvar_put(nc      = nc_ghcn,
            varid   = netcdf_climdex_rx1day_a,
            vals    = climdex.rx1day.a,
            verbose = FALSE )  
  
  ncvar_put(nc      = nc_ghcn,
            varid   = netcdf_climdex_rx5day_a,
            vals    = climdex.rx5day.a,
            verbose = FALSE )  
  
  ncvar_put(nc      = nc_ghcn,
            varid   = netcdf_seas_avg_interv_btn_003mm_a,
            vals    = seas.interarrival.r03mm_mean.a,
            verbose = FALSE )  
  
  ncvar_put(nc      = nc_ghcn,
            varid   = netcdf_seas_avg_interv_btn_013mm_a,
            vals    = seas.interarrival.r13mm_mean.a,
            verbose = FALSE )  
  
  ncvar_put(nc      = nc_ghcn,
            varid   = netcdf_seas_max_interv_btn_003mm_a,
            vals    = seas.interarrival.r03mm_max.a,
            verbose = FALSE )  
  
  ncvar_put(nc      = nc_ghcn,
            varid   = netcdf_seas_max_interv_btn_013mm_a,
            vals    = seas.interarrival.r13mm_max.a,
            verbose = FALSE )  
  
  ncvar_put(nc      = nc_ghcn,
            varid   = netcdf_seas_med_interv_btn_003mm_a,
            vals    = seas.interarrival.r03mm_median.a,
            verbose = FALSE )  
  
  ncvar_put(nc      = nc_ghcn,
            varid   = netcdf_seas_med_interv_btn_013mm_a,
            vals    = seas.interarrival.r13mm_median.a,
            verbose = FALSE )      
  
  
  nc_close(nc_ghcn)
  