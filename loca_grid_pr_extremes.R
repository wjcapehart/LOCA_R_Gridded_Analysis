
path = "/Users/"

library("ncdf4")
library("lubridate") # processing dates and time
library("maps")
library("RColorBrewer")
library("colorRamps")
library("grDevices")
library("extRemes")


################################################################
#
# Years
#
################################################################

PDF_on                  = TRUE
ROW_print_diagnostics   = TRUE
POINT_print_diagnostics = FALSE
OPENDAP_on              = FALSE
nprocs                  = 8


  period_span = 30

  start_year  = c(1976, 2020)
  start_year  = c(2020)
  period = 1
  end_year    = start_year + period_span - 1



  point_target_i = 51
  point_target_j = 28




  return_intervals = seq(from =             2,
                         to   = period_span/2,
                         by   =             1)

  n_return_intervals = length(return_intervals)


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

variable = "pr"

variable_human_name = "Total Precipitation"

variable_prefered_units = "kg m-2"

yearly_aggregation_label = "Total"

standard_name = "precipitation_amount"

yearly_aggregation_method = "sum"

scenario = "rcp45"

file_prefix = "SODAK_LOCA_"

thredds_URL_front = paste(thredds_URL_prefix,
                          "/",
                          variable,
                          "/",
                          scenario,
                          "/",
                          file_prefix,
                          variable,
                          sep = "")

thredds_URL_tail = paste(scenario,
                         ".nc",
                         sep = "")



if (PDF_on) {
  pdf(paste("./return_statistics_",
            file_prefix,
            scenario,
            "_",
            variable,
            "_",
            start_year,
            "_to_",
            end_year,
            ".pdf",
            sep=""))
}

################################################################
#
# Loading Ensemble Information
#
################################################################

   ensemble_site  =  c("ACCESS1-0_r1i1p1",
                       "ACCESS1-3_r1i1p1",
                       "CCSM4_r6i1p1",
                       "CESM1-BGC_r1i1p1",
                       "CESM1-CAM5_r1i1p1",
                       "CMCC-CMS_r1i1p1",
                       "CMCC-CM_r1i1p1",
                       "CNRM-CM5_r1i1p1",
                       "CSIRO-Mk3-6-0_r1i1p1",
                       "CanESM2_r1i1p1",
                       "FGOALS-g2_r1i1p1",
                       "GFDL-CM3_r1i1p1",
                       "GFDL-ESM2G_r1i1p1",
                       "GFDL-ESM2M_r1i1p1",
                       "HadGEM2-AO_r1i1p1",
                       "HadGEM2-CC_r1i1p1",
                       "HadGEM2-ES_r1i1p1",
                       "IPSL-CM5A-LR_r1i1p1",
                       "IPSL-CM5A-MR_r1i1p1",
                       "MIROC-ESM-CHEM_r1i1p1",
                       "MIROC-ESM_r1i1p1",
                       "MIROC5_r1i1p1",
                       "MPI-ESM-LR_r1i1p1",
                       "MPI-ESM-MR_r1i1p1",
                       "MRI-CGCM3_r1i1p1",
                       "NorESM1-M_r1i1p1",
                       "bcc-csm1-1-m_r1i1p1")

   n_ensembles = length(ensemble_site)

   ensemble_site_char_length = max(nchar(ensemble_site))



################################################################
#
# Extracting Mapping Information
#
################################################################

   ens = 2


   thredds_URL      = paste(thredds_URL_front,
                            ensemble_site[ens],
                            thredds_URL_tail,
                            sep = "_")

   variable_name    = paste(variable,
                            ensemble_site[ens],
                            scenario,
                            sep = "_")

   nc.file = nc_open(filename = thredds_URL)

   latitude = ncvar_get(nc      = nc.file,    # netcdf file ID
                        varid   = "lat",      # variable name from file
                        verbose = FALSE)      # print diagnostic data

   longitude = ncvar_get(nc      = nc.file,    # netcdf file ID
                         varid   = "lon",      # variable name from file
                         verbose = FALSE)      # print diagnostic data

   longitude[longitude>180] = longitude[longitude>180] - 360

   nx = length(longitude)
   ny = length(latitude)



################################################################
#
# Extracting Time Information
#
################################################################


  # get time values

  # historical time values

  time      = ncvar_get(nc    = nc.file,
                        varid = "time")

  tunits    = ncatt_get(nc      = nc.file,
                        varid   = "time",
                        attname = "units")

  tustr     = strsplit(tunits$value, " ")

  time_reference_point_hist = paste(unlist(tustr)[3],
                                    unlist(tustr)[4])


  time = as.Date(x      = time,
                 origin = time_reference_point_hist,
                dts     = "UTC")



  time_year  = year(time)
  time_month = month(time)

  nt = length(time)

  period_start = time[min(which(time_year == start_year[period]))]
  period_end   = time[max(which(time_year ==   end_year[period]))]+1

  time_period = time[min(which(time_year == start_year[period])):
                     max(which(time_year ==   end_year[period]))]

  #  bounds and netcdf time coordinates

  climate_period_bounds = matrix(data     = c(period_start,period_end),
                                 nrow     = 2,
                                 ncol     = 1)


  climate_year_bounds   = matrix(data     = NA,
                                 nrow     = 2,
                                 ncol     = period_span)

  years = start_year[period] : end_year[period]


  climate_period = c(time[min(which(time_year == years[1]))])

  climate_year   = array(data = NA,
                         dim  = c(period_span))

  for (y in 1:period_span)  {
    climate_year_bounds[1, y] = time[min(which(time_year == years[y]))]
    climate_year_bounds[2, y] = time[max(which(time_year == years[y]))]+1
    climate_year[y]           = climate_year_bounds[1, y]
  }

  ################################################################
  #
  # Create Empty Arrays
  #
  ################################################################

  period = 1

  arr_xyer =  array(data    = NA,
                    dim      = c(nx,
                                 ny,
                                 n_ensembles,
                                 n_return_intervals),
                    dimnames = list(longitude,
                                    latitude,
                                    ensemble_site,
                                    return_intervals
                    )
  )

  arr_xye  =  array(data    = NA,
                    dim      = c(nx,
                                 ny,
                                 n_ensembles),
                    dimnames = list(longitude,
                                    latitude,
                                    ensemble_site
                    )
  )

  arr_xyde =  array(data    = NA,
                    dim      = c(nx,
                                 ny,
                                 period_span,
                                 n_ensembles),
                    dimnames = list(longitude,
                                    latitude,
                                    start_year[period]:end_year[period],
                                    ensemble_site
                    )
  )


################################################################
#
# Test Plot of Temperature for One year
#
################################################################

  # make an empty array






  sumprec_xye = arr_xye

  period = 1

  t_start = min(which(time_year == start_year[period]))
  t_end   = max(which(time_year == start_year[period]))




  t_count = t_end - t_start + 1

  arr_xyt =  array(data     = NA,
                   dim      = c(nx, ny, t_count),
                   dimnames = list(longitude,
                                   latitude,
                                   time[t_start:t_end]))
  arr_xyd  =  array(data     = NA,
                    dim      = c(nx, ny, period_span),
                    dimnames = list(longitude,
                                   latitude,
                                   start_year[period]:end_year[period]))


  for (ens in 1:n_ensembles)  {

      thredds_URL      = paste(thredds_URL_front,
                               ensemble_site[ens],
                               thredds_URL_tail,
                               sep = "_")

      variable_name    = paste(variable,
                               ensemble_site[ens],
                               scenario,
                               sep = "_")

      nc.file = nc_open(filename = thredds_URL)


      arr_xyt[,,] = ncvar_get(nc           = nc.file,
                              varid        = variable_name,
                              start        = c(  1,  1, t_start),
                              count        = c( nx, ny, t_count),
                              raw_datavals = FALSE,
                              verbose      = FALSE)

      units    = ncatt_get(nc      = nc.file,
                           varid   = variable_name,
                           attname = "units")

      nc_close(nc.file)

      plot(x    = time[t_start:t_end],
           y    = arr_xyt[point_target_i,
                          point_target_i,
                          ],
           type = "h",
           main = paste(variable_name,
                        " (",
                        units$value,
                        ")",
                        sep = ""),
           xlab  = time_year[t_start],
           ylab  = paste(variable_human_name,
                         " (",
                         units$value,
                         ")",
                         sep = "")
           )


      print( paste(variable_name,
                   " (",
                   units$value,
                   ") ",
                   min(arr_xyt,na.rm=TRUE),
                   ":",
                   max(arr_xyt,na.rm=TRUE),
                   sep = "")
             )


      if (units$value ==  "kg m-2 s-1") {
        arr_xyt = arr_xyt * 86400.
      }

      if (units$value ==  "K") {
        arr_xyt = arr_xyt - 273.15
      }

      sumprec_xye[,,ens]    = apply(arr_xyt, c(1,2), sum)




      nlevels = 9
      filled.contour(x         = longitude,
                     y         = latitude,
                     z         = sumprec_xye[,,ens],
                     main      = paste(start_year[period],
                                       " ",
                                       ensemble_site[ens],
                                       " ",
                                       variable_human_name,
                                       " (",
                                       variable_prefered_units,
                                       ")",
                                       sep = ""),
                     asp       = 1,
                     color = function(x)rev(topo.colors(x)),
                     plot.axes = { map(database = "county",
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

  meanprec_xy    = apply(sumprec_xye, c(1,2), mean)
  stdvprec_xy    = apply(sumprec_xye, c(1,2), sd)

  nlevels = 9
  filled.contour(x         = longitude,
                 y         = latitude,
                 z         = meanprec_xy,
                 main      = paste(start_year[period],
                                   " ",
                                   "Ensemble Mean ",
                                   variable_human_name,
                                   " (",
                                   variable_prefered_units,
                                   ")",
                                   sep = ""),
                 asp       = 1,
                 color = function(x)rev(topo.colors(x)),
                 plot.axes = { map(database = "county",
                                     add      = TRUE,
                                     col      = "black",
                                     lwd      = 1);
                                 map(database = "state",
                                     add      = TRUE,
                                     col      = "black",
                                     lwd      = 2)
                             }
                 )

  nlevels = 9
  filled.contour(x         = longitude,
                 y         = latitude,
                 z         = stdvprec_xy,
                 main      = paste(start_year[period],
                                   " ",
                                   "Ensemble StDev ",
                                   variable_human_name,
                                   " (",
                                   variable_prefered_units,
                                   ")",
                                   sep = ""),
                 asp       = 1,
                 color = function(x)rev(topo.colors(x)),
                 plot.axes = { map(database = "county",
                                     add      = TRUE,
                                     col      = "black",
                                     lwd      = 1);
                                 map(database = "state",
                                     add      = TRUE,
                                     col      = "black",
                                     lwd      = 2)
                             }
                 )



################################################################
#
# R hates large datasets :-( which means that we must do any
# long-game gridded analysis on a grid-cell by grid-cell basis.
#
################################################################


    arr_xyer =  array(data    = NA,
                     dim      = c(nx, ny, n_ensembles, n_return_intervals),
                     dimnames = list(longitude,
                                     latitude,
                                     ensemble_site,
                                     return_intervals
                                     )
                     )

    arr_xye  =  array(data    = NA,
                      dim      = c(nx,
                                   ny,
                                   n_ensembles),
                      dimnames = list(longitude,
                                      latitude,
                                      ensemble_site
                                      )
                      )

    arr_xyde =  array(data    = NA,
                      dim      = c(nx,
                                   ny,
                                   period_span,
                                   n_ensembles),
                      dimnames = list(longitude,
                                      latitude,
                                      start_year[period]:end_year[period],
                                      ensemble_site
                                      )
                      )



    mean_xye    = arr_xye
    stdv_xye    = arr_xye
    mean_xyde   = arr_xyde
    return_xyer = arr_xyer

    t_start = min(which(time_year == start_year[period]))
    t_end   = max(which(time_year == end_year[period]))
    t_count = t_end - t_start + 1


    for (ens in 1:n_ensembles) {

      variable_name = paste(variable,
                            ensemble_site[ens],
                            scenario,
                            sep = "_")

      thredds_URL   = paste(thredds_URL_front,
                            ensemble_site[ens],
                            thredds_URL_tail,
                            sep = "_")

      variable_name  = paste(variable,
                             ensemble_site[ens],
                             scenario,
                             sep = "_")

      nc.file = nc_open(filename = thredds_URL)

      units    = ncatt_get(nc      = nc.file,
                           varid   = variable_name,
                           attname = "units")



      for (i in 1:nx) {

        time_series = ncvar_get(nc           = nc.file,
                                varid        = variable_name,
                                start        = c(  i,  1, t_start),
                                count        = c(  1, ny, t_count),
                                raw_datavals = FALSE,
                                verbose      = FALSE)


        if (units$value ==  "K") {
          time_series = time_series - 273.15
        }
        if (units$value ==  "kg m-2 s-1") {
          time_series = time_series * 86400.
        }

        for (j in 1:ny)  {

          mean_xye[i,j,ens]  = mean(x = time_series[j,])

          d = 0
          for (my_year in start_year[period]:end_year[period])  {

            d = 1 + d


            time_for_j    = time_series[j,]

            my_timeseries =  time_for_j[which(time_year == my_year)]

            my_time       =        time[which(time_year == my_year)]

            mean_xyde[i,j,d,ens] = mean(x = my_timeseries)
          }

          stdv_xye[i,j,ens] = sd(x = mean_xyde[i,j, ,ens])

          time_for_j    = time_series[j,]


          threshold_value = quantile(x     = time_for_j[time_for_j > 0.254],
                                     probs = 0.80,
                                     names = FALSE
                                     )

          fit_GP  = fevd(x          = time_series[j,],
                         verbose    = FALSE,
                         threshold  = threshold_value,
                         units      = "mm",
                         time.units = "365/year",
                         type       = "GP",
                         span       = period_span
                         )

          return_GP = return.level(x             = fit_GP,
                                   return.period = return_intervals,
                                   do.ci         = FALSE)

          return_xyer[i,j,ens, ] = return_GP

          if (POINT_print_diagnostics) {
            print(paste(i,j,
                        ensemble_site[ens],
                        threshold_value,return_xyer[i,j,ens,9]))
          }

        } #end j loop


        if (ROW_print_diagnostics) {
          print(paste(i,10,
                      ensemble_site[ens],
                      threshold_value,return_xyer[i,j,ens,9]))
        }




     } #end i looop

      plot(x    = time_period,
           y    = time_series[ny,],
           type = "h",
           main = paste("LLC ",
                        ensemble_site[ens],
                        variable_human_name,
                        " (",
                        variable_prefered_units,
                        ")",
                        sep = "")
            )




      plot(fit_GP, type = "qq")
      plot(fit_GP, type = "density")
      plot(fit_GP, type = "qq2")
      plot(fit_GP, type = "rl")

      nc_close(nc.file)

      nlevels = 9
      filled.contour(x         = longitude,
                     y         = latitude,
                     z         = mean_xye[,,ens],
                     main      = paste(start_year[period],
                                       "-",
                                       end_year[period],
                                       " ",
                                       ensemble_site[ens],
                                       " Mean Annual Total Rainfall (mm)",
                                       sep = ""),
                     asp       = 1,
                     color = function(x)rev(topo.colors(x)),
                     plot.axes = { map(database = "county",
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

    med_return_xyr    = apply(return_xyer, c(1,2,4), median)

    sd_return_xyr    = apply(return_xyer, c(1,2,4), sd)

  save(time,
       start_year,
       end_year,
       latitude,
       longitude,
       ensemble_site,
       return_intervals,
       sumprec_xye,
       meanprec_xy,
       mean_xye,
       mean_xyde,
       stdv_xye,
       return_xyer,
       stdvprec_xy,
       med_return_xyr,
       sd_return_xyr,
       file =  paste("./return_statistics_",
                     file_prefix,
                     scenario,
                     "_",
                     variable,
                     "_",
                     start_year,
                     "_to_",
                     end_year,
                     ".RData",
                     sep="")
       )



  ###############################################


  ens     = 5
  ret     = 2
  nlevels = 9
  filled.contour(x         = longitude,
                 y         = latitude,
                 z         = med_return_xyr[ , , ret],
                 main      = paste(start_year[period],
                                   " ",
                                   "Ensemble Median ",
                                   return_intervals[ret],
                                   "-y Return Period ",
                                   " Daily Total Rainfall (mm)",
                                   sep = ""),
                 asp       = 1,
                 color = function(x)rev(topo.colors(x)),
                 plot.axes = { map(database = "county",
                                   add      = TRUE,
                                   col      = "black",
                                   lwd      = 1);
                   map(database = "state",
                       add      = TRUE,
                       col      = "black",
                       lwd      = 2)
                 }
  )



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


  netcdf_output_file_name = paste("./return_statistics_",
                                  file_prefix,
                                  scenario,
                                  "_",
                                  variable,
                                  "_",
                                  start_year,
                                  "_to_",
                                  end_year,
                                  ".nc",
                                  sep="")


  netcdf_year_dim  = ncdim_def(name    = "year",
                               units   = "days since 1970-01-01 00:00:00",
                               val     = climate_year,
                               unlim   = FALSE,
                               calendar="standard")

  netcdf_period_dim  = ncdim_def(name  = "climate_long_period",
                               units = "days since 1970-01-01 00:00:00",
                               val   = start_year,
                               unlim = FALSE,
                               calendar="standard")

  netcdf_bounds_dim  = ncdim_def(name  = "bnds",
                                 units = "",
                                 val   = 1:2,
                                 unlim = FALSE,
                                 create_dimvar = FALSE)

  netcdf_lon_dim     = ncdim_def(name  = "lon",
                                 units = "degrees_east",
                                 val   = longitude,
                                 unlim = FALSE)

  netcdf_lat_dim     = ncdim_def(name  = "lat",
                                 units = "degrees_north",
                                 val   = latitude,
                                 unlim = FALSE)

  netcdf_retper_dim  = ncdim_def(name          = "extreme_event_return_interval",
                                 units         = "years",
                                 val           = return_intervals,
                                 create_dimvar = TRUE)

  netcdf_enschar_dim  = ncdim_def(name          = "ensemble_member_characters",
                                 units         = "",
                                 val           =  1:ensemble_site_char_length,
                                 create_dimvar = FALSE)

  netcdf_ens_dim     = ncdim_def(name          = "ensemble_member",
                                 units         = "",
                                 val           = 1:n_ensembles,
                                 create_dimvar = FALSE)






  fill_value = 9.96921e+36

  netcdf_yearly_bounds = ncvar_def(nam      = "year_bnds",
                                   units    = "days since 1970-01-01 00:00:00",
                                   dim      = list(netcdf_bounds_dim,
                                                   netcdf_year_dim),
                                   missval  = fill_value,
                                   longname = "Climate Year Bounds",
                                   prec     = "single")

  netcdf_climate_bounds = ncvar_def(nam      = "climate_long_period_bnds",
                                 units    = "days since 1970-01-01 00:00:00",
                                 dim      = list(netcdf_bounds_dim,
                                                 netcdf_period_dim),
                                 missval  = fill_value,
                                 longname = "Extended Period Bounds",
                                 prec     = "single")

  netcdf_ensemble = ncvar_def(nam      = "ensemble_member",
                              units    = "",
                              dim      = list(netcdf_enschar_dim,
                                              netcdf_ens_dim),
                              longname = "Ensemble Member",
                              prec     = "char")

  # mean_xye
  netcdf_mean_xye = ncvar_def(nam      = "mean_period_total_annual_precip",
                              units    = "kg m-2",
                              dim      = list(netcdf_lon_dim,
                                              netcdf_lat_dim,
                                              netcdf_ens_dim),
                              missval  = fill_value,
                              longname = "Mean Total Annual Precip for Period",
                              prec     = "single")

  #stdv_xye
  netcdf_stdv_xye = ncvar_def(nam      = "stdev_period_total_annual_precip",
                              units    = "kg m-2",
                              dim      = list(netcdf_lon_dim,
                                              netcdf_lat_dim,
                                              netcdf_ens_dim),
                              missval  = fill_value,
                              longname = "Std Dev Total Annual Precip for Period",
                              prec     = "single")


  # mean_xyde
  netcdf_mean_xyde = ncvar_def(nam      = "total_annual_precip",
                              units    = "kg m-2",
                              dim      = list(netcdf_lon_dim,
                                              netcdf_lat_dim,
                                              netcdf_year_dim,
                                              netcdf_ens_dim),
                              missval  = fill_value,
                              longname = "Total Annual Precip",
                              prec     = "single")

  #   return_xyer = arr_xyer
  netcdf_return_xyer = ncvar_def(nam      = "daily_precip_return",
                                 units    = "kg m-2",
                                 dim      = list(netcdf_lon_dim,
                                                 netcdf_lat_dim,
                                                 netcdf_ens_dim,
                                                 netcdf_retper_dim),
                                 missval  = fill_value,
                                 longname = "Daily Maximum Precipitation Returns",
                                 prec     = "single")





  ##############
  nc_ghcn = nc_create(filename = netcdf_output_file_name,
                      vars     = list(netcdf_climate_bounds,
                                      netcdf_yearly_bounds,
                                      netcdf_mean_xye,
                                      netcdf_stdv_xye,
                                      netcdf_mean_xyde,
                                      netcdf_return_xyer,
                                      netcdf_ensemble),
                      force_v4 = FALSE,
                      verbose  = FALSE )
  ##############

  ncatt_put(nc         = nc_ghcn,
            varid      = "year",
            attname    = "standard_name",
            attval     = "time",
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE )

  ncatt_put(nc         = nc_ghcn,
            varid      = "year",
            attname    = "long_name",
            attval     = "Year Inside Climate Period",
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE )

  ncatt_put(nc         = nc_ghcn,
            varid      = "year",
            attname    = "bounds",
            attval     = "climate_year_bnds",
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE )


  ncatt_put(nc         = nc_ghcn,
            varid      = "climate_long_period",
            attname    = "standard_name",
            attval     = "time",
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE )


  ncatt_put(nc         = nc_ghcn,
            varid      = "climate_long_period",
            attname    = "bounds",
            attval     = "climate_long_period_bnds",
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE )

  ncatt_put(nc         = nc_ghcn,
            varid      = "climate_long_period",
            attname    = "long_name",
            attval     = "First Year of Extended Climate Period",
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE )

  ncatt_put(nc         = nc_ghcn,
            varid      = "climate_long_period",
            attname    = "period_duration",
            attval     = period_span,
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE )

  ncatt_put(nc         = nc_ghcn,
            varid      = "extreme_event_return_interval",
            attname    = "long_name",
            attval     = "Return Period Interval",
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE )


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
            varid      = netcdf_yearly_bounds,
            attname    = "standard_name",
            attval     = "time",
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE )


  ncatt_put(nc         = nc_ghcn,
            varid      = netcdf_mean_xye,
            attname    = "standard_name",
            attval     = standard_name,
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE )

  ncatt_put(nc         = nc_ghcn,
            varid      = netcdf_mean_xye,
            attname    = "cell_methods",
            attval     = "period: mean",
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE )

  ncatt_put(nc         = nc_ghcn,
            varid      = netcdf_stdv_xye,
            attname    = "standard_name",
            attval     = standard_name,
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE )

  ncatt_put(nc         = nc_ghcn,
            varid      = netcdf_stdv_xye,
            attname    = "cell_methods",
            attval     = "period: stdev",
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE )

  ncatt_put(nc         = nc_ghcn,
            varid      = netcdf_mean_xyde,
            attname    = "standard_name",
            attval     = standard_name,
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE )

  ncatt_put(nc         = nc_ghcn,
            varid      = netcdf_mean_xyde,
            attname    = "cell_methods",
            attval     = paste("year: ",
                               yearly_aggregation_method,
                               seq=""),
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE )


  ncatt_put(nc         = nc_ghcn,
            varid      = netcdf_return_xyer,
            attname    = "standard_name",
            attval     = standard_name,
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE )

  ncatt_put(nc         = nc_ghcn,
            varid      = netcdf_return_xyer,
            attname    = "cell_methods",
            attval     = "year: maximum",
            prec       = NA,
            verbose    = FALSE,
            definemode = FALSE )



  ##############


  ncvar_put(nc      = nc_ghcn,
            varid   = netcdf_climate_bounds,
            vals    = climate_period_bounds,
            verbose = FALSE )

  ncvar_put(nc      = nc_ghcn,
            varid   = netcdf_yearly_bounds,
            vals    = climate_year_bounds,
            verbose = FALSE )

  ncvar_put(nc      = nc_ghcn,
            varid   = netcdf_mean_xye,
            vals    = mean_xye,
            verbose = FALSE )

  ncvar_put(nc      = nc_ghcn,
            varid   = netcdf_stdv_xye,
            vals    = stdv_xye,
            verbose = FALSE )

  ncvar_put(nc      = nc_ghcn,
            varid   = netcdf_mean_xyde,
            vals    = mean_xyde,
            verbose = FALSE )

  ncvar_put(nc      = nc_ghcn,
            varid   = netcdf_return_xyer,
            vals    = return_xyer,
            verbose = FALSE )

  ncvar_put(nc      = nc_ghcn,
            varid   = netcdf_return_xyer,
            vals    = return_xyer,
            verbose = FALSE )

  ncvar_put(nc      = nc_ghcn,
            varid   = netcdf_ensemble,
            vals    = ensemble_site,
            verbose = FALSE )


  nc_close( nc_ghcn )

  if (PDF_on) {
    dev.off()
  }
