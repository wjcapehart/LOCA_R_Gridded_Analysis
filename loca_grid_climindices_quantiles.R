
library("ncdf4")
library("lubridate") # processing dates and time
library("PCICt")
library("climdex.pcic")



################################################################
#
# Years
#
################################################################

PDF_on                  = FALSE
ROW_print_diagnostics   = TRUE
OPENDAP_on              = TRUE
nprocs                  = 8


  period_span = 30

  start_year  = c(1976, 2020)
  start_year  = c(1961)
  period = 1
  end_year    = start_year + period_span - 1



  point_target_i = 51
  point_target_j =  28




  percentiles = seq(from = 0.00,
                    to   = 1.00,
                    by   = 0.01)

  n_percentiles = length(percentiles)


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



file_prefix = "SODAK_LOCA_"



if (PDF_on) {
  pdf(paste("/projects/ECEP/LOCA_MACA_Ensembles/LOCA/LOCA_R_Gridded_Analysis/",
            "etccdi_cci_percentiles_",
           "SODAK_LOCA_",
           "historical",
           "_",
           "tasmin_tasmax_pr",
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

   variable = "tasmin"
   scenario = "historical"
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


  nc_close(nc.file)

  remove(tustr,
         tunits,
         nc.file,
         variable,
         variable_name)
  
   
  ################################################################
  #
  # Create Empty Arrays
  #
  ################################################################

  period = 1

  arr_xyep =  array(data    = NA,
                    dim      = c(nx,
                                 ny,
                                 n_ensembles,
                                 n_percentiles),
                    dimnames = list(longitude,
                                    latitude,
                                    ensemble_site,
                                    percentiles)
                    )
                
  doy = 1:365
#  arr_xyepd =  array(data    = NA,
#                    dim      = c(nx,
#                                 ny,
#                                 n_ensembles,
#                                 n_percentiles,
#                                 365),
#                    dimnames = list(longitude,
#                                    latitude,
#                                    ensemble_site,
#                                    percentiles,
#                                    doy)
#  )
  
#  tmax_outbase_quantiles = arr_xyepd
#  tmin_outbase_quantiles = arr_xyepd
#  prec_outbase_quantiles = arr_xyepd
  
################################################################
#
# R hates large datasets :-( which means that we must do any
# long-game gridded analysis on a grid-cell by grid-cell basis.
#
################################################################

    pr_xyep       = arr_xyep
    pr0_xyep      = arr_xyep
    tasmin_xyep   = arr_xyep
    tasmax_xyep   = arr_xyep

    t_start = min(which(time_year == start_year[period]))
    t_end   = max(which(time_year == end_year[period]))
    t_count = t_end - t_start + 1


    for (ens in 1:n_ensembles) {
      
      variable = "tasmin"
        scenario = "historical"
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
        
        thredds_URL      = paste(thredds_URL_front,
                                 ensemble_site[ens],
                                 thredds_URL_tail,
                                 sep = "_")
        
        variable_name_tasmin = paste(variable,
                                    ensemble_site[ens],
                                    scenario,
                                    sep = "_")
        
        nc.file_tasmin = nc_open(filename = thredds_URL)
      
        
      variable = "tasmax"
        scenario = "historical"
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
        
        thredds_URL      = paste(thredds_URL_front,
                                 ensemble_site[ens],
                                 thredds_URL_tail,
                                 sep = "_")
        
        variable_name_tasmax = paste(variable,
                                 ensemble_site[ens],
                                 scenario,
                                 sep = "_")
        
        nc.file_tasmax = nc_open(filename = thredds_URL)
        
        
      variable = "pr"
        scenario = "historical"
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
        
        thredds_URL      = paste(thredds_URL_front,
                                 ensemble_site[ens],
                                 thredds_URL_tail,
                                 sep = "_")
        
        variable_name_pr    = paste(variable,
                                    ensemble_site[ens],
                                    scenario,
                                    sep = "_")
        
        nc.file_pr = nc_open(filename = thredds_URL)
        
        
         
        


      for (i in 1:nx) {

        time_series.pr = ncvar_get(nc           = nc.file_pr,
                                   varid        = variable_name_pr,
                                   start        = c(  i,  1, t_start),
                                   count        = c(  1, ny, t_count),
                                   raw_datavals = FALSE,
                                   verbose      = FALSE)

        
        time_series.tasmax = ncvar_get(nc           = nc.file_tasmax,
                                       varid        = variable_name_tasmax,
                                       start        = c(  i,  1, t_start),
                                       count        = c(  1, ny, t_count),
                                       raw_datavals = FALSE,
                                       verbose      = FALSE)
 
        time_series.tasmin = ncvar_get(nc           = nc.file_tasmin,
                                       varid        = variable_name_tasmin,
                                       start        = c(  i,  1, t_start),
                                       count        = c(  1, ny, t_count),
                                       raw_datavals = FALSE,
                                       verbose      = FALSE)   
        
        time_for_period = time[t_start:t_end]
        
        time_for_period_PCICt = as.PCICt(x   = as.character(x = time_for_period), 
                                         cal = "gregorian")
        
        for (j in 1:ny)  {


          time_series.pr_ij     = time_series.pr[j,]
          time_series.tasmax_ij = time_series.tasmax[j,]
          time_series.tasmin_ij = time_series.tasmin[j,]
          
          
          temp = get.outofbase.quantiles(tmax        = time_series.tasmax_ij,
                                         tmin        = time_series.tasmin_ij,
                                         prec        = time_series.pr_ij,
                                         tmax.dates  = time_for_period_PCICt, 
                                         tmin.dates  = time_for_period_PCICt, 
                                         prec.dates  = time_for_period_PCICt,
                                         base.range  = c(start_year, end_year), 
                                         n = 5, 
                                         temp.qtiles = percentiles,
                                         prec.qtiles = percentiles, 
                                         min.base.data.fraction.present = 0.1)
          
   #       print(temp)
          
         # tmin_outbase_quantiles[i,j,ens,,] = temp$tmax$outbase
        #  tmax_outbase_quantiles[i,j,ens,,] = temp$tmin$outbase 
        #  prec_outbase_quantiles[i,j,ens,,] = temp$prec$outbase 
        
          tasmax_xyep[i,j,ens,] = quantile(x     = time_series.tasmax_ij, 
                                           probs = percentiles) 

          tasmin_xyep[i,j,ens,] = quantile(x     = time_series.tasmin_ij, 
                                           probs = percentiles) 
          
          pr_xyep[i,j,ens,]     = quantile(x     = time_series.pr_ij[time_series.pr_ij > 0.254], 
                                           probs = percentiles) 
          pr0_xyep[i,j,ens,]     = quantile(x     = time_series.pr_ij, 
                                           probs = percentiles) 
        } #end j loop


        if (ROW_print_diagnostics) {
          print(paste(i,10,
                      ensemble_site[ens],
                      tasmax_xyep[i,10,ens,19],
                      tasmin_xyep[i,10,ens,3],
                      pr_xyep[i,10,ens,16]))
        }

     } #end i looop

      plot(x    = time_period,
           y    = time_series.tasmax_ij,
           type = "l",
           main = paste("LLC ",
                        ensemble_site[ens],
                        variable_name_tasmax,
                        " (degC)",
                        sep = "")
            )

      
      plot(x    = time_period,
           y    = time_series.tasmin_ij,
           type = "l",
           main = paste("LLC ",
                        ensemble_site[ens],
                        variable_name_tasmin,
                        " (degC)",
                        sep = "")
           )
      
      plot(x    = time_period,
           y    = time_series.pr_ij,
           type = "h",
           main = paste("LLC ",
                        ensemble_site[ens],
                        variable_name_pr,
                        " (mm)",
                        sep = "")
           )
      
      

   nc_close(nc.file_pr)
   nc_close(nc.file_tasmax)
   nc_close(nc.file_tasmin)
   
 

    } #end ensemble looop


  save(time_period,
       start_year,
       end_year,
       doy,
       latitude,
       longitude,
       ensemble_site,
       percentiles,
       pr_xyep,
       pr0_xyep,
       tasmin_xyep,
       tasmax_xyep,
       file =  paste("/projects/ECEP/LOCA_MACA_Ensembles/LOCA/LOCA_R_Gridded_Analysis/",
                     "etccdi_cci_percentiles_",
                     file_prefix,
                     scenario,
                     "_",
                     "tasmin_tasmax_pr",
                     "_",
                     start_year,
                     "_to_",
                     end_year,
                     ".RData",
                     sep="")
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
  
  
  
  netcdf_output_file_name = paste("/projects/ECEP/LOCA_MACA_Ensembles/LOCA/LOCA_R_Gridded_Analysis/",
                                  "etccdi_cci_percentiles_",
                                  "SODAK_LOCA_",
                                  "historical",
                                  "_",
                                  "tasmin_tasmax_pr",
                                  "_",
                                  start_year,
                                  "_to_",
                                  end_year,
                                  ".nc",
                                  sep="")
  
  
  netcdf_lon_dim     = ncdim_def(name  = "lon",
                                 units = "degrees_east",
                                 val   = longitude,
                                 unlim = FALSE)
  
  netcdf_lat_dim     = ncdim_def(name  = "lat",
                                 units = "degrees_north",
                                 val   = latitude,
                                 unlim = FALSE)
  
  netcdf_ptile_dim   = ncdim_def(name          = "percentiles",
                                 units         = "",
                                 val           = percentiles,
                                 unlim         = FALSE)
  
  
  netcdf_enschar_dim  = ncdim_def(name          = "ensemble_member_characters",
                                  units         = "",
                                  val           =  1:max(nchar(ensemble_site)),
                                  create_dimvar = FALSE)
  
  
  netcdf_ens_dim     = ncdim_def(name          = "ensemble_member",
                                 units         = "",
                                 val           = 1:length(ensemble_site),
                                 create_dimvar = FALSE)
  
  fill_value = 9.96921e+36
  
  netcdf_ensemble = ncvar_def(nam      = "ensemble_member",
                              units    = "",
                              dim      = list(netcdf_enschar_dim,
                                              netcdf_ens_dim),
                              longname = "Ensemble Member",
                              prec     = "char")
  
  netcdf_tasmax_xyep = ncvar_def(nam      = "tasmax_percentiles",
                                 units    = "degC",
                                 dim      = list(netcdf_lon_dim,
                                                 netcdf_lat_dim,
                                                 netcdf_ens_dim,
                                                 netcdf_ptile_dim),
                                 missval  = fill_value,
                                 longname = "Maximium Daily Temperature Percentiles",
                                 prec     = "single")
  
  netcdf_tasmin_xyep = ncvar_def(nam      = "tasmin_percentiles",
                                 units    = "degC",
                                 dim      = list(netcdf_lon_dim,
                                                 netcdf_lat_dim,
                                                 netcdf_ens_dim,
                                                 netcdf_ptile_dim),
                                 missval  = fill_value,
                                 longname = "Minimium Daily Temperature Percentiles",
                                 prec     = "single")
  
  netcdf_pr0_xyep = ncvar_def(nam      = "pr_percentiles",
                             units    = "kg m-2",
                             dim      = list(netcdf_lon_dim,
                                             netcdf_lat_dim,
                                             netcdf_ens_dim,
                                             netcdf_ptile_dim),
                             missval  = fill_value,
                             longname = "Daily Precipitation Percentiles",
                             prec     = "single")
  
  netcdf_pr_xyep = ncvar_def(nam      = "pr_nonzero_percentiles",
                             units    = "kg m-2",
                             dim      = list(netcdf_lon_dim,
                                             netcdf_lat_dim,
                                             netcdf_ens_dim,
                                             netcdf_ptile_dim),
                             missval  = fill_value,
                             longname = "Daily Non-Zero-Day Precipitation Percentiles",
                             prec     = "single")  
  
  ##############
  nc_ghcn = nc_create(filename = netcdf_output_file_name,
                      vars     = list(netcdf_ensemble,
                                      netcdf_tasmax_xyep,
                                      netcdf_tasmin_xyep,
                                      netcdf_pr_xyep,
                                      netcdf_pr0_xyep),
                      force_v4 = FALSE,
                      verbose  = FALSE )
  
  
  ncvar_put(nc      = nc_ghcn,
            varid   = netcdf_ensemble,
            vals    = ensemble_site,
            verbose = FALSE )
  
  ncvar_put(nc      = nc_ghcn,
            varid   = netcdf_tasmin_xyep,
            vals    = tasmin_xyep,
            verbose = FALSE )
  
  ncvar_put(nc      = nc_ghcn,
            varid   = netcdf_tasmax_xyep,
            vals    = tasmax_xyep,
            verbose = FALSE )

  ncvar_put(nc      = nc_ghcn,
            varid   = netcdf_pr0_xyep,
            vals    = pr0_xyep,
            verbose = FALSE )
    
  ncvar_put(nc      = nc_ghcn,
            varid   = netcdf_pr_xyep,
            vals    = pr_xyep,
            verbose = FALSE )
  
  nc_close(nc_ghcn)
  
  if (PDF_on) {
    dev.off()
  }
