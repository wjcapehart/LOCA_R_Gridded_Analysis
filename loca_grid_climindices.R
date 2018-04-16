
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

  file_prefix = "SODAK_LOCA_"
  
  
  
  requested_start_year  = 1950
  requested_period_span = 2005-requested_start_year+1
  requested_scenario    = "historical"

  requested_start_year  = 2006
  requested_period_span = 2099-requested_start_year+1
  requested_period_span = 2035-requested_start_year+1
  requested_scenario    = "rcp45"
  
  ################################################################
  #
  # Extracting Climatologies
  #
  ################################################################
  
  print("==========================================================================")
  print("==")
  print("== etccdi_cci_percentiles_SODAK_LOCA_historical_tasmin_tasmax_pr_1961_to_1990")
  print("==")
  

  load(file =  paste("/projects/ECEP/LOCA_MACA_Ensembles/LOCA/LOCA_R_Gridded_Analysis/",
                     "etccdi_cci_percentiles_SODAK_LOCA_historical_tasmin_tasmax_pr_1961_to_1990.RData",
                     sep=""),
       verbose = TRUE)
  print("==")
  print("==========================================================================")
  
  percentiles = seq(from = 0.00,
                    to   = 1.00,
                    by   = 0.01)
  
  n_percentiles = length(percentiles)
  
  percentile_label = paste("q",
                           sprintf("%0.2d",percentiles*100),
                           sep="")
  
  pr_xyep = array(data    = pr_xyep,
                  dim     = c(length(longitude),
                              length(latitude),
                              length(ensemble_site),
                              length(percentile_label)),
                  dimnames = list(longitude,
                                  latitude,
                                  ensemble_site,
                                  percentile_label))
  
  tasmax_xyep = array(data    = tasmax_xyep,
                      dim     = c(length(longitude),
                                  length(latitude),
                                  length(ensemble_site),
                                  length(percentile_label)),
                      dimnames = list(longitude,
                                      latitude,
                                      ensemble_site,
                                      percentile_label))
  
  tasmin_xyep = array(data    = tasmin_xyep,
                      dim     = c(length(longitude),
                                  length(latitude),
                                  length(ensemble_site),
                                  length(percentile_label)),
                      dimnames = list(longitude,
                                      latitude,
                                      ensemble_site,
                                      percentile_label))
  
  remove(time_period)
  
  ################################################################
  #
  # Extracting Mapping Information
  #
  ################################################################
  
  ################################################################
  #
  # Time Control and Climate Periods
  #
  ################################################################
  

  period_span = requested_period_span
  scenario    = requested_scenario
  start_year  = requested_start_year
  period      = 1
  end_year    = start_year + period_span - 1


  

  point_target_i = 51
  point_target_j = 28


  
  
  if (PDF_on) {
    pdf(paste("/projects/ECEP/LOCA_MACA_Ensembles/LOCA/LOCA_R_Gridded_Analysis/",
              "etccdi_cci_indicies_",
              file_prefix,
              scenario,
              "_",
              "tasmin_tasmax_pr",
              "_",
              start_year,
              "_to_",
              end_year,
              ".pdf",
              sep=""))
  }

  print(paste(start_year,
              "to",
              end_year))
  
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


   ens = 2

   variable = "tasmin"
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

  
################################################################
#
# R hates large datasets :-( which means that we must do any
# long-game gridded analysis on a grid-cell by grid-cell basis.
#
################################################################

 
    t_start = min(which(time_year == start_year[period]))
    t_end   = max(which(time_year == end_year[period]))
    t_count = t_end - t_start + 1

    time_for_period = time[t_start:t_end]
    year_in_time_period = start_year : end_year
    
    date_yymm = time[day(time)==1]     
    nYm       = length(date_yymm)
    
    time_for_period = time[t_start:t_end]
    
    time_for_period_PCICt = as.PCICt(x   = as.character(x = time_for_period), 
                                     cal = "gregorian")
    
    
    arr_xyYe =  array(data    = NA,
                      dim      = c(nx,
                                   ny,
                                   period_span,
                                   n_ensembles),
                      dimnames = list(longitude,
                                      latitude,
                                      year_in_time_period,
                                      ensemble_site) )
    
    # arr_xyMe =  array(data    = NA,
    #                   dim      = c(nx,
    #                                ny,
    #                                nYm,
    #                                n_ensembles),
    #                   dimnames = list(longitude,
    #                                   latitude,
    #                                   date_yymm,
    #                                   ensemble_site) )
    
    
    climdex.su      = arr_xyYe
    climdex.id      = arr_xyYe
    climdex.fd      = arr_xyYe
    climdex.tr      = arr_xyYe
    climdex.sdii    = arr_xyYe
    climdex.r10mm   = arr_xyYe
    climdex.r20mm   = arr_xyYe
    climdex.r03mm   = arr_xyYe
    climdex.r13mm   = arr_xyYe
    climdex.cdd     = arr_xyYe
    climdex.csdi    = arr_xyYe
    climdex.wsdi    = arr_xyYe
    climdex.r95ptot = arr_xyYe
    climdex.r99ptot = arr_xyYe
    climdex.prcptot = arr_xyYe
    climdex.gsl     = arr_xyYe 
    
    climdex.txx.a    = arr_xyYe
    climdex.txn.a    = arr_xyYe
    climdex.tnx.a    = arr_xyYe
    climdex.tnn.a    = arr_xyYe
    climdex.rx1day.a = arr_xyYe
    climdex.rx5day.a = arr_xyYe
    climdex.dtr.a    = arr_xyYe  
    
    seas.interarrival.r03mm_mean.a = arr_xyYe      
    seas.interarrival.r13mm_mean.a = arr_xyYe      
    
    seas.interarrival.r03mm_max.a = arr_xyYe              
    seas.interarrival.r13mm_max.a = arr_xyYe      
    
    seas.interarrival.r03mm_median.a = arr_xyYe      
    seas.interarrival.r13mm_median.a = arr_xyYe               
    
 
    #  climdex.tx10p.a  = arr_xyYe
    #  climdex.tx90p.a  = arr_xyYe
    #  climdex.tn10p.a  = arr_xyYe
    #  climdex.tn90p.a  = arr_xyYe
    
       
    #  climdex.txx.m    = arr_xyMe
    #  climdex.txn.m    = arr_xyMe
    #  climdex.tnx.m    = arr_xyMe
    #  climdex.tnn.m    = arr_xyMe
    #  climdex.rx1day.m = arr_xyMe
    #  climdex.rx5day.m = arr_xyMe
    #  climdex.dtr.m    = arr_xyMe          
    
    #  climdex.tx10p.m  = arr_xyMe
    #  climdex.tx90p.m  = arr_xyMe
    #  climdex.tn10p.m  = arr_xyMe
    #  climdex.tn90p.m  = arr_xyMe
    
    
    for (ens in 1:n_ensembles) {
      
      print("==========================================================================")
      print("==")
      print(paste("== ", ensemble_site[ens]))
      
      variable = "tasmin"
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

        time_series.pr = array(data    = ncvar_get(nc           = nc.file_pr,
                                                   varid        = variable_name_pr,
                                                   start        = c(  i,  1, t_start),
                                                   count        = c(  1, ny, t_count),
                                                   raw_datavals = FALSE,
                                                   verbose      = FALSE),
                               dim      = c(length(latitude),
                                            length(time_for_period_PCICt)),
                               dimnames = list(latitude,
                                               time_for_period_PCICt))
                               
        time_series.tasmax = array(data    = ncvar_get(nc           = nc.file_tasmax,
                                                       varid        = variable_name_tasmax,
                                                       start        = c(  i,  1, t_start),
                                                       count        = c(  1, ny, t_count),
                                                       raw_datavals = FALSE,
                                                       verbose      = FALSE),
                                   dim      = c(length(latitude),
                                                length(time_for_period_PCICt)),
                                   dimnames = list(latitude,
                                               time_for_period_PCICt))

        time_series.tasmin = array(data    = ncvar_get(nc           = nc.file_tasmin,
                                                       varid        = variable_name_tasmin,
                                                       start        = c(  i,  1, t_start),
                                                       count        = c(  1, ny, t_count),
                                                       raw_datavals = FALSE,
                                                       verbose      = FALSE),
                                   dim      = c(length(latitude),
                                                length(time_for_period_PCICt)),
                                   dimnames = list(latitude,
                                                   time_for_period_PCICt)) 
   
        
        
        for (j in 1:ny)  {

          ptile_tmax = tasmax_xyep[i,j,ens,]
          ptile_tmin = tasmin_xyep[i,j,ens,]
          ptile_prec = pr_xyep[    i,j,ens,]
          ptile_tavg = (ptile_tmax + ptile_tmin) / 2

          tmax = time_series.tasmax[j,]
          tmin = time_series.tasmin[j,]
          prec = time_series.pr[j,]
          tavg = (tmax + tmin) / 2
          
          wetting_rain_010in = prec
          wetting_rain_050in = prec
          
          attributes(wetting_rain_010in) <- NULL
          attributes(wetting_rain_050in) <- NULL
          
          wetting_rain_010in[ prec <  2.54] = 0   # dry
          wetting_rain_010in[ prec >  2.54] = 1   # not dry

          wetting_rain_050in[ prec < 12.70] = 0   # dry
          wetting_rain_050in[ prec > 12.70] = 1   # not dry
          
          quantiles_for_ccis = list(tmax=list(outbase=list(q10=(ptile_tmax[10+1]), 
                                                           q90=(ptile_tmax[90+1])),
                                              inbase= list(q10=(ptile_tmax[10+1]), 
                                                           q90=(ptile_tmax[90+1]))),
                                    tmin=list(outbase=list(q10=(ptile_tmin[10+1]), 
                                                           q90=(ptile_tmin[90+1])),
                                              inbase= list(q10=(ptile_tmin[10+1]), 
                                                           q90=(ptile_tmin[90+1]))),
                                    tavg=list(outbase=list(q10=(ptile_tavg[10+1]), 
                                                           q90=(ptile_tavg[90+1])),
                                              inbase= list(q10=(ptile_tavg[10+1]), 
                                                           q90=(ptile_tavg[90+1]))),
                                    prec=             list(q95=(ptile_prec[95+1]), 
                                                           q99=(ptile_prec[99+1]))  )     
                    
          clim_indata = climdexInput.raw(tmax                = time_series.tasmax[j,], 
                                         tmin                = time_series.tasmin[j,], 
                                         prec                = time_series.pr[j,],
                                         tmax.dates          = time_for_period_PCICt, 
                                         tmin.dates          = time_for_period_PCICt, 
                                         prec.dates          = time_for_period_PCICt,
                                         base.range          = c(1961, 1990), 
                                         n                   = 5, 
                                         northern.hemisphere = TRUE,
                                         tavg                = tavg,
                                         tavg.dates          = time_for_period_PCICt, 
                                         quantiles           = quantiles_for_ccis, 
                                         temp.qtiles         = c(0.10, 0.90), 
                                         prec.qtiles         = c(0.95, 0.99), 
                                         max.missing.days    = c(annual  = 15,
                                                                 monthly =  3), 
                                         min.base.data.fraction.present = 0.1) 
          

          climdex = climdex.get.available.indices(ci = clim_indata, function.names = TRUE)
          
          climdex.su[i,j, ,ens]      = climdex.su(      ci = clim_indata)
          climdex.id[i,j, ,ens]      = climdex.id(      ci = clim_indata)
          climdex.fd[i,j, ,ens]      = climdex.fd(      ci = clim_indata)
          climdex.tr[i,j, ,ens]      = climdex.tr(      ci = clim_indata)
          climdex.sdii[i,j, ,ens]    = climdex.sdii(    ci = clim_indata)
          climdex.r10mm[i,j, ,ens]   = climdex.r10mm(   ci = clim_indata)
          climdex.r20mm[i,j, ,ens]   = climdex.r20mm(   ci = clim_indata)
          climdex.r03mm[i,j, ,ens]   = climdex.rnnmm(   ci = clim_indata, threshold =  2.54)
          climdex.r13mm[i,j, ,ens]   = climdex.rnnmm(   ci = clim_indata, threshold = 12.70)
          climdex.cdd[i,j, ,ens]     = climdex.cdd(     ci = clim_indata, spells.can.span.years = FALSE)
          climdex.csdi[i,j, ,ens]    = climdex.csdi(    ci = clim_indata, spells.can.span.years = FALSE)
          climdex.wsdi[i,j, ,ens]    = climdex.wsdi(    ci = clim_indata, spells.can.span.years = FALSE)
          climdex.r95ptot[i,j, ,ens] = climdex.r95ptot( ci = clim_indata)
          climdex.r99ptot[i,j, ,ens] = climdex.r99ptot( ci = clim_indata)
          climdex.prcptot[i,j, ,ens] = climdex.prcptot( ci = clim_indata)
          climdex.gsl[i,j, ,ens]     = climdex.gsl(     ci = clim_indata, gsl.mode= "GSL")
           
          climdex.txx.a[i,j, ,ens]     = climdex.txx(    ci = clim_indata, freq = "annual")
          climdex.txn.a[i,j, ,ens]     = climdex.txn(    ci = clim_indata, freq = "annual")
          climdex.tnx.a[i,j, ,ens]     = climdex.tnx(    ci = clim_indata, freq = "annual")
          climdex.tnn.a[i,j, ,ens]     = climdex.tnn(    ci = clim_indata, freq = "annual")
          climdex.rx1day.a[i,j, ,ens]  = climdex.rx1day( ci = clim_indata, freq = "annual")
          climdex.rx5day.a[i,j, ,ens]  = climdex.rx5day( ci = clim_indata, freq = "annual")
          climdex.dtr.a[i,j, ,ens]     = climdex.dtr(    ci = clim_indata, freq = "annual")
          
          for (y in 1:period_span) {
            
              w010in_for_1_year = wetting_rain_010in[ year(time_for_period) == year_in_time_period[y] ]
              w050in_for_1_year = wetting_rain_050in[ year(time_for_period) == year_in_time_period[y] ]
              
              rle_w010in = rle(w010in_for_1_year)
              rle_w050in = rle(w050in_for_1_year)
              
              interrarrivals_w010in = rle_w010in$lengths[rle_w010in$values==0] 
              interrarrivals_w050in = rle_w050in$lengths[rle_w050in$values==0] 
                
              seas.interarrival.r03mm_max.a[i,j,y,ens] = max(interrarrivals_w010in)
              seas.interarrival.r13mm_max.a[i,j,y,ens] = max(interrarrivals_w050in)

              seas.interarrival.r03mm_mean.a[i,j,y,ens] = mean(interrarrivals_w010in)
              seas.interarrival.r13mm_mean.a[i,j,y,ens] = mean(interrarrivals_w050in)
 
              seas.interarrival.r03mm_median.a[i,j,y,ens] = median(interrarrivals_w010in)
              seas.interarrival.r13mm_median.a[i,j,y,ens] = median(interrarrivals_w050in)
              
          }              
          #  climdex.tx10p.a[i,j, ,ens]   = climdex.tx10p(  ci = clim_indata, freq = "annual")
          #  climdex.tx90p.a[i,j, ,ens]   = climdex.tx90p(  ci = clim_indata, freq = "annual")
          #  climdex.tn10p.a[i,j, ,ens]   = climdex.tn10p(  ci = clim_indata, freq = "annual")
          #  climdex.tn90p.a[i,j, ,ens]   = climdex.tn90p(  ci = clim_indata, freq = "annual")
          

          #  climdex.txx.m[i,j,yrmo,ens]     = climdex.txx(    ci = clim_indata, freq = "monthly")
          #  climdex.txn.m[i,j,yrmo,ens]     = climdex.txn(    ci = clim_indata, freq = "monthly")
          #  climdex.tnx.m[i,j,yrmo,ens]     = climdex.tnx(    ci = clim_indata, freq = "monthly")
          #  climdex.tnn.m[i,j,yrmo,ens]     = climdex.tnn(    ci = clim_indata, freq = "monthly")
          #  climdex.rx1day.m[i,j,yrmo,ens]  = climdex.rx1day( ci = clim_indata, freq = "monthly")
          #  climdex.rx5day.m[i,j,yrmo,ens]  = climdex.rx5day( ci = clim_indata, freq = "monthly")
          #  climdex.dtr.m[i,j,yrmo,ens]     = climdex.dtr(    ci = clim_indata, freq = "monthly")          
          
          #  climdex.tx10p.m[i,j,yrmo,ens]   = climdex.tx10p(  ci = clim_indata, freq = "monthly")
          #  climdex.tx90p.m[i,j,yrmo,ens]   = climdex.tx90p(  ci = clim_indata, freq = "monthly")
          #  climdex.tn10p.m[i,j,yrmo,ens]   = climdex.tn10p(  ci = clim_indata, freq = "monthly")
          #  climdex.tn90p.m[i,j,yrmo,ens]   = climdex.tn90p(  ci = clim_indata, freq = "monthly")
          
        } #end j loop

        if (ROW_print_diagnostics) {
          print(paste(i,10,
                      ensemble_site[ens],
                      scenario,
                      paste(start_year,end_year,sep="-"),
                      mean(tmax),
                      mean(tmin),
                      mean(prec)))
        }



     } #end i looop

      plot(x    = time_period,
           y    = tmax,
           type = "l",
           main = variable_name_tasmax,
           xlab = "Date",
           ylab = "Max Daily Temperature (°C)"
            )

      
      plot(x    = time_period,
           y    = tmin,
           type = "l",
           main = variable_name_tasmin,
           xlab = "Date",
           ylab = "Min Daily Temperature (°C)"
           )
      
      plot(x    = time_period,
           y    = prec,
           type = "h",
           main = variable_name_pr,
           xlab = "Date",
           ylab = "Total Daily Precip (mm)"
           )
      
      

   nc_close(nc.file_pr)
   nc_close(nc.file_tasmax)
   nc_close(nc.file_tasmin)
   
 
   print("==")
   print("==========================================================================")
   
  } #end ensemble looop




    save(time_for_period,
         start_year,
         end_year,
         latitude,
         longitude,
         ensemble_site,
         climdex.su,
         climdex.id,
         climdex.fd,
         climdex.tr,
         climdex.sdii,
         climdex.r10mm,
         climdex.r20mm,
         climdex.r03mm,
         climdex.r13mm,
         climdex.cdd,
         climdex.csdi,
         climdex.wsdi,
         climdex.r95ptot,
         climdex.r99ptot,
         climdex.prcptot,
         climdex.gsl,
         climdex.txx.a,
         climdex.txn.a,
         climdex.tnx.a,
         climdex.tnn.a,
         climdex.rx1day.a,
         climdex.rx5day.a,
         climdex.dtr.a,
         seas.interarrival.r03mm_mean.a,  
         seas.interarrival.r13mm_mean.a,      
         seas.interarrival.r03mm_max.a,              
         seas.interarrival.r13mm_max.a,      
         seas.interarrival.r03mm_median.a,      
         seas.interarrival.r13mm_median.a,
         file =  paste("/projects/ECEP/LOCA_MACA_Ensembles/LOCA/LOCA_R_Gridded_Analysis/",
                       "etccdi_cci_indicies_",
                       file_prefix,
                       scenario,
                       "_",
                       "tasmin_tasmax_pr",
                       "_",
                       start_year,
                       "_to_",
                       end_year,
                       ".RData",
                       sep=""))

    
    
    if (PDF_on) {
      dev.off()
    }
    
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
    
    time_bounds = array( 0,  dim=c(2,period_span), dimnames=list(bnds,year))
    
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
    
    
    netcdf_output_file_name = paste("/projects/ECEP/LOCA_MACA_Ensembles/LOCA/LOCA_R_Gridded_Analysis/",
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
                                             missval  = fill_value_float,
                                             longname = "Mean Wetting Rain Return Period (prec>0.1\")",
                                             prec     = "short")      

    netcdf_seas_med_interv_btn_003mm_a =  ncvar_def(nam = "median_interv_btn_003mm_a",
                                                   units    = "days",
                                                   dim      = list(netcdf_lon_dim,
                                                                   netcdf_lat_dim,
                                                                   netcdf_time_dim,
                                                                   netcdf_ens_dim),
                                                   missval  = fill_value_float,
                                                   longname = "Median Wetting Rain Return Period (prec>0.1\")",
                                                   prec     = "short")      
    
    netcdf_seas_max_interv_btn_003mm_a =  ncvar_def(nam = "maximum_interv_btn_003mm_a",
                                                   units    = "days",
                                                   dim      = list(netcdf_lon_dim,
                                                                   netcdf_lat_dim,
                                                                   netcdf_time_dim,
                                                                   netcdf_ens_dim),
                                                   missval  = fill_value_float,
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
                                                    missval  = fill_value_float,
                                                    longname = "Median Wetting Rain Return Period (prec>0.5\")",
                                                    prec     = "short")      
    
    netcdf_seas_max_interv_btn_013mm_a =  ncvar_def(nam = "max_interv_btn_013mm_a",
                                                    units    = "days",
                                                    dim      = list(netcdf_lon_dim,
                                                                    netcdf_lat_dim,
                                                                    netcdf_time_dim,
                                                                    netcdf_ens_dim),
                                                    missval  = fill_value_float,
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
                                        netcdf_seas_avg_interv_btn_003mm_a,
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
    
