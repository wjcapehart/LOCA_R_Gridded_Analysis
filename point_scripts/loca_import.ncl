


begin

   ;;;; Set up NetCDF4 Setup

   setfileoption("nc", "FileStructure", "Advanced")
   setfileoption("nc", "Format",        "NetCDF4")

   ;;;; Target Labels

   location_name_array = (/ "Rapid City, SD", \
                            "Brookings, SD",  \
                            "Phillip, SD"     /)

   location_abbr_array = (/ "KRAP", \
                            "KBKX", \
                            "KPHP"  /)

   ;;;; Target Latitudes and Longitudes

   targ_lon_array = (/ -103.0573611, \
                        -96.8189167, \
                       -101.5988405  /) + 360.0

   targ_lat_array = (/  44.0453333, \
                        44.3045278, \
                        44.0487306  /)

   ;;;; Historical and Future Files

   historical_full_url   = "https://cida.usgs.gov/thredds/dodsC/loca_historical"
   future_full_url       = "https://cida.usgs.gov/thredds/dodsC/loca_future"

   ;;;; Generate Ensemble Model Coordinates Coordinates

   ensemble_model = (/ "ACCESS1-0_r1i1p1",      \
                       "ACCESS1-3_r1i1p1",      \
                       "CCSM4_r6i1p1",          \
                       "CESM1-BGC_r1i1p1",      \
                       "CESM1-CAM5_r1i1p1",     \
                       "CMCC-CMS_r1i1p1",       \
                       "CMCC-CM_r1i1p1",        \
                       "CNRM-CM5_r1i1p1",       \
                       "CSIRO-Mk3-6-0_r1i1p1",  \
                       "CanESM2_r1i1p1",        \
                       "FGOALS-g2_r1i1p1",      \
                       "GFDL-CM3_r1i1p1",       \
                       "GFDL-ESM2G_r1i1p1",     \
                       "GFDL-ESM2M_r1i1p1"      \
                       "HadGEM2-AO_r1i1p1"      \
                       "HadGEM2-CC_r1i1p1"      \
                       "HadGEM2-ES_r1i1p1"      \
                       "IPSL-CM5A-LR_r1i1p1"    \
                       "IPSL-CM5A-MR_r1i1p1"    \
                       "MIROC-ESM-CHEM_r1i1p1", \
                       "MIROC-ESM_r1i1p1",      \
                       "MIROC5_r1i1p1",         \
                       "MPI-ESM-LR_r1i1p1",     \
                       "MPI-ESM-MR_r1i1p1",     \
                       "MRI-CGCM3_r1i1p1",      \
                       "NorESM1-M_r1i1p1",      \
                       "bcc-csm1-1-m_r1i1p1"    /)

      ensemble_model!0                  = "ensemble_model"
      ensemble_model&ensemble_model  = ensemble_model
      ensemble_model@description     = "Ensemble Model Member"
      ensemble_model@long_name       = ensemble_model@description
   nens = dimsizes(ensemble_model)

   ;;;; Generate CMIP-5 Scenario Coordinates

   RCP_scenario = (/ "RCP 4.5", "RCP 8.5" /)
      RCP_scenario!0            = "RCP_scenario"
      RCP_scenario&RCP_scenario = RCP_scenario
      RCP_scenario@description  = "CMIP-5 Representative Concentration Pathway Scenarios"
      RCP_scenario@long_name    = RCP_scenario@description
   nscen   = dimsizes(RCP_scenario)

   ;;;; Generate Historical CMIP-5 Scenario Inventory Coordinates

   scenario_list   = (/ "Historical", "RCP 4.5", "RCP 8.5" /)
      scenario_list!0             = "scenario_list"
      scenario_list&scenario_list = scenario_list
      scenario_list@description   = "List of Scenarios for Inventory"
      scenario_list@standard_name    = scenario_list@description

   ;;;; Generate Variable Inventory Coordinates

   variable_list   = (/ "Daily Maximum Temperature", \
                        "Daily Maximum Temperature", \
                        "Daily Total Precipitation" /)
      variable_list!0             = "variable_list"
      variable_list&variable_list = variable_list
      variable_list@description   = "List of Variables for Inventory"
      variable_list@standard_name = variable_list@description

   ;;;; Create Historical CMIP-5 Scenario Inventory

   available_inventory                   = new((/ 3, nens, 3 /), integer )
      available_inventory!0              = "variable_list"
      available_inventory&variable_list  = variable_list
      available_inventory!1              = "ensemble_model"
      available_inventory&ensemble_model = ensemble_model
      available_inventory!2              = "scenario_list"
      available_inventory&scenario_list  = scenario_list
      available_inventory@description    = "Inventory of LOCA Scenarios"
      available_inventory@standard_name  = available_inventory@description
      available_inventory@comment        = "1 = available ;  0 = not available"

   available_inventory(:,:,:) = 0

   ;;;; Crack open OPeNDAP files for reading

   fhist = addfile(historical_full_url, "r")
   ffutr = addfile(future_full_url,     "r")

   ;;;; Pull Latitude and Longitude from the Original LOCA Files

   longitude_loca = fhist->lon
   latitude_loca  = fhist->lat

   ;;;; Create Historical Arrays

   time_hist = fhist->time
      time_hist!0             = "time_hist"
      time_hist&time_hist     = time_hist
      time_hist@description   = "Time (Historical Period)"
      time_hist@long_name     = time_hist@description
      time_hist@standard_name = "time"
      time_hist@calendar      = "standard"
      time_hist@axis          = "T"
      delete(time_hist@bounds)
      delete(time_hist@_ChunkSizes)
   nth = dimsizes(time_hist)

   tmax_hist = new ((/ nth, nens /), float )
      tmax_hist!0              = "time_hist"
      tmax_hist&time_hist      = time_hist
      tmax_hist!1              = "ensemble_model"
      tmax_hist&ensemble_model = ensemble_model
      tmax_hist@description    = "Maximum Daily Temperature (Historical)"
      tmax_hist@long_name      = tmax_hist@description
      tmax_hist@standard_name  = "air_temperature"
      tmax_hist@units          = "K"
      tmax_hist@cell_methods   = "tmax_hist:maximum"
      tmax_hist@_FillValue     = default_fillvalue("float")
      tmax_hist@coordinates    = "time_hist latitude longitude"

   tmin_hist = tmax_hist
      tmin_hist@description    = "Maximum Daily Temperature (Historical)"
      tmin_hist@long_name      = tmin_hist@description
      tmin_hist@standard_name  = "air_temperature"
      tmin_hist@cell_methods   = "time_hist:maximum"

   prec_hist = tmax_hist
      prec_hist@description    = "Total Daily Precipitation (Historical)"
      prec_hist@long_name      = prec_hist@description
      prec_hist@standard_name  = "precipitation_amount"
      prec_hist@units          = "kg m-2"
      prec_hist@cell_methods   = "time_hist:sum"

   ;;;; Create Future CMIP-5 Scenario Arrays

   time_futr = ffutr->time
      time_futr!0             = "time_futr"
      time_futr&time_futr     = time_futr
      time_futr@description   = "Time (Projected Period)"
      time_futr@long_name     = time_futr@description
      time_futr@standard_name = "time"
      time_futr@calendar      = "standard"
      time_futr@axis          = "T"
      delete(time_futr@bounds)
      delete(time_futr@_ChunkSizes)
   ntf = dimsizes(time_futr)

   tmax_futr = new ((/ ntf, nens, nscen /), float )
      tmax_futr!0              = "time_futr"
      tmax_futr&time_futr      = time_futr
      tmax_futr!1              = "ensemble_model"
      tmax_futr&ensemble_model = ensemble_model
      tmax_futr!2              = "RCP_scenario"
      tmax_futr&RCP_scenario   = RCP_scenario
      tmax_futr@description    = "Maximum Daily Temperature (Projected)"
      tmax_futr@long_name      = tmax_futr@description
      tmax_futr@standard_name  = "air_temperature"
      tmax_futr@units          = "K"
      tmax_futr@cell_methods   = "tmax_futr:maximum"
      tmax_futr@_FillValue     = default_fillvalue("float")
      tmax_futr@coordinates    = "time_futr latitude longitude"

   tmin_futr = tmax_futr
      tmin_futr@description    = "Maximum Daily Temperature (Projected)"
      tmin_futr@long_name      = tmin_futr@description
      tmin_futr@standard_name  = "air_temperature"
      tmin_futr@cell_methods   = "time_futr:maximum"

   prec_futr = tmax_futr
      prec_futr@description    = "Total Daily Precipitation (Projected)"
      prec_futr@long_name      = prec_futr@description
      prec_futr@standard_name  = "precipitation_amount"
      prec_futr@units          = "kg m-2"
      prec_futr@cell_methods   = "time_futr:sum"

   ;;; Begin Looping through Locations and Sites

   do loc = 0, dimsizes(location_name_array)-1

      print("=================================================")

      netcdf_outfile = "./"+location_abbr_array(loc)+"_LOCA.nc"

      ;;;; Load Station-Specific Metadata

      latitude_station = (/ targ_lat_array(loc) /)
         latitude_station!0             = "ncl_scalar"
         latitude_station@description   = "Latitude of Station"
         latitude_station@long_name     = latitude_station@description
         latitude_station@standard_name = "latitude"
         latitude_station@units         = "degrees_north"

      longitude_station = (/ targ_lon_array(loc) /)
         longitude_station!0             = "ncl_scalar"
         longitude_station@description   = "Longitude of Station"
         longitude_station@long_name     = longitude_station@description
         longitude_station@standard_name = "longitude"
         longitude_station@units         = "degrees_east"

      station_name = (/ location_name_array(loc) /)
         station_name!0           = "ncl_scalar"
         station_name@description = "Station Name"
         station_name@long_name   = station_name@description

      station_icao = (/ location_abbr_array(loc) /)
         station_icao!0           = "ncl_scalar"
         station_icao@description = "Station ICAO Designator"
         station_icao@long_name   = station_icao@description

      latitude = (/ latitude_loca({latitude_station}) /)
         latitude!0             = "ncl_scalar"
         latitude@description   = "Latitude of Grid Point"
         latitude@long_name     = latitude@description
         latitude@standard_name = "latitude"
         latitude@units         = "degrees_north"
         latitude@axis          = "Y"

      longitude = (/ longitude_loca({longitude_station}) /)
         longitude!0             = "ncl_scalar"
         longitude@description   = "Longitude of Grid Point"
         longitude@long_name     = longitude@description
         longitude@standard_name = "longitude"
         longitude@units         = "degrees_west"
         longitude@axis          = "X"

      great_circle_distance_error_in_km = gc_latlon(latitude_station,longitude_station,latitude,longitude,1,4)
         great_circle_distance_error_in_km!0           = "ncl_scalar"
         great_circle_distance_error_in_km@description = "Separation Distance Between ICAO station and Grid Cell Center"
         great_circle_distance_error_in_km@long_name   = great_circle_distance_error_in_km@description
         great_circle_distance_error_in_km@units       = "km"
         great_circle_distance_error_in_km@comment     = "created using gc_latlon() function"



      tmax_hist = tmax_hist@_FillValue
      tmin_hist = tmin_hist@_FillValue
      prec_hist = prec_hist@_FillValue

      tmax_futr = tmax_futr@_FillValue
      tmin_futr = tmin_futr@_FillValue
      prec_futr = prec_futr@_FillValue


      ;;;; Loop through Ensemble Model Members

      do ens = 0, nens-1

         ;;;; Maximum Daily Temperature

         vname = "tasmax_" + ensemble_model(ens) + "_historical"
            if (isfilevar(fhist,vname)) then
               print("Processing " + station_icao + " " + vname)
               deleteme            = fhist->$vname$( :, {latitude}, {longitude})
               if (isatt(deleteme, "scale_factor")) then
                  scale            = deleteme@scale_factor
               else
                  scale            = 1.0
               end if
               if (deleteme@units .eq. "C") then
                  tmax_hist(:,ens) = (/ tofloat(deleteme) * scale + 273.15 /)
               else
                  tmax_hist(:,ens) = (/ tofloat(deleteme) * scale  /)
               end if
               print("    - range: " + min(tmax_hist(:,ens)) + " : " \
                                     + max(tmax_hist(:,ens)))
               delete(deleteme)
               available_inventory(0,ens,0) = 1
            end if

         vname = "tasmax_" + ensemble_model(ens) + "_rcp45"
            if (isfilevar(ffutr,vname)) then
               print("Processing " + station_icao + " " + vname)
               deleteme            = ffutr->$vname$( :, {latitude}, {longitude})
               if (isatt(deleteme, "scale_factor")) then
                  scale            = deleteme@scale_factor
               else
                  scale            = 1.0
               end if
               if (deleteme@units .eq. "C") then
                  tmax_futr(:,ens,0) = (/ tofloat(deleteme) * scale + 273.15 /)
               else
                  tmax_futr(:,ens,0) = (/ tofloat(deleteme) * scale  /)
               end if
               print("    - range: " + min(tmax_futr(:,ens,0)) + " : " \
                                     + max(tmax_futr(:,ens,0)))
               delete(deleteme)
               available_inventory(0,ens,1) = 1
            end if

         vname = "tasmax_" + ensemble_model(ens) + "_rcp85"
            if (isfilevar(ffutr,vname)) then
               print("Processing " + station_icao + " " + vname)
               deleteme            = ffutr->$vname$( :, {latitude}, {longitude})
               if (isatt(deleteme, "scale_factor")) then
                  scale            = deleteme@scale_factor
               else
                  scale            = 1.0
               end if
               if (deleteme@units .eq. "C") then
                  tmax_futr(:,ens,1) = (/ tofloat(deleteme) * scale + 273.15 /)
               else
                  tmax_futr(:,ens,1) = (/ tofloat(deleteme) * scale  /)
               end if
               print("    - range: " + min(tmax_futr(:,ens,1)) + " : " \
                                     + max(tmax_futr(:,ens,1)))
               delete(deleteme)
               available_inventory(0,ens,2) = 1
            end if

            print("- - - - - - - - - - - - - - - - - - - - - - - - -")

         ;;;; Minimum Daily Temperature

         vname = "tasmin_" + ensemble_model(ens) + "_historical"
            if (isfilevar(fhist,vname)) then
               deleteme         = fhist->$vname$( :, {latitude}, {longitude})
               if (isatt(deleteme, "scale_factor")) then
                  scale            = deleteme@scale_factor
               else
                  scale            = 1.0
               end if
               if (deleteme@units .eq. "C") then
                  tmin_hist(:,ens) = (/ tofloat(deleteme) * scale + 273.15 /)
               else
                  tmin_hist(:,ens) = (/ tofloat(deleteme) * scale  /)
               end if
               print("    - range: " + min(tmin_hist(:,ens)) + " : " \
                                     + max(tmin_hist(:,ens)))
               delete(deleteme)
               available_inventory(1,ens,0) = 1
         end if

         vname = "tasmin_" + ensemble_model(ens) + "_rcp45"
            if (isfilevar(ffutr,vname)) then
               print("Processing " + station_icao + " " + vname)
               deleteme           = ffutr->$vname$( :, {latitude}, {longitude})
               if (isatt(deleteme, "scale_factor")) then
                  scale            = deleteme@scale_factor
               else
                  scale            = 1.0
               end if
               if (deleteme@units .eq. "C") then
                  tmin_futr(:,ens,0) = (/ tofloat(deleteme) * scale + 273.15 /)
               else
                  tmin_futr(:,ens,0) = (/ tofloat(deleteme) * scale  /)
               end if
               print("    - range: " + min(tmin_futr(:,ens,0)) + " : " \
                                     + max(tmin_futr(:,ens,0)))
               delete(deleteme)
               available_inventory(1,ens,1) = 1
            end if

         vname = "tasmin_" + ensemble_model(ens) + "_rcp85"
            if (isfilevar(ffutr,vname)) then
               print("Processing " + station_icao + " " + vname)
               deleteme           = ffutr->$vname$( :, {latitude}, {longitude})
               if (isatt(deleteme, "scale_factor")) then
                  scale            = deleteme@scale_factor
               else
                  scale            = 1.0
               end if
               if (deleteme@units .eq. "C") then
                  tmin_futr(:,ens,1) = (/ tofloat(deleteme) * scale + 273.15 /)
               else
                  tmin_futr(:,ens,1) = (/ tofloat(deleteme) * scale  /)
               end if
               print("    - range: " + min(tmin_futr(:,ens,1)) + " : " \
                                     + max(tmin_futr(:,ens,1)))
               delete(deleteme)
               available_inventory(1,ens,2) = 1
            end if

            print("- - - - - - - - - - - - - - - - - - - - - - - - -")

         ;;;; Daily Total Precipitation

         vname = "pr_" + ensemble_model(ens) + "_historical"
            if (isfilevar(fhist,vname)) then
               print("Processing " + station_icao + " " + vname)
               deleteme         = fhist->$vname$( :, {latitude}, {longitude})
               if (isatt(deleteme, "scale_factor")) then
                  scale            = deleteme@scale_factor
               else
                  scale            = 1.0
               end if
               if (deleteme@units .eq. "mm") then
                  prec_hist(:,ens) = (/ tofloat(deleteme) * scale /)
               else
                  prec_hist(:,ens) = (/ tofloat(deleteme) * scale * 86400.  /)
               end if
               print("    - range: " + min(prec_hist(:,ens)) + " : " \
                                     + max(prec_hist(:,ens)))
            delete(deleteme)
            available_inventory(2,ens,0) = 1
         end if

         vname = "pr_" + ensemble_model(ens) + "_rcp45"
            if (isfilevar(ffutr,vname)) then
               print("Processing " + station_icao + " " + vname)
               deleteme           = ffutr->$vname$( :, {latitude}, {longitude})
               if (isatt(deleteme, "scale_factor")) then
                  scale            = deleteme@scale_factor
               else
                  scale            = 1.0
               end if
               if (deleteme@units .eq. "mm") then
                  prec_futr(:,ens,0) = (/ tofloat(deleteme) * scale /)
               else
                  prec_futr(:,ens,0) = (/ tofloat(deleteme) * scale * 86400.  /)
               end if
               print("    - range: " + min(prec_futr(:,ens,0)) + " : " \
                                     + max(prec_futr(:,ens,0)))
               delete(deleteme)
               available_inventory(2,ens,1) = 1
            end if

         vname = "pr_" + ensemble_model(ens) + "_rcp85"
            if (isfilevar(ffutr,vname)) then
               print("Processing " + station_icao + " " + vname)
               deleteme           = ffutr->$vname$( :, {latitude}, {longitude})
               if (isatt(deleteme, "scale_factor")) then
                  scale            = deleteme@scale_factor
               else
                  scale            = 1.0
               end if
               if (deleteme@units .eq. "mm") then
                  prec_futr(:,ens,1) = (/ tofloat(deleteme) * scale /)
               else
                  prec_futr(:,ens,1) = (/ tofloat(deleteme) * scale * 86400.  /)
               end if
               print("    - range: " + min(prec_futr(:,ens,1)) + " : " \
                                     + max(prec_futr(:,ens,1)))
               delete(deleteme)
               available_inventory(2,ens,2) = 1
            end if

         print("-------------------------------------------------")

      end do

      print("=================================================")

      ;;;; Output to File

      system("rm -frv "+netcdf_outfile)

      fout = addfile(netcdf_outfile, "c")

         ;;;; Metadata

         fout@title               = "Historical LOCA Statistical Downscaling (Localized Constructed Analogs) Statistically downscaled CMIP5 climate projections for North America Historical LOCA Statistical Downscaling (Localized Constructed Analogs) Statistically downscaled CMIP5 climate projections for North America"
         fout@creator_name        = "Original: David Piercem (OUI-USGS); " + tochar(10) + \
                                    "Modified: Bill Capehart SD School of Mines"
         fout@creator_email       = "Original: dpierce@ucsd.edu; " + tochar(10) + \
                                    "Modified: William.Capehart@sdsmt.edu"
         fout@institution         = "Original: USGS Office of Water Information; " + tochar(10) + \
                                    "Modified: SD School of Mines and Technology"
         fout@Conventions         = "CF-1.4"
         fout@project             = "Comparative Analysis of Downscaled Climate Simulations, Providing Guidance to End Users"
         fout@cdm_data_type       = "Station"
         fout@summary             = "LOCA is a statistical downscaling technique that uses past history to add improved fine-scale detail to global climate models. " + tochar(10) + \
                                    "We have used LOCA to downscale 32 global climate models from the CMIP5 archive at a 1/16th degree spatial resolution, covering North America from central Mexico through Southern Canada. " + tochar(10) + \
                                    "The historical period is 1950-2005, and there are two future scenarios available: RCP 4.5 and RCP 8.5 over the period 2006-2100 (although some models stop in 2099). " + tochar(10) + \
                                    "The variables currently available are daily minimum and maximum temperature, and daily precipitation. " + tochar(10) + \
                                    "For more information visit: http://loca.ucsd.edu/"
         fout@acknowledgment      = "Pierce, D. W., D. R. Cayan, and B. L. Thrasher, 2014: Statistical downscaling using Localized Constructed Analogs (LOCA). Journal of Hydrometeorology, volume 15, page 2558-2585." + tochar(10) + \
                                    "We acknowledge the World Climate Research Programme's Working Group on Coupled Modeling, which is responsible for CMIP, and we thank the climate modeling groups for producing and making available their model output." + tochar(10) + \
                                    "For CMIP, the U.S. Department of Energy's Program for Climate Model Diagnosis and Intercomparison provides coordinating support and led development of software infrastructure in partnership with the Global Organization for Earth System Science Portals."
         fout@comment             = "Original Prioduct Extracted to a Single Station Point, Original Prioduct Calibrated to full units, Forecast Model Ensembles Colated into a single variable and by CMIP-5 RCP Scenario for future run data"
         fout@history             = "Dataset was deflated and aggregated for publication by the USGS by dblodgett@usgs.gov in May 2016.  " + tochar(10) + \
                                    "Dataset was extracted for a single point and converted to common SI units by William.Capehart@sdsmt.edu in October 2017"

         fout@time_coverage_start = "1950-01-01T00:00"
         fout@time_coverage_end   = "2100-12-31T00:00"
         fout@station_name        = station_name
         fout@station_icao        = station_icao
         fout@latitude_station    = latitude_station
         fout@longitude_station   = longitude_station
         fout@geospatial_lat_min  = latitude
         fout@geospatial_lat_max  = latitude
         fout@geospatial_lon_min  = longitude
         fout@geospatial_lon_max  = longitude
         fout@keywords            = "precipitation, temperature"
         fout@license             = "Freely available"

         ;;;; Coordinates

         fout->latitude                          = latitude
         fout->longitude                         = longitude
         fout->station_name                      = station_name
         fout->station_icao                      = station_icao
         fout->latitude_station                  = latitude_station
         fout->longitude_station                 = longitude_station
         fout->great_circle_distance_error_in_km = great_circle_distance_error_in_km
         fout->time_hist                         = time_hist
         fout->time_futr                         = time_futr
         fout->ensemble_model                    = ensemble_model
         fout->RCP_scenario                      = RCP_scenario
         fout->scenario_list                     = scenario_list
         fout->available_inventory               = available_inventory

         ;;;; Historical Data

         fout->tmax_hist         = tmax_hist
         fout->tmin_hist         = tmin_hist
         fout->prec_hist         = prec_hist

         ;;;; Projected RCP 4.5 & RCP 8.5 Data

         fout->tmax_futr         = tmax_futr
         fout->tmin_futr         = tmin_futr
         fout->prec_futr         = prec_futr

         delete(fout)

         tmax_hist = tmax_hist@_FillValue
         tmin_hist = tmin_hist@_FillValue
         prec_hist = prec_hist@_FillValue

         tmax_futr = tmax_futr@_FillValue
         tmin_futr = tmin_futr@_FillValue
         prec_futr = prec_futr@_FillValue

   end do




end
