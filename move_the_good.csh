#!/bin/bash


export  INDIR=/projects/ECEP/LOCA_MACA_Ensembles/LOCA/LOCA_OUTPUT/grid
export OUTDIR=/projects/ECEP/LOCA_MACA_Ensembles/LOCA/LOCA_OUTPUT

declare -a ENSEMBLE=(
                     "SODAK_LOCA_tasmin_CanESM2_r1i1p1_rcp85.nc"
                     "SODAK_LOCA_tasmax_IPSL-CM5A-MR_r1i1p1_rcp85.nc"
                     "SODAK_LOCA_tasmax_FGOALS-g2_r1i1p1_rcp85.nc"
                     "SODAK_LOCA_tasmin_GFDL-ESM2G_r1i1p1_rcp45.nc"
                     "SODAK_LOCA_tasmin_HadGEM2-ES_r1i1p1_rcp45.nc"
                     "SODAK_LOCA_tasmin_IPSL-CM5A-LR_r1i1p1_rcp45.nc"
                     "SODAK_LOCA_tasmin_MIROC-ESM-CHEM_r1i1p1_rcp45.nc"
                     "SODAK_LOCA_tasmin_MIROC-ESM_r1i1p1_rcp45.nc"
                     "SODAK_LOCA_tasmax_CanESM2_r1i1p1_historical.nc"
                     "SODAK_LOCA_tasmax_FGOALS-g2_r1i1p1_historical.nc"
                     "SODAK_LOCA_tasmax_GFDL-CM3_r1i1p1_historical.nc"
                     "SODAK_LOCA_tasmax_GFDL-ESM2G_r1i1p1_historical.nc"
                     "SODAK_LOCA_tasmax_GFDL-ESM2M_r1i1p1_historical.nc"
                     "SODAK_LOCA_tasmax_HadGEM2-AO_r1i1p1_historical.nc"
                     "SODAK_LOCA_tasmax_HadGEM2-CC_r1i1p1_historical.nc"
                     "SODAK_LOCA_tasmax_HadGEM2-ES_r1i1p1_historical.nc"
                     "SODAK_LOCA_tasmax_IPSL-CM5A-LR_r1i1p1_historical.nc"
                     "SODAK_LOCA_tasmax_MIROC-ESM-CHEM_r1i1p1_historical.nc"
                     "SODAK_LOCA_tasmax_MIROC5_r1i1p1_historical.nc"
                     "SODAK_LOCA_tasmax_MPI-ESM-LR_r1i1p1_historical.nc"
                     "SODAK_LOCA_tasmax_MPI-ESM-MR_r1i1p1_historical.nc"
                     "SODAK_LOCA_tasmax_MRI-CGCM3_r1i1p1_historical.nc"
                     "SODAK_LOCA_tasmax_NorESM1-M_r1i1p1_historical.nc"
                     "SODAK_LOCA_tasmin_ACCESS1-0_r1i1p1_historical.nc"
                     "SODAK_LOCA_tasmin_ACCESS1-3_r1i1p1_historical.nc"
                     "SODAK_LOCA_tasmin_CCSM4_r6i1p1_historical.nc"
                     "SODAK_LOCA_tasmin_CESM1-BGC_r1i1p1_historical.nc"
                     "SODAK_LOCA_tasmin_CESM1-CAM5_r1i1p1_historical.nc"
                     "SODAK_LOCA_tasmin_CMCC-CMS_r1i1p1_historical.nc"
                     "SODAK_LOCA_tasmin_CMCC-CM_r1i1p1_historical.nc"
                     "SODAK_LOCA_tasmin_CNRM-CM5_r1i1p1_historical.nc"
                     "SODAK_LOCA_tasmin_CSIRO-Mk3-6-0_r1i1p1_historical.nc"
                     "SODAK_LOCA_tasmin_CanESM2_r1i1p1_historical.nc"
                     "SODAK_LOCA_tasmin_FGOALS-g2_r1i1p1_historical.nc"
                     "SODAK_LOCA_tasmin_GFDL-CM3_r1i1p1_historical.nc"
                     "SODAK_LOCA_tasmin_GFDL-ESM2G_r1i1p1_historical.nc"
                     "SODAK_LOCA_tasmin_HadGEM2-AO_r1i1p1_historical.nc"
                     "SODAK_LOCA_tasmin_HadGEM2-CC_r1i1p1_historical.nc"
                     "SODAK_LOCA_tasmin_HadGEM2-ES_r1i1p1_historical.nc"
                     "SODAK_LOCA_tasmin_IPSL-CM5A-MR_r1i1p1_historical.nc"
                     "SODAK_LOCA_tasmin_MIROC-ESM_r1i1p1_historical.nc"
                     "SODAK_LOCA_tasmin_MPI-ESM-LR_r1i1p1_historical.nc"
                     "SODAK_LOCA_tasmin_NorESM1-M_r1i1p1_historical.nc"
                     "SODAK_LOCA_tasmin_bcc-csm1-1-m_r1i1p1_historical.nc"
  )


echo ${SCENARIO}
# next one to use us log_for_histtemps.txt

for GOODFILES in "${ENSEMBLE[@]}"
do
   echo  mv -vf ${INDIR}/${GOODFILES}  ${OUTDIR}
   mv -vf ${INDIR}/${GOODFILES}  ${OUTDIR}
done
