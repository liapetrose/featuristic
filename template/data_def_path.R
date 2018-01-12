# Data Paths --- MASTER TEMPLATE
#----------------------------------------------------------------------------#

# data path  [MODIFY]
#----------------------------------------------------------------------------#
data_folder                     <- "/data/zolab/edw_cohort_data/bwh_ed_2010_2015/Rds/"

# data files [MODIFY]
#----------------------------------------------------------------------------#

# BWH - EDW
edw_dia_xwalked <- paste0(data_folder, "dia_xwalked_bwh_ed_2010_2015_cohort.Rds")

# BWH - RPDR
#-------------------------------------------
rpdr_dem_master_bwh_ed_100k 	<- paste0(data_folder,'rpdr_dem_master_bwh_ed_100k.Rds')
rpdr_dem_master_dfci          	<- paste0(data_folder,'rpdr_dem_master_dfci.Rds')

rpdr_mic_master_bwh_ed_100k 	<- paste0(data_folder,'rpdr_mic_master_bwh_ed_100k.Rds')

rpdr_dia_master_bwh_ed_100k 	<- paste0(data_folder,'rpdr_dia_master_bwh_ed_100k.Rds')
rpdr_dia_master_dfci       		<- paste0(data_folder,'rpdr_dia_master_dfci.Rds')

rpdr_enc_master_bwh_ed_100k 	<- paste0(data_folder,'rpdr_enc_master_bwh_ed_100k.Rds')
rpdr_enc_master_dfci       		<- paste0(data_folder,'rpdr_enc_master_dfci.Rds')

rpdr_lvs_master_bwh_ed_100k 	<- paste0(data_folder,'rpdr_lvs_master_bwh_ed_100k.Rds')
rpdr_lvs_master_dfci       		<- paste0(data_folder,'rpdr_lvs_master_dfci.Rds')

rpdr_prc_master_bwh_ed_100k 	<- paste0(data_folder,'rpdr_prc_master_bwh_ed_100k.Rds')
rpdr_prc_master_dfci       		<- paste0(data_folder,'rpdr_prc_master_dfci.Rds')

rpdr_med_master_bwh_ed_100k 	<- paste0(data_folder,'rpdr_med_master_bwh_ed_100k.Rds')
rpdr_med_master_dfci       		<- paste0(data_folder,'rpdr_med_master_dfci.Rds')

rpdr_lab_master_bwh_ed_100k   	<- paste0(data_folder,'rpdr_lab_master_bwh_ed_100k.Rds')

# BWH - EDADMIN
#-------------------------------------------
edadmin_ed_master_bwh_ed_100k   <- paste0(data_folder,'edadmin_ed_master_bwh_ed_100k.Rds')

# DFCI - ONCDRS
#-------------------------------------------
oncdrs_chemo_master_dfci        <- paste0(data_folder,'oncdrs_chemo_master_dfci.Rds')
oncdrs_med_master_dfci          <- paste0(data_folder,'oncdrs_med_master_dfci.Rds')
oncdrs_enc_master_dfci          <- paste0(data_folder,'oncdrs_enc_master_dfci.Rds')
oncdrs_lab_master_dfci          <- paste0(data_folder,'oncdrs_lab_master_dfci.Rds')
oncdrs_dia_master_dfci          <- paste0(data_folder,'oncdrs_dia_master_dfci.Rds')

#----------------------------------------------------------------------------#
