#----------------------------------------------------------------------------#

# Purpose:     Feature Construction Global Settings --- MASTER TEMPLATE
# Project:     Cowbell
# Author:      Clara Marquardt
# Date:        Nov 2016


#----------------------------------------------------------------------------#

#----------------------------------------------------------------------------#
#                               Control Section                              #
#----------------------------------------------------------------------------#

# set-up (DEFAULT)
#-------------------------------------------------#
current_date <- as.character(format(Sys.time(), "%d_%m_%Y")) 

# dependencies  - project specific (CUSTOM)
#----------------------------------------------------------------------------#

# project folders (these folders should be located in the cohort specific folder on 
# the server//should in the case of (a) the raw data folder contain the 
# relevant raw data files and (b) the code folder contain a control.R file 
# with the global project settings. 
modified_folder       <- "/data/zolab/pants/modified_data/"
output_folder         <- "/data/zolab/pants/output/"
output_folder_stored  <- "/data/zolab/pants/output_stored/"
code_folder           <- "/data/zolab/pants/code/"
vis_folder            <- "/data/zolab/pants/vis/"
temp_folder            <- "/data/zolab/pants/temp/"

# dependencies - feature_construction (DEFAULT)
#----------------------------------------------------------------------------#
wd_path   <- "/data/zolab/methods_new/base_code/feature_construction/"

libraries <-  "machine_code/library.R"
functions <-  "machine_code/function.R"
helpers   <-  "machine_code/helper.R"
theme     <-  "machine_code/theme.R"

# code dependencies - feature_construction (DEFAULT)
#--------------------------------#
dem_feature_code <- "file_code/rpdr_dem/dem_feature_code.R"  						
enc_feature_code <- "file_code/rpdr_enc/enc_feature_code.R"  						
dia_feature_code <- "file_code/rpdr_dia/dia_feature_code.R" 				    	 
prc_feature_code <- "file_code/rpdr_prc/prc_feature_code.R"  						
lvs_feature_code <- "file_code/rpdr_lvs/lvs_feature_code.R"  						
lab_feature_code <- "file_code/rpdr_lab/lab_feature_code.R"  						
med_feature_code <- "file_code/rpdr_med/med_feature_code.R"  						
mic_feature_code <- "file_code/rpdr_mic/mic_feature_code.R"  						
ed_feature_code  <- "file_code/edadmin_ed/ed_feature_code.R"     						
dia_oncdrs_feature_code   <- "file_code/oncdrs_dia/dia_oncdrs_feature_code.R"     
chemo_oncdrs_feature_code <- "file_code/oncdrs_chemo/chemo_oncdrs_feature_code.R" 
med_oncdrs_feature_code   <- "file_code/oncdrs_med/med_oncdrs_feature_code.R" 	  
enc_oncdrs_feature_code   <- "file_code/oncdrs_enc/enc_oncdrs_feature_code.R"  	
lab_oncdrs_feature_code   <- "file_code/oncdrs_lab/lab_oncdrs_feature_code.R"  	

enc_oncdrs_rpdr_feature_code         <- "file_code/oncdrs_rpdr_enc/enc_oncdrs_rpdr_feature_code.R"  				 
dia_oncdrs_rpdr_feature_code         <- "file_code/oncdrs_rpdr_dia/dia_oncdrs_rpdr_feature_code.R" 				     
med_chemo_oncdrs_rpdr_feature_code   <- "file_code/oncdrs_rpdr_med_chemo/med_chemo_oncdrs_rpdr_feature_code.R" 
lab_oncdrs_rpdr_feature_code 	     <- "file_code/oncdrs_rpdr_lab/lab_oncdrs_rpdr_feature_code.R"  				 
lno_targex_lvs_num_num_feature_code  <- "file_code/targex_rpdr_lvs/lno_targex_lvs_num_num_feature_code.R"                   

vis_update_code                      <- "/data/zolab/methods_new/vis/server_vis/vis_update.sh"  

# samples and cohorts (CUSTOM)
#----------------------------------------------------------------------------#

# cohort file
# the cohort file should have the following columns/column names - 
# c("outcome_id","outcome","empi", "pred_date") (can have additional column 
# names)
cohort_file <- paste0(modified_folder, "cross_cohort_model_cohort_alt_ext.Rds")

# cohort var modifications 
outcome_name_mod <- "obs_outcome"  # name of outcome var
pred_date_mod    <- "mic_date"     # name of outcome date var
outcome_id_mod   <- "cohort_id"    # name of outcome id
cohort_mod       <- "ml_subset==1" # expression to be used for subsetting (NA if no subsetting is needed)

test_train       <- TRUE    # cohort contains test/train flag
test_train_mod   <- "train" # name of test/train flg

# file name extension for generated output (cohort_name) - identify all output
cohort_name_prefix <- "cross_cohort_model_cohort_alt__excl_ed_patient__first_in_seq__dem_check"
cohort_id <- "1"

cohort_name <- paste0(cohort_name_prefix, "_", cohort_id) 

## optional (CUSTOM) - cohort name for which to read in pre-assembled features
cohort_name_assemble <- cohort_name
# cohort_name_assemble <- [exisiting cohort name]

# path to raw (i.e. master) data sets (CUSTOM)
#----------------------------------------------------------------------------#

# default files from data_repo
rpdr_dem_master_bwh_ed_100k <- '/data/zolab/master_data/data_repo/partners/up_to_date/data/rpdr_dem/rpdr_dem_master_bwh_ed_100k.Rds'
rpdr_dem_master_pe <- '/data/zolab/master_data/data_repo/partners/up_to_date/data/rpdr_dem/rpdr_dem_master_pe.Rds'
rpdr_dem_master_icmp <- '/data/zolab/master_data/data_repo/partners/up_to_date/data/rpdr_dem/rpdr_dem_master_icmp.Rds'
rpdr_dem_master_hdlab <- '/data/zolab/master_data/data_repo/partners/up_to_date/data/rpdr_dem/rpdr_dem_master_hdlab.Rds'

rpdr_mic_master_bwh_ed_100k <- '/data/zolab/master_data/data_repo/partners/up_to_date/data/rpdr_mic/rpdr_mic_master_bwh_ed_100k.Rds'
rpdr_mic_master_pe <- '/data/zolab/master_data/data_repo/partners/up_to_date/data/rpdr_mic/rpdr_mic_master_pe.Rds'
rpdr_mic_master_icmp <- '/data/zolab/master_data/data_repo/partners/up_to_date/data/rpdr_mic/rpdr_mic_master_icmp.Rds'
rpdr_mic_master_hdlab <- '/data/zolab/master_data/data_repo/partners/up_to_date/data/rpdr_mic/rpdr_mic_master_hdlab.Rds'

rpdr_dia_master_bwh_ed_100k <- '/data/zolab/master_data/data_repo/partners/up_to_date/data/rpdr_dia/rpdr_dia_master_bwh_ed_100k.Rds'
rpdr_dia_master_pe <- '/data/zolab/master_data/data_repo/partners/up_to_date/data/rpdr_dia/rpdr_dia_master_pe.Rds'
rpdr_dia_master_icmp <- '/data/zolab/master_data/data_repo/partners/up_to_date/data/rpdr_dia/rpdr_dia_master_icmp.Rds'
rpdr_dia_master_hdlab <- '/data/zolab/master_data/data_repo/partners/up_to_date/data/rpdr_dia/rpdr_dia_master_hdlab.Rds'

rpdr_enc_master_bwh_ed_100k <- '/data/zolab/master_data/data_repo/partners/up_to_date/data/rpdr_enc/rpdr_enc_master_bwh_ed_100k.Rds'
rpdr_enc_master_pe <- '/data/zolab/master_data/data_repo/partners/up_to_date/data/rpdr_enc/rpdr_enc_master_pe.Rds'
rpdr_enc_master_icmp <- '/data/zolab/master_data/data_repo/partners/up_to_date/data/rpdr_enc/rpdr_enc_master_icmp.Rds'
rpdr_enc_master_hdlab <- '/data/zolab/master_data/data_repo/partners/up_to_date/data/rpdr_enc/rpdr_enc_master_hdlab.Rds'

rpdr_lvs_master_bwh_ed_100k <- '/data/zolab/master_data/data_repo/partners/up_to_date/data/rpdr_lvs/rpdr_lvs_master_bwh_ed_100k.Rds'
rpdr_lvs_master_pe <- '/data/zolab/master_data/data_repo/partners/up_to_date/data/rpdr_lvs/rpdr_lvs_master_pe.Rds'
rpdr_lvs_master_icmp <- '/data/zolab/master_data/data_repo/partners/up_to_date/data/rpdr_lvs/rpdr_lvs_master_icmp.Rds'
rpdr_lvs_master_hdlab <- '/data/zolab/master_data/data_repo/partners/up_to_date/data/rpdr_lvs/rpdr_lvs_master_hdlab.Rds'

rpdr_prc_master_bwh_ed_100k <- '/data/zolab/master_data/data_repo/partners/up_to_date/data/rpdr_prc/rpdr_prc_master_bwh_ed_100k.Rds'
rpdr_prc_master_pe <- '/data/zolab/master_data/data_repo/partners/up_to_date/data/rpdr_prc/rpdr_prc_master_pe.Rds'
rpdr_prc_master_icmp <- '/data/zolab/master_data/data_repo/partners/up_to_date/data/rpdr_prc/rpdr_prc_master_icmp.Rds'
rpdr_prc_master_hdlab <- '/data/zolab/master_data/data_repo/partners/up_to_date/data/rpdr_prc/rpdr_prc_master_hdlab.Rds'

rpdr_med_master_bwh_ed_100k <- '/data/zolab/master_data/data_repo/partners/up_to_date/data/rpdr_med/rpdr_med_master_bwh_ed_100k.Rds'
rpdr_med_master_pe <- '/data/zolab/master_data/data_repo/partners/up_to_date/data/rpdr_med/rpdr_med_master_pe.Rds'
rpdr_med_master_icmp <- '/data/zolab/master_data/data_repo/partners/up_to_date/data/rpdr_med/rpdr_med_master_icmp.Rds'
rpdr_med_master_hdlab <- '/data/zolab/master_data/data_repo/partners/up_to_date/data/rpdr_med/rpdr_med_master_hdlab.Rds'

rpdr_lab_master_bwh_ed_100k <- '/data/zolab/master_data/data_repo/partners/up_to_date/data/rpdr_lab/rpdr_lab_master_bwh_ed_100k.Rds'
rpdr_lab_master_pe <- '/data/zolab/master_data/data_repo/partners/up_to_date/data/rpdr_lab/rpdr_lab_master_pe.Rds'
rpdr_lab_master_icmp <- '/data/zolab/master_data/data_repo/partners/up_to_date/data/rpdr_lab/rpdr_lab_master_icmp.Rds'
rpdr_lab_master_hdlab <- '/data/zolab/master_data/data_repo/partners/up_to_date/data/rpdr_lab/rpdr_lab_master_hdlab.Rds'


# rpdr
# note: rpdr_dem_master_bwh_ed_100k, etc. - correspond to paths
dem_file_mod <- list(rpdr_dem_master_bwh_ed_100k, rpdr_dem_master_pe, rpdr_dem_master_icmp, 
	rpdr_dem_master_hdlab)  
mic_file_mod <- list(rpdr_mic_master_bwh_ed_100k, rpdr_mic_master_pe, rpdr_mic_master_icmp, 
	rpdr_mic_master_hdlab)  
dia_file_mod <- list(rpdr_dia_master_bwh_ed_100k, rpdr_dia_master_pe, rpdr_dia_master_icmp, 
	rpdr_dia_master_hdlab) 
enc_file_mod <- list(rpdr_enc_master_bwh_ed_100k, rpdr_enc_master_pe, rpdr_enc_master_icmp, 
	rpdr_enc_master_hdlab)
lvs_file_mod <- list(rpdr_lvs_master_bwh_ed_100k, rpdr_lvs_master_pe, rpdr_lvs_master_icmp, 
	rpdr_lvs_master_hdlab) 
prc_file_mod <- list(rpdr_prc_master_bwh_ed_100k, rpdr_prc_master_pe, rpdr_prc_master_icmp, 
	rpdr_prc_master_hdlab)
med_file_mod <- list(rpdr_med_master_bwh_ed_100k, rpdr_med_master_pe, rpdr_med_master_icmp, 
	rpdr_med_master_hdlab)
lab_file_mod <- list(rpdr_lab_master_bwh_ed_100k, rpdr_lab_master_pe, rpdr_lab_master_icmp, 
	rpdr_lab_master_hdlab)

# oncdrs
chemo_oncdrs_file_mod <- oncdrs_chemo_master_dfci
med_oncdrs_file_mod   <- oncdrs_med_master_dfci
enc_oncdrs_file_mod   <- oncdrs_enc_master_dfci
lab_oncdrs_file_mod   <- oncdrs_lab_master_dfci
dia_oncdrs_file_mod   <- oncdrs_dia_master_dfci

# edamin
ed_enc_file_mod <- edadmin_ed_master_bwh_ed_100k

# targex
#rpdr_lno_targex_lvs_num <- list(rpdr_lno_targex_lvs_num_num_bwh_ed_100k, rpdr_lno_targex_lvs_num_num_pe, 
#	rpdr_lno_targex_lvs_num_num_icmp, rpdr_lno_targex_lvs_num_num_hdlab)

# feature lists - compile/assemble
#----------------------------------------------------------------------------#

# features to assemble (CUSTOM)
#--------------------------------#
assemble_list <- list(
	## main - rpdr/edadmin
	"dem", "prc", "lvs", "mic", "ed", "enc", 	
	# "lab", "med", "dia", 
	# oncdrs
	# "dia_oncdrs", "chemo_oncdrs", "med_oncdrs","lab_oncdrs",
	# combined
	"dia_oncdrs_rpdr", "med_chemo_oncdrs_rpdr", "lab_oncdrs_rpdr",
	"enc_oncdrs"
)


# features to compile (CUSTOM)
#--------------------------------#
compile_list <- list(
	## main - rpdr/edadmin
	"dem", "prc", "lvs", "mic", "ed", "enc", 	
	# "lab", "med", "dia", 
	# oncdrs
	# "dia_oncdrs", "chemo_oncdrs", "med_oncdrs","lab_oncdrs",
	# combined
	"dia_oncdrs_rpdr", "med_chemo_oncdrs_rpdr", "lab_oncdrs_rpdr",
	"enc_oncdrs"
)

# feature selection
#----------------------------------------------------------------------------#

# manual (de)selection (step #1)  (CUSTOM)
#--------------------------------#
# variable_list_file <- paste0(code_folder, "feature_construction/variable_list.csv") ## SET TO USE DEFAULT VAR_LIST FILE (INCLUDED IN PACKAGE)

variable_list_file_selection <- "var_all"
# variable_list_file_selection <- "var_basic"
# variable_list_file_selection <- "var_basic_merged"

# missingness (quant/indic vars) (step #2)  (CUSTOM)
#--------------------------------#
# omit data points that are missing for > quant_missing_threshold threshold % of observations (numeric/factor vars) (max, ... , longest timframes) &
# omit data points that do not occur (==0) for > than indic_missing_threshold (max, ... , longest timframes) 
# threshold of observations (interger)

# select whether or not to impute missing indicator variables to 0 (TRUE - impute - ->0 ) 
# miss_imp  <- FALSE
miss_imp  <- TRUE

#### missing_imp  <- FALSE
#---------------#
# quant_missing_threshold <- list(100,100,100)
# indic_missing_threshold <- list(100,100,100)

# quant_missing_threshold <- list(30,50,40)
# indic_missing_threshold <- list(30,50,40)

# non_impute_var_cat <- "^(lab|lvs|mic|dia)"

#### missing_imp  <- TRUE
#---------------#
quant_missing_threshold <- list(100,100,100)
indic_missing_threshold <- list(100,100,100)
	
# quant_missing_threshold <- list(30,50,40)
# indic_missing_threshold <- list(30,50,40)	

# timeframe splits (CUSTOM)
#----------------------------------------------------------------------------#

############################# No time split ################################

#### time_split <- FALSE
#---------------#
# time_split <- FALSE

# timeframe_long <- 365L
# timeframe_long_name <- "timeframe_0m_12m"

# timeframe_max = timeframe_long
# timeframe_max_name <- "max"

# name_ext_name <- c(timeframe_long_name)
# name_ext_name_extended <- c("timeframe_max",name_ext_name)

# name_ext <- c("_long")
# name_ext_extended <- c("_max", name_ext)

##########################################################################

######################### Do the time split #############################

#### time_split <- TRUE
#---------------#
time_split <- TRUE

# timeframe_outcome <- 0L
# timeframe_outcome_name <- "outcome_day"

timeframe_short <- 30L
timeframe_short_name <- "timeframe_1m"

timeframe_long <- 365L
timeframe_long_name <- "timeframe_1m_12m"

timeframe_max = timeframe_long
timeframe_max_name <- "max"

# name_ext <- c("_outcome", "_short", "_long")
# name_ext_name <- c(timeframe_outcome_name, timeframe_short_name, 
# 	timeframe_long_name)
name_ext_name <- c(timeframe_short_name, timeframe_long_name)
name_ext_name_extended <- c("timeframe_max",name_ext_name)

# name_ext <- c("_outcome", "_short", "_long")
# name_ext_extended <- c("_max", name_ext)
name_ext <- c("_short", "_long")
name_ext_extended <- c("_max", name_ext)

###############################################################################


# leakage control (CUSTOM)
#----------------------------------------------------------------------------#

# NA - omit no data / 0 - omit day of outcome  / >1 omit date 
# of outcome to date of outcome - leak_day

leak_dem_day 	<- 0
leak_enc_day 	<- 0
leak_dia_day 	<- 0
leak_prc_day 	<- 0
leak_lvs_day 	<- 0
leak_lab_day 	<- 0
leak_med_day 	<- 0
leak_mic_day 	<- 0
leak_ed_day  	<- 0

leak_list <- c("leak_dem_day","leak_enc_day", "leak_dia_day", "leak_prc_day", 
	"leak_lvs_day", "leak_lab_day", "leak_med_day","leak_med_day", "leak_mic_day",
	"leak_ed_day")

# align 
leak_oncdrs_dia_day   <- leak_dia_day
leak_oncdrs_chemo_day <- leak_med_day
leak_oncdrs_med_day   <- leak_med_day
leak_oncdrs_enc_day   <- leak_enc_day
leak_oncdrs_lab_day   <- leak_lab_day

# imputation
#----------------------------------------------------------------------------#
fill_na <- FALSE
# fill_na <- TRUE
# fill_na_method <- "median_imputation"

# output 
#----------------------------------------------------------------------------#

# settings (CUSTOM)
#--------------------------------#

# feature vital signs 
feature_vital_sign_loc <-  paste0(output_folder_stored, 
	"feature_vital_sign.csv")

# vis
port_remote     <- 9015 # port on which to display feature vis

# backend settings (DEFAULT)
#--------------------------------#

# vis parameters and dependencies
vis_folder_path <- paste0(code_folder, "machine/feature_vis/")
vis_file_path   <- paste0("vis_basic.html")

vis_folder_spec <- paste0(vis_folder, "feature_structure", "_", cohort_name, "_", 
	variable_list_file_selection, "_", paste0(quant_missing_threshold, collapse="_"),"_",
	paste0(indic_missing_threshold, collapse="_"), "_", "port_", port_remote)
vis_loc <- paste0(vis_folder_spec, "/feature_structure.csv")

# vital signs parameters
update_name <- "//"
update_date <- current_date

# execution settings (DEFAULT)
#----------------------------------------------------------------------------#
# other settings 
miss_hist <- FALSE       # generate missingness histograms ?

# production vs. testing (DEFAULT)
#----------------------------------------------------------------------------#
test_raw_file <- FALSE  # use subset of data files ?
test_row 	  <- 20000  # rows in subsets of cohort/data files 


#----------------------------------------------------------------------------#
#                                  End                                       #
#----------------------------------------------------------------------------#
