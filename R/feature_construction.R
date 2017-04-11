#----------------------------------------------------------------------------#

#' @title Generate features (Stage I).
#'
#' @description \
#'
#' @export
#' @import data.table
#' @param control_file_path Path to control file (see template incl. in package)
#' @return
#' @examples

feature_construction <- function(control_file_path) {

	# set-up
	#-------------------------------------------------#
	current_date <- as.character(format(Sys.time(), "%d_%m_%Y")) 
	
	print(control_file_path)
	source(control_file_path)
	
	# dependencies
	#-------------------------------------------------#
	setwd(wd_path) 
	source(libraries)
	source(functions)
	source(helpers)
	source(theme)
	
	required_helpers <- c(
	# No helpers required for feature compilation)
	)
	load_helpers(required_helpers)
		
	#----------------------------------------------------------------------------#
	#                		  MODIFY / FORMAT COHORT/VAR LIST                    #
	#----------------------------------------------------------------------------#
	
	cohort <- readRDS(cohort_file)
	
	print(nrow(cohort))
	
	# modify cohort (according to specification in control file)
	# ----------------------------------------------- #
	if(!is.na(outcome_name_mod)) {
		setnames(cohort, outcome_name_mod, "outcome")
	}
	
	if(!is.na(pred_date_mod)) {
		setnames(cohort, pred_date_mod, "pred_date")
	}
	
	if(!is.na(outcome_id_mod)) {
		setnames(cohort, outcome_id_mod, "outcome_id")
	}
	
	if(!is.na(cohort_mod)) {
		cohort <- cohort[eval(parse(text=cohort_mod))]
	}
	
	print(nrow(cohort))
	
	# cohort variables names
	# ----------------------------------------------- #
	
	ifelse(all(c("outcome_id", "outcome", "pred_date", "empi") %in% names(cohort)), 
	  "cohort columns names correct", stop("cohort column names incorrect"))
	
	print(sprintf("number of observations in cohort: %d", nrow(cohort)))
	
	cohort_key_var <- c("outcome_id", "pred_date", "empi")
	
	# store 'extra' cohort variables for later merging with final prediction set
	# ----------------------------------------------- #
	
	cohort_extra_col <- cohort[, mget(c("outcome_id", setdiff(names(cohort), 
		cohort_key_var)))]
	
	cat(sprintf("extra cohort columns: %s", 
		paste0(c("\n", names(cohort_extra_col),"\n"),collapse='\n')))
	
	# format cohort dates & generate timeframes & setkeys/generate numeric dates 
	# (for foverlaps) & define key variables to be used for joining 
	# ----------------------------------------------- #
	
	invisible(parse_date(cohort, c("pred_date")))
	
	for (i in name_ext) {
		cohort[,c(paste0("pred_date_beg",i)):=pred_date-
		  get(paste0("timeframe", i))]
	}
	
	setkeyv(cohort, c("empi", paste0("pred_date_beg", name_ext[length(name_ext)]), 
		"pred_date"))
	
	cohort_key_var_merge <- c("empi", "outcome_id", "pred_date", 
		grep("pred_date_", names(cohort), value=T))
	
	if (test_cohort==TRUE) {
		assign("cohort_copy", copy(cohort), envir = .GlobalEnv)
		assign("cohort", cohort[1:test_row], envir = .GlobalEnv)
	}
	
	# load the variable list  & check version
	# ----------------------------------------------- #
	# variable_list <- fread(variable_list_file)
	variable_list <- copy(variable_list_default)
		
	# specify variable selection 
	# ----------------------------------------------- #
	if (!is.na(variable_list_file_selection)) {
		setnames(variable_list, variable_list_file_selection, "include")
	}
	
	#----------------------------------------------------------------------------#
	#                		       FEATURE GENERATION                            #
	#----------------------------------------------------------------------------#
		
	# rpdr
	#----------------------------------------------------------------------------#
	
	# dem
	#----------------------------------------------------------------------------#
	if("dem" %in% assemble_list) {
	
		source(dem_feature_code)
		dem_feature <- dem_feature_gen()
	
		feature_check("dem_feature")
	
		#  save 
		saveRDS(dem_feature, paste0(modified_folder, "dem_feature_", 
			cohort_name, ".Rds"))
	
		rm(dem_feature)
	}
	
	# enc
	#----------------------------------------------------------------------------#
	if("enc" %in% assemble_list) {
	
		source(enc_feature_code)
		enc_feature <- enc_feature_gen()
	
		feature_check("enc_feature")
	
		#  save 
		saveRDS(enc_feature, paste0(modified_folder, "enc_feature_", 
			cohort_name, ".Rds"))
		
		rm(enc_feature)
	}
	
	# dia
	#----------------------------------------------------------------------------#
	if("dia" %in% assemble_list) {
	
		source(dia_feature_code)
		dia_feature <- dia_feature_gen(dia_file_mod, leak_dia_day)
	
		feature_check("dia_feature")
	
		#  save 
		saveRDS(dia_feature, paste0(modified_folder, "dia_feature_", 
			cohort_name, ".Rds"))
		
		rm(dia_feature)
	}
	
	
	# prc
	#----------------------------------------------------------------------------#
	if("prc" %in% assemble_list) {
	
		source(prc_feature_code)
		prc_feature <- prc_feature_gen()
	
		feature_check("prc_feature")
	
		#  save 
		saveRDS(prc_feature, paste0(modified_folder, "prc_feature_", 
			cohort_name, ".Rds"))
		
		rm(prc_feature)
	}
	
	# lvs
	#----------------------------------------------------------------------------#
	if("lvs" %in% assemble_list) {
	
		source(lvs_feature_code)
		lvs_feature <- lvs_feature_gen(lvs_file_mod, leak_lvs_day)
	
		feature_check("lvs_feature")
	
		#  save 
		saveRDS(lvs_feature, paste0(modified_folder, "lvs_feature_", 
			cohort_name, ".Rds"))
		
		rm(lvs_feature)
	}
	
	
	# lab
	#----------------------------------------------------------------------------#
	if("lab" %in% assemble_list) {
	
		source(lab_feature_code)
		lab_feature <- lab_feature_gen(lab_file_mod, leak_lab_day)
	
		feature_check("lab_feature")
	
		#  save 
		saveRDS(lab_feature, paste0(modified_folder, "lab_feature_", 
			cohort_name, ".Rds"))
		
		rm(lab_feature)
	}
	
	
	# med
	#----------------------------------------------------------------------------#
	if("med" %in% assemble_list) {
	
		source(med_feature_code)
		med_feature <- med_feature_gen(med_file_mod, leak_med_day)
	
		feature_check("med_feature")
	
		#  save 
		saveRDS(med_feature, paste0(modified_folder, "med_feature_", 
			cohort_name, ".Rds"))
		
		rm(med_feature)
	}
	
	# mic
	#----------------------------------------------------------------------------#
	if("mic" %in% assemble_list) {
	
		source(mic_feature_code)
		mic_feature <- mic_feature_gen()
	
		feature_check("mic_feature")
	
		#  save 
		saveRDS(mic_feature, paste0(modified_folder, "mic_feature_", 
			cohort_name, ".Rds"))
		
		rm(mic_feature)
	}
	
	
	# edadmin
	#----------------------------------------------------------------------------#
	
	# ed
	#----------------------------------------------------------------------------#
	if("ed" %in% assemble_list) {
	
		source(ed_feature_code)
		ed_feature <- ed_feature_gen()
	
		feature_check("ed_feature")
	
		#  save 
		saveRDS(ed_feature, paste0(modified_folder, "ed_feature_", 
			cohort_name, ".Rds"))
		
		rm(ed_feature)
	}
	
	
	# oncdrs
	#----------------------------------------------------------------------------#
	
	# dia_oncdrs
	#----------------------------------------------------------------------------#
	if("dia_oncdrs" %in% assemble_list) {
	
		source(dia_oncdrs_feature_code)
		dia_oncdrs_feature <- dia_oncdrs_feature_gen()
	
		feature_check("dia_oncdrs_feature")
	
		#  save 
		saveRDS(dia_oncdrs_feature, paste0(modified_folder, "dia_oncdrs_feature_", 
			cohort_name, ".Rds"))
		
		rm(dia_oncdrs_feature)
	}
	
	
	# med_oncdrs
	#----------------------------------------------------------------------------#
	if("med_oncdrs" %in% assemble_list) {
	
		source(med_oncdrs_feature_code)
		med_oncdrs_feature <- med_oncdrs_feature_gen()
	
		feature_check("med_oncdrs_feature")
	
		#  save 
		saveRDS(med_oncdrs_feature, paste0(modified_folder, "med_oncdrs_feature_", 
			cohort_name, ".Rds"))
		
		rm(med_oncdrs_feature)
	}
	
	
	# chemo_oncdrs
	#----------------------------------------------------------------------------#
	if("chemo_oncdrs" %in% assemble_list) {
	
		source(chemo_oncdrs_feature_code)
		chemo_oncdrs_feature <- chemo_oncdrs_feature_gen()
	
		feature_check("chemo_oncdrs_feature")
	
		#  save 
		saveRDS(chemo_oncdrs_feature, paste0(modified_folder, "chemo_oncdrs_feature_", 
			cohort_name, ".Rds"))
	
		rm(chemo_oncdrs_feature)
	
	}
	
	# enc_oncdrs
	#----------------------------------------------------------------------------#
	if("enc_oncdrs" %in% assemble_list) {
	
		source(enc_oncdrs_feature_code)
		enc_oncdrs_feature <- enc_oncdrs_feature_gen()
	
		feature_check("enc_oncdrs_feature")
	
		#  save 
		saveRDS(enc_oncdrs_feature, paste0(modified_folder, "enc_oncdrs_feature_", 
			cohort_name, ".Rds"))
	
		rm(enc_oncdrs_feature)
		
	}
	
	# lab_oncdrs
	#----------------------------------------------------------------------------#
	if("lab_oncdrs" %in% assemble_list) {
	
		source(lab_oncdrs_feature_code)
		lab_oncdrs_feature <- lab_oncdrs_feature_gen()
	
		feature_check("lab_oncdrs_feature")
	
		#  save 
		saveRDS(lab_oncdrs_feature, paste0(modified_folder, "lab_oncdrs_feature_", 
			cohort_name, ".Rds"))
	
		rm(lab_oncdrs_feature)
		
	}
	
	# oncdrs + rpdr
	#----------------------------------------------------------------------------#
	
	
	# dia_oncdrs_rpdr
	#----------------------------------------------------------------------------#
	if("dia_oncdrs_rpdr" %in% assemble_list) {
	
		source(dia_oncdrs_rpdr_feature_code)
		dia_oncdrs_rpdr_feature <- dia_oncdrs_rpdr_feature_gen()
	
		feature_check("dia_oncdrs_rpdr_feature")
	
		#  save 
		saveRDS(dia_oncdrs_rpdr_feature, paste0(modified_folder, "dia_oncdrs_rpdr_feature_", 
			cohort_name, ".Rds"))
	
		rm(dia_oncdrs_rpdr_feature)
		
	}
	
	
	# med_chemo_oncdrs_rpdr
	#----------------------------------------------------------------------------#
	if("med_chemo_oncdrs_rpdr" %in% assemble_list) {
	
		source(med_chemo_oncdrs_rpdr_feature_code)
		med_chemo_oncdrs_rpdr_feature <- med_chemo_oncdrs_rpdr_feature_gen()
	
		feature_check("med_chemo_oncdrs_rpdr_feature")
	
		#  save 
		saveRDS(med_chemo_oncdrs_rpdr_feature, paste0(modified_folder, "med_chemo_oncdrs_rpdr_feature_", 
			cohort_name, ".Rds"))
	
		rm(med_chemo_oncdrs_rpdr_feature)
		
	}
	
	# lab_oncdrs_rpdr
	#----------------------------------------------------------------------------#
	if("lab_oncdrs_rpdr" %in% assemble_list) {
	
		source(lab_oncdrs_rpdr_feature_code)
		lab_oncdrs_rpdr_feature <- lab_oncdrs_rpdr_feature_gen()
	
		feature_check("lab_oncdrs_rpdr_feature")
	
		#  save 
		saveRDS(lab_oncdrs_rpdr_feature, paste0(modified_folder, "lab_oncdrs_rpdr_feature_", 
			cohort_name, ".Rds"))
	
		rm(lab_oncdrs_rpdr_feature)
		
	}
	

}	

#----------------------------------------------------------------------------#
