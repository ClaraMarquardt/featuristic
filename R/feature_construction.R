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
	source(control_file_path) # sourced globally
	
	# initialise
	#-------------------------------------------------#	
	feature_initialisation()

	#----------------------------------------------------------------------------#
	#                		       FEATURE GENERATION                            #
	#----------------------------------------------------------------------------#
		
	inv_lapply(assemble_list, function(feature_set) {


		# source feature generation
		temp_feature <- indiv_feature_gen(feature_set, cohort=cohort, 
			cohort_key_var_merge=cohort_key_var_merge, cohort_key_var=cohort_key_var)
	
		feature_check(temp_feature, paste0(feature_set, "_feature"), cohort_dt=cohort,
			cohort_key_var_list=cohort_key_var, extra_var_list=names(cohort_extra_col))

		#  save 
		saveRDS(temp_feature, paste0(modified_folder, paste0(feature_set, "_feature_"), 
			cohort_name, ".Rds"))
	
		rm("temp_feature")

	})	

}	

#----------------------------------------------------------------------------#
