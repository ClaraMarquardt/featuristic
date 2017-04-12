#----------------------------------------------------------------------------#

#' @title Initialise feature construction machinery. 
#'
#' @description \
#'
#' @export
#' @import data.table
#' @return
#' @examples

feature_initialisation <- function() {	

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
		
	# load the variable list  & check version
	# ----------------------------------------------- #
	# variable_list <- fread(variable_list_file)
	variable_list <- copy(variable_list_default)
		
	# specify variable selection 
	# ----------------------------------------------- #
	if (!is.na(variable_list_file_selection)) {
		setnames(variable_list, variable_list_file_selection, "include")
	}

	# return (or assign globally)
	# ----------------------------------------------- #

	# return(list(cohort=cohort, cohort_key_var=cohort_key_var, cohort_key_var_merge=cohort_key_var_merge, 
	# cohort_extra_col=cohort_extra_col, variable_list=variable_list))
	
	cohort <<- cohort
	cohort_key_var_merge <<- cohort_key_var_merge
	cohort_key_var <<- cohort_key_var
	cohort_extra_col <<- cohort_extra_col
	variable_list <<- variable_list
}

#----------------------------------------------------------------------------#
