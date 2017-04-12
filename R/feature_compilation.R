#----------------------------------------------------------------------------#

#' @title Compile features (Stage II).
#'
#' @description \
#'
#' @export
#' @import data.table
#' @param control_file_path Path to control file (see template incl. in package)
#' @return
#' @examples

feature_compilation <- function(control_file_path) {
	
	# set-up
	#-------------------------------------------------#
	current_date <- as.character(format(Sys.time(), "%d_%m_%Y")) 
	
	print(control_file_path)
	source(control_file_path) # sourced globally
	
	# initialise
	#-------------------------------------------------#	
	feature_initialisation()
	
	#----------------------------------------------------------------------------#
	#                        COMPILE THE FEATURES                               #
	#----------------------------------------------------------------------------#

	# load the feature files  & set key to outcome_id 
    #----------------------------------------------------------------------------#
	invisible(lapply (compile_list, function(x) assign(paste0(x, "_feature"), 
	  setkey(as.data.table(readRDS(paste0(modified_folder, x, "_feature_", 
	  cohort_name_assemble, ".Rds"))), outcome_id), envir = sys.frame(sys.parent(n=2)))))

	# temp section - ensure backward compatibility
	#-----------------------------#
    inv_lapply(mget(paste0(compile_list, "_feature")), function(x) {

    	if("outcome_date" %in% names(x)) {
    		setnames(x, "outcome_date", "pred_date")
    	}
    
    })

	# merge the feature files with the cohort - merge on outcome_id & merge in 
	# cohort extra columns
    #----------------------------------------------------------------------------#
 	pred_set <- Reduce(mymerge, mget(paste0(compile_list, "_feature")))

    if (nrow(pred_set)!=nrow(cohort)) {
    
    	## in case subsetting post assembly
		print(sprintf("subsetting prediction set ---- number of observations in pred_set: %d vs. number of observations 
			in cohort: %d", nrow(pred_set), nrow(cohort)))

    	pred_set <- pred_set[outcome_id %in% cohort$outcome_id]

    }


	if (nrow(cohort_extra_col)>0) {
	  pred_set <- cohort_extra_col[pred_set, on=c("outcome_id")]
	}


	print(sprintf("number of observations in pred_set: %d vs. number of observations 
		in cohort: %d", nrow(pred_set), nrow(cohort)))

	gc()

	inv_lapply(paste0(compile_list,"_feature"), function(x) rm(x))

	gc()

	# ensure that time_min, time_max variables are included in cohort_extra_col
	#----------------------------------------------------------------------------#
	cohort_extra_col <- cbind(cohort_extra_col, pred_set[, 
		mget(grep("_time_min|_time_max", names(pred_set), value=T))])

	raw_feature_coll <- feature_coll(pred_set)

	# save (temp)
	saveRDS(pred_set, paste0(temp_folder, "pred_set_temp_", cohort_name, ".Rds"))
	saveRDS(cohort_extra_col, paste0(temp_folder, "cohort_extra_col_temp_", 
		cohort_name, ".Rds"))

	write.csv(raw_feature_coll, paste0(temp_folder, "raw_feature_coll_", cohort_name, "_", 
		current_date, ".csv"), row.names=F)
   
	# subset features - missingess/completeness
    #----------------------------------------------------------------------------#
	obs_check(pred_set)


	# subset features - feature selection (manual)
	#---------------------------------------------#
	var_list <- paste(paste0(variable_list[include==1]$var_cat, "xx"), collapse="|")
	col_omit_select <- which(!(gsub("\\.\\.", "xx", colnames(pred_set)) %like% var_list))
	col_omit_select  <- names(pred_set[, col_omit_select , with=F])
	col_omit_select  <- setdiff(col_omit_select , union(cohort_key_var, names(cohort_extra_col)))

	print(sprintf("columns that are deselected (%d)", length(unique(col_omit_select))))
	deselect_col <- length(unique(col_omit_select))

	pred_set[, col_omit_select:=NULL, with=F]

	obs_check(pred_set)

	# identify quant/qual var
    #---------------------------------------------#
    variable_type <- data.table(var_type=sapply(pred_set, class))
	variable_type[, var_type_count:=.N, by=c("var_type")]
	print(unique(variable_type, by=c("var_type")))

	num_factor_var   <- setdiff(names(pred_set[, (c(which(sapply(pred_set, function(x) 
		class(x)[1]) %in% c("numeric", "factor")))), with=F]), union(cohort_key_var, 
		names(cohort_extra_col)))
	write.csv(num_factor_var, paste0(temp_folder, "_num_var.csv"), row.names=F)

	indic_var <- setdiff(names(pred_set[, (c(which(sapply(pred_set, function(x) 
		class(x)[1]) %in% c("integer")))), with=F]), union(cohort_key_var, 
		names(cohort_extra_col)))
	write.csv(indic_var, paste0(temp_folder, "_indic_var.csv"), row.names=F)
	print("a")
	if (miss_imp==FALSE) {

		num_factor_var_mod <- setdiff(unique(c(num_factor_var, grep(non_impute_var_cat, 
			names(pred_set), value=T))),  c(cohort_key_var, names(cohort_extra_col), 
			grep("_days_to_last", names(pred_set),value=T)))
	    write.csv(num_factor_var_mod, paste0(temp_folder, "_num_factor_var_mod_raw.csv"), row.names=F)

		indic_var_mod      <- setdiff(indic_var, num_factor_var_mod)
		write.csv(indic_var_mod, paste0(temp_folder, "_indic_var_mod.csv"), row.names=F)

		indic_var <- indic_var_mod
		num_factor_var <- num_factor_var_mod

	} 
	print("a")

    # Missingness output - documentation (1)
	# ----------------------------------------------------------------------------#
	
	if (miss_hist==TRUE) {
	
		incomplete_hist(pred_set[, mget(setdiff(names(pred_set),
			union(names(cohort_extra_col), cohort_key_var)))], num_factor_var, indic_var, output_folder, 
			cohort_name, "raw")
	}
	print("a")


	# impose thresholds
	#---------------------------------------------#
	if (miss_imp==TRUE) {

		## no steps

	} else if (miss_imp==FALSE) {

		# indic vars mod - -0 -> NA
		#---------------------------------------------#
		pred_set_non_num <- pred_set[,mget(setdiff(names(pred_set), num_factor_var))]
		pred_set_num     <- pred_set[,mget(num_factor_var)]
		set_zero_na(pred_set_non_num, 0)

		pred_set <- cbind(pred_set_non_num, pred_set_num)
		
		rm(pred_set_num)
		rm(pred_set_non_num)
		gc()

	}
	print("a")

	# deal with missing (numeric data)
   	#---------------------------------------------#
	pred_set_missing <- sapply(pred_set[, mget(c(num_factor_var))], function(y) sum(is.na(y)))
	pred_set_missing_perc <- perc(pred_set_missing, (nrow(pred_set)), digit=0)
	 	print("a")

	# deal with 0,1 data (imputed dummy data)
	#---------------------------------------------#
	pred_set_zero <- sapply(pred_set[, mget(indic_var)], function(y) sum(y==0, na.rm=T))
	pred_set_zero_perc <- perc(pred_set_zero, (nrow(pred_set)), digit=0)	
		print("a")

	# identify 100% missing/0 observations
	#---------------------------------------------#
	col_omit_missing <- pred_set_missing_perc[pred_set_missing_perc==100]
	col_omit_zero    <- pred_set_zero_perc[pred_set_zero_perc == 100]	
		print("a")

	# deal with  missing  - ext (numeric data)
   	#---------------------------------------------#	
	for (i in 1:length(name_ext_extended)) {
		temp <- pred_set_missing_perc[names(pred_set_missing_perc) %like% paste0(name_ext_extended[i], "$") & 
			pred_set_missing_perc>quant_missing_threshold[[i]]]
			col_omit_missing <- c(col_omit_missing, temp)
	}
		print("a")

	# deal with 0,1 data  - ext (imputed dummy data)
	#---------------------------------------------#
	for (i in 1:length(name_ext_extended)) {
		temp <- pred_set_zero_perc[names(pred_set_zero_perc) %like% paste0(name_ext_extended[i], "$") & 
			pred_set_zero_perc>indic_missing_threshold[[i]]]
			col_omit_zero <- c(col_omit_zero, temp)
	}
 	print("a")

	col_omit_missing_name <- names(col_omit_missing)
	col_omit_zero_name    <- names(col_omit_zero)
	print("a")

	col_omit_missing <- col_omit_missing[names(col_omit_missing) %in% col_omit_missing_name]
	col_omit_zero    <- col_omit_zero[names(col_omit_zero) %in% col_omit_zero_name]

	col_omit <- unique(union(col_omit_missing_name, col_omit_zero_name))
	print("a")

	cat(sprintf("columns that are ommitted - all / more than %s percent of data points missing (%d) [numeric/factor] (%s) \n// all / more than %s percent of data points 0 (%d) [indic] (%s) \n// total omissions (%d) \n", 
		paste0(paste0(quant_missing_threshold, "%"), collapse=" / "), 
		length(unique(col_omit_missing_name)), paste0(name_ext_name_extended, collapse= " / "), 
		paste0(paste0(indic_missing_threshold, "%"), collapse=" / "), 
		length(unique(col_omit_zero_name)), paste0(name_ext_name_extended, collapse= " / "), 
		length(unique(col_omit))))
	print("a")

	zero_col <- length(unique(col_omit_zero_name))
	na_col   <- length(unique(col_omit_missing_name))
    	
    if (length(col_omit>0)) {
		pred_set[, c(col_omit):=NULL]
	}
	print("a")

	obs_check(pred_set)

    # Missingness output - documentation (2)
	# ----------------------------------------------------------------------------#
    variable_type <- data.table(var_type=sapply(pred_set, class))
	variable_type[, var_type_count:=.N, by=c("var_type")]
	print(unique(variable_type, by=c("var_type")))

	num_factor_var   <- setdiff(names(pred_set[, (c(which(sapply(pred_set, function(x) 
		class(x)[1]) %in% c("numeric","factor")))), with=F]), union(cohort_key_var, 
		names(cohort_extra_col)))
	indic_var <- setdiff(names(pred_set[, (c(which(sapply(pred_set, function(x) 
		class(x)[1]) %in% c("integer")))), with=F]), union(cohort_key_var, 
		names(cohort_extra_col)))

	if (miss_imp==FALSE) {

		num_factor_var_mod <- setdiff(unique(c(num_factor_var, grep(non_impute_var_cat, 
			names(pred_set), value=T))),  c(cohort_key_var, names(cohort_extra_col), 
			grep("_days_to_last", names(pred_set),value=T)))
	    write.csv(num_factor_var_mod, paste0(temp_folder, "_num_factor_var_mod_mod.csv"), row.names=F)

		indic_var_mod      <- setdiff(indic_var, num_factor_var_mod)
		write.csv(indic_var_mod, paste0(temp_folder, "_indic_var_mod.csv"), row.names=F)

		indic_var <- indic_var_mod
		num_factor_var <- num_factor_var_mod

	} 

	if (miss_hist==TRUE) {
	
		incomplete_hist(pred_set[, mget(setdiff(names(pred_set),
			union(names(cohort_extra_col), cohort_key_var)))], num_factor_var, indic_var,output_folder, 
			cohort_name, "mod")
	}

    # Median Imputation
	# ----------------------------------------------------------------------------#
	if (fill_na==TRUE) {

		missing <- sapply(pred_set[,mget(setdiff(names(pred_set),c(cohort_key_var,
			names(cohort_extra_col))))], function(y) sum(is.na(y)))

		number_of_obs <- ncol(pred_set[,mget(setdiff(names(pred_set),c(cohort_key_var,
			names(cohort_extra_col))))]) * 
			nrow(pred_set[,mget(setdiff(names(pred_set),c(cohort_key_var,
			names(cohort_extra_col))))])
		impute_value_perc <- perc(sum(missing), number_of_obs)

	    missing_var <- setdiff(names(missing[missing>0]), c(cohort_key_var, 
	    	names(cohort_extra_col)))

		pred_set_missing     <- pred_set[, mget(c(missing_var))]
		pred_set_non_missing <- pred_set[, mget(c(setdiff(names(pred_set), c(missing_var))))]
	
		pred_set_missing <- impute(pred_set_missing)

		pred_set <- cbind(pred_set_non_missing, pred_set_missing)

	    obs_check(pred_set)

	}

	# SAVE
	#---------------------------------------------#
	saveRDS(pred_set, file = paste0(modified_folder, "pred_set_raw_", 
		cohort_name, ".Rds"))

	write.csv(as.data.table(names(pred_set)), file=paste0(output_folder, 
		"pred_set_var_name_raw_", cohort_name, ".csv"),row.names=F)

	write.csv(unique(data.table(var_name=c(col_omit_missing_name, "x"), perc=c(col_omit_missing, "_")))[order(-perc)],
	 file=paste0(output_folder, "pred_set_col_omit_missing_", cohort_name, ".csv"),row.names=F)
	write.csv(unique(data.table(var_name=c(col_omit_zero_name,"x"), perc=c(col_omit_zero, "_")))[order(-perc)], 
		file=paste0(output_folder, "pred_set_col_omit_zero_", cohort_name, ".csv"),row.names=F)
	write.csv(as.data.table(c(col_omit_select, "x")), file=paste0(output_folder, 
		"pred_set_col_omit_select_", cohort_name, ".csv"),row.names=F)

	#----------------------------------------------------------------------------#
	#                        FORMAT THE FEATURES                                 #
	#----------------------------------------------------------------------------#

	# Store the unformatted column names & dates
	# ----------------------------------------------------------------------------#
	var_name_raw   <- copy(names(pred_set))
	var_name_final <- copy(names(pred_set))
	
	date_col <- pred_set[, c(which(sapply(pred_set, function(x) 
		class(x)[1]) %in% c("Date"))), with=F]
    date_col_table <- data.table(date=t(date_col[1]), 
    	var_name=names(date_col))

	setnames(date_col_table, "date.V1", "date")
	date_col_table[, date:=as.IDate(date, "%Y-%m-%d")]

	date_col_table <- rbindlist(list(date_col_table[var_name!="pred_date"], 
		data.table(var_name=c("pred_date_min", "pred_date_max"),
		date=c(as.IDate(min(pred_set$pred_date), "%Y-%m-%d"), 
		as.IDate(max(pred_set$pred_date), "%Y-%m-%d")))), use.names=T)

	print(date_col_table)

    pred_set <- pred_set[, !(c(which(sapply(pred_set, function(x) 
		class(x)[1]) %in% c("Date")))), with=F]
	
	var_name_final <- copy(names(pred_set))

	# Format the column names
	# ----------------------------------------------------------------------------#

	# main timeframes
	for (i in name_ext) {
	   var_name_final <- gsub(paste0(gsub("_", "", i), "$"), get(paste0("timeframe", i, "_name")),
	   	var_name_final)
	}

	# max timeframe
	var_name_final <- gsub("(?<!time)_max$", "_timeframe_max", var_name_final, perl=T)

	# difference timeframes
	for (i in 1:length(name_ext)) {
	   var_name_final <- gsub(paste0(name_ext[i], name_ext[i+1], "_diff", "$"), 
	   	paste0("_timeframe_diff",gsub("timeframe", "", name_ext_name[i]), 
	   		gsub("timeframe", "", name_ext_name[i+1])), var_name_final)
	}

	var_name_final <- gsub("max_diff$", "timeframe_diff_max", var_name_final)
    
    # rename
    setnames(pred_set, var_name_final)

    # add 'var' to identify features
	setnames(pred_set, setdiff(names(pred_set), c(cohort_key_var, names(cohort_extra_col))),
	 paste0("var_", setdiff(names(pred_set), c(cohort_key_var, names(cohort_extra_col)))))
	var_name_final <- copy(names(pred_set))

    # remove final _
    var_name_final <- gsub("_$", "", var_name_final)
	setnames(pred_set,var_name_final)

    # update varname vis 
    var_name_vis <- copy(var_name_final)

	
	# Reformat - integer / numeric
	# ----------------------------------------------------------------------------#

	# cast the factor columns such that factors are converted to independent indicator variables
	factor_var <- names(pred_set)[which(sapply(pred_set, function(x) class(x)[1]) %in% c("factor"))]
	pred_set[, c(factor_var):=lapply(.SD, function(x) gsub("[^a-zA-Z0-9\\._ ]", "", x)), .SDcols=factor_var]
	for (x in factor_var) {
		# what are the factors we are looking to cast? (be sure to remove NA as a factor!)
		factors <- setdiff(as.character(unique(pred_set[, get(x)])), NA)
		# do not do this if the factors are already indicators
		# if(all.equal(sort(factors), c("0", "1"))) 
		if(is.na(sum(as.numeric(factors)))){
			casting_formula <- as.formula(paste0("outcome_id + ... ~ ", as.character(x)))
			pred_set <- data.table(dcast(pred_set, casting_formula, fun.agg = length, value.var = x))
			# drop the NA column that is created when we cast a factor column with at least one NA value
			pred_set[, "NA" := NULL, with = FALSE]
			setnames(pred_set, as.character(factors), gsub(" ", "_", paste0(x, "_", factors)))
		}
	}

	# pred_set[, c(factor_var):=lapply(.SD, function(x) as.integer(x)), .SDcols=names(pred_set)[factor_var]]

    # format names
	# ----------------------------------------------------------------------------#
	setnames(pred_set, make.names(names(pred_set)))

    # Subset - omit date var & omit max var
	# ----------------------------------------------------------------------------#
	pred_set_final <- pred_set[, mget(setdiff(names(pred_set), 
		grep("_time_min|_time_max|_timeframe_max|timeframe_diff_max", 
		names(pred_set), value=T)))]


	final_feature_coll <- feature_coll(pred_set_final)

	# rename
    var_name_final <- gsub("var_([^_]*_)(.*)$", "var_\\2", names(pred_set_final))
    var_name_final <- gsub("_$", "", var_name_final)
    setnames(pred_set_final, var_name_final)


	write.csv(final_feature_coll, paste0(output_folder, "final_feature_coll_", 
		cohort_name, "_", current_date, ".csv"), row.names=F)



	# save
	# ----------------------------------------------------------------------------#
	saveRDS(pred_set, file = paste0(temp_folder, "pred_set_final_temp_", cohort_name,".Rds"))
	saveRDS(pred_set_final, file = paste0(modified_folder, "pred_set_final_", cohort_name,".Rds"))
	
	write.csv(as.data.table(names(pred_set_final)), file = paste0(output_folder, 
		"pred_set_final_var_name_", cohort_name, ".csv"),row.names=F)


	# ----------------------------------------------------------------------------#
	#                          Code V - STATS & OUTPUT                           #
	# ----------------------------------------------------------------------------#

	# overview stats
	# ----------------------------------------------------------------------------#

	sink(paste0(output_folder, "feature_stat_temp_", cohort_name, "_", current_date), 
		split=TRUE)

	# number of features / complete cases
	feature_count        <- ncol(pred_set_final[, mget(grep("^var", names(pred_set_final), value=T))])
	feature_unique_count <- length(unique(gsub("(_days_to_last){0,}_timeframe.*", "", names(pred_set_final[, 
								mget(grep("^var", names(pred_set_final), value=T))]))))
	obs_count            <- nrow(pred_set_final)
	obs_count_complete   <- nrow(pred_set_final[complete.cases(pred_set_final[, 
								mget(grep("^var", names(pred_set_final), value=T))])])

	if (test_train==TRUE) {

		obs_count_train <- nrow(pred_set_final[get(test_train_mod)==1])
		obs_count_complete_train <- nrow(pred_set_final[complete.cases(pred_set_final[, 
								mget(grep("^var", names(pred_set_final), value=T))])][get(test_train_mod)==1])

		obs_count_test <- nrow(pred_set_final[get(test_train_mod)==0])
		obs_count_complete_test <- nrow(pred_set_final[complete.cases(pred_set_final[, 
								mget(grep("^var", names(pred_set_final), value=T))])][get(test_train_mod)==0])

	}

	cat("\n\n")

	cat(sprintf("final number of features: %d, final number of unique features: %d, final number of obs: %d, 
		final number of complete obs: %d (perc: %f)", 
		feature_count, feature_unique_count, 
		obs_count, obs_count_complete, perc(obs_count_complete, obs_count)))

	cat("\n\n")

	# variable types
	variable_type <- data.table(var_type=sapply(pred_set_final, class))
	variable_type[, var_type_count:=.N, by=c("var_type")]

	variable_type_var <- data.table(var_type=sapply(pred_set_final[, mget(grep("^var_", 
		names(pred_set_final), value=T))], class))
	variable_type_var[, var_type_count:=.N, by=c("var_type")]
	
	cat("var type all - pred set final")
	cat("\n")
	print(unique(variable_type, by=c("var_type")))

	cat("\n\n")

	cat("var type var only - pred set final")
	cat("\n")
	print(unique(variable_type_var, by=c("var_type")))

	cat("\n\n")

	# other cols 
	cat("cohort_extra_col")
	cat("\n")
	print(names(cohort_extra_col)[names(cohort_extra_col) %in% 
		names(pred_set_final)])
	
	cat("\n\n")

	cat("other extra_col")
	cat("\n")
	print(names(pred_set_final)[!(names(pred_set_final) %like% "^var_" |  
		names(pred_set_final) %in% names(cohort_extra_col))])

	cat("\n\n")

	sink()

 #    output stats
	# ----------------------------------------------------------------------------#

	# preparation
    leak_table <- data.table(var_name=leak_list, leak_day=mget(leak_list))

	# set-up
	feature_vital_sign <- list()
	space <- 1
 
	# vars
	feature_vital_sign$cohort_name <- cohort_name
	feature_vital_sign$cohort_id   <- cohort_id

	list_space("feature_vital_sign")

    feature_vital_sign$id_var_name      <- outcome_id_mod	
    feature_vital_sign$date_var_name    <- pred_date_mod	
    feature_vital_sign$outcome_var_name <- outcome_name_mod	
    feature_vital_sign$cohort_subset    <- cohort_mod	

	list_space("feature_vital_sign")
    
    feature_vital_sign$timeperiod_name       <- name_ext_name	
    feature_vital_sign$timeperiod_count      <- length(name_ext_name)	

	list_space("feature_vital_sign")
 
    feature_vital_sign$feature_type             <- compile_list	

    list_space("feature_vital_sign")

    feature_vital_sign$feature_count            <- feature_count
    feature_vital_sign$feature_unique_count     <- feature_unique_count	

	list_space("feature_vital_sign")

    feature_vital_sign$obs_count                <- obs_count	
    feature_vital_sign$obs_count_complete       <- obs_count_complete

    if (test_train==TRUE) {

    	feature_vital_sign$obs_count_train               <- obs_count_train	
    	feature_vital_sign$obs_count_complete_train      <- obs_count_complete_train

    	feature_vital_sign$obs_count_test                <- obs_count_test
    	feature_vital_sign$obs_count_complete_test       <- obs_count_complete_test

    }

	list_space("feature_vital_sign")

    feature_vital_sign$indic_missing_zero_imputation      <- miss_imp	
    feature_vital_sign$var_selection                      <- sprintf("%s (omit: %d)", variable_list_file_selection, 
    															deselect_col)	

    feature_vital_sign$numeric_factor_missing_threshold   <- sprintf("%s (omit: %d)", paste0(unlist(quant_missing_threshold),
    															collapse=" - "), na_col)	
    feature_vital_sign$indic_missing_threshold            <- sprintf("%s (omit: %d)", paste0(unlist(indic_missing_threshold),
    															collapse=" - "), zero_col)
    feature_vital_sign$missing_imputation                 <- ifelse(fill_na, sprintf("%s (method: %s / perc.values imputed: %d)", as.character(fill_na),
     															fill_na_method, impute_value_perc), sprintf("Missing imputation disabled\n."))	

  
    list_space("feature_vital_sign")

	feature_vital_sign$leak_day 	      <-  dt_paste(leak_table, "", line_sep=TRUE, length=nrow(leak_table))
 

    list_space("feature_vital_sign")

	feature_vital_sign$outcome_earliest  <- as.character(as.Date(min(date_col_table[var_name=="pred_date_min", date]),
												"%Y-%m-%d"))
	feature_vital_sign$outcome_latest 	 <- as.character(as.Date(max(date_col_table[var_name=="pred_date_max", date]), 
												"%Y-%m-%d"))
	feature_vital_sign$feature_earliest  <- as.character(as.Date(min(date_col_table[var_name %like% "min" & 
												!(var_name %like% "pred_date"), date]), "%Y-%m-%d"))
	feature_vital_sign$feature_latest    <- as.character(as.Date(max(date_col_table[var_name %like% "max" & !(var_name %like% 
												"pred_date"), date]), "%Y-%m-%d"))

    list_space("feature_vital_sign")

    feature_vital_sign$feature_earliest_specific <- dt_paste(date_col_table[var_name %like% "min" & !(var_name %like% "pred_date")], "", line_sep=TRUE, 
    													length=nrow(date_col_table[var_name %like% "min" & !(var_name %like% "pred_date")])) 
    feature_vital_sign$feature_latest_specific   <- dt_paste(date_col_table[var_name %like% "max" & !(var_name %like% "pred_date")], "", line_sep=TRUE, 
    													length=nrow(date_col_table[var_name %like% "max" & !(var_name %like% "pred_date")])) 

    list_space("feature_vital_sign")

	# read in vital signs and merge & save
	#----------------------------------------------------------------------------#

	ps("sucessfully generated feature vital signs")

	# unnest
	feature_vital_sign <- as.list(unlist(feature_vital_sign))
	
	print(head(feature_vital_sign))

	if (file.exists(feature_vital_sign_loc)) {
			
		feature_vital_sign_raw <- fread(feature_vital_sign_loc)

		feature_vital_sign_table <- list_table(feature_vital_sign, current_date,
 			vital_signs_merge=T, vital_signs_old=feature_vital_sign_raw)

		ps("sucessfully generated feature vital signs _table_ (appended)")

		print(head(feature_vital_sign_table))

	} else {
			
		feature_vital_sign_table <- list_table(feature_vital_sign, current_date,
 			vital_signs_merge=F)

		ps("sucessfully generated feature vital signs _table_ (new)")

		print(head(feature_vital_sign_table))

	}
	
	# save
	write.csv(feature_vital_sign_table, feature_vital_sign_loc, row.names=F, 
		na="")


    # output vis - features
	#----------------------------------------------------------------------------#
	print("start generating feature vis db")

	return_mult[var_list_output] <- feature_structure_vis()

	# create specific folder & save
	dir.create(vis_folder_spec)
	write.csv(var_list_output, vis_loc, row.names=F)
	
    print("sucessfully generated feature vis db")


}


#----------------------------------------------------------------------------#

	