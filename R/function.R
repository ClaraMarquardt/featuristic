# # ##########################################
# # DESCRIPTION: This file prepares the data set that is to be used to develop 
# # a predicitve model for positive microbiology results 
# # ##########################################
# # Creator: Clara Marquardt
# # Date: 5th January 
# # ##########################################
# # Language: R
# # ##########################################
# # NOTE
# # All below function are sourced in the global environment & should be written 
# # to be callable within any other function, i.e. environment specifications have 
# # to adapted to make nested funcion calls possible
# # ##########################################
# # TO-DO-LIST 


# ################################################################################
# ########################## FUNCTION SPECIFICATION ##############################
# ##########################     GENERIC            ##############################
# ################################################################################
# ## source function master file
# source("/data/zolab/methods_new/base_code/generic_function/generic_function.R")

# ################################################################################
# ########################## FUNCTION SPECIFICATION ##############################
# ##########################     SPECIFIC            ##############################
# ################################################################################

# ################################################################################
# ### split DTs based on the timeframe (timeframe_long/timeframe_short) 

# timeframe_split <- function(DT_list, colname) {
#   lapply(DT_list, function(x) {
#     timeframe_comb_list <- list()
#     timeframe_comb_list[[1]] <- get(x,sys.frame(
#         sys.parent(n=3)))[get(colname) >= get(paste0("pred_date_beg", 
#           name_ext[length(name_ext)])) & get(colname) <=pred_date]
#     timeframe_comb_list[[2]] <- get(x,sys.frame(
#         sys.parent(n=3)))[get(colname) >= get(paste0("pred_date_beg", 
#           name_ext[1])) & get(colname) <=pred_date]

#     if(length(name_ext)>1) {
#       for (i in 2:length(name_ext)) {
#         timeframe_comb_list[[i+1]] <- get(x,sys.frame(
#           sys.parent(n=3)))[get(colname) >= get(paste0("pred_date_beg", name_ext[i])) & 
#           get(colname) < get(paste0("pred_date_beg", name_ext[i-1]))]
#       }
#     }

#     names(timeframe_comb_list) <- c("timeframe_comb_max",paste0("timeframe_comb", name_ext))

#     assign(paste0(x, "_timeframe_comb"),timeframe_comb_list, 
#       envir = sys.frame(sys.parent(n=3)))
#   })
# }

# ################################################################################
# ### combine DTs based on the timeframe (timeframe_long/timeframe_short) 

# timeframe_combine <- function(DT_list) {
#   lapply(DT_list, function(x) {
#    assign(x, (Reduce(function(...) merge(..., all = TRUE, 
#     by=cohort_key_var), get(x, sys.frame(sys.parent(n=3))))), sys.frame(sys.parent(n=3)))
#     # assign(x, as.data.table(merge(get(x,sys.frame(sys.parent(n=3)))$timeframe_short, 
#     #   get(x, sys.frame(sys.parent(n=3)))$timeframe_long, 
#     #   all = TRUE, by=cohort_key_var)), sys.frame(sys.parent(n=3)))
#   })     
# }

# ################################################################################
# ### check generated feature set - obs.count & completeness

# feature_check <- function(feature_dt_name, 
#   cohort_dt=cohort,cohort_key_var_list=cohort_key_var, 
#   extra_var_list=names(cohort_extra_col), save_name=TRUE) {

#   # number of obs check
#   print(sprintf("number of cohort observations: %d", nrow(cohort_dt)))
#   print(sprintf("number of %s observations: %d", feature_dt_name, 
#     nrow(get(feature_dt_name))))
#   ifelse(nrow(cohort_dt)==nrow(get(feature_dt_name)), print("correct number of rows"), 
#     print("incorrect number of rows"))

#   # complete check
#   feature_missing <- sum(sapply(get(feature_dt_name), function(y) sum(is.na(y))))
#   feature_missing_perc <- (feature_missing/(nrow(get(feature_dt_name)[, 
#     setdiff(names(get(feature_dt_name)), c(cohort_key_var_list, extra_var_list)), with=F])*
#     ncol(get(feature_dt_name)[,setdiff(names(get(feature_dt_name)), c(cohort_key_var_list, 
#     extra_var_list)), with=F])))*100
#   complete <- nrow(get(feature_dt_name)[complete.cases(get(feature_dt_name))])

#   print(sprintf("%s file contains %f missing values %f perc missing values -- %f complete observations", 
#     feature_dt_name,feature_missing, feature_missing_perc, complete))

#   if (save_name==TRUE) {

#     ps("feature names saved: %s", paste0(temp_folder, "var_name_raw_", 
#       feature_dt_name, "_", cohort_name, ".csv"))

#     write.csv(data.table(var_name=names(get(feature_dt_name)), 
#        var_type=sapply(get(feature_dt_name), function(x) class(x)[1])), 
#       paste0(temp_folder, "var_name_raw_", feature_dt_name, "_", cohort_name, ".csv"), 
#       row.names=F)

#     var_dt <- data.table(var_type=sapply(get(feature_dt_name)[, setdiff(names(get(feature_dt_name)), 
#       c(cohort_key_var_list, extra_var_list)), with=F], function(x) class(x)[1]))
#     print(table_mod(var_dt$var_type))

#   }
# }

# ################################################################################
# ### visualize features

# feature_structure_vis <- function(var_name_raw_list=var_name_vis) {

#   # # convert variable names to data.table
#   var_list_output <- data.table(var_name=var_name_raw_list[var_name_raw_list %like% "^var" & 
#     !(var_name_raw_list %like% "_max$")])
#   var_list_output[, var_name:=gsub("var_", "", var_name)]

#   # extract category names & add "feature" as the top node
#   var_list_output[, var_cat:=paste0("feature_", gsub("\\.\\..*", "", var_name))]

#   # reduce short/long-term timeframe variables to one category (time) & maintain a 
#   # separate category for variables which refer to the difference between the two 
#   # time frames (diff) & which are time_independent (statis)
#   var_list_output[var_name %like% "timeframe_diff", var_cat_type:="diff"]
#   for (i in name_ext_name_extended) {
#     var_list_output[var_name %like% paste0(i, "$"), var_cat_type:="time"]
#   }
#   var_list_output[var_name %like% "days_to_last", var_cat_type:="time"]
#   var_list_output[is.na(var_cat_type), var_cat_type:="static"]

#   var_list_output[, var_name:=gsub("_days_to_last", "", var_name)]

#   # split category names into sequences of child - parent - parent....
#   parent_max <- max(str_count(var_list_output$var_cat, "_")+1)
#   var_list_output[, paste0("parent_", 1:parent_max):=
#   tstrsplit(var_cat, "_")]  
 
#   var_list_output[, paste0("parent_", parent_max+1):=
#     gsub(paste0(gsub("feature_", "",var_cat), "\\.\\."), "", var_name),
#     by=1:nrow(var_list_output)]

#   for (i in 1:length(name_ext_extended[2:length(name_ext_extended)])) {
#     var_list_output[, paste0("parent_", parent_max+1):=
#     gsub(paste0(name_ext[i], name_ext[i+1], "_diff", "$"), "", 
#     get(paste0("parent_", parent_max+1))), by=1:nrow(var_list_output)]
#   }
 
#   var_list_output[, paste0("parent_", parent_max+1):=
#     gsub(paste0(name_ext[1], name_ext[length(name_ext)], "_diff", "$"), "", 
#     get(paste0("parent_", parent_max+1))), by=1:nrow(var_list_output)]
 
#   for (i in name_ext_name_extended) {
#   var_list_output[, paste0("parent_", parent_max+1):=
#     gsub(paste0(i, "$"), "", get(paste0("parent_", parent_max+1))),
#     by=1:nrow(var_list_output)]
#   }

#   for (i in name_ext_name_extended) {
#   var_list_output[, paste0("parent_", parent_max+1):=
#     gsub("timeframe.*$", "", get(paste0("parent_", parent_max+1))),
#     by=1:nrow(var_list_output)]
#   }

#   setcolorder(var_list_output, c("var_name", "var_cat", "var_cat_type", 
#     paste0("parent_", (parent_max+1):1)))

  
#   # transform the var_list_output into a flat tree structur
#   var_list_output[, ':='(var_cat=NULL, var_name=NULL)]
#   var_list_output <- unique(var_list_output)

#   # fill all columns - i.e. copy entries from parent_1 to parent_2 to ... to 
#   # parent_5 until there are no NAs left
#   for (i in parent_max:1) {
#     j <- 1
#     while (nrow(var_list_output[is.na(get(paste0("parent_", i)))])>0) {
#     var_list_output[is.na(get(paste0("parent_", i))), 
#     paste0("parent_", i):=get(paste0("parent_", i-j))]
#     j <- j +1
#     }
#   }

#   # split the output into column pairs (child - parent pairs which are bound together)
#   var_list_output_mod <- var_list_output[,.(parent_1, parent_2)]
#   for (i in 1:(parent_max-1)) {
#     var_list_output_mod <- rbind(var_list_output_mod, setnames(var_list_output[, 
#   mget(paste0("parent_", c(i+1, i+2)))], c("parent_1", "parent_2"))) 
#   }

#   var_list_output_mod <- setnames(unique(var_list_output_mod), 
#   c("parent", "child"))[!(is.na(child)|child==parent)]

#   # merge the modified output with the var_cat type/var stat - classify all non-terminal 
#   # nodes as "non_terminal" & insert record for top node
#   setnames(var_list_output, c(paste0("parent_", parent_max+1), "var_cat_type"), 
#   c("child", "child_type"))
#   setnames(var_list_output, paste0("parent_", parent_max), "parent_max_col")
#   var_list_output <- var_list_output[var_list_output_mod, 
#     mget(c("child", "parent", "child_type")), 
#     on=c(child="child", parent_max_col="parent")][
#     is.na(child_type), child_type:="null"]

#   var_list_output <- unique(var_list_output, by=c("child", "parent"))
  
#   # implement tweaks to make generated flat hierarchy compatible with code that 
#   # transforms the generated strucutre into a D3 tree structure

#   ## insert record for top node
#   var_list_output <- rbind(var_list_output, list(child="features.in.data.set", 
#   parent=NA, child_type="null"), fill=T)
#   var_list_output[parent=="feature", parent:="features.in.data.set"]
  
#   # ## alter names of duplicate terminal nodes of different types
#   # # XXX TO-DO: Better way of doing this - only change names of 2nd or higher 
#   # # row in each group
#   # var_list_output[, child:=paste0(child, c("", 1:(.N-1))), by=c("child")]

#   # setnames
#   setnames(var_list_output, c("child", "parent", "child_type"), 
#   c("name", "parent", "type"))

#   ## final checks & modifications
#   # final check to remove commas -- necessary as csv file does not include quotes
#   var_list_output[name %like% ",", name:=gsub(",","",name)]

#   # final check to enure that no duplicates which will get tree confused
#   # identify cases where a child has two different recorded parents
#   var_list_output[, parent_count:=length(unique(parent)), by=c("name")]
#   # print(var_list_output[parent_count>1 & type=="null"][order(name)])
  
#   # XXX NOTE: This section may need to be modified on a case-by-case basis
#   # example fix, e.g. lab has both feature and clinic.cat as parent --
#   # (a) alter name in one of the two cases to labs -
#   # # (b) alter parents name from lab to labs for all the relevant children 
#   var_list_output[name=="lab" & parent=="features.in.data.set", name:="labs"]
#   var_list_output[parent=="lab", parent:="labs"]
#   var_list_output[, parent_count:=length(unique(parent)), by=c("name")]
#   # print(var_list_output[parent_count>1 & type=="null"][order(name)])

#   var_list_output[name=="ed" & type=="null", name:="ed_enc"]
#   var_list_output[parent=="ed", parent:="ed_enc"]
#   var_list_output[, parent_count:=length(unique(parent)), by=c("name")]
#   # print(var_list_output[parent_count>1 & type=="null"][order(name)])

#   var_list_output[, parent_count:=NULL]

#   # final formatting
#   var_list_output[, parent:=gsub("_$", "", parent)]
#   var_list_output[, name:=gsub("_$", "", name)]

#   # variable count check
#   print(sprintf("number of unique features in tree: %d",
#     nrow(var_list_output[type!="null"])))

#   # return the collapsed and vis structures
#   # return(list(var_list_output_coll, var_list_output_coll_vital_signs, var_list_output))
#   return(list(var_list_output))

# }

# ################################################################################
# ### visualize data incompleteness

# incomplete_hist <- function(dt, num_factor_var_list, indic_var_list, output_folder_path=output_folder, 
#   cohort_name_path=cohort_name, version_path) {

#   ### 0 hist
#   zero <- sapply(dt[, mget(indic_var_list)], function(y) sum(y==0, na.rm=T))
#   zero_perc <- perc(zero, nrow(dt), digit=0)
#   zero_perc_dt <- data.table(var_name=names(zero_perc), 
#     zero_perc_value=zero_perc)
#   zero_perc_dt[,var_cat_type:=gsub("^([^_]*)_.*", "\\1", var_name)]

#   zero_hist <- ggplot(data=zero_perc_dt) +
#     geom_histogram(aes(x=zero_perc_value), breaks=seq(-0.5, 100.5, by=1)) +
#     geom_vline(xintercept=unlist(indic_missing_threshold), linetype="dashed", colour="grey30", size=0.1) +
#     labs(
#         y="Number of indic features\n", 
#         x="\nPerc of observations - zero", 
#         title=paste0("Feature Incompleteness - Indicator Variables"), 
#         subtitle=paste0("Cohort: ", cohort_name_path,
#           " / Total number of features: ", ncol(dt)," / Total number of indicator features: ", 
#           ncol(dt[, mget(indic_var_list)])),
#         caption="Missingness for indicator variables: '0'") +
#       theme_basic() +
#       theme_legend_bottom()

#  zero_hist_non_extreme <- ggplot(data=zero_perc_dt) +
#     geom_histogram(aes(x=zero_perc_value), breaks=seq(-0.5, 100.5, by=1)) +
#     geom_vline(xintercept=unlist(indic_missing_threshold), linetype="dashed", colour="grey30", size=0.1) +
#     labs(
#         y="Number of indic features\n", 
#         x="\nPerc of observations - zero", 
#         title=paste0("Feature Incompleteness - Indicator Variables"), 
#         subtitle=paste0("Cohort: ", cohort_name_path,
#           " / Total number of features: ", ncol(dt)," / Total number of indicator features: ", 
#           ncol(dt[, mget(indic_var_list)])),
#         caption="Missingness for indicator variables: '0'") +
#       coord_cartesian(ylim=c(0, 0.1*ncol(dt[, mget(indic_var_list)]))) + 
#       theme_basic() +
#       theme_legend_bottom()
 
#   max_value <- max(ggplot_build(zero_hist_non_extreme)$data[[1]]$count)
#   zero_hist_non_extreme <- zero_hist_non_extreme + 
#     coord_cartesian(ylim=c(0, max_value*0.5)) 

#   zero_hist_detail <- ggplot(data=zero_perc_dt) +
#     geom_histogram(aes(x=zero_perc_value, 
#       fill=var_cat_type), breaks=seq(-0.5, 100.5, by=1)) +
#     geom_vline(xintercept=unlist(indic_missing_threshold), linetype="dashed", colour="grey30", size=0.1) +
#     labs(
#         y="Number of indic features\n", 
#         x="\nPerc of observations - zero", 
#         title=paste0("Feature Incompleteness - Indicator Variables"), 
#         subtitle=paste0("Cohort: ", cohort_name_path,
#           " / Total number of features: ", ncol(dt)," / Total number of indicator features: ", 
#           ncol(dt[, mget(indic_var_list)])),
#         caption="Missingness for indicator variables: '0'") +
#       theme_basic() +
#       theme_legend_bottom()

#   ### missing hist
#   missing <- sapply(dt[, mget(num_factor_var_list)], function(y) sum(is.na(y)))
#   missing_perc <- perc(missing, nrow(dt), digit=0)
#   missing_perc_dt <- data.table(var_name=names(missing_perc), 
#     missing_perc_value=missing_perc)
#   missing_perc_dt[,var_cat_type:=gsub("^([^_]*)_.*", "\\1", var_name)]

#   label_exp <- expression()


#   na_hist <- ggplot(data=missing_perc_dt) +
#     geom_histogram(aes(x=missing_perc_value), breaks=seq(-0.5, 100.5, by=1)) +
#     geom_vline(xintercept=unlist(quant_missing_threshold), linetype="dashed", colour="grey30", size=0.1) +
#     labs(
#         y="Number of numeric_factor features\n", 
#         x="\nPerc of observations - na", 
#         title=paste0("Feature Incompleteness - Numeric/Factor Variables"), 
#         subtitle=paste0("Cohort: ", cohort_name_path,
#           " / Total number of features: ", ncol(dt)," / Total number of numeric_factor features: ", 
#         ncol(dt[, mget(num_factor_var_list)])),
#         caption="Missingness for numeric_factor variables: 'na'") +
#     theme_basic() +
#     theme_legend_bottom()

#  na_hist_non_extreme <- ggplot(data=missing_perc_dt) +
#     geom_histogram(aes(x=missing_perc_value), breaks=seq(-0.5, 100.5, by=1)) +
#     geom_vline(xintercept=unlist(quant_missing_threshold), linetype="dashed", colour="grey30", size=0.1) +
#     labs(
#         y="Number of numeric_factor features\n", 
#         x="\nPerc of observations - na", 
#         title=paste0("Feature Incompleteness - Numeric/Factor Variables"), 
#         subtitle=paste0("Cohort: ", cohort_name_path,
#           " / Total number of features: ", ncol(dt)," / Total number of numeric_factor features: ", 
#           ncol(dt[, mget(num_factor_var_list)])),
#         caption="Missingness for numeric_factor variables: 'na'") +
#       theme_basic() +
#       theme_legend_bottom()
  
#   max_value <- max(ggplot_build(na_hist_non_extreme)$data[[1]]$count)
#   na_hist_non_extreme <- na_hist_non_extreme + 
#     coord_cartesian(ylim=c(0, max_value*0.5)) 

#   na_hist_detail <- ggplot(data=missing_perc_dt) +
#     geom_histogram(aes(x=missing_perc_value, 
#       fill=var_cat_type), breaks=seq(-0.5, 100.5, by=1)) +
#     geom_vline(xintercept=unlist(quant_missing_threshold), linetype="dashed", colour="grey30", size=0.1) +
#     labs(
#         y="Number of numeric_factor features\n", 
#         x="\nPerc of observations - na", 
#         title=paste0("Feature Incompleteness - Numeric/Factor Variables"), 
#         subtitle=paste0("Cohort: ", cohort_name_path,
#           " / Total number of features: ", ncol(dt)," / Total number of numeric_factor features: ", 
#           ncol(dt[, mget(num_factor_var_list)])),
#         caption="Missingness for numeric_factor variables: 'na'") +
#       theme_basic() +
#       theme_legend_bottom()
 
  
#   ### return
#   # return(list(zero_hist, zero_hist_non_extreme, 
#   #   zero_hist_detail, na_hist, na_hist_non_extreme, na_hist_detail))


#   ggsave(paste0(output_folder_path, "incomplete_feat_hist_zero_hist_", version_path, "_" ,cohort_name_path, ".pdf"), 
#     zero_hist)
#   ggsave(paste0(output_folder_path, "incomplete_feat_hist_zero_hist_non_extreme", version_path, "_",cohort_name_path, ".pdf"),
#     zero_hist_non_extreme)
#   ggsave(paste0(output_folder_path, "incomplete_feat_hist_zero_hist_detail", version_path, "_",cohort_name_path, ".pdf"), 
#     zero_hist_detail)
#   ggsave(paste0(output_folder_path, "incomplete_feat_hist_na_hist", version_path, "_",cohort_name_path, ".pdf"), 
#     na_hist)
#   ggsave(paste0(output_folder_path, "incomplete_feat_hist_na_hist_non_extreme", version_path, "_",cohort_name_path, ".pdf"), 
#     na_hist_non_extreme)
#   ggsave(paste0(output_folder, "incomplete_feat_hist_na_hist_detail", version_path, "_",cohort_name_path, ".pdf"), 
#     na_hist_detail)


# }


# ################################################################################
# ### quantify missing and completeness

# obs_check <- function(dt, cohort_key_var_list=cohort_key_var, 
#   cohort_extra_var_list=names(cohort_extra_col)) {

#   timepattern_1 <- paste0(paste0(name_ext_extended, "$"), collapse="|")
#   timepattern_2 <- paste0(paste0(name_ext_name_extended, "$"), collapse="|")
#   timepattern_3 <- "_days_to_last_.*|timeframe.*$"
#   timepattern   <- paste(timepattern_1, timepattern_2, timepattern_3, sep="|")

#   timepattern_diff_1 <- paste0("(", "(", paste0(name_ext_extended,collapse="|"), ")", "(", 
#     paste0(name_ext_extended,collapse="|"), ")", "_diff$", ")")
#   timepattern_diff_2 <- paste0("(", "(", paste0(name_ext_name_extended,collapse="|"), ")", "(", 
#     paste0(name_ext_name_extended,collapse="|"), ")", "_diff$", ")")
#   timepattern_diff_3 <- "_max_diff$|(timeframe_diff_.*$)"
#   timepattern_diff <- paste(timepattern_diff_1, timepattern_diff_2, timepattern_diff_3, sep="|")

#   timepattern_comb <- paste(timepattern, timepattern_diff, sep="|")

#   ps("number of rows: %f, number of complete cases: %f, numer of complete cases as a perc of all rows: %f, number of columns: %d, number of features: %d, number of features excl. timeframe max: %d, number of unique features exl.timeframe max: %d", 
#     nrow(dt), 
#     nrow(dt[complete.cases(dt[, mget(setdiff(names(dt),
#       c(cohort_key_var_list, cohort_extra_var_list) ))])]),
#     nrow(dt[complete.cases(dt[, mget(setdiff(names(dt),
#       c(cohort_key_var_list, cohort_extra_var_list)))])])/nrow(dt)*100, 
#     ncol(dt),
#     ncol(dt[, mget(setdiff(names(dt),c(cohort_key_var_list, cohort_extra_var_list)))]),
#     ncol(dt[, mget(setdiff(names(dt),c(cohort_key_var_list, cohort_extra_var_list, 
#       grep("_max$|_max_diff$", names(dt),value=T))))]), 
#     length(unique(gsub("_$", "", gsub(timepattern_comb, "", names(dt[, mget(setdiff(names(dt),c(cohort_key_var_list, 
#       cohort_extra_var_list, grep("_max$|_max_diff$",names(dt),value=T))))]))))))

# }


# ################################################################################
# ### generate collapsed feature hierarchy

# feature_coll <- function(dt, cohort_key_var_list=cohort_key_var, 
#   cohort_extra_var_list=names(cohort_extra_col)) {

#   dt_temp <- dt[, mget(setdiff(names(dt), union(cohort_key_var_list, 
#     cohort_extra_var_list)))]

#   temp <- data.table(var_cat=names(dt_temp))

#   temp[var_cat %like% "timeframe" | var_cat %like% paste0(name_ext, collapse="|") | var_cat %like% paste0(name_ext_extended, collapse="|"), var_type:="time"]
#   temp[var_cat %like% "timeframe_diff" | var_cat %like% "diff$", var_type:="diff"]
#   temp[is.na(var_type), var_type:="static"]

#   temp[, var_cat:=gsub("^var_", "", var_cat)]
#   temp[, var_source:=gsub("([^_]*)_.*", "\\1", var_cat)]
 
#   temp[, var_cat:=gsub("(.*)\\.\\..*", "\\1", var_cat)]
#   temp[, var_cat:=gsub("([^_]*)_(.*)", "\\2", var_cat)]

#   temp[, var_count:=.N, by=c("var_cat")]


#   temp[,var_type:=do.call(paste0, list(unique(var_type),collapse=";")), by=c("var_cat")]

#   temp <- unique(temp, by=c("var_cat"))
#   temp <- rbindlist(list(temp, data.table(var_count=sum(temp$var_count), 
#     var_cat="all features")), use.names=T, fill=T)

#   return(temp)

# }

# ################################################################################
# ### generate days to last features

# feature_var_format <- function(dt_list, function_ext_1="_length_", 
#   function_ext_2="_function_") {
  
#   lapply(dt_list, function(x)  if(nrow(x)>1) {setnames(x, grep(paste0("time_diff", function_ext_2), 
#     names(x), value=T), paste0(grep(paste0("time_diff", function_ext_2), names(x), 
#     value=T), "_days_to_last"))})

#   lapply(dt_list, function(x)  if(nrow(x)>1) {setnames(x, gsub(paste0("time_diff", 
#      "(", function_ext_1, "|", function_ext_2,")"), "", names(x)))})

#   return(dt_list)
  
# }

# feature_var_format_2 <- function(dt, timeframe_split_var=time_split) {

#   day_count_var <- grep("days_to_last", names(dt), value=T)

#     if (timeframe_split_var==FALSE) {
    
#     dt[, c(day_count_var):=lapply(.SD, function(x) as.numeric(x)), 
#       .SDcols=day_count_var]

#     set_na_zero(dt, replace=NA, subset_col=day_count_var)

#     } else if (timeframe_split_var==TRUE) {

#      dt[, c(day_count_var):=NULL]

#   } 

# }

# ################################################################################
# ##################################  END  #######################################
# ################################################################################


