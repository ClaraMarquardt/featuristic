#----------------------------------------------------------------------------#

#' @title Visualise missingness across features. 
#'
#' @description \
#'
#' @export
#' @import data.table
#' @param dt 
#' @param num_factor_var_list 
#' @param indic_var_list 
#' @param output_folder_path 
#' @param cohort_name_path 
#' @param version_path 
#' @return
#' @examples


incomplete_hist <- function(dt, num_factor_var_list, indic_var_list, output_folder_path=output_folder, 
  cohort_name_path=cohort_name, version_path) {


  ### 0 hist
  zero <- sapply(dt[, mget(indic_var_list)], function(y) sum(y==0, na.rm=T))
  zero_perc <- perc(zero, nrow(dt), digit=0)
  zero_perc_dt <- data.table(var_name=names(zero_perc), 
    zero_perc_value=zero_perc)
  zero_perc_dt[,var_cat_type:=gsub("^([^_]*)_.*", "\\1", var_name)]

  zero_hist <- ggplot(data=zero_perc_dt) +
    geom_histogram(aes(x=zero_perc_value), breaks=seq(-0.5, 100.5, by=1)) +
    geom_vline(xintercept=unlist(indic_missing_threshold), linetype="dashed", colour="grey30", size=0.1) +
    labs(
        y="Number of indic features\n", 
        x="\nPerc of observations - zero", 
        title=paste0("Feature Incompleteness - Indicator Variables"), 
        subtitle=paste0("Cohort: ", cohort_name_path,
          " / Total number of features: ", ncol(dt)," / Total number of indicator features: ", 
          ncol(dt[, mget(indic_var_list)])),
        caption="Missingness for indicator variables: '0'") +
      theme_basic() +
      theme_legend_bottom()

 zero_hist_non_extreme <- ggplot(data=zero_perc_dt) +
    geom_histogram(aes(x=zero_perc_value), breaks=seq(-0.5, 100.5, by=1)) +
    geom_vline(xintercept=unlist(indic_missing_threshold), linetype="dashed", colour="grey30", size=0.1) +
    labs(
        y="Number of indic features\n", 
        x="\nPerc of observations - zero", 
        title=paste0("Feature Incompleteness - Indicator Variables"), 
        subtitle=paste0("Cohort: ", cohort_name_path,
          " / Total number of features: ", ncol(dt)," / Total number of indicator features: ", 
          ncol(dt[, mget(indic_var_list)])),
        caption="Missingness for indicator variables: '0'") +
      coord_cartesian(ylim=c(0, 0.1*ncol(dt[, mget(indic_var_list)]))) + 
      theme_basic() +
      theme_legend_bottom()
 
  max_value <- max(ggplot_build(zero_hist_non_extreme)$data[[1]]$count)
  zero_hist_non_extreme <- zero_hist_non_extreme + 
    coord_cartesian(ylim=c(0, max_value*0.5)) 

  zero_hist_detail <- ggplot(data=zero_perc_dt) +
    geom_histogram(aes(x=zero_perc_value, 
      fill=var_cat_type), breaks=seq(-0.5, 100.5, by=1)) +
    geom_vline(xintercept=unlist(indic_missing_threshold), linetype="dashed", colour="grey30", size=0.1) +
    labs(
        y="Number of indic features\n", 
        x="\nPerc of observations - zero", 
        title=paste0("Feature Incompleteness - Indicator Variables"), 
        subtitle=paste0("Cohort: ", cohort_name_path,
          " / Total number of features: ", ncol(dt)," / Total number of indicator features: ", 
          ncol(dt[, mget(indic_var_list)])),
        caption="Missingness for indicator variables: '0'") +
      theme_basic() +
      theme_legend_bottom()

  ### missing hist
  missing <- sapply(dt[, mget(num_factor_var_list)], function(y) sum(is.na(y)))
  missing_perc <- perc(missing, nrow(dt), digit=0)
  missing_perc_dt <- data.table(var_name=names(missing_perc), 
    missing_perc_value=missing_perc)
  missing_perc_dt[,var_cat_type:=gsub("^([^_]*)_.*", "\\1", var_name)]

  label_exp <- expression()


  na_hist <- ggplot(data=missing_perc_dt) +
    geom_histogram(aes(x=missing_perc_value), breaks=seq(-0.5, 100.5, by=1)) +
    geom_vline(xintercept=unlist(quant_missing_threshold), linetype="dashed", colour="grey30", size=0.1) +
    labs(
        y="Number of numeric_factor features\n", 
        x="\nPerc of observations - na", 
        title=paste0("Feature Incompleteness - Numeric/Factor Variables"), 
        subtitle=paste0("Cohort: ", cohort_name_path,
          " / Total number of features: ", ncol(dt)," / Total number of numeric_factor features: ", 
        ncol(dt[, mget(num_factor_var_list)])),
        caption="Missingness for numeric_factor variables: 'na'") +
    theme_basic() +
    theme_legend_bottom()

 na_hist_non_extreme <- ggplot(data=missing_perc_dt) +
    geom_histogram(aes(x=missing_perc_value), breaks=seq(-0.5, 100.5, by=1)) +
    geom_vline(xintercept=unlist(quant_missing_threshold), linetype="dashed", colour="grey30", size=0.1) +
    labs(
        y="Number of numeric_factor features\n", 
        x="\nPerc of observations - na", 
        title=paste0("Feature Incompleteness - Numeric/Factor Variables"), 
        subtitle=paste0("Cohort: ", cohort_name_path,
          " / Total number of features: ", ncol(dt)," / Total number of numeric_factor features: ", 
          ncol(dt[, mget(num_factor_var_list)])),
        caption="Missingness for numeric_factor variables: 'na'") +
      theme_basic() +
      theme_legend_bottom()
  
  max_value <- max(ggplot_build(na_hist_non_extreme)$data[[1]]$count)
  na_hist_non_extreme <- na_hist_non_extreme + 
    coord_cartesian(ylim=c(0, max_value*0.5)) 

  na_hist_detail <- ggplot(data=missing_perc_dt) +
    geom_histogram(aes(x=missing_perc_value, 
      fill=var_cat_type), breaks=seq(-0.5, 100.5, by=1)) +
    geom_vline(xintercept=unlist(quant_missing_threshold), linetype="dashed", colour="grey30", size=0.1) +
    labs(
        y="Number of numeric_factor features\n", 
        x="\nPerc of observations - na", 
        title=paste0("Feature Incompleteness - Numeric/Factor Variables"), 
        subtitle=paste0("Cohort: ", cohort_name_path,
          " / Total number of features: ", ncol(dt)," / Total number of numeric_factor features: ", 
          ncol(dt[, mget(num_factor_var_list)])),
        caption="Missingness for numeric_factor variables: 'na'") +
      theme_basic() +
      theme_legend_bottom()
 
  
  ### return
  # return(list(zero_hist, zero_hist_non_extreme, 
  #   zero_hist_detail, na_hist, na_hist_non_extreme, na_hist_detail))


  ggsave(paste0(output_folder_path, "incomplete_feat_hist_zero_hist_", 
  	version_path, "_" ,cohort_name_path, ".pdf"), zero_hist)
  ggsave(paste0(output_folder_path, "incomplete_feat_hist_zero_hist_non_extreme", 
  	version_path, "_",cohort_name_path, ".pdf"), zero_hist_non_extreme)
  ggsave(paste0(output_folder_path, "incomplete_feat_hist_zero_hist_detail", 
  	version_path, "_",cohort_name_path, ".pdf"), zero_hist_detail)
  ggsave(paste0(output_folder_path, "incomplete_feat_hist_na_hist", 
  	version_path, "_",cohort_name_path, ".pdf"), na_hist)
  ggsave(paste0(output_folder_path, "incomplete_feat_hist_na_hist_non_extreme", 
  	version_path, "_",cohort_name_path, ".pdf"), na_hist_non_extreme)
  ggsave(paste0(output_folder, "incomplete_feat_hist_na_hist_detail", 
  	version_path, "_",cohort_name_path, ".pdf"),  na_hist_detail)


}


#----------------------------------------------------------------------------#


