#----------------------------------------------------------------------------#

#' @title Generate a child-node file used to visualise the features as a tree diagram. 
#'
#' @description \
#'
#' @export
#' @import data.table
#' @param var_name_raw_list 
#' @return
#' @examples


feature_structure_vis <- function(var_name_raw_list=var_name_vis) {

  # # convert variable names to data.table
  var_list_output <- data.table(var_name=var_name_raw_list[var_name_raw_list %like% "^var" & 
    !(var_name_raw_list %like% "_max$")])
  var_list_output[, var_name:=gsub("var_", "", var_name)]

  # extract category names & add "feature" as the top node
  var_list_output[, var_cat:=paste0("feature_", gsub("\\.\\..*", "", var_name))]

  # reduce short/long-term timeframe variables to one category (time) & maintain a 
  # separate category for variables which refer to the difference between the two 
  # time frames (diff) & which are time_independent (statis)
  var_list_output[var_name %like% "timeframe_diff", var_cat_type:="diff"]
  for (i in name_ext_name_extended) {
    var_list_output[var_name %like% paste0(i, "$"), var_cat_type:="time"]
  }
  var_list_output[var_name %like% "days_to_last", var_cat_type:="time"]
  var_list_output[is.na(var_cat_type), var_cat_type:="static"]

  var_list_output[, var_name:=gsub("_days_to_last", "", var_name)]

  # split category names into sequences of child - parent - parent....
  parent_max <- max(str_count(var_list_output$var_cat, "_")+1)
  var_list_output[, paste0("parent_", 1:parent_max):=
  tstrsplit(var_cat, "_")]  
 
  var_list_output[, paste0("parent_", parent_max+1):=
    gsub(paste0(gsub("feature_", "",var_cat), "\\.\\."), "", var_name),
    by=1:nrow(var_list_output)]

  for (i in 1:length(name_ext_extended[2:length(name_ext_extended)])) {
    var_list_output[, paste0("parent_", parent_max+1):=
    gsub(paste0(name_ext[i], name_ext[i+1], "_diff", "$"), "", 
    get(paste0("parent_", parent_max+1))), by=1:nrow(var_list_output)]
  }
 
  var_list_output[, paste0("parent_", parent_max+1):=
    gsub(paste0(name_ext[1], name_ext[length(name_ext)], "_diff", "$"), "", 
    get(paste0("parent_", parent_max+1))), by=1:nrow(var_list_output)]
 
  for (i in name_ext_name_extended) {
  var_list_output[, paste0("parent_", parent_max+1):=
    gsub(paste0(i, "$"), "", get(paste0("parent_", parent_max+1))),
    by=1:nrow(var_list_output)]
  }

  for (i in name_ext_name_extended) {
  var_list_output[, paste0("parent_", parent_max+1):=
    gsub("timeframe.*$", "", get(paste0("parent_", parent_max+1))),
    by=1:nrow(var_list_output)]
  }

  setcolorder(var_list_output, c("var_name", "var_cat", "var_cat_type", 
    paste0("parent_", (parent_max+1):1)))

  
  # transform the var_list_output into a flat tree structur
  var_list_output[, ':='(var_cat=NULL, var_name=NULL)]
  var_list_output <- unique(var_list_output)

  # fill all columns - i.e. copy entries from parent_1 to parent_2 to ... to 
  # parent_5 until there are no NAs left
  for (i in parent_max:1) {
    j <- 1
    while (nrow(var_list_output[is.na(get(paste0("parent_", i)))])>0) {
    var_list_output[is.na(get(paste0("parent_", i))), 
    paste0("parent_", i):=get(paste0("parent_", i-j))]
    j <- j +1
    }
  }

  # split the output into column pairs (child - parent pairs which are bound together)
  var_list_output_mod <- var_list_output[,.(parent_1, parent_2)]
  for (i in 1:(parent_max-1)) {
    var_list_output_mod <- rbind(var_list_output_mod, setnames(var_list_output[, 
  mget(paste0("parent_", c(i+1, i+2)))], c("parent_1", "parent_2"))) 
  }

  var_list_output_mod <- setnames(unique(var_list_output_mod), 
  c("parent", "child"))[!(is.na(child)|child==parent)]

  # merge the modified output with the var_cat type/var stat - classify all non-terminal 
  # nodes as "non_terminal" & insert record for top node
  setnames(var_list_output, c(paste0("parent_", parent_max+1), "var_cat_type"), 
  c("child", "child_type"))
  setnames(var_list_output, paste0("parent_", parent_max), "parent_max_col")
  var_list_output <- var_list_output[var_list_output_mod, 
    mget(c("child", "parent", "child_type")), 
    on=c(child="child", parent_max_col="parent")][
    is.na(child_type), child_type:="null"]

  var_list_output <- unique(var_list_output, by=c("child", "parent"))
  
  # implement tweaks to make generated flat hierarchy compatible with code that 
  # transforms the generated strucutre into a D3 tree structure

  ## insert record for top node
  var_list_output <- rbind(var_list_output, list(child="features.in.data.set", 
  parent=NA, child_type="null"), fill=T)
  var_list_output[parent=="feature", parent:="features.in.data.set"]
  
  # ## alter names of duplicate terminal nodes of different types
  # # XXX TO-DO: Better way of doing this - only change names of 2nd or higher 
  # # row in each group
  # var_list_output[, child:=paste0(child, c("", 1:(.N-1))), by=c("child")]

  # setnames
  setnames(var_list_output, c("child", "parent", "child_type"), 
  c("name", "parent", "type"))

  ## final checks & modifications
  # final check to remove commas -- necessary as csv file does not include quotes
  var_list_output[name %like% ",", name:=gsub(",","",name)]

  # final check to enure that no duplicates which will get tree confused
  # identify cases where a child has two different recorded parents
  var_list_output[, parent_count:=length(unique(parent)), by=c("name")]
  # print(var_list_output[parent_count>1 & type=="null"][order(name)])
  
  # XXX NOTE: This section may need to be modified on a case-by-case basis
  # example fix, e.g. lab has both feature and clinic.cat as parent --
  # (a) alter name in one of the two cases to labs -
  # # (b) alter parents name from lab to labs for all the relevant children 
  var_list_output[name=="lab" & parent=="features.in.data.set", name:="labs"]
  var_list_output[parent=="lab", parent:="labs"]
  var_list_output[, parent_count:=length(unique(parent)), by=c("name")]
  # print(var_list_output[parent_count>1 & type=="null"][order(name)])

  var_list_output[name=="ed" & type=="null", name:="ed_enc"]
  var_list_output[parent=="ed", parent:="ed_enc"]
  var_list_output[, parent_count:=length(unique(parent)), by=c("name")]
  # print(var_list_output[parent_count>1 & type=="null"][order(name)])

  var_list_output[, parent_count:=NULL]

  # final formatting
  var_list_output[, parent:=gsub("_$", "", parent)]
  var_list_output[, name:=gsub("_$", "", name)]

  # variable count check
  print(sprintf("number of unique features in tree: %d",
    nrow(var_list_output[type!="null"])))

  # return the collapsed and vis structures
  # return(list(var_list_output_coll, var_list_output_coll_vital_signs, var_list_output))
  return(list(var_list_output))

}

#----------------------------------------------------------------------------#



