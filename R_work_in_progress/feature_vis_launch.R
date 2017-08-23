# #----------------------------------------------------------------------------#

# # Purpose:     Update/Reinstate given feature vis at given port
# # Project:     /
# # Author:      Clara Marquardt
# # Date:        2016
# # Notes:       /

# #----------------------------------------------------------------------------#

# #----------------------------------------------------------------------------#
# #                               Control Section                              #
# #----------------------------------------------------------------------------#

# # set-up
# #-------------------------------------------------#
# print(sessionInfo())
# print(Sys.time())
# current_date <- as.character(format(Sys.taime(), "%d_%m_%Y")) 

# # control parameters & parameters
# #-------------------------------------------------#
# vis_update_code_path <- "/data/zolab/methods_new/base_code/generic_vis/server_vis/"
# vis_update_code      <- "vis_update.sh"
# vis_folder_path      <- "/data/zolab/methods_new/base_code/feature_construction/machine_code/feature_vis/"
# vis_file_path        <- "vis_basic.html"

# # vis_folder_spec <-"/data/zolab/pants/vis/feature_structure_cross_cohort_model_cohort_alt__excl_ed_patient__first_in_seq__dem_check_4_all_100_100_100_100_100_100_port_9015"
# vis_folder_spec <- commandArgs(trailingOnly = TRUE)[1]
# print(vis_folder_spec)

# # dependencies
# #-------------------------------------------------#
# # none

# #----------------------------------------------------------------------------#
# #                                    Code                                    #
# #----------------------------------------------------------------------------#

# # extract port
# port_remote <- gsub(".*_([0-9]{1,})$", "\\1", vis_folder_spec)
# print(port_remote)

# # launch
# setwd(vis_update_code_path)
# command_call <- sprintf("./%s %s %s %s %d", vis_update_code, vis_folder_spec, vis_folder_path, 
# 	vis_file_path, as.numeric(port_remote))
# print(command_call)
# system(command_call)


# #----------------------------------------------------------------------------#
# #                                    End                                     #
# #----------------------------------------------------------------------------#

