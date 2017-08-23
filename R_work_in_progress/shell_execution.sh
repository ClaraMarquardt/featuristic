#----------------------------------------------------------------------------#
#              						  START               					 #
#----------------------------------------------------------------------------#
# Master shell_execution.sh script

# settings
#----------------------------------------------------------------------------#
queue=big-multi
mem_res=200000
mem_lim=500000
core_multi=4

current_date=`date +%d_%m_%Y`

proj_specific_code_folder_path=[project specific code folder path]
proj_specific_vis_folder_path=[project/cohort specific visualisation folder path]


# feature_construction & compilation
#----------------------------------------------------------------------------#

## TO BE ADDED

# feature_vis_launch.R — execution
#----------------------------------------------------------------------------#

# script to terminate processes
port=$(echo ${proj_specific_vis_folder_path} | sed s/_/\\n/g | tail -n 1)
process_id=$(ps aux | grep 'node /' | grep "${port_num}" | awk '{print $2}')
    
if  [ -n "${process_id}" ];  then
  for i in process_id; do
  	kill ${process_id}
  done
fi 

# execute job
command="--args ${proj_specific_vis_folder_path}"
eval $"bsub -q $queue -M $mem_lim -n ${core_multi,}, -R 'rusage[mem=$mem_res]' R CMD BATCH '"$command"' \
	feature_vis_launch.R ${proj_specific_code_folder_path}/feature_vis_launch.Rout"


#----------------------------------------------------------------------------#
#              						    END               					 #
#----------------------------------------------------------------------------#



