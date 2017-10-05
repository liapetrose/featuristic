## job submission


## job parameters
queue=big-multi
mem_res=10000
mem_lim=500000
core_multi=4

# stage specification ('1','2','12')
stage='2'

cd  /data/zolab/featuristic/codebase/featuristic/template
command="--args ${stage}" 
echo $command
current_date_time=`date +%d_%m_%Y_%H_%M_%S`
echo $current_date_time
eval $"bsub -q $queue -M $mem_lim -R 'rusage[mem=$mem_res]' R CMD BATCH  '"$command"' feature_generation_script.R /data/zolab/featuristic/test_project/Rout/feature_generation_script_stage_${stage}_${current_date_time}.Rout"
