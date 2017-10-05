## job submission

queue=big-multi
mem_res=10000
mem_lim=500000
core_multi=6

cd  /data/zolab/featuristic/codebase/featuristic/template
eval $"bsub -q $queue -M $mem_lim -R 'rusage[mem=$mem_res]' R CMD BATCH feature_generation_script.R feature_generation_script.Rout"

