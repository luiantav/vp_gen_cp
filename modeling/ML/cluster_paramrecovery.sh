
PATH_BASE="${HOME}/vp_gen/modeling/ML"
# Give message to user
echo "strt"
echo PATCH_BASE
N_CPUS=1
# maximum number of threads per process:
N_THREADS=1
# memory demand in *GB*
MEM_MB=1
# data directory
#PATH_DATA="${PATH_BASE}/data/temp"

#subjects=("1" "2" "3" "4" "5" "6" "7" "8" "9" "10" "11" "12" "13" "14" "15" "16" "17" "18" "19" "20" "21" "22" "23" "24" "25" "26" "27" "28" "29" "30" "31" "32" "33" "34" "35" "36" "37" "38" "39" "40" "41" "42" "43" "44" "45" "46" "47" "48" "49" "50")

subjects=($(seq 1 120))
for sub in "${subjects[@]}"; do
    JOB_NAME="prec_${sub}"


# Create job file
echo "#!/bin/bash" > job.slurm
# name of the job
echo "#SBATCH --job-name ${JOB_NAME}" >> job.slurm
# set the expected maximum running time for the job:
echo "#SBATCH --time 20:00:00" >> job.slurm
# determine how much RAM your operation needs:
echo "#SBATCH --mem ${MEM_MB}GB" >> job.slurm
# determine number of CPUs
echo "#SBATCH --cpus-per-task ${N_CPUS}" >> job.slurm
# write to log folder
#echo "#SBATCH --output ${PATH_LOG}/slurm-${JOB_NAME}.%j.out" >> job.slurm
echo "#SBATCH --output /home/mpib/verra/logs/slurm_${JOB_NAME}_%j.out" >> job.slurm
echo "#SBATCH --partition short" >> job.slurm
    
# Load R module
echo "module unload R" >> job.slurm
echo "module load R/4.2" >> job.slurm
echo "Rscript ${PATH_BASE}/parameter_recoveryTRY.R \
    -s ${sub}" >> job.slurm


# submit job to cluster queue and remove it to avoid confusion:
sbatch job.slurm
rm -f job.slurm
sleep 6

done



