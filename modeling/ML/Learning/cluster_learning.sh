
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


#put all subjects in a vector here

subjects=("a39zpu2zr3xl47k" "2bqu5ktkg43fh3p" "brsvshywlogvxcq" "9oe8onshjdy2khe" "regcubm60dchcxr" "ds2ot3x1v6q63xg" "hkdemly7qs0y8be" "nvk3jd25saegh24" "gnd1v5zo5x7qjad" "duzv7z4jux5olpv" "mdtwfshb8x8v70p" "x6cu75oy88jq32r" "wwv18727elhq22u" "2nzfnkk2uulwmkz" "8es63ovt8uw2mgg" "jvcerddevc3rwm0" "k7xa918wsdj832n" "6be4c2orpqft8ul" "5pgqs0v0feqyejp" "7pjw0pz87exlgmd" "v2rgepeoz23lhyb" "djb1vpwld9c7hmk" "8le0rzndxgpur8r" "n56opcxqwcyuk97" "2usnf1r3h88bett" "3jrram86yd3uhgb" "pv8gfb6dwc0u8md" "m603l2rxt7hfqax" "qeaducsgetncgeb" "hvawyrkxreuq7d8" "jk1c41xga9cmsrf" "1tq0jt4dd31u8lq" "r018cukle69r1bf" "uaf4fed1xj02ps4" "swmufx7egboodpe" "qw7q50m2o58ugvl" "fqnkabfbs65dy5r" "ws44qtt9xen2xsl" "t53tjn7r8ydtmtq" "qreecv8wv03uqw8" "rf8fvjnsk6r07uc" "gg80fe1zyat447z" "oemn8dnelyez9dr" "9ho9tv1b4jdy3p6" "lucnd0k3f5h8ez4" "kp5boe0z4uwwu7r" "cl4vc26ycu0woo2" "kug2nygmmndpth5" "5jwar0bvwr53h40" "unp160hltucmgwb" "pjp6ertnjw1vb6j" "96deaxjjts0yvax" "hjgqzm1pq2mo1u7" "u22e26jm4cy25av" "zdzld8scpb2ydmo" "vnoz56ecccya7hk" "m8fuvpkggx3r7sw" "lnhuzvcq72odm0l" "bx1xq7e3jhb986v" "g35rvb9r3l07tgx" "hd4d44n06f6cawh" "tk2emgkdo0xmkfn" "u5kwzdtotz39jwp" "s7enbyv4of56b3e" "p2v49387cgj37sn" "ggkxy092ul5l6n0" "c2se350v0tr4hzv" "bdov3ye1qvundbp" "ryf84f70g5thl4l" "x0nfe7hwu2yt5az" "hm4cdxlz8t0281v" "q4o2n439h3o4hux" "nrv57g0kvh6lz31" "k3j8o90rkvw7tal" "fq39wkqs1solhdu" "3t9q259awsqqnjn" "x0ztn8ulz4wsjnf" "re53t0m5rdemzxb" "dvywvnh6comawyk" "gs1p8zvs22f3u03" "p2sp49zl9919q87" "2mzs709cn5dlwj2" "p0ptwsbjrbx9q9b" "57znvber9kc1tnj" "mmjryby874fs60o" "9jfa3uck1pm1a27" "5o4n9abwu4u5bb5" "hd9k76vgdu5mql0" "ug2w7q3f6pgxf6e" "8epzoljwn0gbtge" "ghbypuj01msjo4h" "fehlcunq37bd57x" "rn0an20lmaomk57" "jdewbyn3agn01fj" "dtua0nm03dd9yb7" "9h1m4d4lp6ksfjt" "b3jqszmpuje1kzj" "np2r7s44c0nxgg5" "m72afjrfmn9ycnm" "rxol1dxl6nen4em" "4g3g9s2nj7eh7g9" "6wyn194o3vd7jmv" "oeu4jokoabrwtua" "lq01y2l4zvsfb6u" "y71fvh7ol8vycb2" "sh3z56uk3q0ddzd" "zqr31esvemh6qkz" "wf4mq2s0e98ce4z" "au7m9jjmgy8d4gj" "ajztaxb5eyxtfch" "2qu63bjtm9kxnnb" "pojfe0tpsdoned2" "7bwng7h1w9q6djl" "ssbsk72ruz2tdg1" "4ns4fa2xqw95lgx" "le6z045m543xskj" "s79rv87vd0ov68q" "lqk83qqxe3xs6ss" "je7gumyhw5zyax5" "34aswcgc3uv5ln0" "1qs0vvu9krr584a" "x31a150jh7x0sc3" "mghqo7uxufw84xb" "u556z6wn629kwu5" "35q8u5ekgfaq26g" "gstz99vr89krpfz" "hmfoy1dyl3p0oyx" "5cp1qjka53bl73q" "4tcxev3874jk89a" "cavjhtxfs1z091j" "ktwcurcgfv59jnc" "9p57f5mpldrqae2" "22260uys7arz8j6" "70kcrqfjf2xhh4m" "h9du65sl07e0gv9" "as5hvtlqa9ouc2z" "fwexbmobod79xhj" "l7kobq2fs7s15go" "wey1upork0e0epr" "85aq8xl8ayntz3l")
#subjects=("a39zpu2zr3xl47k" "2bqu5ktkg43fh3p" "brsvshywlogvxcq" "9oe8onshjdy2khe" "regcubm60dchcxr" "ds2ot3x1v6q63xg" "hkdemly7qs0y8be" "nvk3jd25saegh24" "gnd1v5zo5x7qjad" "duzv7z4jux5olpv" "mdtwfshb8x8v70p" "x6cu75oy88jq32r" "wwv18727elhq22u" "2nzfnkk2uulwmkz" "8es63ovt8uw2mgg" "jvcerddevc3rwm0" "k7xa918wsdj832n" "6be4c2orpqft8ul" "5pgqs0v0feqyejp" "7pjw0pz87exlgmd")
#subjects=("9p57f5mpldrqae2" "22260uys7arz8j6")
for sub in "${subjects[@]}"; do
    JOB_NAME="rw_${sub}"
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
echo "Rscript ${PATH_BASE}/Learning/fit_learning.R \
    -s ${sub}" >> job.slurm


# submit job to cluster queue and remove it to avoid confusion:
sbatch job.slurm
rm -f job.slurm
sleep 6


done




