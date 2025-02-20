#!/bin/bash
#sh 1_0_tracking_tracktor.sh {YYYYMMDDhhmmss} $SGE_TASK_ID $CPU

date=${1:0:8}
echo $date

wd=/home/daikisato/projects/tracking_fly
mkdir -p  ${wd}/video_images/${date}
mkdir -p  ${wd}/rawdata/${date}
mkdir -p  ${wd}/finaldata/${date}

files=($(ls ${wd}/videos/${date}/${1}*no*male*.mp4 | xargs basename -a | rev | cut -c 5- | rev | uniq))
index=$(( $2-1 ))
file=${files[$index]}

echo $file
prefix=${file:0:19}
echo $prefix

ffmpeg -y -i ${wd}/videos/${date}/${file}.mp4 -ss 1 -t 1 -r 1 ${wd}/video_images/${date}/${file}_image.jpg
echo 'Copied a image of a frame.'
python ${wd}/codes/1_1_run_tracktor.py ${wd}/videos/${date}/${file}.mp4 ${wd}/video_images/${date}/${file}_image.jpg ${wd}/rawdata/${date}/ $3
echo 'Finished tracking by tracktor.'
ffmpeg -y -i ${wd}/rawdata/${date}/${file}_tracked.avi ${wd}/rawdata/${date}/${file}_tracked.mp4
python ${wd}/codes/1_2_fix_tracks.py ${wd}/rawdata/${date}/${file}_tracked.csv $3
python ${wd}/codes/1_3_integrate_info.py ${wd}/rawdata/${date}/${file}_tracked_fixed.tsv ${wd}/videos/${date}/${prefix}.tsv ${wd}/finaldata/${date}/${file}_tracked_fixed_final.tsv
