#!/bin/sh

command="tensorboard --logdir tf_logs/"

date_str=$(date '+%Y/%m/%d %H:%M:%S')
date_time=$(echo "$date_str" | sed -e "s/ /-/g" -e "s/\//-/g")
logdir="$HOME/tmp/logs"
logfile_name="tensorboard.${date_time}"
logfile="${logdir}/${logfile_name}"

touch $logfile
$command >> $logfile 2>&1 &

