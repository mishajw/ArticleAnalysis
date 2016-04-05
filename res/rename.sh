#!/bin/bash

in_dir=$1
out_dir=$2
name=$3
count=1

for line in $in_dir/*; do
  echo $line

  ./docx-to-txt.sh "$line" > $out_dir/$name$count

  count=$((count + 1))
done

