#!/bin/bash
# Take the filename
path=$1
name=.filename.txt
remote=$1$name
echo "$remote"
while IFS= read -r line
do
  echo "$line"
  parti='/dev/shm/'
  filename=$parti$line
# Check the file is exists or not
echo "$filename"
if [[ -f $filename ]]
then
    # Remove  the file
    rm "$filename"
else
    echo File does not exist
fi
done < "$remote"
