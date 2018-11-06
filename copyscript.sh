#!/bin/bash

# copies OPTIONS.txt file to all artist folders
readarray myarray < foldernamesnoquotes.txt
wait
for i in "${myarray[@]}"; do cp -p /home/saurabh/Patreon/showresultsloop.py $i; done
