#!/bin/bash
readarray myarray < foldernamesnoquotes.txt

# To start loop from 44 do n=0
# for i in "${myarray[@]:43}" 

# index begins from 0
n=0
for i in "${myarray[@]}"

do
	((n++))	
	echo "Started processing creator $n"	
	cd $i
	/home/brewer/local/anaconda3/bin/python3.6 patreonsg.py > output.txt
	wait
	echo "Completed patreonsg" >> output.txt
	./main >> output.txt
	wait
	echo "Completed generating samples" >> output.txt
	/home/brewer/local/anaconda3/bin/python3.6 showresultsloop.py >> output.txt
	wait
	echo "Completed all tasks for index" >> output.txt
	cd ..
	echo "Completed all tasks for creator $n"
	sleep 10m
	wait
done
