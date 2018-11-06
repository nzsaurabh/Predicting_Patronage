#!/bin/bash
/home/brewer/local/anaconda3/bin/python3.6 patreonsg.py
wait
echo "Completed patreonsg"
./main
wait
echo "Completed generating samples"
/home/brewer/local/anaconda3/bin/python3.6 showresultsloop.py
wait
echo "Completed all tasks"
