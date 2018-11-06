#!/bin/bash

# copies OPTIONS.txt file to all artist folders
readarray myarray < filepath.txt
wait
for i in "${myarray[@]}"; do grep "log(Z) = " $i >> logz_out.txt; done 
