#!/bin/bash

cd data/recommendations
grep "Mean" *.txt | sed -e 's/.txt:Mean Rating//' | cut -d ";" -f 1-5 --output-delimiter " " | sort -k 2
cd ../..

