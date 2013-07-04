#!/bin/bash

cd data/recommendations
grep "Mean" *.txt | sed -e 's/.txt:Mean Rating//' | awk '{print d";"$1}' "d=$(date +'%Y-%m-%d')" > tmp.csv
cut -d ";" tmp.csv -f 1-3 --output-delimiter "   " | sort -k 3 >> recommendations.csv
rm tmp.csv
cd ../..
