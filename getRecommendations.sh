#!/bin/bash

if [ -z "$1" ]; then 
	echo usage: $0 symbolsfile
	exit
fi

for i in `cat $1`; do 
	./parseRecommendation.pl $i > data/recommendations/"$i.txt"; 
done
