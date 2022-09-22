#!/bin/bash
entries=$(grep Acinetobacter ${1} | wc -l)
patternMatchedAccessions=$(grep Acinetobacter ${1} | grep "WP_005" | wc -l)
largestAccession=$(grep Acinetobacter ${1} | sort -n -t ',' -k 4 -r | head -1 | cut -f 1 -d ',')
echo Working with data file ${1}.
echo The total number of Acinetobacter entries is ${entries} .
echo The total number of Acinetobacter entries is $patternMatchedAccessions
echo The accession code for the Acinetobacter entry with the largest taxid is $largestAccession
