# Name: Alvin Han
# SciNet username: tmp_ahan
# Description: Shell script to run processTTC.R against all csv files in data/

for filename in data/*csv
do
    Rscript processTTC.R $filename
done
