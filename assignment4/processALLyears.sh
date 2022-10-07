for filename in data/*csv
do
    Rscript processTTC.R $filename
done
