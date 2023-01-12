## Update README file of given directory with list of files

# Date and time
dt=$(date '+%d/%m/%Y %H:%M:%S');

# Loop over files in given directory
for entry in "$1"/*
do
  if [[ -f $entry ]]
  then
    echo "${entry:0} $dt" >> "$1/README.md";
  fi
done

# Sort README file
sort -o "$1/README.md"{,}
