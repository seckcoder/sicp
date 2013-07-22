filename=$1
filetype=${1##*.}
if [ $filetype != "scm" ]
then
    filename=$filename.scm
fi
petite --libdirs lib --script $filename
