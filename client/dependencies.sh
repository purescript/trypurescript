echo Try PureScript uses the following dependencies.
echo Their licenses are reproduced here:

for dir in bower_components/*
do
    echo
    basename $dir
    echo
    cat $dir/LICENSE* || (echo 'No license file found. bower.json says:' && cat $dir/bower.json | jq '.license')
done
