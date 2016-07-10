# Copy externs files to the appropriate folder.
#
# Usage:
#
# $ find flare/output -name 'externs.json' | xargs -L 1 ./stage.sh flare
cp $2 ../$1/$(echo $(basename $(dirname $2))).json
