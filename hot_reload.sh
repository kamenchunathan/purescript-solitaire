#/bin/env bash
npm run dev&

while true; do
    inotifywait -e modify,create,delete -r src public && 

    if [ "$(ps | awk '{ print $4 }' | sed -n '/node/p' | wc -l)" -gt 0 ]
    then
        pkill node
    fi

    npm run dev&
done
