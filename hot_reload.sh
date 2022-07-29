#/bin/env bash
npm run build && npm run serve&

while true; do
    inotifywait -e modify,create,delete -r src public && 

    if [ "$(ps | awk '{ print $4 }' | sed -n '/node/p' | wc -l)" -gt 0 ]
    then
        pkill node
    fi

    npm run build && npm run serve&
done
