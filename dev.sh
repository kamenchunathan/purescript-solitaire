while true; do

    inotifywait -e modify,create,delete -r src && 

    if [ "$(ps | awk '{ print $4 }' | sed -n '/node/p' | wc -l)" -gt 0 ]
    then
        pkill node
    else 
        npm run build && npm run serve&
    fi

done
