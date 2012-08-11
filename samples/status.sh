#!/bin/sh


STATUSPIPE="/tmp/xmobar_status_jrk"
NORMAL='#eee8d5'
MUTED='#cb4b16'
FGCOLOR="#657b83"

function isMuted () {
    # retrieve mute status
    # return an arbitrary string for true or nothing at all
    echo
}

function getPercent () {
    # somehow retrieve the percent value as plain int (e.g. "66")
    echo "66"
}


function percentBar () {
    local res= i=1
    local percent=$( getPercent )

    if [ -n "$( isMuted )" ]; then
        res="<fc=$MUTED>"
    else
        res="<fc=$NORMAL>"
    fi

    while [ $i -lt $percent ]; do
        res+='#'
        i=$((i+1))
    done

    res+="</fc><fc=$FGCOLOR>"

    while [ $i -lt 100 ]; do
        res+='-'
        i=$((i+1))
    done

    echo "$res</fc>"
}


echo "$( percentBar )" > "$STATUSPIPE"
