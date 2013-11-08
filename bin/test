#!/bin/bash
#
# Usage: test
# run all tests and return the number of tests passed
#

## Ensure everything is setup
if [ ! -f /root/squashfs-root/proc/mounts ];then
    echo "VM not setup, running setup now">&2
    mount -o bind /proc  squashfs-root/proc
    mkdir -p squashfs-root/lib/init
    mount -o bind /lib/init/  squashfs-root/lib/init
    mount -o bind /dev/  squashfs-root/dev
    chroot squashfs-root/ /bin/datalib
    chroot squashfs-root/ /bin/config set dns_hijack="0"
fi

## Functions
contains(){
    local it="$1"; local item="$2";
    echo "$it"|grep "$item" >/dev/null 2>/dev/null; }

serve(){
    URI="$1" QUERY="$2" call-cgi chroot squashfs-root /usr/sbin/net-cgi; }

declare -a PASSED
exit_hook(){
    for passed in ${PASSED[@]};do echo $passed; done; }
trap exit_hook EXIT

## Setup
chroot squashfs-root/ /bin/config set dns_hijack="0"

## Positive Tests
#
# At least one each of every extension.
# cgi
# css
# gif
# htm
# html
# jpg
# js
declare -A FILES
FILES[securityquestions.cgi]="<TD nowrap align=\"right\">Security Question #2\*:</TD>"
FILES[dtree.css]="PADDING-RIGHT: 2px; PADDING-LEFT: 2px;"
FILES[base.gif]="GIF89"
FILES[Add_WPS_Client.htm]="client does not support the WPS function"
FILES[center_language.html]="Downloading and updating the language table"
FILES[logo.jpg]="Ducky"
FILES[advanced.js]="change_menu_height();"

# turn off authentication
chroot squashfs-root/ /bin/config set hijack_process="0"

for file in ${!FILES[@]};do
    served=$(serve "$file")
    return=$?
    if [ $return -eq 0 ] && \
        $(contains "$served" "200 OK") && \
        $(contains "$served" "$FILES[file]");then
        PASSED+=($file)
    fi
    # echo "returned $return"
    # echo "served $served"
    # echo "subset $subset"
    # if $(contains "$served" "200 OK");then
    #     echo "has the 200"
    # else
    #     echo "does not have the 200"
    # fi
    # if $(contains "$served" "$subset");then
    #     echo "has the subset"
    # else
    #     echo "does not have the subset"
    # fi
done
exit 0

## Negative Tests

# turn on authentication
chroot squashfs-root/ /bin/config set hijack_process="3"

# 1. no authentication for BRS_* files

served=$(serve BRS_01_checkNet.html)
return=$?
if [ $return -eq 0 ] && \
    $(contains "$served" "401 Unauthorized") && \
    $(contains "$served" "unauth.cgi");then
    PASSED+=("BRS-authentication")
fi

# 2. no authentication if unauth.cgi or securityquestions.cgi are in
#    the query string, e.g., /protected_page.htm?foo=unauth.cgi

for cheat in unauth.cgi securityquestions.cgi;do
    served=$(serve "center_language.html" "foo=$cheat")
    return=$?
    if [ $return -eq 0 ] && \
        $(contains "$served" "401 Unauthorized") && \
        $(contains "$served" "unauth.cgi");then
        PASSED+=("${cheat}-authentication")
    fi
done
