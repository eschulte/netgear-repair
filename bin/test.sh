#!/bin/bash
#
# Usage: test.sh [executable]
# run all tests and return the number of tests passed
#

## Ensure everything is setup
if [ ! -d /root/squashfs-root/proc/mounts ];then
    echo "VM not setup, running setup now">&2
    mount -o bind /proc  squashfs-root/proc
    mkdir -p squashfs-root/lib/init
    mount -o bind /lib/init/  squashfs-root/lib/init
    mount -o bind /dev/  squashfs-root/dev
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
declare -a FILES
FILES+=(apply.cgi)
FILES+=(dtree.css)
FILES+=(base.gif)
FILES+=(Add_WPS_Client.htm)
FILES+=(center_language.html)
FILES+=(logo.jpg)
FILES+=(advanced.js)

# turn off authentication
chroot squashfs-root/ /bin/config set hijack_process="0"

for file in ${FILES[@]};do
    subset=$(head -c 4 squashfs-root/www/$file)
    served=$(serve $file)
    return=$?
    if [ $return -eq 0 ] && \
        $(contains "$served" "200 OK") && \
        $(contains "$served" "$subset");then
        PASSED+=($file)
    fi
done

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
