#!/bin/bash

function usage {
cat >&2 <<EOF
#####################################################################################
# [ usage ]
#   ./build.sh
#
#####################################################################################
EOF
exit 1
}

SCRIPT_DIR=$(cd $(dirname $0); pwd)
echo  "build dict ..."
$SCRIPT_DIR/midomoji build-dict || exit 1
echo  "build matrix ..."
$SCRIPT_DIR/midomoji build-matrix || exit 1
echo  "build config ..."
$SCRIPT_DIR/midomoji build-config || exit 1
echo  "build pos-info ..."
$SCRIPT_DIR/midomoji build-pos-info || exit 1
echo  "check dict ..."
$SCRIPT_DIR/midomoji check-dict || exit 1
echo  "check matrix ..."
$SCRIPT_DIR/midomoji check-matrix || exit 1
