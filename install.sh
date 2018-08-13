#!/bin/bash

function usage {
cat >&2 <<EOF
#####################################################################################
# [ usage ]
#   ./install.sh
#
#####################################################################################
EOF
exit 1
}

SCRIPT_DIR=$(cd $(dirname $0); pwd)
echo  "build source ..."
sbt clean assembly || exit 1
$SCRIPT_DIR/midomoji build-dict || exit 1
$SCRIPT_DIR/midomoji build-matrix || exit 1
$SCRIPT_DIR/midomoji build-config || exit 1
$SCRIPT_DIR/midomoji build-pos-info || exit 1
$SCRIPT_DIR/midomoji check-dict || exit 1
$SCRIPT_DIR/midomoji check-matrix || exit 1
