#!/bin/bash

function usage {
cat >&2 <<EOF
#####################################################################################
# [ usage ]
#   ./install.sh
#
#####################################################################################
EOF
}

SCRIPT_DIR=$(cd $(dirname $0); pwd)
echo  "build source ..."
sbt clean assembly
$SCRIPT_DIR/midomoji -m build || exit 1
$SCRIPT_DIR/midomoji -m check || exit 1
