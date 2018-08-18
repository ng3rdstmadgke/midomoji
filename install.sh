#!/bin/bash

function usage {
cat >&2 <<EOF
#####################################################################################
# [ usage ]
#   ./install.sh [IPADIC_DIR]
#
#####################################################################################
EOF
exit 1
}

SCRIPT_DIR=$(cd $(dirname $0); pwd)
IPADIC_DIR=$1
if [ -z "$IPADIC_DIR" -o ! -d "$IPADIC_DIR" ]; then
  usage
fi
echo  "build source ..."
sbt clean assembly || exit 1
$SCRIPT_DIR/import.sh || exit 1
$SCRIPT_DIR/build.sh || exit 1
