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
$SCRIPT_DIR/midomoji build-dict || exit 1
$SCRIPT_DIR/midomoji build-matrix || exit 1
$SCRIPT_DIR/midomoji build-config || exit 1
$SCRIPT_DIR/midomoji build-pos-info || exit 1
$SCRIPT_DIR/midomoji build-meta-info || exit 1
$SCRIPT_DIR/midomoji check-dict || exit 1
$SCRIPT_DIR/midomoji check-matrix || exit 1
cp $SCRIPT_DIR/dictionary/dict.bin $SCRIPT_DIR/src/main/resources/
cp $SCRIPT_DIR/dictionary/matrix.bin $SCRIPT_DIR/src/main/resources/
cp $SCRIPT_DIR/dictionary/config.bin $SCRIPT_DIR/src/main/resources/
cp $SCRIPT_DIR/dictionary/pos_info.bin $SCRIPT_DIR/src/main/resources/
cp $SCRIPT_DIR/dictionary/meta_info.bin $SCRIPT_DIR/src/main/resources/
