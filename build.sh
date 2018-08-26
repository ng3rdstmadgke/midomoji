#!/bin/bash
set -e

function usage {
cat >&2 <<EOF
#####################################################################################
# [ usage ]
#   ./build.sh [IPADIC_DIR] [options]
#
# [ option ]
#   --force      元ファイルに変更がなくても辞書のビルドを行う
#   --dict-only  scalaのソースのビルドは行わず、辞書のビルドのみ行う
#
#
#####################################################################################
EOF
exit 1
}

SCRIPT_DIR=$(cd $(dirname $0); pwd)
FORCE=
DICT_ONLY=
IPADIC_DIR=
while [ "$#" != 0 ]; do
  if [ "$1" = "--force" ]; then
    FORCE=$1
  elif [ "$1" = "--dict-only" ]; then
    DICT_ONLY=$1
  elif [ -z "$IPADIC_DIR" ]; then
    IPADIC_DIR=$1
  fi
  shift
done

if [ -z "$IPADIC_DIR" -o ! -d "$IPADIC_DIR" ]; then
  usage
fi

if [ -z "$DICT_ONLY" ]; then
  sbt clean assembly
fi

$SCRIPT_DIR/import.sh $IPADIC_DIR
$SCRIPT_DIR/midomoji build-dict $FORCE
$SCRIPT_DIR/midomoji build-matrix $FORCE
$SCRIPT_DIR/midomoji build-config $FORCE
$SCRIPT_DIR/midomoji build-pos-info $FORCE
$SCRIPT_DIR/midomoji build-meta-info $FORCE
$SCRIPT_DIR/midomoji check-dict
$SCRIPT_DIR/midomoji check-matrix

if [ -z "$DICT_ONLY" ]; then
  sbt clean assembly
fi
