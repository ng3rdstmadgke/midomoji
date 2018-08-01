#!/bin/bash

function usage {
cat >&2 <<EOF
#####################################################################################
# [ usage ]
#   ./install.sh [ options ]
#
# [ option ]
#   --check 辞書の構築は行わず、チェックのみ行う
#
#####################################################################################
EOF
}

function errlog {
  MSG=$1
  echo "[Error] $MSG" >&2
  exit 1
}

function infolog {
  MSG=$1
  echo "[Info] $MSG" >&2
}

CHECK_ONLY=
while [ "$#" != 0 ]; do
  if [ "$1" = "--check" ]; then
    CHECK_ONLY=$1
  fi
  shift
done

SCRIPT_DIR=$(cd $(dirname $0); pwd)
DICT_DIR=$SCRIPT_DIR/dictionary
MATRIX=$DICT_DIR/matrix.def
MORPHEME=$DICT_DIR/morpheme.csv

if [ ! -r "$MATRIX" ]; then
  errlog "$MATRIX is not found."
elif [ ! -r "$MORPHEME" ]; then
  errlog "$MORPHEME is not found."
fi

infolog "build source ..."
sbt clean assembly

DICT_SET=$DICT_DIR/dictionary_set.bin
JAR_FILE=$SCRIPT_DIR/target/scala-2.12/midomoji.jar

if [ -z "$CHECK_ONLY" ]; then
  infolog "build dictionary ..."
  java -jar $JAR_FILE --build $MATRIX $MORPHEME $DICT_SET
fi

if [ ! -e "$DICT_SET" ]; then
  errlog "$DICT_SET is not found."
fi

infolog "check matrix ..."
java -jar $JAR_FILE --check-matrix $DICT_SET $MATRIX
infolog "check prefixtree ..."
java -jar $JAR_FILE --check-prefixtree $DICT_SET $MORPHEME


