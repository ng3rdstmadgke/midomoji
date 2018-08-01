#!/bin/bash

function usage {
cat >&2 <<EOF
#####################################################################################
# [ usage ]
#   ./import-dictionary.sh [IPADIC_DIR]
#
#####################################################################################
EOF
exit 1
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

IPADIC_DIR=$1
SCRIPT_DIR=$(dirname $0)
DICT_DIR=$SCRIPT_DIR/dictionary
if [ -z "$IPADIC_DIR" -o ! -d "$IPADIC_DIR" ]; then
  usage
fi
if ! $(which nkf > /dev/null 2>&1); then
  errlog "nkf not found"
elif ! $(which sort > /dev/null 2>&1); then
  errlog "sort not found"
elif ! $(which python3 > /dev/null 2>&1); then
  errlog "sort not found"
fi

if [ ! -d "$DICT_DIR" ]; then
  mkdir -p $DICT_DIR
fi

MATRIX=$IPADIC_DIR/matrix.def
POS_ID=$IPADIC_DIR/pos-id.def
# LEFT_ID=$IPADIC_DIR/left-id.def
# RIGHT_ID=$IPADIC_DIR/right-id.def
# UNK=$IPADIC_DIR/unk.def
# CHAR=$IPADIC_DIR/char.def
# FEATURE=$IPADIC_DIR/feature.def
# REWRITE=$IPADIC_DIR/rewrite.def

for file in $MATRIX $POS_ID; do
  if [ ! -r "$file" ]; then
    errlog "$file not found."
  fi
  nkf -Luw $file > $DICT_DIR/$(basename $file)
done

ls $IPADIC_DIR/*.csv | xargs cat | nkf -Luw | python3 $SCRIPT_DIR/normalize.py | LC_ALL=C sort -t',' -k1 > $DICT_DIR/morpheme.csv
infolog "Complete!!"
