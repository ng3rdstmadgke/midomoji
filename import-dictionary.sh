#!/bin/bash
set -e

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
CONFIG_DIR=$SCRIPT_DIR/config
if [ -z "$IPADIC_DIR" -o ! -d "$IPADIC_DIR" ]; then
  usage
fi
if ! $(which nkf > /dev/null 2>&1); then
  errlog "nkf not found"
elif ! $(which sort > /dev/null 2>&1); then
  errlog "sort not found"
elif ! $(which python3 > /dev/null 2>&1); then
  errlog "python3 not found"
fi

if [ ! -d "$DICT_DIR" ]; then
  mkdir -p $DICT_DIR
fi

MATRIX=matrix.def
# POS_ID=$IPADIC_DIR/pos-id.def
# LEFT_ID=$IPADIC_DIR/left-id.def
# RIGHT_ID=$IPADIC_DIR/right-id.def
# REWRITE=$IPADIC_DIR/rewrite.def
# FEATURE=$IPADIC_DIR/feature.def

for file in $MATRIX; do
  SRC=$IPADIC_DIR/$file
  if [ ! -r "$SRC" ]; then
    errlog "$SRC not found."
  fi
done

CHAR=char.def
CHAR_TYPE=char_type.def
UNK=unk.def

for file in $CHAR $CHAR_TYPE $UNK; do
  CONFIG=$CONFIG_DIR/$file
  if [ ! -r "$CONFIG" ]; then
    errlog "$CONFIG not found."
  fi
done

infolog "build matrix.def"
nkf -Luw $IPADIC_DIR/$MATRIX | tr " " "\t"  > $DICT_DIR/matrix.tsv
infolog "normalize and sort morpheme.csv"
ls $IPADIC_DIR/*.csv | xargs cat | nkf -Luw | tr "," "\t" | python3 $SCRIPT_DIR/normalize.py | LC_ALL=C sort -t$'\t' -k1 > $DICT_DIR/morpheme.tsv.tmp
infolog "build pos.tsv"
cat $DICT_DIR/morpheme.tsv.tmp | awk -F$'\t' '{print $5"\t"$6"\t"$7"\t"$8}' | LC_ALL=C sort | uniq > $DICT_DIR/pos.tsv
infolog "build katsuyou_gata.tsv"
cat $DICT_DIR/morpheme.tsv.tmp | awk -F$'\t' '{print $9}' | LC_ALL=C sort | uniq > $DICT_DIR/katsuyou_gata.tsv
infolog "build katsuyou_kei.tsv"
cat $DICT_DIR/morpheme.tsv.tmp | awk -F$'\t' '{print $10}' | LC_ALL=C sort | uniq > $DICT_DIR/katsuyou_kei.tsv
infolog "build morpheme.tsv"
cat $DICT_DIR/morpheme.tsv.tmp | python3 $SCRIPT_DIR/morpheme_converter.py $DICT_DIR/pos.tsv $DICT_DIR/katsuyou_gata.tsv $DICT_DIR/katsuyou_kei.tsv > $DICT_DIR/morpheme.tsv
rm $DICT_DIR/morpheme.tsv.tmp
infolog "copy config"
cat $CONFIG_DIR/$CHAR | sed -r "s/[[:space:]]*#.*//" | grep -v "^$" > $DICT_DIR/char.tsv
cat $CONFIG_DIR/$CHAR_TYPE | sed -r "s/[[:space:]]*#.*//" | grep -v "^$" > $DICT_DIR/char_type.tsv
cat $CONFIG_DIR/$UNK | sed -r "s/[[:space:]]*#.*//" | grep -v "^$" > $DICT_DIR/unk.tsv
infolog "Complete!!"
