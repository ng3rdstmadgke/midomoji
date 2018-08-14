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
POS_ID=$IPADIC_DIR/pos-id.def
# LEFT_ID=$IPADIC_DIR/left-id.def
# RIGHT_ID=$IPADIC_DIR/right-id.def
# REWRITE=$IPADIC_DIR/rewrite.def
# FEATURE=$IPADIC_DIR/feature.def

for file in $MATRIX $POS_ID; do
  SRC=$IPADIC_DIR/$file
  if [ ! -r "$SRC" ]; then
    errlog "$SRC not found."
  fi
done

USER_POS=pos.def
USER_CHAR=char.def
USER_CHAR_TYPE=char_type.def
USER_UNK=unk.def

for file in $USER_CHAR $USER_CHAR_TYPE $USER_UNK; do
  CONFIG=$CONFIG_DIR/$file
  if [ ! -r "$CONFIG" ]; then
    errlog "$CONFIG not found."
  fi
done

infolog "build matrix.def"
nkf -Luw $IPADIC_DIR/$MATRIX | tr " " "\t"  > $DICT_DIR/matrix.tsv
infolog "normalize and sort morpheme.csv"
ls $IPADIC_DIR/*.csv | xargs cat | nkf -Luw | tr "," "\t" | python3 $SCRIPT_DIR/normalize.py | LC_ALL=C sort -t$'\t' -k1 > $DICT_DIR/raw_morpheme.tsv
infolog "build pos.tsv"
cat $DICT_DIR/raw_morpheme.tsv | awk -F$'\t' '{print $5"\t"$6"\t"$7"\t"$8"\t"$9"\t"$10}' | LC_ALL=C sort | uniq > $DICT_DIR/pos.tsv
infolog "copy config"
cat $CONFIG_DIR/$USER_POS | sed -r "s/[[:space:]]*#.*//" | grep -v "^$" >> $DICT_DIR/pos.tsv
cat $CONFIG_DIR/$USER_CHAR | sed -r "s/[[:space:]]*#.*//" | grep -v "^$" > $DICT_DIR/char.tsv
cat $CONFIG_DIR/$USER_CHAR_TYPE | sed -r "s/[[:space:]]*#.*//" | grep -v "^$" > $DICT_DIR/char_type.tsv
cat $CONFIG_DIR/$USER_UNK | sed -r "s/[[:space:]]*#.*//" | grep -v "^$" | python3 $SCRIPT_DIR/morpheme_converter.py unk $DICT_DIR/pos.tsv | LC_ALL=C sort | uniq  > $DICT_DIR/unk.tsv
infolog "build morpheme.tsv"
cat $DICT_DIR/raw_morpheme.tsv | python3 $SCRIPT_DIR/morpheme_converter.py morpheme $DICT_DIR/pos.tsv > $DICT_DIR/morpheme.tsv
rm $DICT_DIR/raw_morpheme.tsv
infolog "Complete!!"
