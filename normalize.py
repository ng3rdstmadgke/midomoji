#!/usr/bin/env python3
import unicodedata

while True:
  try:
    terms = input().split(",");
    terms[0] = unicodedata.normalize('NFKC', terms[0])
    print(",".join(terms))
  except EOFError:
    break
