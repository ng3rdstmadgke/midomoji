#!/usr/bin/env python3
import unicodedata

print_buffer = []
cnt = 0
while True:
  try:
    line = unicodedata.normalize('NFKC', input())
    print_buffer.append(line)
    cnt += 1
    if cnt % 1000 == 0:
      print("\n".join(print_buffer))
      print_buffer = []
      cnt = 0
  except EOFError:
    break

if cnt > 0:
  print("\n".join(print_buffer))
