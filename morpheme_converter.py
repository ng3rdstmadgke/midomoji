#!/usr/bin/env python3
import sys

args = sys.argv
pos_path = args[1]
kg_path  = args[2]
kk_path  = args[3]

def create_map(path):
  cnt = 0
  ret = {}
  with open(path) as f:
    while True:
      line = f.readline()
      if line == '':
        break
      ret[line.strip()] = cnt
      cnt += 1
  return ret

pos_map = create_map(pos_path)
kg_map  = create_map(kg_path)
kk_map  = create_map(kk_path)

print_buffer = []
cnt = 0
while True:
  try:
    line = input()
    ss = line.split("\t")
    length = len(ss)
    if length != 12:
      terms = [ss[i] if (i < length) else "" for i in range(13) ]
      prefix = "\t".join(terms[0: 4])
      pos = pos_map["\t".join(terms[4: 8])]
      kg  = kg_map[terms[8]]
      kk  = kk_map[terms[9]]
      suffix = "\t".join(terms[10: 13])
      out = "{}\t{}\t{}\t{}\t{}".format(prefix, pos, kg, kk, suffix)
      print_buffer.append(out)
      cnt += 1
      if cnt % 1000 == 0:
        print("\n".join(print_buffer))
        print_buffer = []
        cnt = 0
    else:
      sys.stderr.write("[Error] convert failed : {}".format(line))
  except EOFError:
    break

if cnt > 0:
  print("\n".join(print_buffer))
