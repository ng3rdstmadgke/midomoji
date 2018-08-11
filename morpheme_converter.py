#!/usr/bin/env python3
import sys

args = sys.argv
mode     = args[1]
pos_path = args[2]
kg_path  = args[3]
kk_path  = args[4]

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

def parse_morpheme(terms):
  if len(terms) != 13:
    return None
  front = "\t".join(terms[0:4])
  pos   = pos_map["\t".join(terms[4:8])]
  kg    = kg_map[terms[8]]
  kk    = kk_map[terms[9]]
  back  = "\t".join(terms[10: 13])
  return "{}\t{}\t{}\t{}\t{}".format(front, pos, kg, kk, back)

def parse_unk(terms):
  if len(terms) != 5:
    return None
  terms[4]   = str(pos_map[terms[4].replace(",", "\t")])
  return "\t".join(terms)


pos_map = create_map(pos_path)
kg_map  = create_map(kg_path)
kk_map  = create_map(kk_path)

if mode == "unk":
  processor = parse_unk
else:
  processor = parse_morpheme
print_buffer = []
cnt = 0
while True:
  try:
    line = input()
    out = processor(line.split("\t"))
    if out is None:
      sys.stderr.write("[Error] convert failed : {}\n".format(line))
      continue
    print_buffer.append(out)
    cnt += 1
    if cnt % 1000 == 0:
      print("\n".join(print_buffer))
      print_buffer = []
      cnt = 0
  except EOFError:
    break
  except Exception as e:
    sys.stderr.write("[Error] convert failed : {}\n".format(line))
    print(e)

if cnt > 0:
  print("\n".join(print_buffer))
