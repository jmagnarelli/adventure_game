#!/usr/bin/python

import re
import sys

binary_term = re.compile("(\w+)\(([\d\w]+),([\d\w]+)\)")

def display_maze(facts):
  """turn a list of ansprolog facts into a nice ascii-art maze diagram"""
  max_x = 1
  max_y = 1
  solid = {}
  mine = {}
  lava = {}
  volcano = {}

  for fact in facts:
    m = binary_term.match(fact)
    if m:
      functor, x, y = m.groups()
      x, y = int(x), int(y)
      pos = (x,y)
      max_x, max_y = max(x, max_x), max(y, max_y)
      if functor == "solid":
        solid[pos] = True
      if functor == "mine":
        mine[pos] = True
      if functor == "volcano":
        volcano[pos] = True
      if functor == "islava":
        lava[pos] = True

  def code(x,y):
    """decide how a maze cell should be typeset"""
    pos = (x,y)
    if pos in solid:
      return "x"
    elif pos in mine:
      return "m"
    elif pos in volcano:
      return "v"
    elif pos in lava:
      return "l"
    else:
      return "."

  for y in range(1,max_y+1):
    print "".join([code(x,y)*2 for x in range(1,max_x+1)])

def main():
  """look for lines that contain logical facts and try to turn each of those
  into a maze"""
  for line in sys.stdin.xreadlines():
    line = line.strip()
    if line:
      if line[0].islower():
        facts = line.split(' ')
        display_maze(facts)
      else:
        print "% " + line

if __name__ == "__main__":
  main()
