import re
s = 'azcbobobegghakl'

l = len(s)
start = 0 
end = 3
c = 0

if l < 3:
  print("Number of times bob occurs is: 0")
else:
  while (l >= 3):
    match = s[start:end]
    if match == "bob":
      c += 1
    end += 1
    start += 1
    l -=1
  print("Number of times bob occurs is:", c)