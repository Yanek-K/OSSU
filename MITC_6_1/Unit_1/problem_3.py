s = 'azcbobobegghakl'

cur = lng = s[0]
for c in s[1:] + '':
  if c < cur[-1]:
    if len(cur) > len(lng):
      lng = cur 
    cur = ''
  cur += c
print("Longest substring in alphabetical order is:", lng)
