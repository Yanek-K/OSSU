# def genPrimes(x):
#   p = [1]
#   while len(p) < 3:
#     for n in p:
#       if x % n != 0:
#         p.append(x)
#     x += 2
#   return p




def genPrimes(x):
  p = [2]
  for n in range(x):
    if n % 2 != 0:
      p.append(n)
      print(p)

print(genPrimes(30))
