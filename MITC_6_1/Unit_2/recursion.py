import math

# RECURSIVE N TO POWER OF EXP

def recurPower(base, exp):
    '''
    base: int or float.
    exp: int >= 0

    returns: int or float, base^exp
    '''
    if exp <= 0:
        return 1
    return base * recurPower(base, exp - 1)
        
# print(recurPower(4, 3)


##########################################
# ITERATIVE GREATES COMMON DIVISOR

def gcdIter(a,b): 
  """
  a, b: positive integers

  returns: a positive integer, the greatest common divisor of a & b 
  """
  base = min(a,b)

  while base > 0:
    if a % base == 0 and b % base == 0:
      return base 
    else: 
      base -= 1



# print(gcdIter(17, 12))
    

# RECURSIVE GREATEST COMMON DIVISOR

def gcd(a, b):
  """
  a, b: positive integers

  returns: a positive integer, the greatest common divisor of a & b 
  """
  if b == 0:
    return a
  else: 
    return gcd(b, a%b)

# print(gcd(1071, 462))

# RECURSIVE BISECTION SEARCH, IS CHAR IN STR

def isIn(char, aStr):
  """
  char: a single character
  aStr: an alphabetical string

  returns: True is char is in aStr, False otherwise
  """
  length = len(aStr)
  if length == 1 and char != aStr or length == 0: 
    return False

  center = math.floor(length / 2)

  if char == aStr[center]:
    return True

  elif char < aStr[center]:
    return isIn(char, aStr[:center])

  elif char > aStr[center]:
    return isIn(char, aStr[center:])

  else: 
    return False

print(isIn('h', 'cfffgi'))









