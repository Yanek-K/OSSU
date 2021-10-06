def oddTuples(aTup):
  '''
  aTup: A tuple

  returns: tuple, every other element of aTup
  '''
  
  word = ()
  index = 0
  while index < len(aTup):
    word += (aTup[index],)
    index += 2
  return word

print(oddTuples((1,2,3)))
