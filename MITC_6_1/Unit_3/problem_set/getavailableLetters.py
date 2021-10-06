import string

def getAvailableLetters(lettersGuessed):
  '''
  lettersGuessed: list, what letters have been guessed so far
  returns: string, comprised of letters that represents what letters have not
    yet been guessed.
    '''

  allLetters = string.ascii_lowercase
  result = ''

  for letter in allLetters:
    if letter not in lettersGuessed:
      result += letter
  return result
  

# lettersGuessed = ['e','i','k','p','r','s']
# print(getAvailableLetters(lettersGuessed))
