def getGuessedWord(secretWord, lettersGuessed):
  '''
  secretWord: string, the word the user is guessing
  lettersGuessed: list, what letters have been guessed so far
  returns: string, comprised of letters and underscores that represents
    what letters in secretWord have been guessed so far.
  '''
  splitSecretWord = ''

  for char in secretWord:
    if char in lettersGuessed:
      splitSecretWord += char
    else:
      splitSecretWord += "_"

  return splitSecretWord

# lettersGuessed =  ['a', 'p', 'l', 'e', 't']
# print(getGuessedWord('apple', lettersGuessed))
   