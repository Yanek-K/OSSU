def isWordGuessed(secretWord, lettersGuessed):
  '''
  secretWord: string, the word the user is guessing
  lettersGuessed: list, what letters have been guessed so far
  returns: boolean, True if all the letters of secretWord are in lettersGuessed;
    False otherwise
  '''
  splitSecretWord = []

  for char in secretWord:
    splitSecretWord += char
    if char in lettersGuessed:
        splitSecretWord.remove(char)

  return len(splitSecretWord) == 0


# lettersGuessed =  ['a', 'l', 'p', 'e', 'p']
# print(isWordGuessed('apple', lettersGuessed))
  