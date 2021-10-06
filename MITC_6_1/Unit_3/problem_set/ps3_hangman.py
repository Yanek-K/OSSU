# Hangman game
#

# -----------------------------------
# Helper code
# You don't need to understand this helper code,
# but you will have to know how to use the functions
# (so be sure to read the docstrings!)

import random
import string

WORDLIST_FILENAME = "words.txt"

def loadWords():
    """
    Returns a list of valid words. Words are strings of lowercase letters.
    
    Depending on the size of the word list, this function may
    take a while to finish.
    """
    print("Loading word list from file...")
    # inFile: file
    inFile = open(WORDLIST_FILENAME, 'r')
    # line: string
    line = inFile.readline()
    # wordlist: list of strings
    wordlist = line.split()
    print("  ", len(wordlist), "words loaded.")
    return wordlist

def chooseWord(wordlist):
    """
    wordlist (list): list of words (strings)

    Returns a word from wordlist at random
    """
    return random.choice(wordlist)

# end of helper code
# -----------------------------------

# Load the list of words into the variable wordlist
# so that it can be accessed from anywhere in the program
wordlist = loadWords()

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
    

def hangman(secretWord):
    '''
    secretWord: string, the secret word to guess.

    Starts up an interactive game of Hangman.

    * At the start of the game, let the user know how many 
      letters the secretWord contains.

    * Ask the user to supply one guess (i.e. letter) per round.

    * The user should receive feedback immediately after each guess 
      about whether their guess appears in the computers word.

    * After each round, you should also display to the user the 
      partially guessed word so far, as well as letters that the 
      user has not yet guessed.

    Follows the other limitations detailed in the problem write-up.
    '''

    # Start message
    secretWord = chooseWord(wordlist)
    print("Welcome to the game, Hangman!")
    print("I am thinking of a word that is:", len(secretWord), 'letters long')
    print('-------------')
    print("The secret word is:", secretWord)

    # Setup variables
    lettersGuessed = ''
    mistakesMade = 0
    guessesLeft = 8
    allGuesses = ''

    # Start Game 
    while mistakesMade < guessesLeft:
      print("You have", guessesLeft, 'guesses left.' )

      availableLetters = getAvailableLetters(lettersGuessed)
      print("Available Letters:", availableLetters)

      guess = input("Please guess a letter: ")
      guessInLowerCase = guess.lower()
      lettersGuessed += guessInLowerCase


      if isWordGuessed(secretWord, lettersGuessed) == True:
        print("Good guess:", getGuessedWord(secretWord, lettersGuessed))
        print('-------------') 
        print("Congratulations, you won!")
        break

      elif guessInLowerCase in secretWord and guessInLowerCase in allGuesses:
        print("Oops! You've already guessed that letter:", getGuessedWord(secretWord, lettersGuessed))
        print('-------------') 
      
      elif guessInLowerCase in secretWord and guessInLowerCase not in allGuesses:
          print("Good guess:", getGuessedWord(secretWord, lettersGuessed))
          print('-------------') 
          allGuesses += guessInLowerCase

      elif guessInLowerCase not in secretWord and guessInLowerCase not in allGuesses:
          print('Oops! That letter is not in my word:', getGuessedWord(secretWord, lettersGuessed))
          print('-------------') 
          guessesLeft -= 1
          allGuesses += guessInLowerCase

      else:
        print("Oops! You've already guessed that letter:", getGuessedWord(secretWord, lettersGuessed))
        print('-------------') 

    # End game
    if mistakesMade == guessesLeft:
      print('Sorry, you ran out of guesses. The word was', secretWord)
      

secretWord = chooseWord(wordlist)
hangman(secretWord)
