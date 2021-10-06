print("Please think of a number between 0 and 100!")

start = 0
end = 100
guess = 50
c = False

while c != True:
  print("Is your secret number " + str(guess) + "?")
  close = input("Enter 'h' to indicate the guess is too high. Enter 'l' to indicate the guess is too low. Enter 'c' to indicate I guessed correctly. ")
  if close == 'h':
    end = guess
  elif close == 'l':
    start = guess
  elif close == 'c':
    c = True
  else: print("Sorry, I did not understand your input.")
  guess = (start + end) // 2

print("Game over. Your secrent number was: " + str(guess))


 