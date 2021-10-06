balance = 3329
annualInterestRate = 20

def isEnough(monthlyPayment, remBal, annualInterestRate):
  for month in range(0, 12):
    remBal -= monthlyPayment
    remBal += annualInterestRate / 12 * remBal
  return remBal > 0

monthlyPayment = int(balance / 12 - (balance / 12) % 10)
remBal = balance

while isEnough(monthlyPayment, remBal, annualInterestRate):
  monthlyPayment += 0.01

print("Lowest Payment:", monthlyPayment)
