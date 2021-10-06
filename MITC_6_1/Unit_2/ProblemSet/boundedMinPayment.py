balance = 320000
annualInterestRate = 0.2
interestPmonth = annualInterestRate / 12

lower = balance / 12
upper = (balance * (1 + annualInterestRate) ** 12) / 12

def isEnoughBounded(lower, upper, balance, interestPmonth):
  remBal = balance
  payment = (upper - lower / 2) + lower

  if upper - lower < 0.01:
    return(round(upper,2))

  for month in range(0, 12):
    remBal -= payment
    remBal += (interestPmonth * remBal)

    if remBal > 0:
      return(isEnoughBounded(payment, upper, balance, interestPmonth))
    elif remBal < -0.01:
      return(isEnoughBounded(lower, payment, balance, interestPmonth))
    else:
      return (round(payment,2))


print("Lowest Payment:", isEnoughBounded(lower, upper, balance, interestPmonth))

def minPay(lower, upper, balance, annualInterestRate):

    remBal = balance
    payment = round((upper - lower) / 2 + lower,2)

    if upper - lower < 0.01:
        return(upper)

    for month in range(0,12):
        remBal -= payment
        remBal += annualInterestRate / 12 * remBal

    if remBal > 0:
        return(minPay(payment, upper, balance, annualInterestRate))
    elif remBal < -0.01:
        return(minPay(lower, payment, balance, annualInterestRate))
    else:
        return(payment)

lower = round(balance / 12,2)
upper = round((balance * (1 + annualInterestRate) ** 12) / 12,2)

print("Lowest Payment:", minPay(lower, upper, balance, annualInterestRate))
