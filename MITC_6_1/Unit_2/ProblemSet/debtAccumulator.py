 # calculates monthly balance after one year
 # if one only pays the minimum monthly payment each month

balance = 42
interestPYear =  0.2
minPaymentRate = 0.04
mon = 12

for month in range(0,12):
  balance -= (minPaymentRate * balance)
  balance += (interestPYear / 12) * balance

print(round(balance,2))
