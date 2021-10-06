def iterPower(base, exp):
    '''
    base: int or float.
    exp: int >= 0
 
    returns: int or float, base^exp
    '''
    b = exp
    while b > 1:
        base  *=  2
        b -= 1
        print(b)
    return base


print(iterPower(2, 4))
print(2**4)