animals = { 'a': ['aardvark'], 'b': ['baboon'], 'c': ['coati']}

animals['b'] = ['donkey']
animals['b'].append('dog')
animals['b'].append('dingo')

# # First Solution

# def how_many(aDict):
#     '''
#     aDict: A dictionary, where all the values are lists.

#     returns: int, how many values are in the dictionary.
#     '''
  
#     lst = []
#     animals = aDict.values()

#     for i in animals:
#       if len(i) > 1:
#         for j in i:
#           lst.append(j)
#       else:
#         lst.append(i)
#     print(len(lst))


# # Final Solution after checking answer

# def how_many(aDict):
#     '''
#     aDict: A dictionary, where all the values are lists.

#     returns: int, how many values are in the dictionary.
#     '''
#     result = 0
#     for value in aDict.values():
#       result += len(value)
#       return result


def biggest(aDict):
  """
  aDict: a Dictionary, where all values are lists

  returns: The key with the largest number of values associated with it
  """




def biggest(aDict):
    '''
    aDict: A dictionary, where all the values are lists.

    returns: The key with the largest number of values associated with it
    '''
    result = 0
    solution = ''
    for key in aDict.keys():
      length = len(aDict[key])
      if length > result:
        solution = key
    return solution

print(biggest(animals)) 

  