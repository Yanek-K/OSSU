class intSet(object):
    """An intSet is a set of integers
    The value is represented by a list of ints, self.vals.
    Each int in the set occurs in self.vals exactly once."""

    def __init__(self):
        """Create an empty set of integers"""
        self.vals = []

    def insert(self, e):
        """Assumes e is an integer and inserts e into self""" 
        if not e in self.vals:
            self.vals.append(e)

    def member(self, e):
        """Assumes e is an integer
           Returns True if e is in self, and False otherwise"""
        return e in self.vals

    def remove(self, e):
        """Assumes e is an integer and removes e from self
           Raises ValueError if e is not in self"""
        try:
            self.vals.remove(e)
        except:
            raise ValueError(str(e) + ' not found')

    def __str__(self):
        """Returns a string representation of self"""
        self.vals.sort()
        return '{' + ','.join([str(e) for e in self.vals]) + '}'

    def intersect(self, other):
        """Returns a new intSet containing elemtents that appear in both sets"""
        result = []
        for num in self.vals:
          if num in other.vals:
            result.append(num)
        return '{' + ','.join([str(e) for e in result]) + '}'

    
    def __len__(self):
        """Returns the number of elements in self"""
        count = 0
        for num in self.vals:
          count += 1
        return count

setA = intSet()
setB = intSet()
setA.insert(12)
setA.insert(1)
setA.insert(2)
setB.insert(12)
setB.insert(1)
setB.insert(4)
setB.insert(16)

print(setA.intersect(setB))