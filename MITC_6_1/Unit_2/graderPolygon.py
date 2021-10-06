import math

def areaOfPolygon(n, s):
  """ 
  n: number of sides of the polygon
  s: lenght of each side

  returns: the area of this polygon
  """

  numerator = (0.25 * n * s**2)
  denominator = math.tan((math.pi/n))
  area = (numerator / denominator)
  return area

def perimeterOfPolygon(n, s):
  """
  n: number of sides of the polygon
  s: lenght of each side

  returns: length of boundary of polygon
  """
  return (n*s)

def polysum(n, s):
  """
  n: number of sides of the polygon
  s: lenght of each side

  returns: the sum of area and perimeter of polygon
  """
  area = areaOfPolygon(n, s)
  perimeter = perimeterOfPolygon(n, s)
  squarePerimeter = perimeter ** 2
  sum = (area + squarePerimeter)
  return round(sum, 4)


print(polysum(10, 60))