a = Read(10S)
b = Read(10T)

c = Product(a,b)
d = Filter(c, 0 == 1 AND 4 == 5)

e = Select(2,3 FROM d)
Output(e)
