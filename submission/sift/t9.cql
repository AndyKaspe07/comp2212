a = Read(9R)
b = Read(9R)

c = Product(a,b)
d = Filter(c, 1 == 2)
e = Select(0,1,3 FROM d)

f = Product(e, a)
g = Filter(f, 2 == 3)
h = Select(0,4 FROM g)

Output(h)
