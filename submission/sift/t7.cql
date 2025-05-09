a = Read(7F)
b = Read(7G)
c = Product(a,b)

d = Filter(c, 1 == 3 AND 2 == 4 AND 1 != "" AND 2 != "")
e = Select(0,5 FROM d)

Output(e)
