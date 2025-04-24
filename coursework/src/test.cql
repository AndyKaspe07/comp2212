f1 = Read(test)
f2 = Filter(f1, 0 ~= "in" OR 1 ~= "in")
Output(f2)