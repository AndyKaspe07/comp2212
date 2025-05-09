a = Read(8P)
b = Read(8Q)
aSwapped = Select(3,0,1,2 FROM a)
bSwapped = Select(3,0,1,2 FROM b)

c = InnerJoin(aSwapped, bSwapped)
d = Select(1,2,3,0,5,6,7,4 FROM c)

e = Select(IF 4 == "" THEN 0 ELSE 4,
           IF 5 == "" THEN 1 ELSE 5,
           IF 6 == "" THEN 2 ELSE 6,
           7 FROM d)


Output(e)