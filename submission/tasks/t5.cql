f1 = Read(P) //Stores contents of P.csv
f2 = Read(Q) //Stores contents of Q.csv
f3 = InnerJoin(f1,f2) //InnerJoin on col0

//Some logic to select the right columns
final = Select(0,
	       IF 1 == "" THEN 5 ELSE 1,
               IF 2 == "" THEN 6 ELSE 2,
               IF 3 == "" THEN 7 ELSE 3
               FROM f3)

Output(final) //Prints contents of final
