firstFile = Read(A) //Stores the contents of A.csv in firstFile
secondFile = Read(B) //Stores the contents of B.csv in secondFile
finalFile = Product(firstFile, secondFile) //Stores the cartesian product of firstFile, secondFile in finalFile
Output(finalFile) //Prints the contents of finalFile to stdout
