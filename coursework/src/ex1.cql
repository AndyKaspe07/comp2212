firstFile = Read(EX1A) //Stores the contents of EX1A.csv in firstFile
secondFile = Read(EX1B) //Stores the contents of EX1B.csv in secondFile
finalFile = Product(firstFile, secondFile) //Stores the cartesian product of firstFile, secondFile in finalFile
Output(finalFile) //Prints the contents of finalFile to stdout
