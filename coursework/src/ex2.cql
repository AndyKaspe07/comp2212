firstFile = Read(EX2A)
stepFile = Filter(firstFile, 0 == 1)
finalFile = Select(2,0 FROM stepFile) 
Output (finalFile)