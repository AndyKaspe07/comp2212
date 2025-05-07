firstFile = Read(A) //Stores A.csv in firstFile
stepFile = Filter(firstFile, 0 == 1) //Filters for rows where element in col 0 and col 1 are equal
finalFile = Select(2,0 FROM stepFile) //Selects cols 2 & 0 from stepFile
Output (finalFile) //Prints finalFile