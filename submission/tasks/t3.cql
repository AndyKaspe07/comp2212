firstFile = Read(A) //Stores A.csv in firstFile
finalFile = Filter(firstFile, 1 != "") //Filters for rows in firstFile where col1 is not empty
Output(finalFile) //Prints finalFile