firstFile = Read(EX4A) //Stores EX4A.csv in firstFile
finalFile = Select(0, "foo", 0 FROM firstFile) //Makes a new file using col0 of firstFile and the string "foo"
Output(finalFile) //Prints contents of finalFile