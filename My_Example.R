library(XLConnect)
my_variable = loadWorkbook("C:/Users/Alec/Documents/nsnextract.xlsx")

worksheet = readWorksheet(my_variable, sheet = "Manatees_2010", header = TRUE)

print(worksheet)


years=worksheet[0:28,1]

print(years)