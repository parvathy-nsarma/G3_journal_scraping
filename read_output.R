#read the output in csv form
journal_csv = read.csv("C:/Users/Parvathy/OneDrive/Desktop/Data Analytics with R/G3.csv")
#read the output in text form
journal_txt = read.table("C:/Users/Parvathy/OneDrive/Desktop/Data Analytics with R/G3.txt", header = TRUE, sep = "\t")

#displaying first few records
head(journal_txt)
head(journal_csv)
