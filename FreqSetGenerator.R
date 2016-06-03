
#### APRIORI ALGORITHM FOR FREQUENT ITEMSET/RULE GENERATION
# Reads a Java-prestructured, tab-delimited text file
# Returns Tableau-ready text file


#### 1) SET YOUR DIRECTORY

setwd("/Users/...")


#### 2) CHOOSE YOUR INPUT FILE
#			Sample input:	"input1-r.txt" (Transaction X Product)


# Read in the data
x <- scan("input1-r.txt", what="", sep="\n")
# Separate elements by one or more tab
y <- strsplit(x, '\t+')
# Extract the first vector element and set it as the list element name
names(y) <- sapply(y, `[[`, 1)
# Remove the first vector element from each list element
y <- lapply(y, `[`, -1)
# Build Transaction List
trans <- as(y, "transactions")


#### 3) CHOOSE SUPPORT/CONFIDENCE LEVELS FOR FREQUENT ITEMSET/RULE GENERATION

## Find Frequent Items Sets Using Apriori Algo
capture.output(cat("\nFREQUENT ITEMSET GENERATION:  minSup = 1/100\n"), file = "output.txt")
capture.output(cat("---------------------------------------------\n"), file = "output.txt", append=TRUE)
itemSets <- apriori(trans, parameter = list(supp=1/100, target = "frequent itemsets"))
capture.output(cat("\n\nFREQUENT ITEMSETS\n"), file = "output.txt", append = TRUE)
capture.output(cat("-----------------\n\n"), file = "output.txt", append=TRUE)
capture.output(inspect(sort(itemSets, by="support")), file = "output.txt", append=TRUE)
# Find max frequent itemsets (no frequent superset)
maxSets <- itemSets[is.maximal(itemSets), by="support"]
capture.output(cat("\n\nMAX FREQUENT ITEMSETS\n"), file = "output.txt", append = TRUE)
capture.output(cat("---------------------\n\n"), file = "output.txt", append=TRUE)
capture.output(inspect(sort(maxSets, by="support")), file = "output.txt", append=TRUE)
# Find closed frequent itemsets (no frequent superset of equal support)
closedSets <- itemSets[is.closed(itemSets), by="support"]
capture.output(cat("\n\nCLOSED FREQUENT ITEMSETS\n"), file = "output.txt", append=TRUE)
capture.output(cat("------------------------\n\n"), file = "output.txt", append=TRUE)
capture.output(inspect(sort(closedSets, by="support")), file = "output.txt", append=TRUE)

## Find Frequent Association Rules Using Apriori
rules <- apriori(trans,parameter = list(minlen=2, maxlen =5, supp=1/100, conf=1/10, ext = F))
capture.output(cat("\n\nFREQUENT ASSOCIATION RULE GENERATION:  minSup = 1/100, minConf = 1/10\n"), file = "output.txt", append = TRUE)
capture.output(cat("---------------------------------------------------------------------\n\n"), file = "output.txt", append=TRUE)
capture.output(inspect(sort(rules, by="lift")), file = "output.txt", append=TRUE)
