#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#     Create Table_1.csv and Table_2.csv from the IC 
# 
# Dorleta Garc?a 
# 2021/04/14
#  modified:  2022-04-27 16:43:14 (ssanchez@azti.es) - adapt given WKANGHAKE2022
#             2023-04-25 08:08:49 (ssanchez@azti.es) - update for WGBIE2023
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
require(dplyr)

data_file <- file.path(taf.data.path(), "InterCatch", "CatchAndSampleDataTables.txt")

# Total number of tons in the data
aux   <- strsplit(read.table(file.path(taf.data.path(), "InterCatch", "caton.txt"), skip = 7)[1,1], ",")[[1]]
caton <- as.numeric(paste0(aux[1], aux[2]))

# Total number of individuals in the data
aux   <- strsplit(as.character(read.table(file.path(taf.data.path(), "InterCatch", "canum.txt"), skip = 7)),  ",")
canum <- sum(as.numeric(sapply(aux, function(x) paste0(x[1], ifelse(is.na(x[2]),"", x[2]), ifelse(is.na(x[3]),"", x[3])))))

lines <- readLines(data_file)

loc.tab1 <- which(lines == "TABLE 1.")
loc.tab2 <- which(lines == "TABLE 2.")

first.row.tab1 <- loc.tab1 + 7
last.row.tab1  <- loc.tab2 - 3

first.row.tab2 <- loc.tab2 + 7 # in Table 2 we don"t need the last row because we can read until the end of the file.


colN <- read.table(data_file, header = FALSE, skip = loc.tab1 + 3, nrows = 1, sep = "\t") 

# tab1 with the catch data in weight.
tab1 <- as_tibble(read.table(data_file, header = FALSE,  sep = "\t", 
                              skip = first.row.tab1 - 1 , col.names = colN,
                              nrow = last.row.tab1 - first.row.tab1 + 1))
tab1$CATON <- tab1$CATON/1000 # CATON in kg

# We replace the "," in "All - reported, nonreported and misreported" to avoid problems in the generation of the csv.
tab1$ReportingCategory <- ifelse(tab1$ReportingCategory ==  "All - reported, nonreported and misreported", 
                                 "All - reported nonreported and misreported", tab1$ReportingCategory)



# Check that the kilograms in tab1 are the same as those in file 
if(round(sum(tab1$CATON)/caton, 3) != 1) 
  stop("Something is wrong! the catch in TABLE 1 is different from number in caton.txt file")

# tab2 with the catch data in length.
colN <- read.table(data_file, header = FALSE, skip = loc.tab2 + 3, nrows = 1, sep = "\t")
tab2 <- as_tibble(read.table(data_file, header = FALSE,  sep = "\t", 
                             skip = first.row.tab2 - 1 , col.names = colN))

# We replace the "," in "All - reported, nonreported and misreported" to avoid problems in the generation of the csv.
tab2$ReportingCategory <- ifelse(tab2$ReportingCategory ==  "All - reported, nonreported and misreported", 
                                 "All - reported nonreported and misreported", tab2$ReportingCategory)

# Check that the kilograms in tab1 are the same as those in file 
if(round(sum(tab2$CANUM)/canum, 3) != 1) 
  stop("Something is wrong! the number of individuals in TABLE 2 is different from number in canum.txt file")

# Write the tables 
write.csv(tab1, file = file.path(catd_wd, "Table_1.csv"), row.names = FALSE, quote = FALSE)
write.csv(tab2, file = file.path(catd_wd, "Table_2.csv"), row.names = FALSE, quote = FALSE)
          
