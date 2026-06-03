# Function 3

split_peptides <- function(peptides) {
  library(stringr)
  lapply(peptides, str_split, pattern="")
}
