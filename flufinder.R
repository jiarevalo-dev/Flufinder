# Function 3

split_peptides <- function(peptides) {
  library(stringr)
  lapply(peptides, str_split, pattern="")
}

# Function 6

ggbarplot <- function(peptide_counts_table) {
  
  library(ggplot2)
  ggplot(peptide_counts_table) +
    aes(rownames(peptide_counts_table), peptide_counts) +
    geom_col(fill="blue", width=0.5) +
    theme_bw() +
    labs(x="Flu Strain", y="Peptide Counts")
  
}