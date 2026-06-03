# Function 1

upload_fasta <- function(fasta_filename) {
  #Opening seqinr library for handling FASTA files; make sure you have seqinr installed
  
  library(seqinr)
  #Reading the fasta file
  read.fasta(fasta_filename, seqtype = "AA", as.string = TRUE,
             set.attributes = FALSE)
}

#Function 2 

trypsinize <- function(proteins) {
  
  library(stringr)
  
  lapply(proteins, str_split_1, pattern="(?<=R|K)")
}

# Function 3

split_peptides <- function(peptides) {
  library(stringr)
  lapply(peptides, str_split, pattern="")
}


# Function 4

splitpeptides_to_masses <- function(aa) {
  # Generating a vector of masses for each amino acid
  aa_masses <- c(A=71.037, R=156.101, N=114.042, D=115.026, C=103.009,
                 Q=128.058, E=129.042, G=57.021, H=137.058, I=113.084, L=113.084,
                 K=128.094, M=131.040, F=147.068, P=97.052, S=87.032, T=101.047,
                 W=186.079, Y=163.063, V=99.068)
  peptide_masses <- aa
  for(i in 1:length(aa)) {
    peptide_masses[[i]] <- lapply(aa[[i]],
                                  function(x) sum(aa_masses[x]))
  }
  
  lapply(peptide_masses, unlist)
}
#Function 5

count_matching_masses <- 
  function(protein_masses, sample) 
  {
    df <- as.data.frame(sapply(protein_masses, function (x)
      sum(as.character(sample) %in% as.character(x))))
    names(df) <- "peptide_counts"
    return(df)
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