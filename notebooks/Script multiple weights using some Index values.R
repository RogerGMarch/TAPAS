library(igraph)
library(dplyr)
library(rgexf)
library(stringr)

all_years <- seq(2003, 2014, by = 2 )
all_ages <- seq(1, 16, by = 2)

folder_path <- "C:/Users/OMEN/Box/Andrea_Universities/Conferences 2025/Complexity72h/Temporal diseases Project/Data"

setwd(folder_path)

#--------------
# Functions
#--------------
clean <- function(mat) {
  mat[is.na(mat)] <- 0
  mat[is.nan(mat)] <- 0
  diag(mat) <- 0
  return(mat)
}

compute_similarity_matrices <- function(CTables, num_diag, file_prefix, alpha = 0.5, beta = 0.5) {

  for (age in 1:8) {
    # Initialize empty similarity matrices
    Jaccard <- matrix(0, num_diag, num_diag)
    Dice <- matrix(0, num_diag, num_diag)
    Overlap <- matrix(0, num_diag, num_diag)
    Tversky <- matrix(0, num_diag, num_diag)
    JaccardDist <- matrix(0, num_diag, num_diag)
    cases <- matrix(0, num_diag, num_diag)
    
    for (i in 1:num_diag) {
      for (k in 1:num_diag) {
        # Extract 2x2 tables across all year strata for this age group
        data <- CTables[i, k, , age, ]
        
        # Aggregate values from all strata
        a <- sum(data[, 1])  # Both A and B
        b <- sum(data[, 2])  # A but not B
        c <- sum(data[, 3])  # B but not A
        d <- sum(data[, 4])  # Neither A nor B
        
        denom <- a + b + c  # Union of A and B
        if (denom > 0) {
          # Jaccard Index
          J <- a / denom
          Jaccard[i, k] <- J
          
          # Sørensen–Dice coefficient
          Dice[i, k] <- (2 * a) / (2 * a + b + c)
          
          # Overlap (Szymkiewicz–Simpson)
          min_ab <- min(a + b, a + c)
          Overlap[i, k] <- ifelse(min_ab > 0, a / min_ab, 0)
          
          # Tversky index (asymmetric generalization of Jaccard/Dice)
          denom_t <- a + alpha * b + beta * c
          Tversky[i, k] <- ifelse(denom_t > 0, a / denom_t, 0)
          
          # Jaccard Distance
          JaccardDist[i, k] <- 1 - J
          
          # Count of all positive cases
          cases[i, k] <- denom
        }
      }
    }
    
    # Clean matrices to avoid NaNs and fill diagonals with 0
    Jaccard <- clean(Jaccard)
    Dice <- clean(Dice)
    Overlap <- clean(Overlap)
    Tversky <- clean(Tversky)
    JaccardDist <- clean(JaccardDist)
    
    # Write each matrix to CSV file (edit output path as needed)
    write.table(Jaccard, paste0("3.AdjacencyMatrices/Adj_Matrix_Jaccard_", file_prefix, "_age_", age, ".csv"), row.names = FALSE, col.names = FALSE)
    write.table(Dice, paste0("3.AdjacencyMatrices/Adj_Matrix_Dice_", file_prefix, "_age_", age, ".csv"), row.names = FALSE, col.names = FALSE)
    write.table(Overlap, paste0("3.AdjacencyMatrices/Adj_Matrix_Overlap_", file_prefix, "_age_", age, ".csv"), row.names = FALSE, col.names = FALSE)
    write.table(Tversky, paste0("3.AdjacencyMatrices/Adj_Matrix_Tversky_", file_prefix, "_age_", age, ".csv"), row.names = FALSE, col.names = FALSE)
    write.table(JaccardDist, paste0("3.AdjacencyMatrices/Adj_Matrix_JaccardDist_", file_prefix, "_age_", age, ".csv"), row.names = FALSE, col.names = FALSE)
    
    print(paste("Done with age group", age))
  }
}

#---------------------------------------------------------------------------
#                                    Blocks
#---------------------------------------------------------------------------
num_diag = 131
# *************** AGE GROUPS ***********************************************
# Female
CTables <- readRDS("C:/Users/OMEN/Box/Andrea_Universities/Conferences 2025/Complexity72h/Temporal diseases Project/Data/2.ContingencyTables/Blocks_ContingencyTables_Female_Final.rds")
str(CTables)

compute_similarity_matrices(CTables, num_diag, file_prefix = "Female_Blocks", alpha = 0.5, beta = 0.5)

# Male
CTables <- readRDS("C:/Users/OMEN/Box/Andrea_Universities/Conferences 2025/Complexity72h/Temporal diseases Project/Data/2.ContingencyTables/Blocks_ContingencyTables_Male_Final.rds")
str(CTables)

compute_similarity_matrices(CTables, num_diag, file_prefix = "Male_Blocks", alpha = 0.5, beta = 0.5)

#---------------------------------------------------------------------------
#                                    ICD
#---------------------------------------------------------------------------
num_diag=1080
# *************** AGE GROUPS ***********************************************
# Female

CTables <- readRDS("C:/Users/OMEN/Box/Andrea_Universities/Conferences 2025/Complexity72h/Temporal diseases Project/Data/2.ContingencyTables/ICD_ContingencyTables_Female_Final.rds")
str(CTables)

compute_similarity_matrices(CTables, num_diag, file_prefix = "Female_ICD", alpha = 0.5, beta = 0.5)

# Male

CTables <- readRDS("C:/Users/OMEN/Box/Andrea_Universities/Conferences 2025/Complexity72h/Temporal diseases Project/Data/2.ContingencyTables/ICD_ContingencyTables_Male_Final.rds")
str(CTables)

compute_similarity_matrices(CTables, num_diag, file_prefix = "Male_Blocks", alpha = 0.5, beta = 0.5)

#---------------------------------------------------------------------------
#                                    Chronic
#---------------------------------------------------------------------------
num_diag=46

# *************** AGE GROUPS ***********************************************
# Female

CTables <- readRDS("C:/Users/OMEN/Box/Andrea_Universities/Conferences 2025/Complexity72h/Temporal diseases Project/Data/2.ContingencyTables/Chronic_ContingencyTables_Female_Final.rds")
str(CTables)

compute_similarity_matrices(CTables, num_diag, file_prefix = "Female_Chronic", alpha = 0.5, beta = 0.5)


# Male

CTables <- readRDS("C:/Users/OMEN/Box/Andrea_Universities/Conferences 2025/Complexity72h/Temporal diseases Project/Data/2.ContingencyTables/Chronic_ContingencyTables_Male_Final.rds")
str(CTables)

compute_similarity_matrices(CTables, num_diag, file_prefix = "Male_Chronic", alpha = 0.5, beta = 0.5)
