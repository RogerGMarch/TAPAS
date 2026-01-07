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
    Lift <- matrix(0, num_diag, num_diag)
    RelRisk <- matrix(0, num_diag, num_diag)
    Phi <- matrix(0, num_diag, num_diag)
    Jaccard <- matrix(0, num_diag, num_diag)
    Cosine <- matrix(0, num_diag, num_diag)
    Kulczynski <- matrix(0, num_diag, num_diag)
    JointPrev<- matrix(0, num_diag, num_diag)
    cases <- matrix(0, num_diag, num_diag)
    
    for (i in 1:num_diag) {
      for (k in 1:num_diag) {
        # Extract 2x2 tables across all year strata for this age group
        data <- CTables[i, k, , age, ]
        
        # Sum over strata
        a <- sum(data[, 1])  # Present in both
        b <- sum(data[, 2])  # Present in A only
        c <- sum(data[, 3])  # Present in B only
        d <- sum(data[, 4])  # Absent in both
        n <- a + b + c + d
        
        if (n > 0) {
          # (1) Lift
          denom_lift <- (a + b) * (a + c)
          Lift[i, k] <- ifelse(denom_lift > 0, (n * a) / denom_lift, 0)
          
          # (2) Relative Risk
          denom_rr <- c * (a + b)
          RelRisk[i, k] <- ifelse(denom_rr > 0, (a * (c + d)) / (c * (a + b)), 0)
          
          # (3) Phi Coefficient
          num_phi <- (a * d) - (b * c)
          denom_phi <- sqrt((a + b) * (a + c) * (b + d) * (c + d))
          Phi[i, k] <- ifelse(denom_phi > 0, num_phi / denom_phi, 0)
          
          # (4) Jaccard
          denom_jac <- a + b + c
          Jaccard[i, k] <- ifelse(denom_jac > 0, a / denom_jac, 0)
          
          # (5) Cosine
          denom_cos <- sqrt((a + b) * (a + c))
          Cosine[i, k] <- ifelse(denom_cos > 0, a / denom_cos, 0)
          
          # (6) Kulczynski
          part1 <- ifelse(a+b > 0, a / (a+b), 0)
          part2 <- ifelse((a+c) > 0, a / (a+c), 0)
          Kulczynski[i, k] <- 0.5 * (part1 + part2)
          
          # (7) Joint Prevalence
          JointPrev[i, k] <- a / n
        }
      }
    }
    
    # Clean matrices
    Lift <- clean(Lift)
    RelRisk <- clean(RelRisk)
    Phi <- clean(Phi)
    Jaccard <- clean(Jaccard)
    Cosine <- clean(Cosine)
    Kulczynski <- clean(Kulczynski)
    JointPrev <- clean(JointPrev)
    
    # Save all matrices
    write.table(Lift, paste0("3.AdjacencyMatrices/Adj_Matrix_Lift_", file_prefix, "_age_", age, ".csv"), row.names = FALSE, col.names = FALSE)
    write.table(RelRisk, paste0("3.AdjacencyMatrices/Adj_Matrix_RelRisk_", file_prefix, "_age_", age, ".csv"), row.names = FALSE, col.names = FALSE)
    write.table(Phi, paste0("3.AdjacencyMatrices/Adj_Matrix_Phi_", file_prefix, "_age_", age, ".csv"), row.names = FALSE, col.names = FALSE)
    write.table(Jaccard, paste0("3.AdjacencyMatrices/Adj_Matrix_Jaccard_", file_prefix, "_age_", age, ".csv"), row.names = FALSE, col.names = FALSE)
    write.table(Cosine, paste0("3.AdjacencyMatrices/Adj_Matrix_Cosine_", file_prefix, "_age_", age, ".csv"), row.names = FALSE, col.names = FALSE)
    write.table(Kulczynski, paste0("3.AdjacencyMatrices/Adj_Matrix_Kulczynski_", file_prefix, "_age_", age, ".csv"), row.names = FALSE, col.names = FALSE)
    write.table(JointPrev, paste0("3.AdjacencyMatrices/Adj_Matrix_JointPrev_", file_prefix, "_age_", age, ".csv"), row.names = FALSE, col.names = FALSE)
    
    print(paste("Done with age group", age))
  }
}

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

compute_similarity_matrices(CTables, num_diag, file_prefix = "Male_ICD", alpha = 0.5, beta = 0.5)