library(igraph)
library(dplyr)
library(rgexf)
library(stringr)

all_years <- seq(2003, 2014, by = 2 )
all_ages <- seq(1, 16, by = 2)

folder_path <- "C:/Users/OMEN/Box/Andrea_Universities/Conferences 2025/Complexity72h/Temporal diseases Project/Data"

setwd(folder_path)
#---------------------------------------------------------------------------
#                                    Blocks
#---------------------------------------------------------------------------
num_diag = 131
# *************** AGE GROUPS ***********************************************
# Female

CTables <- readRDS("C:/Users/OMEN/Box/Andrea_Universities/Conferences 2025/Complexity72h/Temporal diseases Project/Data/2.ContingencyTables/Blocks_ContingencyTables_Female_Final.rds")
str(CTables)

for (age in 1:8) {
  #age=1
  
  
  Jaccard <- matrix(0, num_diag, num_diag)
  cases <- matrix(0, num_diag, num_diag)
  
  for (i in 1:num_diag) {
    for (k in 1:num_diag) {
      #i=1
      #k=2
      data <- CTables[i, k, , age, ]  # All year strata for this age group
      
      # Filtrar las tablas donde hay al menos 5 casos (como antes)
      #data <- data[data[,1] > 5, , drop = FALSE]
      
      
        # Agregar los valores de cada celda (a, b, c, d) por estrato
        a <- sum(data[, 1])
        b <- sum(data[, 2])
        c <- sum(data[, 3])
        d <- sum(data[, 4])
        
        # Solo computar si hay co-ocurrencias
        denom <- a + b + c
        if (denom > 0) {
          Jaccard[i, k] <- a / denom
          cases[i, k] <- denom
        }
      
    }
  }
  
  # Opcionales: filtros para depuración como antes
  Jaccard[is.na(Jaccard)] <- 0
  Jaccard[is.nan(Jaccard)] <- 0
  diag(Jaccard) <- 0
  #Jaccard[which(cases < 100)] <- 0
  #Jaccard[Jaccard < 0.05] <- 0  # filtro opcional para cortar valores muy bajos
  
  # Guardar matriz
  write.table(Jaccard, paste0("3.AdjacencyMatrices/Adj_Matrix_Jaccard_Female_Blocks_Jaccard_age_", age, ".csv"), row.names = FALSE, col.names = FALSE)
  saveRDS(Jaccard, paste0("3.AdjacencyMatrices/Adj_Matrix_Jaccard_Female_Blocks_Jaccard_age_", age, ".rds"))
  
  print(age)
}


# Male

CTables <- readRDS("C:/Users/OMEN/Box/Andrea_Universities/Conferences 2025/Complexity72h/Temporal diseases Project/Data/2.ContingencyTables/Blocks_ContingencyTables_Male_Final.rds")
str(CTables)

for (age in 1:8) {
  #age=1
  
  
  Jaccard <- matrix(0, num_diag, num_diag)
  cases <- matrix(0, num_diag, num_diag)
  
  for (i in 1:num_diag) {
    for (k in 1:num_diag) {
      #i=1
      #k=2
      data <- CTables[i, k, , age, ]  # All year strata for this age group
      
      # Filtrar las tablas donde hay al menos 5 casos (como antes)
      #data <- data[data[,1] > 5, , drop = FALSE]
      
      
      # Agregar los valores de cada celda (a, b, c, d) por estrato
      a <- sum(data[, 1])
      b <- sum(data[, 2])
      c <- sum(data[, 3])
      d <- sum(data[, 4])
      
      # Solo computar si hay co-ocurrencias
      denom <- a + b + c
      if (denom > 0) {
        Jaccard[i, k] <- a / denom
        cases[i, k] <- denom
      }
      
    }
  }
  
  # Opcionales: filtros para depuración como antes
  Jaccard[is.na(Jaccard)] <- 0
  Jaccard[is.nan(Jaccard)] <- 0
  diag(Jaccard) <- 0
  #Jaccard[which(cases < 100)] <- 0
  #Jaccard[Jaccard < 0.05] <- 0  # filtro opcional para cortar valores muy bajos
  
  # Guardar matriz
  write.table(Jaccard, paste0("3.AdjacencyMatrices/Adj_Matrix_Jaccard_Male_Blocks_Jaccard_age_", age, ".csv"), row.names = FALSE, col.names = FALSE)
  saveRDS(Jaccard, paste0("3.AdjacencyMatrices/Adj_Matrix_Jaccard_Male_Blocks_Jaccard_age_", age, ".rds"))
  
  print(age)
}

#---------------------------------------------------------------------------
#                                    ICD
#---------------------------------------------------------------------------
num_diag=1080
# *************** AGE GROUPS ***********************************************
# Female

CTables <- readRDS("C:/Users/OMEN/Box/Andrea_Universities/Conferences 2025/Complexity72h/Temporal diseases Project/Data/2.ContingencyTables/ICD_ContingencyTables_Female_Final.rds")
str(CTables)

for (age in 1:8) {
  #age=1
  
  
  Jaccard <- matrix(0, num_diag, num_diag)
  cases <- matrix(0, num_diag, num_diag)
  
  for (i in 1:num_diag) {
    for (k in 1:num_diag) {
      #i=1
      #k=2
      data <- CTables[i, k, , age, ]  # All year strata for this age group
      
      # Filtrar las tablas donde hay al menos 5 casos (como antes)
      #data <- data[data[,1] > 5, , drop = FALSE]
      
      
      # Agregar los valores de cada celda (a, b, c, d) por estrato
      a <- sum(data[, 1])
      b <- sum(data[, 2])
      c <- sum(data[, 3])
      d <- sum(data[, 4])
      
      # Solo computar si hay co-ocurrencias
      denom <- a + b + c
      if (denom > 0) {
        Jaccard[i, k] <- a / denom
        cases[i, k] <- denom
      }
      
    }
  }
  
  # Opcionales: filtros para depuración como antes
  Jaccard[is.na(Jaccard)] <- 0
  Jaccard[is.nan(Jaccard)] <- 0
  diag(Jaccard) <- 0
  #Jaccard[which(cases < 100)] <- 0
  #Jaccard[Jaccard < 0.05] <- 0  # filtro opcional para cortar valores muy bajos
  
  # Guardar matriz
  write.table(Jaccard, paste0("3.AdjacencyMatrices/Adj_Matrix_Jaccard_Female_ICD_Jaccard_age_", age, ".csv"), row.names = FALSE, col.names = FALSE)
  saveRDS(Jaccard, paste0("3.AdjacencyMatrices/Adj_Matrix_Jaccard_Female_ICD_Jaccard_age_", age, ".rds"))
  
  print(age)
}


# Male

CTables <- readRDS("C:/Users/OMEN/Box/Andrea_Universities/Conferences 2025/Complexity72h/Temporal diseases Project/Data/2.ContingencyTables/ICD_ContingencyTables_Male_Final.rds")
str(CTables)

for (age in 1:8) {
  #age=1
  
  
  Jaccard <- matrix(0, num_diag, num_diag)
  cases <- matrix(0, num_diag, num_diag)
  
  for (i in 1:num_diag) {
    for (k in 1:num_diag) {
      #i=1
      #k=2
      data <- CTables[i, k, , age, ]  # All year strata for this age group
      
      # Filtrar las tablas donde hay al menos 5 casos (como antes)
      #data <- data[data[,1] > 5, , drop = FALSE]
      
      
      # Agregar los valores de cada celda (a, b, c, d) por estrato
      a <- sum(data[, 1])
      b <- sum(data[, 2])
      c <- sum(data[, 3])
      d <- sum(data[, 4])
      
      # Solo computar si hay co-ocurrencias
      denom <- a + b + c
      if (denom > 0) {
        Jaccard[i, k] <- a / denom
        cases[i, k] <- denom
      }
      
    }
  }
  
  # Opcionales: filtros para depuración como antes
  Jaccard[is.na(Jaccard)] <- 0
  Jaccard[is.nan(Jaccard)] <- 0
  diag(Jaccard) <- 0
  #Jaccard[which(cases < 100)] <- 0
  #Jaccard[Jaccard < 0.05] <- 0  # filtro opcional para cortar valores muy bajos
  
  # Guardar matriz
  write.table(Jaccard, paste0("3.AdjacencyMatrices/Adj_Matrix_Jaccard_Male_ICD_Jaccard_age_", age, ".csv"), row.names = FALSE, col.names = FALSE)
  saveRDS(Jaccard, paste0("3.AdjacencyMatrices/Adj_Matrix_Jaccard_Male_ICD_Jaccard_age_", age, ".rds"))
  
  print(age)
}

#---------------------------------------------------------------------------
#                                    Chronic
#---------------------------------------------------------------------------
num_diag=46

# *************** AGE GROUPS ***********************************************
# Female

CTables <- readRDS("C:/Users/OMEN/Box/Andrea_Universities/Conferences 2025/Complexity72h/Temporal diseases Project/Data/2.ContingencyTables/Chronic_ContingencyTables_Female_Final.rds")
str(CTables)

for (age in 1:8) {
  #age=1
  
  
  Jaccard <- matrix(0, num_diag, num_diag)
  cases <- matrix(0, num_diag, num_diag)
  
  for (i in 1:num_diag) {
    for (k in 1:num_diag) {
      #i=1
      #k=2
      data <- CTables[i, k, , age, ]  # All year strata for this age group
      
      # Filtrar las tablas donde hay al menos 5 casos (como antes)
      #data <- data[data[,1] > 5, , drop = FALSE]
      
      
      # Agregar los valores de cada celda (a, b, c, d) por estrato
      a <- sum(data[, 1])
      b <- sum(data[, 2])
      c <- sum(data[, 3])
      d <- sum(data[, 4])
      
      # Solo computar si hay co-ocurrencias
      denom <- a + b + c
      if (denom > 0) {
        Jaccard[i, k] <- a / denom
        cases[i, k] <- denom
      }
      
    }
  }
  
  # Opcionales: filtros para depuración como antes
  Jaccard[is.na(Jaccard)] <- 0
  Jaccard[is.nan(Jaccard)] <- 0
  diag(Jaccard) <- 0
  #Jaccard[which(cases < 100)] <- 0
  #Jaccard[Jaccard < 0.05] <- 0  # filtro opcional para cortar valores muy bajos
  
  # Guardar matriz
  write.table(Jaccard, paste0("3.AdjacencyMatrices/Adj_Matrix_Jaccard_Female_Chronic_Jaccard_age_", age, ".csv"), row.names = FALSE, col.names = FALSE)
  saveRDS(Jaccard, paste0("3.AdjacencyMatrices/Adj_Matrix_Jaccard_Female_Chronic_Jaccard_age_", age, ".rds"))
  
  print(age)
}


# Male

CTables <- readRDS("C:/Users/OMEN/Box/Andrea_Universities/Conferences 2025/Complexity72h/Temporal diseases Project/Data/2.ContingencyTables/Chronic_ContingencyTables_Male_Final.rds")
str(CTables)

for (age in 1:8) {
  #age=1
  
  
  Jaccard <- matrix(0, num_diag, num_diag)
  cases <- matrix(0, num_diag, num_diag)
  
  for (i in 1:num_diag) {
    for (k in 1:num_diag) {
      #i=1
      #k=2
      data <- CTables[i, k, , age, ]  # All year strata for this age group
      
      # Filtrar las tablas donde hay al menos 5 casos (como antes)
      #data <- data[data[,1] > 5, , drop = FALSE]
      
      
      # Agregar los valores de cada celda (a, b, c, d) por estrato
      a <- sum(data[, 1])
      b <- sum(data[, 2])
      c <- sum(data[, 3])
      d <- sum(data[, 4])
      
      # Solo computar si hay co-ocurrencias
      denom <- a + b + c
      if (denom > 0) {
        Jaccard[i, k] <- a / denom
        cases[i, k] <- denom
      }
      
    }
  }
  
  # Opcionales: filtros para depuración como antes
  Jaccard[is.na(Jaccard)] <- 0
  Jaccard[is.nan(Jaccard)] <- 0
  diag(Jaccard) <- 0
  #Jaccard[which(cases < 100)] <- 0
  #Jaccard[Jaccard < 0.05] <- 0  # filtro opcional para cortar valores muy bajos
  
  # Guardar matriz
  write.table(Jaccard, paste0("3.AdjacencyMatrices/Adj_Matrix_Jaccard_Male_Chronic_Jaccard_age_", age, ".csv"), row.names = FALSE, col.names = FALSE)
  saveRDS(Jaccard, paste0("3.AdjacencyMatrices/Adj_Matrix_Jaccard_Male_Chronic_Jaccard_age_", age, ".rds"))
  
  print(age)
}

