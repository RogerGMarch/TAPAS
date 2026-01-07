#----------------------
# Libraries
#---------------------
library(readr)
library(igraph)
library(tidyr)
library(dplyr)
library(purrr)

#-------------------
# Reading the data
#-------------------
# Column names
Blocks_All <- read_csv("C:/Users/OMEN/Box/Andrea_Universities/Conferences 2025/Complexity72h/Temporal diseases Project/Data/Blocks_All.csv")
Chronic_All <- read_csv("C:/Users/OMEN/Box/Andrea_Universities/Conferences 2025/Complexity72h/Temporal diseases Project/Data/Chronic_All.csv")
ICD10_Diagnoses_All <- read_csv("C:/Users/OMEN/Box/Andrea_Universities/Conferences 2025/Complexity72h/Temporal diseases Project/Data/ICD10_Diagnoses_All.csv")
# Prevalence
Prevalence_Sex_Age_Year_ICD <- read_csv("C:/Users/OMEN/Box/Andrea_Universities/Conferences 2025/Complexity72h/Temporal diseases Project/Data/1.Prevalence/Prevalence_Sex_Age_Year_ICD.csv")%>%
  mutate(
    AgeGroup = case_when(
      Age_Group == "0-9"   ~ "age_1",
      Age_Group == "10-19" ~ "age_2",
      Age_Group == "20-29" ~ "age_3",
      Age_Group == "30-39" ~ "age_4",
      Age_Group == "40-49" ~ "age_5",
      Age_Group == "50-59" ~ "age_6",
      Age_Group == "60-69" ~ "age_7",
      Age_Group == "70-79" ~ "age_8",
      TRUE ~ NA_character_  # in case there are other unexpected values
    ))

Degree_Prevalence_ICD=read_csv("C:/Users/OMEN/Box/Andrea_Universities/Conferences 2025/Complexity72h/Temporal diseases Project/Data/Degree_Prevalence_ICD.csv")
# Set the folder path
folder_path <- "C:/Users/OMEN/Box/Andrea_Universities/Conferences 2025/Complexity72h/Temporal diseases Project/Data/3.AdjacencyMatrices"

# List all files that start with 'Adj_Matrix_' and contain Female/Male, Blocks/Chronic/ICD, and age/year
file_list <- list.files(
  path = folder_path,
  pattern = "^Adj_Matrix_(Female|Male)_(Blocks|Chronic|ICD)_(age_\\d+|year_\\d{4}-\\d{4})\\.csv$",
  full.names = TRUE
)

# Check if files were found
cat("Number of matching files:", length(file_list), "\n")

# Function to extract metadata and return a tidy dataframe with added columns
read_and_tag_matrix <- function(file_path) {
  
  #file_path=file_list[29]
  
  # Extract file name
  file_name <- basename(file_path)
  
  # Extract metadata using regular expression
  matches <- regmatches(file_name, regexec("^Adj_Matrix_(Female|Male)_(Blocks|Chronic|ICD)_(age_\\d+|year_\\d{4}-\\d{4})\\.csv$", file_name))[[1]]
  
  gender <- matches[2]
  aggregation <- matches[3]
  age_group <- matches[4]
  
  
  
  # Read the CSV file
  df <- read.csv(file_path, sep = " ",header = F)
  
  # Fix colnames and rownames depending on aggregation type
  if (aggregation == "Blocks") {
    names <- unique(Blocks_All$block_name)
  } else if (aggregation == "ICD") {
    names <- unique(ICD10_Diagnoses_All$icd_code)
  } else if (aggregation == "Cronic") {
    names <- unique(Chronic_All$icd_code)
  } else {
    stop("Invalid aggregation type. Must be one of: 'Blocks', 'ICD', or 'Cronic'.")
  }
  
  # Apply to both columns and rows
  if (length(names) == ncol(df) && length(names) == nrow(df)) {
    colnames(df) <- names
    rownames(df) <- names
  } else {
    warning("The number of unique names does not match the dimensions of the matrix.")
  }
  
  # Convert to long format if matrix-style (wide) data
  df_long <- as.data.frame(df)
  df_long$RowID <- rownames(df)  # Add row index (assumes symmetric matrix with no headers)
  df_long <- tidyr::pivot_longer(df_long, cols = -RowID, names_to = "ColID", values_to = "Value")
  
  # Add metadata columns
  df_long$Gender <- gender
  df_long$Aggregation <- aggregation
  df_long$AgeGroup <- age_group
  
  return(df_long)
}

# Apply the function to all files and combine into one data frame
all_data <- file_list %>%
  lapply(read_and_tag_matrix) %>%
  bind_rows()



test_data=all_data%>%
  filter(Aggregation=="ICD"&Gender=="Female"&AgeGroup=="age_1")

test_data%>%
  left_join(Prevalence_Sex_Age_Year_ICD%>%select(-c(Age_Group))%>%
              group_by(sex,AgeGroup,icd_code)%>%summarise(p=median(p)),by=c("Gender"="sex","AgeGroup"="AgeGroup","RowID"="icd_code"))%>%
  View()


table(unique(Prevalence_Sex_Age_Year_ICD$icd_code)%in%unique(all_data$RowID))

library(dplyr)
library(stringr)

# Suppose:
# all_data: your adjacency matrix long-format
# icd_probs: your ICD p-values data (with 'sex', 'Age_Group', 'year', 'icd_code', 'p')

# Standardize column names
Prevalence_Sex_Age_Year_ICD <- Prevalence_Sex_Age_Year_ICD %>%
  rename(Gender = sex, AgeGroup = Age_Group)

# STEP 1: Create helper to find p_row where icd_code in RowID
match_icd_row <- function(rowid, gender, agegroup) {
  matches <- Prevalence_Sex_Age_Year_ICD %>%
    filter(Gender == gender, str_detect(agegroup, as.character(year)), str_detect(rowid, icd_code))
  if (nrow(matches) == 0) return(NA_real_)
  return(mean(matches$p))  # or max(), first(), etc.
}

# STEP 2: Same for ColID
match_icd_col <- function(colid, gender, agegroup) {
  matches <- Prevalence_Sex_Age_Year_ICD %>%
    filter(Gender == gender, str_detect(agegroup, as.character(year)), str_detect(colid, icd_code))
  if (nrow(matches) == 0) return(NA_real_)
  return(mean(matches$p))
}

# STEP 3: Apply to all rows (this is vectorized using `rowwise`)
all_data <- all_data %>%
  filter()
  rowwise() %>%
  mutate(
    p_row = match_icd_row(RowID, Gender, AgeGroup),
    p_col = match_icd_col(ColID, Gender, AgeGroup)
  ) %>%
  ungroup()







