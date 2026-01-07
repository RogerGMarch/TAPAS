library(igraph)
library(tidyverse)
library(ggplot2)
library(plotly)

options(scipen = 999)
#-------------------
# Reading the data
#-------------------
# Column names
Blocks_All <- read_csv("C:/Users/OMEN/Box/Andrea_Universities/Conferences 2025/Complexity72h/Temporal diseases Project/Data/Blocks_All.csv")
Chronic_All <- read_csv("C:/Users/OMEN/Box/Andrea_Universities/Conferences 2025/Complexity72h/Temporal diseases Project/Data/Chronic_All.csv")
ICD10_Diagnoses_All <- read_csv("C:/Users/OMEN/Box/Andrea_Universities/Conferences 2025/Complexity72h/Temporal diseases Project/Data/ICD10_Diagnoses_All.csv")
# List of files
all_files <- list.files("3.AdjacencyMatrices/", pattern = "^Adj_Matrix_.*\\.csv$", full.names = TRUE)

#------------------
# Functions
#------------------
extract_network_metrics_from_files <- function(file_paths, blocks_metadata) {

  
  all_metrics <- list()
  
  for (file in file_paths) {
    #file_paths=all_files
    #file=file_paths[1]
    #file="3.AdjacencyMatrices/Adj_Matrix_Male_ICD_age_4.csv"
    
    if (!file.exists(file)) {
      warning(paste("File not found:", file))
      next
    }
    
    # Extract metadata from filename
    parts <- str_split(basename(file), "_")[[1]]
    
    # Define known metric names
    known_metrics <- c("Jaccard", "Lift", "RelRisk", "Phi", "Cosine", "Kulczynski", "JointPrev")
    
    # Check if the third part is a known metric
    if (parts[3] %in% known_metrics) {
      # Format: Adj_Matrix_<metric>_<gender>_<type>_age_<N>.csv
      metric_type <- parts[3]
      gender <- parts[4]
      net_type <- parts[5]
      age_group <- gsub("\\.csv", "", parts[7])
    } else {
      # Format: Adj_Matrix_<gender>_<type>_age_<N>.csv → assume OR
      metric_type <- "OR"
      gender <- parts[3]
      net_type <- parts[4]
      age_group <- gsub("\\.csv", "", parts[6])
    }
    
    
    # Read matrix and clean negatives
    adj <- as.matrix(read.table(file, sep = " ", header = FALSE))
    adj[adj < 0] <- 0
    
    # Build weighted graph (weights as similarity)
    g_w <- graph_from_adjacency_matrix(adj, mode = "undirected", weighted = TRUE, diag = FALSE)
    
    # Compute inverse weights for distance-based metrics
    inv_weights <- ifelse(E(g_w)$weight > 0, 1 / E(g_w)$weight, 0)   
    
    # Compute density (custom weighted version)
    total_weight <- sum(E(g_w)$weight, na.rm = TRUE)
    max_weight <- max(E(g_w)$weight, na.rm = TRUE)
    n <- vcount(g_w)
    max_possible_edges <- n * (n - 1) / 2
    weighted_density <- total_weight / (max_weight * max_possible_edges)
    
    # Compute average path length with distance = 1/weight
    avg_path_length <- mean_distance(g_w, weights = inv_weights, directed = FALSE)
    
    # Node IDs
    # Determine node IDs based on metric_type
    if (net_type == "Blocks") {
      node_ids <- unique(Blocks_All$block_name)
    } else if (net_type == "ICD") {
      node_ids <- unique(ICD10_Diagnoses_All$icd_code)
    } else if (net_type == "Chronic") {
      node_ids <- unique(Chronic_All$icd_code)
    } else {
      stop(paste("Unknown net_type:", net_type))
    }
    
    # Compute metrics
    metrics <- tibble(
      node = node_ids,
      weighted_degree = strength(g_w),
      closeness = closeness(g_w, weights = inv_weights, normalized = TRUE),
      betweenness = betweenness(g_w, weights = inv_weights, normalized = TRUE),
      eigen_centrality = eigen_centrality(g_w, weights = E(g_w)$weight)$vector,
      pagerank = page_rank(g_w, weights = E(g_w)$weight)$vector,
      community = membership(cluster_louvain(g_w, weights = E(g_w)$weight)),
      age_group = age_group,
      metric_type = metric_type,
      gender = gender,
      net_type = net_type,
      weighted_density = weighted_density,
      average_path_length = avg_path_length,
      modularity = modularity(cluster_louvain(g_w, weights = E(g_w)$weight), weights = E(g_w)$weight)
    )
    
    all_metrics[[file]] <- metrics
    print(paste("Processed:", gender, net_type, metric_type, age_group))
  }
  
  # Combine and return all results
  network_metrics <- bind_rows(all_metrics)
  return(network_metrics)
}

#-------------------------------------------------
# Results by gender, age and level of resolution
#-------------------------------------------------
# Example Chronic
net_data <- extract_network_metrics_from_files(all_files[grepl("Chronic",all_files)], Blocks_All)
write_csv(net_data, "AllWeightedMetrics_withDistanceInverse_Chronic.csv")

#-------------------------------------------------
# Plots
#-------------------------------------------------
library(ggplot2)
library(dplyr)
library(tidyr)

colnames(net_data)

# Define métricas a incluir
metrics_to_plot <- c("weighted_density", "weighted_degree", "closeness", 
                     "betweenness", "average_path_length", "modularity")

net_data %>%
  pivot_longer(
    cols = all_of(metrics_to_plot),
    names_to = "metric_name",
    values_to = "metric_value"
  ) %>%
  group_by(age_group, metric_type, metric_name, gender) %>%
  summarise(mean_value = mean(metric_value, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(
    x = age_group,
    y = mean_value,
    color = metric_type,
    group = interaction(metric_type, gender),
    linetype = gender
  )) +
  geom_line(size = 1) +
  geom_point() +
  facet_wrap(~ metric_name, scales = "free") +
  scale_linetype_manual(values = c("Female" = "solid", "Male" = "dashed")) +
  labs(
    title = "Chronic Mean Network Metrics by Age Group, Similarity Measure and Gender",
    x = "Age Group",
    y = "Mean Value",
    color = "Similarity Metric",
    linetype = "Gender"
  ) +
  theme_classic() +
  theme(legend.position = "bottom")


#----------------------
# Blocks
#----------------------
net_data <- extract_network_metrics_from_files(all_files[grepl("Blocks",all_files)], Blocks_All)
write_csv(net_data, "AllWeightedMetrics_withDistanceInverse_Blocks.csv")

#-------------------------------------------------
# Plots
#-------------------------------------------------
library(ggplot2)
library(dplyr)
library(tidyr)

colnames(net_data)

# Define métricas a incluir
metrics_to_plot <- c("weighted_density", "weighted_degree", "closeness", 
                     "betweenness", "average_path_length", "modularity")

net_data %>%
  pivot_longer(
    cols = all_of(metrics_to_plot),
    names_to = "metric_name",
    values_to = "metric_value"
  ) %>%
  group_by(age_group, metric_type, metric_name, gender) %>%
  summarise(mean_value = mean(metric_value, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(
    x = age_group,
    y = mean_value,
    color = metric_type,
    group = interaction(metric_type, gender),
    linetype = gender
  )) +
  geom_line(size = 1) +
  geom_point() +
  facet_wrap(~ metric_name, scales = "free") +
  scale_linetype_manual(values = c("Female" = "solid", "Male" = "dashed")) +
  labs(
    title = "Blocks Mean Network Metrics by Age Group, Similarity Measure and Gender",
    x = "Age Group",
    y = "Mean Value",
    color = "Similarity Metric",
    linetype = "Gender"
  ) +
  theme_classic() +
  theme(legend.position = "bottom")


#----------------------
# ICD
#----------------------
net_data <- extract_network_metrics_from_files(all_files[grepl("ICD",all_files)], ICD_All)
write_csv(net_data, "AllWeightedMetrics_withDistanceInverse_ICD.csv")

#-------------------------------------------------
# Plots
#-------------------------------------------------
library(ggplot2)
library(dplyr)
library(tidyr)

colnames(net_data)

# Define métricas a incluir
metrics_to_plot <- c("weighted_density", "weighted_degree", "closeness", 
                     "betweenness", "average_path_length", "modularity")

net_data %>%
  pivot_longer(
    cols = all_of(metrics_to_plot),
    names_to = "metric_name",
    values_to = "metric_value"
  ) %>%
  group_by(age_group, metric_type, metric_name, gender) %>%
  summarise(mean_value = mean(metric_value, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(
    x = age_group,
    y = mean_value,
    color = metric_type,
    group = interaction(metric_type, gender),
    linetype = gender
  )) +
  geom_line(size = 1) +
  geom_point() +
  facet_wrap(~ metric_name, scales = "free") +
  scale_linetype_manual(values = c("Female" = "solid", "Male" = "dashed")) +
  labs(
    title = "ICD Mean Network Metrics by Age Group, Similarity Measure and Gender",
    x = "Age Group",
    y = "Mean Value",
    color = "Similarity Metric",
    linetype = "Gender"
  ) +
  theme_classic() +
  theme(legend.position = "bottom")


