library(igraph)
library(tidyverse)
library(ggplot2)
library(plotly)


options(scipen = 999)

#---------------------------------------------------------------------------
#                                    Blocks
#---------------------------------------------------------------------------
Blocks_All <- read_csv("C:/Users/OMEN/Box/Andrea_Universities/Conferences 2025/Complexity72h/Temporal diseases Project/Data/Blocks_All.csv")

# Folder path
folder_path <- "C:/Users/OMEN/Box/Andrea_Universities/Conferences 2025/Complexity72h/Temporal diseases Project/Data"
setwd(folder_path)

# Metric types and age groups
metrics_list <- c("Jaccard", "Lift", "RelRisk", "Phi", "Cosine","Kulczynski","JointPrev")
age_groups <- 1:8
path <- "3.AdjacencyMatrices/"

#------------
# Female
#------------
# Prepare list to store results
all_metrics <- list()

# Loop over all metrics and age groups
for (metric in metrics_list) {
  for (age in age_groups) {
    
    #metric="Jaccard"
    #age=1
    file <- paste0(path, "Adj_Matrix_", metric, "_Female_Blocks_age_", age, ".csv")
    
    if (file.exists(file)) {
      adj <- as.matrix(read.table(file, sep = " ", header = FALSE))
      # Set negative values to zero
      adj[adj < 0] <- 0
      g <- graph_from_adjacency_matrix(adj, mode = "undirected", weighted = TRUE, diag = FALSE)
      
      # Compute node-level metrics
      metrics <- tibble(
        node = unique(Blocks_All$block_name),
        weighted_degree = strength(g),
        clustering_weighted = transitivity(g, type = "weighted", weights = E(g)$weight),
        eigen_centrality = eigen_centrality(g, weights = E(g)$weight)$vector,
        betweenness_weighted = betweenness(g, weights = E(g)$weight, normalized = TRUE),
        closeness_weighted = closeness(g, weights = E(g)$weight, normalized = TRUE),
        pagerank_weighted = page_rank(g, weights = E(g)$weight)$vector,
        kcore = coreness(g),
        age_group = paste0("age_", age),
        metric_type = metric
      )
      
      # Community detection
      comm <- cluster_louvain(g, weights = E(g)$weight)
      metrics$community <- membership(comm)
      
      # Append to global list
      all_metrics[[paste0(metric, "_", age)]] <- metrics
      print(paste("Processed:", metric, "age", age))
    } else {
      warning(paste("File not found:", file))
    }
  }
}

# Combine all into one dataframe
network_metrics <- bind_rows(all_metrics)

# Save combined data
write_csv(network_metrics, "AllNetworkMetrics_Comparison_Female_Blocks.csv")

# ======= PLOT METRICS OVER TIME BY METRIC TYPE =======

metric_names <- c("weighted_degree", "clustering_weighted", "eigen_centrality","betweenness_weighted","closeness_weighted","pagerank_weighted", "kcore")

# Mean
ggplotly(network_metrics %>%
  pivot_longer(cols = c("weighted_degree", "clustering_weighted", "eigen_centrality", 
                        "betweenness_weighted", "closeness_weighted", "pagerank_weighted", "kcore"),
               names_to = "metric_name",
               values_to = "metric_value")%>%
  group_by(age_group, metric_type, metric_name) %>%
  summarise(mean_value = mean(metric_value, na.rm = TRUE),max_value = max(metric_value, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = age_group, y = mean_value, color = metric_type, group = metric_type)) +
  geom_line(size = 1) + geom_point() +
  facet_wrap(~ metric_name, scales = "free") +
  labs(title = "Female Mean Network Metrics by Age Group and Similarity Measure",
       x = "Age Group", y = "Mean Value", color = "Similarity Metric") +
  theme_classic() +
  theme(legend.position = "bottom"))

# Max
ggplotly(network_metrics %>%
           pivot_longer(cols = c("weighted_degree", "clustering_weighted", "eigen_centrality", 
                                 "betweenness_weighted", "closeness_weighted", "pagerank_weighted", "kcore"),
                        names_to = "metric_name",
                        values_to = "metric_value")%>%
           group_by(age_group, metric_type, metric_name) %>%
           summarise(mean_value = mean(metric_value, na.rm = TRUE),max_value = max(metric_value, na.rm = TRUE), .groups = "drop") %>%
           ggplot(aes(x = age_group, y = max_value, color = metric_type, group = metric_type)) +
           geom_line(size = 1) + geom_point() +
           facet_wrap(~ metric_name, scales = "free") +
           labs(title = "Female Max Network Metrics by Age Group and Similarity Measure",
                x = "Age Group", y = "Mean Value", color = "Similarity Metric") +
           theme_classic() +
           theme(legend.position = "bottom"))


#------------
# Male
#------------
# Prepare list to store results
all_metrics <- list()

# Loop over all metrics and age groups
for (metric in metrics_list) {
  for (age in age_groups) {
    
    #metric="Jaccard"
    #age=1
    file <- paste0(path, "Adj_Matrix_", metric, "_Male_Blocks_age_", age, ".csv")
    
    if (file.exists(file)) {
      adj <- as.matrix(read.table(file, sep = " ", header = FALSE))
      # Set negative values to zero
      adj[adj < 0] <- 0
      g <- graph_from_adjacency_matrix(adj, mode = "undirected", weighted = TRUE, diag = FALSE)
      
      # Compute node-level metrics
      metrics <- tibble(
        node = unique(Blocks_All$block_name),
        weighted_degree = strength(g),
        clustering_weighted = transitivity(g, type = "weighted", weights = E(g)$weight),
        eigen_centrality = eigen_centrality(g, weights = E(g)$weight)$vector,
        betweenness_weighted = betweenness(g, weights = E(g)$weight, normalized = TRUE),
        closeness_weighted = closeness(g, weights = E(g)$weight, normalized = TRUE),
        pagerank_weighted = page_rank(g, weights = E(g)$weight)$vector,
        kcore = coreness(g),
        age_group = paste0("age_", age),
        metric_type = metric
      )
      
      # Community detection
      comm <- cluster_louvain(g, weights = E(g)$weight)
      metrics$community <- membership(comm)
      
      # Append to global list
      all_metrics[[paste0(metric, "_", age)]] <- metrics
      print(paste("Processed:", metric, "age", age))
    } else {
      warning(paste("File not found:", file))
    }
  }
}

# Combine all into one dataframe
network_metrics <- bind_rows(all_metrics)

# Save combined data
write_csv(network_metrics, "AllNetworkMetrics_Comparison_Male_Blocks.csv")

# ======= PLOT METRICS OVER TIME BY METRIC TYPE =======

metric_names <- c("weighted_degree", "clustering_weighted", "eigen_centrality","betweenness_weighted","closeness_weighted","pagerank_weighted", "kcore")

# Mean
ggplotly(network_metrics %>%
           pivot_longer(cols = c("weighted_degree", "clustering_weighted", "eigen_centrality", 
                                 "betweenness_weighted", "closeness_weighted", "pagerank_weighted", "kcore"),
                        names_to = "metric_name",
                        values_to = "metric_value")%>%
           group_by(age_group, metric_type, metric_name) %>%
           summarise(mean_value = mean(metric_value, na.rm = TRUE),max_value = max(metric_value, na.rm = TRUE), .groups = "drop") %>%
           ggplot(aes(x = age_group, y = mean_value, color = metric_type, group = metric_type)) +
           geom_line(size = 1) + geom_point() +
           facet_wrap(~ metric_name, scales = "free") +
           labs(title = "Male Mean Network Metrics by Age Group and Similarity Measure",
                x = "Age Group", y = "Mean Value", color = "Similarity Metric") +
           theme_classic() +
           theme(legend.position = "bottom"))

# Max
ggplotly(network_metrics %>%
           pivot_longer(cols = c("weighted_degree", "clustering_weighted", "eigen_centrality", 
                                 "betweenness_weighted", "closeness_weighted", "pagerank_weighted", "kcore"),
                        names_to = "metric_name",
                        values_to = "metric_value")%>%
           group_by(age_group, metric_type, metric_name) %>%
           summarise(mean_value = mean(metric_value, na.rm = TRUE),max_value = max(metric_value, na.rm = TRUE), .groups = "drop") %>%
           ggplot(aes(x = age_group, y = max_value, color = metric_type, group = metric_type)) +
           geom_line(size = 1) + geom_point() +
           facet_wrap(~ metric_name, scales = "free") +
           labs(title = "Male Max Network Metrics by Age Group and Similarity Measure",
                x = "Age Group", y = "Mean Value", color = "Similarity Metric") +
           theme_classic() +
           theme(legend.position = "bottom"))
