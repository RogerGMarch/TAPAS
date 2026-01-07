library(igraph)
library(tidyverse)

options(scipen = 999)

# Path y nombres de archivos

Blocks_All <- read_csv("C:/Users/OMEN/Box/Andrea_Universities/Conferences 2025/Complexity72h/Temporal diseases Project/Data/Blocks_All.csv")


folder_path <- "C:/Users/OMEN/Box/Andrea_Universities/Conferences 2025/Complexity72h/Temporal diseases Project/Data"

setwd(folder_path)

age_groups <- 1:8
# Female
file_prefix <- "Adj_Matrix_Jaccard_Female_Blocks_Jaccard_age_"
path <- "3.AdjacencyMatrices/"

# Lista para guardar resultados
all_metrics <- list()
all_clusters <- list()

for (age in age_groups) {
  #age=1
  file <- paste0(path, file_prefix, age, ".csv")
  adj <- as.matrix(read.table(file, sep = " ", header = FALSE))
  g <- graph_from_adjacency_matrix(adj, mode = "undirected", weighted = TRUE, diag = FALSE)
  
  # Métricas por nodo
  metrics <- tibble(
    node = unique(Blocks_All$block_name),
    weighted_degree = strength(g),
    clustering = transitivity(g, type = "localundirected", isolates = "zero"),
    eigen_centrality = eigen_centrality(g, weights = E(g)$weight)$vector,
    kcore = coreness(g),
    age_group = paste0("age_", age)
  )
  
  # Clustering comunitario
  comm <- cluster_louvain(g, weights = E(g)$weight)
  metrics$community <- membership(comm)
  
  # Guardar
  all_metrics[[age]] <- metrics
  all_clusters[[age]] <- comm
  
  print(age)
}

# Unir todas las métricas
network_metrics <- bind_rows(all_metrics)

# ======= GRAFICAR MÉTRICAS EN EL TIEMPO =======

metric_names <- c("weighted_degree", "clustering", "eigen_centrality", "kcore")

for (metric in metric_names) {
  metric="eigen_centrality"
  network_metrics %>%
    group_by(age_group) %>%
    summarise(mean_value = mean(.data[[metric]], na.rm = TRUE)) %>%
    ggplot(aes(x = age_group, y = mean_value, group = 1)) +
    geom_line() + geom_point() +
    labs(title = paste("Mean", metric, "over age groups"), y = metric, x = "Age Group") +
    theme_minimal() 
    ggsave(paste0("Figures/Metric_", metric, "_over_time.png"), width = 6, height = 4)
}

# ======= MÉTRICAS POR CLUSTER A TRAVÉS DEL TIEMPO =======

cluster_metrics <- network_metrics %>%
  group_by(age_group, community) %>%
  summarise(across(all_of(metric_names), mean, na.rm = TRUE), .groups = "drop")

# Graficar por cluster
for (metric in metric_names) {
  cluster_metrics %>%
    ggplot(aes(x = age_group, y = .data[[metric]], color = as.factor(community), group = community)) +
    geom_line() + geom_point() +
    labs(title = paste("Mean", metric, "per Cluster"), x = "Age Group", y = metric) +
    theme_minimal() +
    ggsave(paste0("Figures/Cluster_", metric, "_over_time.png"), width = 6, height = 4)
}

# ======= EXPORTAR =======
write_csv(network_metrics, "network_metrics_by_age_and_node.csv")
write_csv(cluster_metrics, "network_metrics_by_age_and_cluster.csv")
