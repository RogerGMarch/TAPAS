#--------------------
# Libraries
#--------------------
library(dplyr)
library(purrr)
library(broom)
library(readr)
#--------------------
# Reading the data
#--------------------
Mortality=read_csv("Communities/ICD_Bridges.csv")%>%
  rename("gender"="Sex","age"="Age_ID","icd_code"="ICD_Code")%>%
  select(-c(Num_Connections,Bridges_To,Community))%>%
  distinct()

temporal_communities_ICD_SBM <- read_csv("Communities/temporal_communities_Female_ICD.csv")%>%
  select(-c("...1"))%>%
  mutate(node=node+1)%>%
  left_join(ICD10_Diagnoses_All%>%select(-c(descr)),by=c("node"="diagnose_id"))%>%
  mutate(gender="Female")%>%
  bind_rows(read_csv("Communities/temporal_communities_Male_ICD.csv")%>%
                                                mutate(node=node+1)%>%
                                                left_join(ICD10_Diagnoses_All%>%select(-c(descr)),by=c("node"="diagnose_id"))%>%
                                                            mutate(gender="Male")
  )%>%
  rename("Community"="block")%>%
  left_join(Mortality,by=c("gender","age","icd_code"))%>%
  mutate(Mortality=ifelse(is.na(Mortality),0,Mortality),
         Community=as.character(Community))

ICD_leiden=read_csv("Communities/ICD_Bridges.csv")%>%
  rename("gender"="Sex","age"="Age_ID","icd_code"="ICD_Code")%>%
  select(-c(Num_Connections,Bridges_To))%>%
  mutate(Mortality=ifelse(is.na(Mortality),0,Mortality),
         Community=as.character(Community))


#------------------------------------
# For communities_ICD_SBM
#------------------------------------
# Step 1: Nest by gender and age_group
anova_results <- temporal_communities_ICD_SBM %>%
  group_by(gender, age) %>%
  nest() %>%
  
  # Step 2: Fit ANOVA model per subgroup
  mutate(anova = map(data, ~ aov(Mortality ~ Community, data = .x)),
         
         # Step 3: Extract ANOVA summary
         anova_summary = map(anova, tidy),
         
         # Step 4: Tukey HSD for post-hoc comparisons
         tukey = map(anova, ~ TukeyHSD(.x) %>% broom::tidy())) %>%
  
  # Optional: select what you want to keep
  select(gender, age, anova_summary, tukey)

# Ver p-values del ANOVA
anova_results %>%
  unnest(anova_summary) %>%
  filter(term == "Community") %>%
  select(gender, age, p.value)

# Ver comparaciones significativas con Tukey
anova_results_tukey_SBM=anova_results %>%
  unnest(tukey) %>%
  filter(adj.p.value < 0.05)

write.csv(anova_results_tukey_SBM%>%
            select(-c(anova_summary)),
          "Communities/anova_results_tukey_SBM.csv")


#------------------------------------
# For communities_ICD_leiden
#------------------------------------
# Step 1: Nest by gender and age_group
anova_results <- ICD_leiden %>%
  group_by(gender, age) %>%
  nest() %>%
  
  # Step 2: Fit ANOVA model per subgroup
  mutate(anova = map(data, ~ aov(Mortality ~ Community, data = .x)),
         
         # Step 3: Extract ANOVA summary
         anova_summary = map(anova, tidy),
         
         # Step 4: Tukey HSD for post-hoc comparisons
         tukey = map(anova, ~ TukeyHSD(.x) %>% broom::tidy())) %>%
  
  # Optional: select what you want to keep
  select(gender, age, anova_summary, tukey)

# Ver p-values del ANOVA
anova_results %>%
  unnest(anova_summary) %>%
  filter(term == "Community") %>%
  select(gender, age, p.value)

# Ver comparaciones significativas con Tukey
anova_results_tukey_leiden=anova_results %>%
  unnest(tukey) %>%
  filter(adj.p.value < 0.05)

write.csv(anova_results_tukey_SBM%>%
            select(-c(anova_summary)),
          "Communities/anova_results_tukey_SBM.csv")

#-------------------------------
# Modeling
#-------------------------------
#------------------------------------
# For communities_ICD_SBM
#------------------------------------
# Step 1: Nest by gender and age_group
model_results_SBM <- temporal_communities_ICD_SBM %>%
  group_by(gender, age) %>%
  nest() %>%
  
  # Step 2: Fit lm model per subgroup
  mutate(
    model = map(data, ~ lm(Mortality ~ Community, data = .x)),
    
    # Step 3: Extract coefficient-level summary
    model_summary = map(model, tidy),
    
    # Step 4: Extract model-level statistics (incl. R²)
    model_stats = map(model, glance)
  ) %>%
  
  select(gender, age, model_summary, model_stats)

# Ver p-values del ANOVA
model_results_SBM %>%
  unnest(model_stats) %>%
  View()

#------------------------------------
# For communities_ICD_leiden
#------------------------------------
# Step 1: Nest by gender and age_group
model_results_leiden <- ICD_leiden %>%
  group_by(gender, age) %>%
  nest() %>%
  
  # Step 2: Fit lm model per subgroup
  mutate(
    model = map(data, ~ lm(Mortality ~ Community, data = .x)),
    
    # Step 3: Extract coefficient-level summary
    model_summary = map(model, tidy),
    
    # Step 4: Extract model-level statistics (incl. R²)
    model_stats = map(model, glance)
  ) %>%
  
  select(gender, age, model_summary, model_stats)

# Ver p-values del ANOVA
model_results_leiden %>%
  unnest(model_stats) %>%
  View()


model_results_leiden %>%
  unnest(model_stats) %>%
  select(c(gender,age,adj.r.squared))%>%
  mutate(Method="leiden")%>%
  bind_rows(model_results_SBM %>%
  unnest(model_stats) %>%
  select(c(gender,age,adj.r.squared))%>%
    mutate(Method="SBM"))%>%
  View()


#-----------------
# Plot
#-----------------
library(ggplot2)

model_results_leiden %>%
  unnest(model_stats) %>%
  select(c(gender,age,adj.r.squared))%>%
  mutate(Method="leiden")%>%
  bind_rows(model_results_SBM %>%
              unnest(model_stats) %>%
              select(c(gender,age,adj.r.squared))%>%
              mutate(Method="SBM"))%>%
  ggplot(aes(x = age, y = adj.r.squared, color = gender)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  facet_wrap(~ Method) +
  theme_minimal() +
  labs(
    title = "Adjusted R² by Age and Gender",
    x = "Age Group",
    y = "Adjusted R²"
  ) +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


#---------------------------
# New groups
#---------------------------
temporal_communities_ICD_SBM_new <- read_csv("Communities/temporal_communities_Female_ICD_assort=False.csv")%>%
  mutate(gender="Female",
         assort="False")%>%
  bind_rows(
    read_csv("Communities/temporal_communities_Female_ICD_assort=True.csv")%>%
      mutate(gender="Female",
             assort="True")
  )%>%
  bind_rows(
    read_csv("Communities/temporal_communities_Male_ICD_assort=True.csv")%>%
      mutate(gender="Male",
             assort="True")
  )%>%
  bind_rows(
    read_csv("Communities/temporal_communities_Male_ICD_assort=False.csv")%>%
      mutate(gender="Male",
             assort="False")
  )%>%
  filter(!is.na(block))%>%
  rename("Community"="block")%>%
  mutate(Mortality=ifelse(is.na(mortality),0,mortality),
         Community=as.character(Community))%>%
  left_join(Mortality%>%
              rename("Mortality_old"="Mortality"),by=c("gender","age","icd_code"))%>%
  mutate(Mortality_old=ifelse(is.na(Mortality_old),0,Mortality_old))
#------------------------------------
# For communities_ICD_SBM New
#------------------------------------
# Step 1: Nest by gender and age_group
anova_results <- temporal_communities_ICD_SBM_new %>%
  group_by(gender, age,assort) %>%
  nest() %>%
  
  # Step 2: Fit ANOVA model per subgroup
  mutate(anova = map(data, ~ aov(Mortality_old ~ Community, data = .x)),
         
         # Step 3: Extract ANOVA summary
         anova_summary = map(anova, tidy),
         
         # Step 4: Tukey HSD for post-hoc comparisons
         tukey = map(anova, ~ TukeyHSD(.x) %>% broom::tidy())) %>%
  
  # Optional: select what you want to keep
  select(gender, age, assort, anova_summary, tukey)

# Ver p-values del ANOVA
anova_results %>%
  unnest(anova_summary) %>%
  filter(term == "Community") %>%
  select(gender, age,assort, p.value)%>%
  mutate(significant=ifelse(p.value<0.05,"Yes","No"))%>%
  View()

# Ver comparaciones significativas con Tukey
anova_results_tukey_SBM=anova_results %>%
  unnest(tukey) %>%
  filter(adj.p.value < 0.05)

write.csv(anova_results_tukey_SBM%>%
            select(-c(anova_summary)),
          "Communities/anova_results_tukey_SBM.csv")

#-------------------------------
# Modeling
#-------------------------------
#------------------------------------
# For communities_ICD_SBM
#------------------------------------
# Step 1: Nest by gender and age_group
model_results_SBM_new <- temporal_communities_ICD_SBM_new %>%
  group_by(gender, age,assort) %>%
  nest() %>%
  
  # Step 2: Fit lm model per subgroup
  mutate(
    model = map(data, ~ lm(Mortality ~ Community, data = .x)),
    
    # Step 3: Extract coefficient-level summary
    model_summary = map(model, tidy),
    
    # Step 4: Extract model-level statistics (incl. R²)
    model_stats = map(model, glance)
  ) %>%
  
  select(gender, age,assort, model_summary, model_stats)

# Ver p-values del ANOVA
model_results_SBM_new %>%
  unnest(model_stats) %>%
  View()

#-----------------
# Plot
#-----------------
library(ggplot2)

model_results_SBM_new %>%
              unnest(model_stats) %>%
              select(c(gender,age,assort,adj.r.squared))%>%
              mutate(Method="SBM")%>%
  ggplot(aes(x = age, y = adj.r.squared, color = gender)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  facet_wrap(~ assort) +
  theme_minimal() +
  labs(
    title = "Adjusted R² by Age and Gender",
    x = "Age Group",
    y = "Adjusted R²"
  ) +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )



