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
# Column names
ICD10_Diagnoses_All <- read_csv("C:/Users/OMEN/Box/Andrea_Universities/Conferences 2025/Complexity72h/Temporal diseases Project/Data/ICD10_Diagnoses_All.csv")

cen_f_mortF_by_age <- read_csv("Mortality/cen_f_mortF_by_age.csv")%>%
  mutate(node=rep(seq(1,1080,1),8))%>%
  left_join(ICD10_Diagnoses_All%>%select(-c(descr)),by=c("node"="diagnose_id"))

# Z score
# Function to compute conditional z-score
z_score_custom <- function(x) {
  # Subset only non-zero values
  non_zero <- x[x != 0]
  
  # Compute mean and SD on non-zero values
  mu <- mean(non_zero)
  sigma <- sd(non_zero)
  
  # Return vector with z-scores, 0 for original zeros
  sapply(x, function(val) {
    if (val == 0) {
      return(0)
    } else {
      return((val - mu) / sigma)
    }
  })
}

# Apply to the data
cen_f_mortF_by_age_new=cen_f_mortF_by_age %>%
  group_by(age_group_index)%>%
  mutate(
    z_cen_f = z_score_custom(cen_f),
    z_mort_F = z_score_custom(mort_F)
  )%>%
  ungroup()

# z_cen_f
ggplot(cen_f_mortF_by_age_new, aes(x = factor(age_group_index), y = z_cen_f)) +
  geom_boxplot() +
  labs(title = "Boxplot of z_cen_f by Age Group",
       x = "Age Group", y = "z-score of cen_f") +
  theme_minimal()

# z_mort_F
ggplot(cen_f_mortF_by_age_new, aes(x = factor(age_group_index), y = z_mort_F)) +
  geom_boxplot() +
  labs(title = "Boxplot of z_mort_F by Age Group",
       x = "Age Group", y = "z-score of mort_F") +
  theme_minimal()

# z_cen_f
ggplot(cen_f_mortF_by_age_new, aes( x = z_cen_f)) +
  geom_histogram() +
  labs(title = "Histogram of z_cen_f by Age Group",
       x = "Age Group", y = "z-score of cen_f") +
  theme_minimal()+
  facet_wrap(~factor(age_group_index))

# z_mort_F
ggplot(cen_f_mortF_by_age_new, aes( x = z_mort_F)) +
  geom_histogram() +
  labs(title = "Histogram of z_mort_F by Age Group",
       x = "Age Group", y = "z-score of mort_F") +
  theme_minimal()+
  facet_wrap(~factor(age_group_index))

ggplot(cen_f_mortF_by_age_new, aes(x = z_cen_f, y = z_mort_F, color = factor(age_group_index))) +
  geom_point(alpha = 0.7, size = 2) +
  labs(title = "Scatter plot of z_cen_f vs z_mort_F by Age Group",
       x = "z_cen_f", y = "z_mort_F", color = "Age Group") +
  theme_minimal()+
  facet_wrap(~factor(age_group_index))



compute_distances <- function(data) {
  data_centered <- scale(data[, c("z_cen_f", "z_mort_F")], center = TRUE, scale = FALSE)
  
  # Try computing Mahalanobis distance
  mahal_dist <- tryCatch({
    mahalanobis(data[, c("z_cen_f", "z_mort_F")],
                colMeans(data[, c("z_cen_f", "z_mort_F")]),
                cov(data[, c("z_cen_f", "z_mort_F")]))
  }, error = function(e) rep(NA, nrow(data)))
  
  data %>%
    mutate(
      mahal = mahal_dist,
      euclidean = sqrt(rowSums(data_centered^2)),
      manhattan = rowSums(abs(data_centered))
      
    )
}
  
df_with_distances <- cen_f_mortF_by_age_new %>%
  #filter(z_cen_f>0&z_mort_F>0)%>%
  group_by(age_group_index) %>%
  group_modify(~ compute_distances(.x)) %>%
  ungroup()


df_with_outliers <- df_with_distances %>%
  group_by(age_group_index) %>%
  mutate(
    outlier_mahal = mahal > quantile(mahal, 0.97, na.rm = TRUE),
    outlier_eucl = euclidean > quantile(euclidean, 0.95, na.rm = TRUE),
    outlier_manh = manhattan > quantile(manhattan, 0.95, na.rm = TRUE)
  ) %>%
  ungroup()

Outliers_nodes_v2=df_with_outliers%>%
  filter(z_cen_f>0&z_mort_F>0,outlier_mahal==T)%>%
  group_by(icd_code)%>%
  summarise(Count=n_distinct(age_group_index))%>%
  select(icd_code)%>%
  distinct()%>%
  pull()

  
write.csv(df_with_outliers%>%select(c(age_group_index,z_cen_f,z_mort_F,outlier_mahal))%>%
            mutate(outlier_mahal=ifelse(is.na(outlier_mahal),FALSE,outlier_mahal)),"Mortality/Nodes with high mortality and centrality by age.csv")


Outliers_nodes=df_with_outliers%>%
  mutate(outlier_mahal=ifelse(is.na(outlier_mahal),FALSE,outlier_mahal))%>%
  filter(outlier_mahal==TRUE)%>%
  group_by(icd_code)%>%
  summarise(Count=n_distinct(age_group_index))%>%
  filter(Count==7)%>%
  select(icd_code)%>%
  distinct()%>%
  pull()
  
  
  
df_with_outliers%>%filter(icd_code=="C78")%>%#%in%Outliers_nodes)%>%
  select(c(icd_code,age_group_index,z_cen_f,z_mort_F,outlier_mahal))%>%
  mutate(outlier_mahal=ifelse(is.na(outlier_mahal),FALSE,outlier_mahal))%>%
  ggplot(aes(x = z_cen_f, y = z_mort_F, group = icd_code, color = icd_code)) +
  geom_path(size = 0.8, alpha = 0.7) +  # Line for trajectory
  geom_point(data = df_with_outliers %>%filter(icd_code=="C78"),#%>% filter(outlier_mahal == TRUE)%>%filter(icd_code%in%Outliers_nodes),
             aes(x = z_cen_f, y = z_mort_F, group = icd_code, color = icd_code),
             size = 2, shape = 21) +  # Highlight outliers
  theme_minimal() +
  labs(x = "Z-score Centrality (z_cen_f)", y = "Z-score Mortality (z_mort_F)",
       title = "Trajectory of ICD Codes across Age Groups") +
  theme(legend.position = "none")+  # Optional: remove legend if too many ICD codes
  xlim(0,1)
  
  View()
  

group_by(age_group_index) %>%
  summarise(Count_eu=n())%>%
  View()

#---------------------------
Q90=cen_f_mortF_by_age_new %>%
  filter(z_cen_f>0&z_mort_F>0)%>%
  mutate(product=z_cen_f*z_mort_F)%>%
  group_by(age_group_index)%>%
  summarise(Q90=quantile(product,0.60))%>%
  ungroup()

#ggplotly(
#Selected_nodes_outliers=

final_nodes=cen_f_mortF_by_age_new%>%
  left_join(Q90,by=c("age_group_index"))%>%
  mutate(product=z_cen_f*z_mort_F)%>%
  mutate(Outlier=ifelse(product>Q90,1,0))%>%
  filter(Outlier==1)%>%
  group_by(icd_code)%>%
  summarise(Count=n_distinct(age_group_index))%>%
  filter(Count>1)%>%select(icd_code)%>%
  distinct()%>%
  pull
  
  outliers_ages=cen_f_mortF_by_age_new%>%
  left_join(Q90,by=c("age_group_index"))%>%
  #  View()
  mutate(product=z_cen_f*z_mort_F)%>%
  mutate(Outlier=ifelse(product>Q90,1,0))%>%
  filter(Outlier==1&icd_code%in%final_nodes)%>%
    select(c(icd_code,Outlier,age_group_index))
    
    
  
  

outliers_nodes=cen_f_mortF_by_age_new%>%
  filter(icd_code%in%final_nodes)%>%
  left_join(outliers_ages,by=c("icd_code","age_group_index"))%>%
  mutate(geom_mean=ifelse(z_mort_F<0&z_cen_f<0,-sqrt(z_mort_F*z_cen_f),
                          ifelse((z_mort_F*z_cen_f)<0,-sqrt(-(z_mort_F*z_cen_f)),
                                 ifelse(z_mort_F>0&z_cen_f>0,sqrt(z_mort_F*z_cen_f),
                                        ifelse(z_mort_F==0|z_cen_f==0,sqrt(z_mort_F*z_cen_f),NA)))))%>%
  mutate(Outlier=ifelse(is.na(Outlier),FALSE,TRUE))
  #filter(is.na(geom_mean))%>%
  #select(c(icd_code,age_group_index,geom_mean))

write.csv(outliers_nodes,"Mortality/outliers_nodes mortality.csv")
  
#mutate(sqrt_product=sqrt(z_cen_f*z_mort_F))%>%
 # ggplot(aes(x=age_group_index,y=sqrt_product,group = icd_code,color = icd_code))+
  #geom_line())
  

#-------------------------------
# Male
#-------------------------------
cen_m_mortM_by_age <- read_csv("Mortality/cen_m_mortM_by_age.csv")%>%
  mutate(node=rep(seq(1,1080,1),8))%>%
  left_join(ICD10_Diagnoses_All%>%select(-c(descr)),by=c("node"="diagnose_id"))

# Apply to the data
cen_m_mortM_by_age_new=cen_m_mortM_by_age %>%
  group_by(age_group_index)%>%
  mutate(
    z_cen_m = z_score_custom(cen_m),
    z_mort_m = z_score_custom(mort_M)
  )%>%
  ungroup()

#---------------------------
Q90=cen_m_mortM_by_age_new %>%
  filter(z_cen_m>0&z_mort_m>0)%>%
  mutate(product=z_cen_m*z_mort_m)%>%
  group_by(age_group_index)%>%
  summarise(Q90=quantile(product,0.60))%>%
  ungroup()

#ggplotly(
#Selected_nodes_outliers=

final_nodes=cen_m_mortM_by_age_new%>%
  left_join(Q90,by=c("age_group_index"))%>%
  mutate(product=z_cen_m*z_mort_m)%>%
  mutate(Outlier=ifelse(product>Q90,1,0))%>%
  filter(Outlier==1)%>%
  group_by(icd_code)%>%
  summarise(Count=n_distinct(age_group_index))%>%
  filter(Count>1)%>%select(icd_code)%>%
  distinct()%>%
  pull

outliers_ages=cen_m_mortM_by_age_new%>%
  left_join(Q90,by=c("age_group_index"))%>%
  #  View()
  mutate(product=z_cen_m*z_mort_m)%>%
  mutate(Outlier=ifelse(product>Q90,1,0))%>%
  filter(Outlier==1&icd_code%in%final_nodes)%>%
  select(c(icd_code,Outlier,age_group_index))





outliers_nodes=cen_m_mortM_by_age_new%>%
  filter(icd_code%in%final_nodes)%>%
  left_join(outliers_ages,by=c("icd_code","age_group_index"))%>%
  mutate(geom_mean=ifelse(z_mort_m<0&z_cen_m<0,-sqrt(z_mort_m*z_cen_m),
                          ifelse((z_mort_m*z_cen_m)<0,-sqrt(-(z_mort_m*z_cen_m)),
                                 ifelse(z_mort_m>0&z_cen_m>0,sqrt(z_mort_m*z_cen_m),
                                        ifelse(z_mort_m==0|z_cen_m==0,sqrt(z_mort_m*z_cen_m),NA)))))%>%
  mutate(Outlier=ifelse(is.na(Outlier),FALSE,TRUE))
#filter(is.na(geom_mean))%>%
#select(c(icd_code,age_group_index,geom_mean))

write.csv(outliers_nodes,"Mortality/outliers_nodes mortality Male.csv")

#mutate(sqrt_product=sqrt(z_cen_m*z_mort_m))%>%
# ggplot(aes(x=age_group_index,y=sqrt_product,group = icd_code,color = icd_code))+
#geom_line())








#------------------------------------
# Edges
#------------------------------------
edges_mortality <- 
  read_csv("Mortality/edge_betweenness_vs_delta_Female.csv")%>%
  mutate(i=i+1,
         j=j+1)%>%
  left_join(ICD10_Diagnoses_All%>%select(-c(descr)),by=c("i"="diagnose_id"))%>%
  rename("icd_code_i"="icd_code")%>%
  left_join(ICD10_Diagnoses_All%>%select(-c(descr)),by=c("j"="diagnose_id"))%>%
  rename("icd_code_j"="icd_code")%>%
  select(-c(i,j))%>%
  mutate(gender="Female")%>%
  rowwise() %>%
  mutate(
    pair_id = paste(sort(c(icd_i, icd_j)), collapse = "_")
  ) %>%
  ungroup() %>%
  group_by(age, pair_id) %>%
  slice(1) %>%  # conservar solo una fila por par y edad
  ungroup() %>%
  select(-pair_id)%>%
  bind_rows(read_csv("Mortality/edge_betweenness_vs_delta_Male.csv")%>%
              mutate(i=i+1,
                     j=j+1)%>%
              left_join(ICD10_Diagnoses_All%>%select(-c(descr)),by=c("i"="diagnose_id"))%>%
              rename("icd_code_i"="icd_code")%>%
              left_join(ICD10_Diagnoses_All%>%select(-c(descr)),by=c("j"="diagnose_id"))%>%
              rename("icd_code_j"="icd_code")%>%
              select(-c(i,j))%>%
              mutate(gender="Male")%>%
              rowwise() %>%
              mutate(
                pair_id = paste(sort(c(icd_i, icd_j)), collapse = "_")
              ) %>%
              ungroup() %>%
              group_by(age, pair_id) %>%
              slice(1) %>%  # conservar solo una fila por par y edad
              ungroup() %>%
              select(-pair_id))%>%
    left_join(Mortality,by=c("gender","age","icd_code_i"="icd_code"))%>%
    rename("Mortality_i"="Mortality")%>%
    left_join(Mortality,by=c("gender","age","icd_code_j"="icd_code"))%>%
    rename("Mortality_j"="Mortality")%>%
  mutate(log_ratio=log(Mortality_i+1)-log(1+Mortality_j))%>%
  select(-c(mortality_i,mortality_j,delta))%>%
  mutate(link=paste0(icd_code_i,"_",icd_code_j),
         dif_mort=abs(Mortality_i-Mortality_j))

# z_score

# Apply to the data
edges_mortality_new=edges_mortality %>%
  group_by(gender,age)%>%
  mutate(
    z_mort = z_score_custom(dif_mort),
    z_ebtw = z_score_custom(ebtw)
  )%>%
  ungroup()
    
#Q90
Q90_edges=edges_mortality_new %>%
  filter(z_mort>0&z_ebtw>0)%>%
  mutate(product=z_mort*z_ebtw)%>%
  group_by(gender,age)%>%
  summarise(Q90=quantile(product,0.80))%>%
  ungroup()

# Product and outliers

selected_top_edges <- edges_mortality_new %>%
  left_join(Q90_edges, by = c("age", "gender")) %>%
  mutate(product = z_mort * z_ebtw,
         Outlier = ifelse(product > Q90, 1, 0)) %>%
  filter(Outlier == 1) %>%
  group_by(gender, age) %>%
  arrange(desc(product), .by_group = TRUE) %>%
  mutate(n_rows = n()) %>%
  filter(row_number() <= if_else(n_rows > 3, ceiling(n_rows * 0.05), 3)) %>%
  ungroup() %>%
  rename("sex"="gender")%>%
  select(sex, age, Mortality_i,Mortality_j,link, product, ebtw, dif_mort)

write.csv(selected_top_edges,"Mortality/Top edges mortality.csv")
library(xtable)
xtable(selected_top_edges)


#final_edges=
  edges_mortality_new%>%
  left_join(Q90_edges,by=c("age","gender"))%>%
  mutate(product=z_mort*z_ebtw)%>%
  mutate(Outlier=ifelse(product>Q90,1,0))%>%
  filter(Outlier==1)%>%
  group_by(gender)%>%
  summarise(Count=n_distinct(age))%>%
  filter(Count>1)%>%select(icd_code)%>%
  distinct()%>%
  pull




#---------------------------
Q90_mort_ebtw=edges_mortality%>%
  group_by(gender,age)%>%
  summarise(Q90_dif_mort=quantile(dif_mort,0.9),
            Q90_ebtw=quantile(ebtw,0.9))

final_edges=edges_mortality%>%
  left_join(Q90_mort_ebtw,by=c("age","gender"))%>%
  mutate(Outlier_mort=ifelse(dif_mort>Q90_dif_mort,1,0),
         Outlier_ebtw=ifelse(ebtw>Q90_ebtw,1,0))%>%
  filter(Outlier_mort==1&Outlier_ebtw==1)%>%
  group_by(gender,link)%>%
  summarise(Count_age=n_distinct(age),
            Count=n())%>%
  filter(Count_age>1)%>%select(gender,link)%>%
  distinct()%>%
  rename("selected_link"="link")


outliers_edges=edges_mortality%>%
  left_join(Q90_mort_ebtw,by=c("age","gender"))%>%
  mutate(Outlier_mort=ifelse(dif_mort>Q90_dif_mort,1,0),
         Outlier_ebtw=ifelse(ebtw>Q90_ebtw,1,0),
         Outlier=ifelse(Outlier_mort+Outlier_ebtw==2,1,0))%>%
  filter(Outlier==1)%>%
  left_join(final_edges,by=c("gender","link"))%>%
  View()
  select(c(link,Outlier,age))

outliers_edges%>%
  group_by(link,age)%>%
  summarise(Count=n())%>%
  View()



final_edges%>%group_by(gender)%>%summarise(Count=n())


#outliers_nodes=
  
  edges_mortality%>%
    mutate(link_female=ifelse(gender=="Female"&link%in%final_edges$selected_link[final_edges$gender=="Female"],1,0),
           link_Male=ifelse(gender=="Male"&link%in%final_edges$selected_link[final_edges$gender=="Male"],1,0))%>%
    filter(link_female==1|link_Male==1)%>%
    select(c(age,gender,dif_mort,ebtw,Mortality_i,Mortality_j,link))%>%
    mutate(product=dif_mort*ebtw)%>%
    View()
  
  
  
  filter(link%in%final_edges)%>%
  left_join(outliers_edges,by=c("link","age"))%>%
  mutate(Outlier=ifelse(is.na(Outlier),FALSE,TRUE))%>%
  View()
#filter(is.na(geom_mean))%>%
#select(c(icd_code,age_group_index,geom_mean))

write.csv(outliers_nodes,"Mortality/outliers_nodes mortality.csv")
