###############################################################
#                   PROJET DATA SCIENCE 2024
###############################################################
install.packages("pacman")
pacman::p_load(
  tidyverse,
  rio,
  here,
  openxlsx,
  labelled,
  gtsummary,
  labelled,
  ggcorrplot,
  questionr,
  glmnet,
  forcats,
  ROSE,
  caret,
  rpart,
  rpart.plot,
  plotly,
  MASS
)


#-----------------------------------------#
#  Data
#-----------------------------------------#
df <- read_delim("Data/train_1.csv", delim = ";", 
                 escape_double = FALSE, trim_ws = TRUE)

#Missing data
sum(is.na(df)) #Non NaN
str(df)

vec_num <- c("Age", "Vintage", "Annual_Premium")
vec_fact <- c("Gender", "Driving_License", "Region_Code", "Previously_Insured", 
              "Vehicle_Age", "Vehicle_Damage", "Policy_Sales_Channel", "Response")

#Traitement des types de variable
df <- df %>% 
  mutate(across(vec_num, as.numeric)) %>% 
  mutate(across(vec_fact, as.factor))

#Correlation
#________Matrice de corrélation variable numérique
df_num <- df %>% dplyr::select(all_of(vec_num))

corr_matrice <- cor(df_num)
corr_matrice
ggcorrplot(corr_matrice)


#________Association entre variables catégorielles 
df_fact <- df %>% dplyr::select(all_of(vec_fact))
variables <- names(df_fact) 

# Initialiser une matrice pour stocker les résultats
matrice_v_cramer <- matrix(nrow = length(variables), ncol = length(variables), dimnames = list(variables, variables))

# Boucler sur chaque paire de variables pour calculer le V de Cramer
for (i in 1:length(variables)) {
  for (j in 1:length(variables)) {
    matrice_v_cramer[i, j] <- cramer.v(table(df_fact[[variables[i]]], df_fact[[variables[j]]]))
  }
}

# Afficher la matrice
print(matrice_v_cramer)
ggcorrplot(matrice_v_cramer)
