###-----------------------###
### TOSSICITA = INTEGRALE ###
###-----------------------###

# 0. Preparazione dataset ----
# Installing libraries
library(openxlsx)
library(dplyr)
library(ggplot2)

# Logistic regression to determine the most relevant n's
# Definizione dell'outcome da analizzare
outcome_name <- "mucosite_orale"  # puoi cambiarlo in "dysphagia", "mucositis", ecc.

FUP <- read.xlsx("2024_11_11_Export_ML_Testa_Collo.xlsx", sheet = 4)
FUP_new <- FUP[FUP$sequenza != "T03_6m", ]
FUP_new <- FUP_new[FUP_new$sequenza != "T00_0_Arruolamento", ]
FUP_new <- FUP_new[FUP_new$sequenza != "T00_Basale", ]
FUP_new[[outcome_name]] <- as.numeric(FUP_new[[outcome_name]])

# Seleziona solo le colonne di interesse: ID, sequenza e outcome
FUP_new <- dplyr::select(FUP_new, id_microlearner, sequenza, all_of(outcome_name))
FUP_new[[outcome_name]] <- as.numeric(FUP_new[[outcome_name]])

# 1. SISTEMAZIONE DATI ----
# ---- Modifiche pazienti singoli ----
FUP_new <- FUP_new %>%
  filter(!(id_microlearner == 2060 & sequenza == "T01_6sett"))
FUP_new <- FUP_new %>%
  filter(!(id_microlearner == 2023 & sequenza == "T01_6sett"))

# ---- Andamento terzo mese e rimozione ----
# Filtra solo i dati del terzo mese
mucosite_orale_3m <- FUP_new %>%
  filter(sequenza == "T03_3m") %>%
  transmute(
    id_microlearner,
    sequenza,
    mucosite_orale = .data[[outcome_name]]
  )

mucosite_orale_3m %>%
  mutate(mucosite_orale = ifelse(is.na(mucosite_orale), "NA", as.character(mucosite_orale))) %>%
  count(mucosite_orale) %>%
  arrange(mucosite_orale)

# Rimuove il valore della tossicità al 3° mese
FUP_new <- FUP_new[FUP_new$sequenza != "T03_3m", ]

# 2. CALCOLO AUC NORMALE
# ---- Creazione mappa settimane e calcolo durata ----
# STEP 1 - Crea la mappa fissa per le T01 (che rappresentano le settimane di terapia)
mappa_base <- c(
  "T01_1sett" = 1,
  "T01_2sett" = 2,
  "T01_3sett" = 3,
  "T01_4sett" = 4,
  "T01_5sett" = 5,
  "T01_6sett" = 6
)

# STEP 2 - Aggiungi la settimana di base (NA per T02)
FUP_new <- FUP_new %>%
  mutate(settimane_base = mappa_base[sequenza])

# STEP 3 - Calcola la durata della terapia: ultimo T01 + 1
durate <- FUP_new %>%
  filter(grepl("^T01", sequenza)) %>%
  group_by(id_microlearner) %>%
  summarise(
    durata_terapia = max(settimane_base, na.rm = TRUE) + 1,
    .groups = "drop"
  )

# STEP 4 - Unisci la durata al dataframe principale
FUP_new <- FUP_new %>%
  left_join(durate, by = "id_microlearner")

# STEP 5 - Calcola la variabile finale 'settimane'
FUP_new <- FUP_new %>%
  mutate(settimane = case_when(
    grepl("^T01", sequenza) ~ settimane_base,
    sequenza == "T02_Fine_RT" ~ durata_terapia,
    TRUE ~ NA_real_
  ))

# ---- Ordina e calcola AUC normalizzato per durata ----
FUP_outcome_name <- FUP_new %>%
  arrange(id_microlearner, settimane) %>%
  group_by(id_microlearner) %>%
  summarise(
    auc = {
      # Tieni solo i punti con mucosite_orale non NA
      df_valid <- tibble(
        settimane = settimane,
        mucosite_orale = mucosite_orale
      ) %>%
        filter(!is.na(mucosite_orale))
      # Se ci sono meno di 2 punti, area = 0
      if (nrow(df_valid) < 2) return(0)
      # Applica metodo dei trapezi tra tutti i punti validi consecutivi
      sum(
        diff(df_valid$settimane) *
          (head(df_valid$mucosite_orale, -1) + tail(df_valid$mucosite_orale, -1)) / 2
      )
    },
    .groups = "drop"
  )

# AUC/durata per calcolare auc normalizzato
FUP_outcome_name <- FUP_outcome_name %>%
  left_join(durate, by = "id_microlearner") 
FUP_outcome_name <- FUP_outcome_name %>%
  mutate(auc_norm = auc / (durata_terapia-1))

# 3. CONTROLLO RISULTATI ----
# ---- Visualizza il nuovo dataframe ----
print(FUP_outcome_name)
mean(FUP_outcome_name$auc_norm)
sd(FUP_outcome_name$auc_norm)
# ---- Controllo pazienti con AUC normalizzato nullo o NA ----
FUP_outcome_name %>%
  filter(auc_norm == 0)
FUP_outcome_name %>%
  filter(is.na(auc_norm))
FUP_new %>%
  filter(id_microlearner %in% c(2022, 2041, 2084, 2132, 2144, 2171, 2204)) %>%
  arrange(id_microlearner, settimane)
# Tutti i valori di mucosite_orale sono 0 in tutte le osservazioni →️ quindi l’area sotto la curva è giustamente 0.
write.csv(FUP_outcome_name, "FUP_outcome_name.csv", row.names = FALSE)

# ---- Visualizzo andamento di un paziente generico: ----
# pazienti con dati mancanti 2023, 2026, 2127, 2168
# paziente 2023 e 2060 avevano la settimana 6 nulla pertanto tolta (sopra all'inizio)

# Seleziona il paziente
paziente = 2196
FUP_new_singolo <- FUP_new[FUP_new$id_microlearner == paziente, ]

# Ordina i livelli dell'asse x (settimane osservate, interi)
settimane_osservate <- sort(unique(FUP_new_singolo$settimane))

# Grafico solo per quel paziente
ggplot(FUP_new_singolo, aes(x = settimane, y = .data[[outcome_name]])) +
  geom_line(color = "steelblue") +
  geom_point(size = 2) +
  scale_x_continuous(breaks = settimane_osservate) +  # mostra solo i valori reali
  labs(
    x = "Settimana",
    y = "Gravità",
    title = paste("Andamento della Gravità nel Tempo per il Paziente", paziente)
  ) +
  theme_minimal()

# 4. TOSSICITA' CATEGORICA ----
# ---- Distribuzione di auc_norm e calcolo Threshold ----
threshold <- quantile(FUP_outcome_name$auc_norm, 0.80)

plot(density(FUP_outcome_name$auc_norm), 
     main = "Density plot di auc_norm mucosite_orale", 
     xlab = "'auc_norm'", 
     col = "darkgreen", 
     lwd = 2)
abline(v = threshold, col = "red", lwd = 2, lty = 2)

# Interpretabilità del Threshold
df_filtrato <- FUP_outcome_name[FUP_outcome_name$auc_norm > threshold, ]
mean(df_filtrato$auc_norm)

# ---- Aggiunta Toxicity1 categorica ----
FUP_outcome_name$Toxicity1 <- ifelse(FUP_outcome_name$auc_norm > threshold, 1, 0)
FUP_outcome_name_01 <- FUP_outcome_name[,c(1,5)]

colnames(FUP_outcome_name_01)[colnames(FUP_outcome_name_01) == "id_microlearner"] <- "ID"

# 5. SCELTA n degli EUD ----
# ---- Preparazione dataset ----
# Selezione solo paziente che non hanno fatto chirurgia (dosaggio corretto di radiazione)
# Leggi i dati
EUD <- read.csv("EUD_final.csv", stringsAsFactors = FALSE)
chirurgia_df <- read.csv("Complete_Dataset.csv", stringsAsFactors = FALSE)
chirurgia_df <- dplyr::select(chirurgia_df, ID, chirurgia_attuale)

# Merge chirurgia dentro EUD
EUD <- merge(EUD, chirurgia_df, by = "ID", all.x = TRUE)
# Merge outcome
EUD_outcome_name <- merge(EUD, FUP_outcome_name_01, by = "ID", all.x = TRUE)

# Filtra per i pazienti con chirurgia_attuale == 0
EUD_outcome_name <- EUD_outcome_name %>%
  filter(chirurgia_attuale == 0)

# ---- Preparazione variabili per Linear Regression ----
# Outcome variable
outcome_var <- "Toxicity1"

# List of predictor variables
eud_vars <- c("EUD_n0.01", "EUD_n0.05", "EUD_n0.1", "EUD_n0.15", "EUD_n0.2", "EUD_n0.25", "EUD_n0.3", "EUD_n0.35", "EUD_n0.4", "EUD_n0.45", "EUD_n0.5", "EUD_n0.55", "EUD_n0.6", "EUD_n0.65", "EUD_n0.7", "EUD_n0.75", "EUD_n0.8", "EUD_n0.85", "EUD_n0.9", "EUD_n0.95", "EUD_n1")

# ---- BONE MANDIBLE ----
# Empty list to store results
results_BM <- list()

subset_Bone_Mandible <- EUD_outcome_name[EUD_outcome_name$structure_name == "Bone_Mandible", ]

# Loop through the predictors
for (eud in eud_vars) {
  
  # Construct formula dynamically
  formula <- as.formula(paste(outcome_var, "~", eud))
  
  # Run the logistic regression model
  model <- glm(formula, data = subset_Bone_Mandible, family = binomial)
  
  # Store coefficients and p-values
  results_BM[[eud]] <- summary(model)$coefficients
  
}

# Convert results into a data frame for better viewing
results_df <- do.call(rbind, lapply(results_BM, function(x) data.frame(coef = x[2, 1], 
                                                                       p_value = x[2, 4],
                                                                       OR = exp(x[2, 1]),
                                                                       CI_lower = exp(confint(model)[2, 1]),
                                                                       CI_upper = exp(confint(model)[2, 2]))))

# View the results
print(results_df)


library(ggplot2)

# Assicurati che results_df abbia una colonna con i nomi delle EUD
results_df$EUD <- rownames(results_df)

# Plotta p-value
p1 <- ggplot(results_df, aes(x = EUD, y = p_value)) +
  geom_bar(stat = "identity", fill = "#377eb8", width = 0.7) +
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "red", linewidth = 1) +
  labs(title = "Bone Mandible",
       x = "EUD",
       y = "p-value") +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0, size = 16, face = "plain"),  # <- modifiche qui
    axis.title = element_text(size = 13),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )
p1

eud_min_p1 <- results_df$EUD[which.min(results_df$p_value)]


# ---- CAVITY ORAL ----
# Empty list to store results
results_CO <- list()

subset_Cavity_Oral <- EUD_outcome_name[EUD_outcome_name$structure_name == "Cavity_Oral", ]

# Loop through the predictors
for (eud in eud_vars) {
  
  # Construct formula dynamically
  formula <- as.formula(paste(outcome_var, "~", eud))
  
  # Run the logistic regression model
  model <- glm(formula, data = subset_Cavity_Oral, family = binomial)
  
  # Store coefficients and p-values
  results_CO[[eud]] <- summary(model)$coefficients
  
}

# Convert results into a data frame for better viewing
results_df <- do.call(rbind, lapply(results_CO, function(x) data.frame(coef = x[2, 1], 
                                                                       p_value = x[2, 4],
                                                                       OR = exp(x[2, 1]),
                                                                       CI_lower = exp(confint(model)[2, 1]),
                                                                       CI_upper = exp(confint(model)[2, 2]))))

# View the results
print(results_df)


library(ggplot2)

# Assicurati che results_df abbia una colonna con i nomi delle EUD
results_df$EUD <- rownames(results_df)

# Plotta p-value
p2 <- ggplot(results_df, aes(x = EUD, y = p_value)) +
  geom_bar(stat = "identity", fill = "#377eb8", width = 0.7) +
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "red", linewidth = 1) +
  labs(title = "Oral Cavity",
       x = "EUD",
       y = "p-value") +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0, size = 16, face = "plain"),
    axis.title = element_text(size = 13),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )
p2

eud_min_p2 <- results_df$EUD[which.min(results_df$p_value)]

# ---- LIPS ----

# Empty list to store results
results_LPs <- list()

subset_Lips <- EUD_outcome_name[EUD_outcome_name$structure_name == "Lips", ]

# Loop through the predictors
for (eud in eud_vars) {
  
  # Construct formula dynamically
  formula <- as.formula(paste(outcome_var, "~", eud))
  
  # Run the logistic regression model
  model <- glm(formula, data = subset_Lips, family = binomial)
  
  # Store coefficients and p-values
  results_LPs[[eud]] <- summary(model)$coefficients
  
}

# Convert results into a data frame for better viewing
results_df <- do.call(rbind, lapply(results_LPs, function(x) data.frame(coef = x[2, 1], 
                                                                        p_value = x[2, 4],
                                                                        OR = exp(x[2, 1]),
                                                                        CI_lower = exp(confint(model)[2, 1]),
                                                                        CI_upper = exp(confint(model)[2, 2]))))

# View the results
print(results_df)


library(ggplot2)

# Assicurati che results_df abbia una colonna con i nomi delle EUD
results_df$EUD <- rownames(results_df)

# Plotta p-value
p3 <- ggplot(results_df, aes(x = EUD, y = p_value)) +
  geom_bar(stat = "identity", fill = "#377eb8", width = 0.7) +
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "red", linewidth = 1) +
  labs(title = "Lips",
       x = "EUD",
       y = "p-value") +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0, size = 16, face = "plain"),
    axis.title = element_text(size = 13),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )
p3

eud_min_p3 <- results_df$EUD[which.min(results_df$p_value)]

# ---- MUSC CONSTRICT ----

# Empty list to store results
results_MC <- list()

subset_Musc_Constrict <- EUD_outcome_name[EUD_outcome_name$structure_name == "Musc_Constrict", ]

# Loop through the predictors
for (eud in eud_vars) {
  
  # Construct formula dynamically
  formula <- as.formula(paste(outcome_var, "~", eud))
  
  # Run the logistic regression model
  model <- glm(formula, data = subset_Musc_Constrict, family = binomial)
  
  # Store coefficients and p-values
  results_MC[[eud]] <- summary(model)$coefficients
  
}

# Convert results into a data frame for better viewing
results_df <- do.call(rbind, lapply(results_MC, function(x) data.frame(coef = x[2, 1], 
                                                                       p_value = x[2, 4],
                                                                       OR = exp(x[2, 1]),
                                                                       CI_lower = exp(confint(model)[2, 1]),
                                                                       CI_upper = exp(confint(model)[2, 2]))))

# View the results
print(results_df)


library(ggplot2)

# Assicurati che results_df abbia una colonna con i nomi delle EUD
results_df$EUD <- rownames(results_df)

# Plotta p-value
p4 <- ggplot(results_df, aes(x = EUD, y = p_value)) +
  geom_bar(stat = "identity", fill = "#377eb8", width = 0.7) +
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "red", linewidth = 1) +
  labs(title = "Musc Constrict",
       x = "EUD",
       y = "p-value") +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0, size = 16, face = "plain"),
    axis.title = element_text(size = 13),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )
p4

eud_min_p4 <- results_df$EUD[which.min(results_df$p_value)]

# ---- TRACHEA ----

# Empty list to store results
results_TR <- list()

subset_Trachea <- EUD_outcome_name[EUD_outcome_name$structure_name == "Trachea", ]

# Loop through the predictors
for (eud in eud_vars) {
  
  # Construct formula dynamically
  formula <- as.formula(paste(outcome_var, "~", eud))
  
  # Run the logistic regression model
  model <- glm(formula, data = subset_Trachea, family = binomial)
  
  # Store coefficients and p-values
  results_TR[[eud]] <- summary(model)$coefficients
  
}

# Convert results into a data frame for better viewing
results_df <- do.call(rbind, lapply(results_TR, function(x) data.frame(coef = x[2, 1], 
                                                                       p_value = x[2, 4],
                                                                       OR = exp(x[2, 1]),
                                                                       CI_lower = exp(confint(model)[2, 1]),
                                                                       CI_upper = exp(confint(model)[2, 2]))))

# View the results
print(results_df)


library(ggplot2)

# Assicurati che results_df abbia una colonna con i nomi delle EUD
results_df$EUD <- rownames(results_df)

# Plotta p-value
p5 <- ggplot(results_df, aes(x = EUD, y = p_value)) +
  geom_bar(stat = "identity", fill = "#377eb8", width = 0.7) +
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "red", linewidth = 1) +
  labs(title = "Trachea",
       x = "EUD",
       y = "p-value") +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0, size = 16, face = "plain"),
    axis.title = element_text(size = 13),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )
p5

eud_min_p5 <- results_df$EUD[which.min(results_df$p_value)]

# ---- ESOPHAGUS ----

# Empty list to store results
results_ES <- list()

subset_Esophagus <- EUD_outcome_name[EUD_outcome_name$structure_name == "Esophagus", ]

# Loop through the predictors
for (eud in eud_vars) {
  
  # Construct formula dynamically
  formula <- as.formula(paste(outcome_var, "~", eud))
  
  # Run the logistic regression model
  model <- glm(formula, data = subset_Esophagus, family = binomial)
  
  # Store coefficients and p-values
  results_ES[[eud]] <- summary(model)$coefficients
  
}

# Convert results into a data frame for better viewing
results_df <- do.call(rbind, lapply(results_ES, function(x) data.frame(coef = x[2, 1], 
                                                                       p_value = x[2, 4],
                                                                       OR = exp(x[2, 1]),
                                                                       CI_lower = exp(confint(model)[2, 1]),
                                                                       CI_upper = exp(confint(model)[2, 2]))))

# View the results
print(results_df)


library(ggplot2)

# Assicurati che results_df abbia una colonna con i nomi delle EUD
results_df$EUD <- rownames(results_df)

# Plotta p-value
p6 <- ggplot(results_df, aes(x = EUD, y = p_value)) +
  geom_bar(stat = "identity", fill = "#377eb8", width = 0.7) +
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "red", linewidth = 1) +
  labs(title = "Esophagus",
       x = "EUD",
       y = "p-value") +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0, size = 16, face = "plain"),
    axis.title = element_text(size = 13),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )
p6

eud_min_p6 <- results_df$EUD[which.min(results_df$p_value)]

# ---- LARYNX ----

# Empty list to store results
results_LX <- list()

subset_Larynx <- EUD_outcome_name[EUD_outcome_name$structure_name == "Larynx", ]

# Loop through the predictors
for (eud in eud_vars) {
  
  # Construct formula dynamically
  formula <- as.formula(paste(outcome_var, "~", eud))
  
  # Run the logistic regression model
  model <- glm(formula, data = subset_Larynx, family = binomial)
  
  # Store coefficients and p-values
  results_LX[[eud]] <- summary(model)$coefficients
  
}

# Convert results into a data frame for better viewing
results_df <- do.call(rbind, lapply(results_LX, function(x) data.frame(coef = x[2, 1], 
                                                                       p_value = x[2, 4],
                                                                       OR = exp(x[2, 1]),
                                                                       CI_lower = exp(confint(model)[2, 1]),
                                                                       CI_upper = exp(confint(model)[2, 2]))))

# View the results
print(results_df)


library(ggplot2)

# Assicurati che results_df abbia una colonna con i nomi delle EUD
results_df$EUD <- rownames(results_df)

# Plotta p-value
p7 <- ggplot(results_df, aes(x = EUD, y = p_value)) +
  geom_bar(stat = "identity", fill = "#377eb8", width = 0.7) +
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "red", linewidth = 1) +
  labs(title = "Larynx",
       x = "EUD",
       y = "p-value") +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0, size = 16, face = "plain"),
    axis.title = element_text(size = 13),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )
p7

eud_min_p7 <- results_df$EUD[which.min(results_df$p_value)]

# ---- PAROTIDS ----
#install.packages("dplyr")

library(dplyr)

EUD_outcome_name_Parotids2 <- EUD_outcome_name %>%
  filter(structure_name %in% c("Parotid_L", "Parotid_R"))

#df_avg <- EUD_outcome_name_Parotids2 %>%
  #group_by(ID) %>%
  #summarise(across(where(is.numeric), mean, na.rm = TRUE), .groups = "drop")
df_avg <- EUD_outcome_name_Parotids2 %>%
  group_by(ID) %>%
  summarise(across(where(is.numeric), function(x) mean(x, na.rm = TRUE)), .groups = "drop")


df_avg <- df_avg %>%
  mutate(structure_name = "Parotids")

EUD_outcome_name_Parotids_Final <- bind_rows(EUD_outcome_name, df_avg)


# Empty list to store results
results_PRs <- list()

subset_Parotids <- EUD_outcome_name_Parotids_Final[EUD_outcome_name_Parotids_Final$structure_name == "Parotids", ]

# Loop through the predictors
for (eud in eud_vars) {
  
  # Construct formula dynamically
  formula <- as.formula(paste(outcome_var, "~", eud))
  
  # Run the logistic regression model
  model <- glm(formula, data = subset_Parotids, family = binomial)
  
  # Store coefficients and p-values
  results_PRs[[eud]] <- summary(model)$coefficients
  
}

# Convert results into a data frame for better viewing
results_df <- do.call(rbind, lapply(results_PRs, function(x) data.frame(coef = x[2, 1], 
                                                                        p_value = x[2, 4],
                                                                        OR = exp(x[2, 1]),
                                                                        CI_lower = exp(confint(model)[2, 1]),
                                                                        CI_upper = exp(confint(model)[2, 2]))))

# View the results
print(results_df)


library(ggplot2)

# Assicurati che results_df abbia una colonna con i nomi delle EUD
results_df$EUD <- rownames(results_df)

# Plotta p-value
p8 <- ggplot(results_df, aes(x = EUD, y = p_value)) +
  geom_bar(stat = "identity", fill = "#377eb8", width = 0.7) +
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "red", linewidth = 1) +
  labs(title = "Parotids",
       x = "EUD",
       y = "p-value") +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0, size = 16, face = "plain"),
    axis.title = element_text(size = 13),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )
p8

eud_min_p8 <- results_df$EUD[which.min(results_df$p_value)]

# ---- GLND SUBMANDS ----

library(dplyr)

EUD_outcome_name_Glnd_Submands <- EUD_outcome_name %>%
  filter(structure_name %in% c("Glnd_Submand_L", "Glnd_Submand_R"))

df_avg2 <- EUD_outcome_name_Glnd_Submands %>%
  group_by(ID) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE), .groups = "drop")

df_avg2 <- df_avg2 %>%
  mutate(structure_name = "Glnd_Submands")

EUD_outcome_name_Glnd_Submands_Final <- bind_rows(EUD_outcome_name, df_avg2)


# Empty list to store results
results_GSs <- list()

subset_Glnd_Submands <- EUD_outcome_name_Glnd_Submands_Final[EUD_outcome_name_Glnd_Submands_Final$structure_name == "Glnd_Submands", ]

# Loop through the predictors
for (eud in eud_vars) {
  
  # Construct formula dynamically
  formula <- as.formula(paste(outcome_var, "~", eud))
  
  # Run the logistic regression model
  model <- glm(formula, data = subset_Glnd_Submands, family = binomial)
  
  # Store coefficients and p-values
  results_GSs[[eud]] <- summary(model)$coefficients
  
}

# Convert results into a data frame for better viewing
results_df <- do.call(rbind, lapply(results_GSs, function(x) data.frame(coef = x[2, 1], 
                                                                        p_value = x[2, 4],
                                                                        OR = exp(x[2, 1]),
                                                                        CI_lower = exp(confint(model)[2, 1]),
                                                                        CI_upper = exp(confint(model)[2, 2]))))

# View the results
print(results_df)


library(ggplot2)

# Assicurati che results_df abbia una colonna con i nomi delle EUD
results_df$EUD <- rownames(results_df)

# Plotta p-value
p9 <- ggplot(results_df, aes(x = EUD, y = p_value)) +
  geom_bar(stat = "identity", fill = "#377eb8", width = 0.7) +
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "red", linewidth = 1) +
  labs(title = "Glnd Submands",
       x = "EUD",
       y = "p-value") +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0, size = 16, face = "plain"),
    axis.title = element_text(size = 13),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )
p9

eud_min_p9 <- results_df$EUD[which.min(results_df$p_value)]

# 6. GRAPHS ----

#install.packages("gridExtra")
library(gridExtra)
library(grid)

grid.arrange(
  p1, p2, p3, p4, p5, p6, p7, p8, p9, 
  nrow = 3, ncol = 3,
  top = textGrob(
    paste(toupper(outcome_name), "thr =", threshold),
    gp = gpar(fontsize = 18, fontface = "bold")
  )
)

eud_min_p1
eud_min_p2
eud_min_p3
eud_min_p4
eud_min_p5
eud_min_p6
eud_min_p7
eud_min_p8
eud_min_p9

