###---------------------###
### EUD-TOXICITY SAVING ###
###-------------------- ###

# Load necessary libraries and prepared data ----
#install.packages("dplyr")
#install.packages("lmtest")

library(lmtest)
library(readr)       # for reading CSV files
library(dplyr)       # for data manipulation
library(caret)       # for splitting data
library(pROC)        # for ROC analysis

# Load your dataset
data <- read.csv("tabella_finale_prova3.csv")

# Il paziente 2086, e 2186 non hanno i dati relativi alla Laringe

data <- data[data$ID != 2186, ]
#data <- data[data$ID != 2086, ]

# Come per la definizione della tossicità per ora escludo i pazienti con id = 2023, 2060, 2127, 2168 che hanno un osservazione in meno e quindi AUC=NA
# Se desideri combinare media e massimo in una singola colonna:
# 
data <- data[data$ID != 2023, ]
# data <- data[data$ID != 2060, ]
data <- data[data$ID != 2127, ]
data <- data[data$ID != 2168, ]

# Provo a usare l'output continuo:
# Rimuovi la colonna "toxicity" da 'data'
data$Toxicity <- NULL

# Linear Model ----
# Select features and target
# Modify the column names to match your dataset
features <- c("BM_1", "CO_0.05", "MC_1", "T_0.45", "E_0.35", "L_0.01", "P_0.05", "SM_0.1")
target <- "auc"

# Costruisce la formula del modello linearmente
formula <- as.formula(
  paste(target, "~", paste(features, collapse = " + "))
)

# Crea il modello lineare
model <- lm(formula, data = data)

# Riassunto del modello
summary(model)
# R-squared: quanto del totale della variabilità di auc è spiegato dal modello (24.2%). Basso/modesto

# Calcola VIF
library(car)
vif(model)
# T_0.45, E_0.35, MC_1 e SM_0.05 hanno VIF > 5 
# → potrebbero esserci correlazioni tra predittori che rendono instabile la stima dei coefficienti

# Escludere predittori con VIF > 5 ----
features_reduced <- c("BM_1", "CO_0.05", "L_0.01", "P_0.05", "SM_0.1")

formula_reduced <- as.formula(paste("auc ~", paste(features_reduced, collapse = " + ")))
model_reduced <- lm(formula_reduced, data = data)
summary(model_reduced)
vif(model_reduced)
# Solo SM_0.05 è significativa nel modello
# Spiega circa il 22.5% della variabilità totale di auc 

# ANALISI DEI RESIDUI ----
# 1. Estrarre residui e valori previsti
residui <- residuals(model)
valori_previsti <- fitted(model)

# 2. Plot dei residui vs valori previsti
plot(valori_previsti, residui,
     main = "Residui vs Valori Previsti",
     xlab = "Valori Previsti",
     ylab = "Residui",
     pch = 20,
     col = "blue")
abline(h = 0, col = "red", lwd = 2)

# 3. QQ-plot dei residui
qqnorm(residui, main = "QQ-Plot dei Residui")
qqline(residui, col = "red", lwd = 2)
# In particolare, la coda destra più lunga corrisponde a valori osservati molto 
# più grandi di quelli previsti, quindi auc alti che il modello sottostima

# 4. Istogramma dei residui
hist(residui, breaks = 20,
     main = "Istogramma dei Residui",
     xlab = "Residui",
     col = "lightblue", border = "white")

# 5. Test di normalità dei residui (Shapiro-Wilk)
shapiro.test(residui)
# p-value < 0.05 → rifiutiamo H₀ → i residui non sono normalmente distribuiti

# 6. Test per eteroschedasticità (Breusch-Pagan)
if (!require(lmtest)) install.packages("lmtest")
library(lmtest)
bptest(model)
# p-value > 0.05 → non rifiutiamo H₀ → nessuna evidenza di eteroschedasticità

# 7. Test per autocorrelazione dei residui (Durbin-Watson)
if (!require(car)) install.packages("car")
library(car)
durbinWatsonTest(model)
# p-value > 0.05 → nessuna evidenza di autocorrelazione 

# 8. Aggiunta di diagnostica avanzata
par(mfrow = c(2, 2))
plot(model)  # Include: Residui vs Valori Previsti, QQ Plot, Scale-Location, e Leverage

# Ripristina il layout grafico
par(mfrow = c(1, 1))

# PROBLEMA: i valori con auc alto -> quindi quelli parecchio malati li considera come fossero outlier!!!!!!!!!!
# Non sono errori o rumare, ma casi di alta tossicità


# Metodo 1: Trasformazione della variabile auc ----
# log(auc + 1)
data$auc_log <- log(data$auc + 1)
formula_log <- as.formula(paste("auc_log ~", paste(features, collapse = " + ")))
# Modello lineare
model_log <- lm(formula_log, data = data)
summary(model_log)
# Residui
residui_log <- residuals(model_log)
qqnorm(residui_log, main = "QQ-Plot dei Residui (log(auc + 1))")
qqline(residui_log, col = "red", lwd = 2)
hist(residui_log, breaks = 20, col = "lightblue", main = "Istogramma Residui (log(auc + 1))")
# Il modello potrebbe sottoperformare su pazienti con tossicità bassa, cioè valori piccoli di auc.
# Il modello con log(auc + 1) ha migliorato la normalità dei residui e ha aumentato l’R² (28%), ma 
# la coda sinistra anomala e la modesta significatività dei predittori indicano che c’è ancora margine per migliorare.

# sqrt(auc)
data$auc_sqrt <- sqrt(data$auc)
formula_sqrt <- as.formula(paste("auc_sqrt ~", paste(features, collapse = " + ")))
model_sqrt <- lm(formula_sqrt, data = data)
summary(model_sqrt)


# Metodo 2: PROVO a pesare di più i dati più alti ----
# Definiamo un peso maggiore per i valori di tossicità più alti
# In questo esempio, aumentiamo il peso in base al punteggio di tossicità
pesatura <- ifelse(data$auc > 15.5, 2, 1)  # Doppio peso per tossicità > 15.5
# Allenamento del modello con pesi
model <- lm(formula, data = data, weights = pesatura)
# Visualizza il sommario del modello
summary(model)


#################################################

library(MASS)

# Stepwise in entrambe le direzioni (forward + backward)
model_step <- stepAIC(model, direction = "both", trace = FALSE)

# Mostra il riepilogo
summary(model_step)


#################################################
####################### MIGLIORA IL MODELLO MA NON VA BENE ASSOLUTAMENTE, TOGLIE I 
####################### VALORI ALTI CHE DOBBIAMO INVECE RIUSCIRE A DESCRIMINARE

# Visualizzare i residui per identificare outliers
plot(model_step, which = 1)  # Residuals vs Fitted plot

# Calcolare i residui
residuals <- residuals(model_step)

# Definire un cut-off per gli outliers
threshold <- 10  # residuo > 3 deviazioni standard viene considerato outlier
outliers <- which(abs(residuals) > threshold)

# Rimuovere gli outliers
data_clean <- data[-outliers, ]

# Riadattare il modello senza outliers
model_no_outliers <- lm(auc ~ E_0.35 + SM_0.05, data = data_clean)
summary(model_no_outliers)

plot(model_no_outliers, which = 1)


