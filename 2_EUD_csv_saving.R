###---------------------###
### EUD-TOXICITY SAVING ###
###-------------------- ###

# Caricamento dati ----
# Mantieni solo FUP_outcome_name e i valori eud_min
rm(list = setdiff(ls(), c("outcome_name", "FUP_outcome_name", "chirurgia_df",
                          "eud_min_p1", "eud_min_p2", "eud_min_p3", "eud_min_p4", "eud_min_p5", 
                          "eud_min_p6", "eud_min_p7", "eud_min_p8", "eud_min_p9")))
colnames(FUP_outcome_name)[colnames(FUP_outcome_name) == "id_microlearner"] <- "ID"

# Carica Dati
dati_modificati <- read.csv("EUD_final_modificato.csv", stringsAsFactors = FALSE)

# Creazione tabella ----
# Abbreviazioni personalizzate per ogni organo
abbreviazioni <- c(
  "Bone_Mandible" = "BM",
  "Cavity_Oral" = "CO",
  "Lips" = "LI",
  "Musc_Constrict" = "MC",
  "Trachea" = "T",
  "Esophagus" = "E",
  "Larynx" = "L",
  "Parotids" = "P",
  "Submandibulars" = "SM"
)

# Ho considerato i p-value minori per tutti: se erano pressochè uguali non ho comunque preso 1!

# Elenco degli organi e colonne da copiare
organi <- names(abbreviazioni)
colonne_finali <- c(eud_min_p1, eud_min_p2, eud_min_p3, eud_min_p4, eud_min_p5, eud_min_p6,
                        eud_min_p7, eud_min_p8, eud_min_p9)

# Inizializza tabella finale
tabella_finale <- data.frame(ID = unique(dati_modificati$ID))

# Ciclo per ogni organo
for (i in seq_along(organi)) {
  organo <- organi[i]
  colonna <- colonne_finali[i]
  
  # Controllo colonna esistente
  if (!(colonna %in% names(dati_modificati))) {
    warning(paste("Colonna mancante:", colonna, "- salto", organo))
    next
  }
  
  # Estrai righe per organo
  sotto_df <- dati_modificati[dati_modificati$structure_name == organo, c("ID", colonna)]
  
  # Estrai valore di n da "EUD_n0.01" → "0.01"
  valore_n <- sub("EUD_n", "", colonna)
  
  # Crea nome colonna finale (es. BM_0.01)
  nome_colonna <- paste0(abbreviazioni[organo], "_", valore_n)
  names(sotto_df)[2] <- nome_colonna
  
  # Unione
  tabella_finale <- merge(tabella_finale, sotto_df, by = "ID", all.x = TRUE)
}

# Aggiungi la colonna "Toxicity" dalla tabella degli outcome
tabella_finale <- merge(tabella_finale, FUP_outcome_name[, c("ID", "Toxicity1")],
                        by.x = "ID", by.y = "ID", all.x = TRUE)


# Aggiungi la colonna "auc_norm" e "chirrugia_attuale -----
tabella_finale <- merge(
  tabella_finale,
  FUP_outcome_name[, c("ID", "auc_norm")],
  by.x = "ID",
  by.y = "ID",
  all.x = TRUE
)

tabella_finale <- merge(tabella_finale, chirurgia_df, by = "ID", all.x = TRUE)

# Conta numero di pazienti con tossicità 1----
# Rinomina correttamente se necessario
names(tabella_finale)[names(tabella_finale) == "Toxicity1"] <- "Toxicity"

# Conta i valori di 1
numero_positivi <- sum(tabella_finale$Toxicity == 1, na.rm = TRUE)
cat("Numero di casi con Toxicity = 1:", numero_positivi, "\n")

# Esporta tabella ----
write.csv(tabella_finale, "tabella_finale_prova3.csv", row.names = FALSE)

# Visualizza
head(tabella_finale)

# Grafici per ciascun organo con EUD crescenti con evidentizati i pazienti tossici ----
library(ggplot2)
library(gridExtra)
library(grid)

# Inizializza lista di plot
plot_list <- list()

for (i in seq_along(organi)) {
  organo <- organi[i]
  abbrev <- abbreviazioni[organo]
  col_eud <- colonne_finali[i]
  
  # Nome colonna nella tabella
  col_name <- grep(paste0("^", abbrev, "_"), names(tabella_finale), value = TRUE)
  
  # Crea df temporaneo
  df_plot <- tabella_finale[, c("ID", "Toxicity", col_name)]
  colnames(df_plot)[3] <- "EUD_value"
  
  # Ordina per EUD crescente
  df_plot <- df_plot[order(df_plot$EUD_value), ]
  df_plot$index <- seq_len(nrow(df_plot))
  
  # Crea grafico
  p <- ggplot(df_plot, aes(x = index, y = EUD_value)) +
    geom_segment(aes(xend = index, y = 0, yend = EUD_value, color = as.factor(Toxicity)), linewidth = 0.8) +
    scale_color_manual(values = c("0" = "grey60", "1" = "red")) +
    labs(
      title = paste0(abbrev, " (", col_eud, ")"),
      x = "Patients",
      y = "EUD"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "none",
      plot.title = element_text(hjust = 0.5, face = "bold")
    )
  print(p)
  plot_list[[i]] <- p
}

# Unisci i plot con gridExtra
grid.arrange(
  grobs = plot_list,
  nrow = 3, ncol = 3,
  top = textGrob(
    "Xerostomia EUD - Toxicity"),
    gp = gpar(fontsize = 18, fontface = "bold")
  )


## tolgo quelli con chirurgia attuale
#tabella_finale2 <- merge(tabella_finale, chirurgia_df, by = "ID", all.x = TRUE)
tabella_finale2 <- tabella_finale
tabella_finale2 <- tabella_finale2[tabella_finale2$chirurgia_attuale != 1, ]

# Conta numero di pazienti con tossicità 1----
# Rinomina correttamente se necessario
names(tabella_finale2)[names(tabella_finale2) == "Toxicity1"] <- "Toxicity"

# Conta i valori di 1
numero_positivi <- sum(tabella_finale2$Toxicity == 1, na.rm = TRUE)
cat("Numero di casi con Toxicity = 1:", numero_positivi, "\n")

# Esporta tabella ----
write.csv(tabella_finale2, "tabella_finale_prova4.csv", row.names = FALSE)

# Visualizza
head(tabella_finale2)

# Grafici per ciascun organo con EUD crescenti con evidentizati i pazienti tossici ----
library(ggplot2)
library(gridExtra)
library(grid)

# Inizializza lista di plot
plot_list <- list()

for (i in seq_along(organi)) {
  organo <- organi[i]
  abbrev <- abbreviazioni[organo]
  col_eud <- colonne_finali[i]
  
  # Nome colonna nella tabella
  col_name <- grep(paste0("^", abbrev, "_"), names(tabella_finale2), value = TRUE)
  
  # Crea df temporaneo
  df_plot <- tabella_finale2[, c("ID", "Toxicity", col_name)]
  colnames(df_plot)[3] <- "EUD_value"
  
  # Ordina per EUD crescente
  df_plot <- df_plot[order(df_plot$EUD_value), ]
  df_plot$index <- seq_len(nrow(df_plot))
  
  # Crea grafico
  p <- ggplot(df_plot, aes(x = index, y = EUD_value)) +
    geom_segment(aes(xend = index, y = 0, yend = EUD_value, color = as.factor(Toxicity)), linewidth = 0.8) +
    scale_color_manual(values = c("0" = "grey60", "1" = "red")) +
    labs(
      title = "Oral Mucositis EUD - Toxicity", #paste0(abbrev, " (", col_eud, ")"),
      x = "Patients",
      y = "EUD"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "none",
      plot.title = element_text(hjust = 0.5, face = "bold")
    )
  print(p)
  plot_list[[i]] <- p
}

# Unisci i plot con gridExtra
grid.arrange(
  grobs = plot_list,
  nrow = 3, ncol = 3,
  top = textGrob(
    "Dysphagia EUD - Toxicity"),
    gp = gpar(fontsize = 18, fontface = "bold")
  )


