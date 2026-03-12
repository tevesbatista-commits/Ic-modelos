library(tidyverse)
library(glmmTMB)
library(lme4)
library(DHARMa) 
library(tsoutliers)
library(ggplot2)

#Efeitos fixos e um aleatório simples, mas sem interação entre eles.
# ── Função auxiliar ───────────────────────────────────────────────────────────
# INSTRUÇÃO: selecione o arquivo ajustar_categoricamiguel.R
source(file.choose())

# ── Carregar dados ────────────────────────────────────────────────────────────
# INSTRUÇÃO: selecione o arquivo dados_sumarizados_v3.csv
dados_sumarizados_v3 <- read.csv(file.choose(), header = TRUE, sep = ",")

#-------------------------
#media
#passou
## Gamma - Identidade
#------------------------

modelo3 <- glmmTMB(GY_media ~ local_estado + Year_Date + (1|GEN), data = dados_sumarizados_v3,
                   family = Gamma(link = "identity"))
summary(modelo3)
residuos_dharma <- simulateResiduals(modelo3)
par(mar = c(3, 3, 1, 1),   # margens: baixo, esquerda, cima, direita
    oma = c(0, 0, 0, 0))   # sem margem externa

plot(residuos_dharma)
testResiduals(residuos_dharma)




testDispersion(modelo3)
testOutliers(modelo3)

#---------------------
#BOXPLOT
#---------------------


# Paleta de azuis interpolada — um tom por local/estado
cores_azul <- colorRampPalette(c("#caf0f8", "#90e0ef", "#00b4d8", "#0077b6", "#48cae4"))(
  length(unique(dados_sumarizados_v3$local_estado))
)

ggplot(dados_sumarizados_v3, aes(x = local_estado, y = GY_media, fill = local_estado)) +
  
  # Boxplot: mediana, quartis e outliers de produtividade por local
  # Outliers em azul escuro para destacar observações extremas
  geom_boxplot(outlier.colour = "#03045e",
               outlier.shape = 21, outlier.size = 2) +
  
  # Jitter: pontos reais sobrepostos para mostrar densidade dos dados
  geom_jitter(width = 0.2, alpha = 0.3, size = 0.8, color = "gray30") +
  
  # Aplica a paleta de azuis criada acima a cada local
  scale_fill_manual(values = cores_azul) +
  
  # Tema limpo com fonte tamanho 12
  theme_bw(base_size = 12) +
  
  # Rótulos do gráfico
  labs(
    title = "Variação de Genótipos por Local (GY_media)",
    x     = "Local",
    y     = "Produtividade Média (kg/ha)"
  ) +
  
  theme(
    # Rotaciona 45° e reduz fonte para os nomes não se sobreporem
    axis.text.x     = element_text(angle = 45, vjust = 1, hjust = 1, size = 5),
    # Remove legenda pois a cor já está identificada no eixo X
    legend.position = "none",
    # Título centralizado e em negrito
    plot.title      = element_text(hjust = 0.5, face = "bold")
  )





#-------------------------
#mediana
#passou
## Gamma - Identidade
#------------------------
modelo3 <- glmmTMB(GY_mediana ~ local_estado + Year_Date + (1|GEN), data = dados_sumarizados_v3,
                   family = Gamma(link = "identity"))
summary(modelo3)
residuos_dharma <- simulateResiduals(modelo3)
par(mar = c(3, 3, 1, 1),   # margens: baixo, esquerda, cima, direita
    oma = c(0, 0, 0, 0))   # sem margem externa

plot(residuos_dharma)
testResiduals(residuos_dharma)




testDispersion(modelo3)
testOutliers(modelo3)




#---------------------
#BOXPLOT
#---------------------


# Paleta de azuis interpolada — um tom por local/estado
cores_azul <- colorRampPalette(c("#caf0f8", "#90e0ef", "#00b4d8", "#0077b6", "#48cae4"))(
  length(unique(dados_sumarizados_v3$local_estado))
)

ggplot(dados_sumarizados_v3, aes(x = local_estado, y = GY_mediana, fill = local_estado)) +
  
  # Boxplot: mediana, quartis e outliers da mediana de produtividade por local
  # Outliers em azul escuro para destacar observações extremas
  geom_boxplot(outlier.colour = "#03045e",
               outlier.shape = 21, outlier.size = 2) +
  
  # Jitter: pontos reais sobrepostos para mostrar densidade dos dados
  geom_jitter(width = 0.2, alpha = 0.3, size = 0.8, color = "gray30") +
  
  # Aplica a paleta de azuis criada acima a cada local
  scale_fill_manual(values = cores_azul) +
  
  # Tema limpo com fonte tamanho 12
  theme_bw(base_size = 12) +
  
  # Rótulos do gráfico
  labs(
    title = "Variação de Genótipos por Local (GY_mediana)",
    x     = "Local",
    y     = "Mediana de Produtividade (kg/ha)"
  ) +
  
  theme(
    # Rotaciona 45° e reduz fonte para os nomes não se sobreporem
    axis.text.x     = element_text(angle = 45, vjust = 1, hjust = 1, size = 5),
    # Remove legenda pois a cor já está identificada no eixo X
    legend.position = "none",
    # Título centralizado e em negrito
    plot.title      = element_text(hjust = 0.5, face = "bold")
  )



#-----------------------
#media
#passou
## Gaussian - log
#-----------------------
modelo3 <- glmmTMB(GY_media ~ local_estado + Year_Date + (1|GEN), data = dados_sumarizados_v3,
                   family = gaussian(link = "log"))
summary(modelo3)

residuos_dharma <- simulateResiduals(modelo3)
par(mar = c(3, 3, 1, 1),   # margens: baixo, esquerda, cima, direita
    oma = c(0, 0, 0, 0))   # sem margem externa

plot(residuos_dharma)
testResiduals(residuos_dharma)




testDispersion(modelo3)
testOutliers(modelo3)


#---------------------
#BOXPLOT
#---------------------


# Paleta de azuis interpolada — um tom por local/estado
cores_azul <- colorRampPalette(c("#caf0f8", "#90e0ef", "#00b4d8", "#0077b6", "#48cae4"))(
  length(unique(dados_sumarizados_v3$local_estado))
)

ggplot(dados_sumarizados_v3, aes(x = local_estado, y = GY_media, fill = local_estado)) +
  
  # Boxplot: mediana, quartis e outliers de produtividade por local
  # Outliers em azul escuro para destacar observações extremas
  geom_boxplot(outlier.colour = "#03045e",
               outlier.shape = 21, outlier.size = 2) +
  
  # Jitter: pontos reais sobrepostos para mostrar densidade dos dados
  geom_jitter(width = 0.2, alpha = 0.3, size = 0.8, color = "gray30") +
  
  # Aplica a paleta de azuis criada acima a cada local
  scale_fill_manual(values = cores_azul) +
  
  # Tema limpo com fonte tamanho 12
  theme_bw(base_size = 12) +
  
  # Rótulos do gráfico
  labs(
    title = "Variação de Genótipos por Local (GY_media)",
    x     = "Local",
    y     = "Produtividade Média (kg/ha)"
  ) +
  
  theme(
    # Rotaciona 45° e reduz fonte para os nomes não se sobreporem
    axis.text.x     = element_text(angle = 45, vjust = 1, hjust = 1, size = 5),
    # Remove legenda pois a cor já está identificada no eixo X
    legend.position = "none",
    # Título centralizado e em negrito
    plot.title      = element_text(hjust = 0.5, face = "bold")
  )





#-----------------------
#mediana
#passou
## Gaussian - log
#-----------------------
modelo3 <- glmmTMB(GY_mediana ~ local_estado + Year_Date + (1|GEN), data = dados_sumarizados_v3,
                   family = gaussian(link = "log"))
summary(modelo3)

residuos_dharma <- simulateResiduals(modelo3)
par(mar = c(3, 3, 1, 1),   # margens: baixo, esquerda, cima, direita
    oma = c(0, 0, 0, 0))   # sem margem externa

plot(residuos_dharma)
testResiduals(residuos_dharma)




testDispersion(modelo3)
testOutliers(modelo3)


#---------------------
#BOXPLOT
#---------------------

# Paleta de azuis interpolada — um tom por local/estado
cores_azul <- colorRampPalette(c("#caf0f8", "#90e0ef", "#00b4d8", "#0077b6", "#48cae4"))(
  length(unique(dados_sumarizados_v3$local_estado))
)

ggplot(dados_sumarizados_v3, aes(x = local_estado, y = GY_mediana, fill = local_estado)) +
  
  # Boxplot: mediana, quartis e outliers da mediana de produtividade por local
  # Outliers em azul escuro para destacar observações extremas
  geom_boxplot(outlier.colour = "#03045e",
               outlier.shape = 21, outlier.size = 2) +
  
  # Jitter: pontos reais sobrepostos para mostrar densidade dos dados
  geom_jitter(width = 0.2, alpha = 0.3, size = 0.8, color = "gray30") +
  
  # Aplica a paleta de azuis criada acima a cada local
  scale_fill_manual(values = cores_azul) +
  
  # Tema limpo com fonte tamanho 12
  theme_bw(base_size = 12) +
  
  # Rótulos do gráfico
  labs(
    title = "Variação de Genótipos por Local (GY_mediana)",
    x     = "Local",
    y     = "Mediana de Produtividade (kg/ha)"
  ) +
  
  theme(
    # Rotaciona 45° e reduz fonte para os nomes não se sobreporem
    axis.text.x     = element_text(angle = 45, vjust = 1, hjust = 1, size = 5),
    # Remove legenda pois a cor já está identificada no eixo X
    legend.position = "none",
    # Título centralizado e em negrito
    plot.title      = element_text(hjust = 0.5, face = "bold")
  )







# ═══════════════════════════════════════════════════════════════════
#        RANKINGS — modelo3 (local_estado + Year_Date + (1|GEN))
# ═══════════════════════════════════════════════════════════════════

cores_azul <- colorRampPalette(c("#caf0f8", "#90e0ef", "#00b4d8", "#0077b6", "#48cae4"))(7)

# ── 1. Ranking de Genótipos ───────────────────────────────────────
ranking_gen <- dados_sumarizados_v3 %>%
  mutate(Predict = predict(modelo3, type = "response")) %>%
  group_by(GEN) %>%
  summarise(
    X_barra         = mean(Predict, na.rm = TRUE),
    sigma           = sd(Predict,   na.rm = TRUE),
    n               = n(),
    erro_padrao     = sigma / sqrt(n),
    limite_inferior = X_barra - (1.96 * erro_padrao),
    limite_superior = X_barra + (1.96 * erro_padrao),
    .groups         = "drop"
  ) %>%
  arrange(desc(X_barra)) %>%
  mutate(Rank = row_number())

p1 <- ggplot(ranking_gen, aes(x = reorder(GEN, X_barra), y = X_barra)) +
  geom_col(aes(fill = X_barra), show.legend = FALSE) +
  geom_errorbar(aes(ymin = limite_inferior, ymax = limite_superior),
                width = 0.3, color = "black", linewidth = 0.7) +
  geom_text(aes(label = paste0("#", Rank)), hjust = -0.3, size = 4, fontface = "bold", color = "#48cae4") +
  scale_fill_gradient(low = "#caf0f8", high = "#0077b6") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  coord_flip() +
  labs(
    title    = "Ranking de Genótipos por Produtividade",
    subtitle = "Média das predições com IC 95% — Gamma identity | local + ano",
    x = "Genótipo",
    y = "GY (kg/ha)"
  ) +
  theme_bw(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(color = "gray40", hjust = 0.5)
  )
print(p1)


# ── 2. Ranking de Locais ──────────────────────────────────────────
ranking_local <- dados_sumarizados_v3 %>%
  mutate(Predict = predict(modelo3, type = "response")) %>%
  group_by(local_estado) %>%
  summarise(
    X_barra         = mean(Predict, na.rm = TRUE),
    sigma           = sd(Predict,   na.rm = TRUE),
    n               = n(),
    erro_padrao     = sigma / sqrt(n),
    limite_inferior = X_barra - (1.96 * erro_padrao),
    limite_superior = X_barra + (1.96 * erro_padrao),
    .groups         = "drop"
  ) %>%
  arrange(desc(X_barra)) %>%
  mutate(Rank = row_number())

p2 <- ggplot(ranking_local, aes(x = reorder(local_estado, X_barra), y = X_barra)) +
  geom_col(aes(fill = X_barra), show.legend = FALSE) +
  geom_errorbar(aes(ymin = limite_inferior, ymax = limite_superior),
                width = 0.3, color = "black", linewidth = 0.7) +
  geom_text(aes(label = paste0("#", Rank)), hjust = -0.3, size = 3.5, fontface = "bold", color = "#48cae4") +
  scale_fill_gradient(low = "#caf0f8", high = "#0077b6") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  coord_flip() +
  labs(
    title    = "Ranking de Locais por Produtividade",
    subtitle = "Média das predições com IC 95% — Gamma identity | local + ano",
    x = "Local",
    y = "GY (kg/ha)"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(color = "gray40", hjust = 0.5)
  )
print(p2)


# ── 3. Ranking de Anos ────────────────────────────────────────────
ranking_ano <- dados_sumarizados_v3 %>%
  mutate(Predict = predict(modelo3, type = "response")) %>%
  group_by(Year_Date) %>%
  summarise(
    X_barra         = mean(Predict, na.rm = TRUE),
    sigma           = sd(Predict,   na.rm = TRUE),
    n               = n(),
    erro_padrao     = sigma / sqrt(n),
    limite_inferior = X_barra - (1.96 * erro_padrao),
    limite_superior = X_barra + (1.96 * erro_padrao),
    .groups         = "drop"
  ) %>%
  arrange(desc(X_barra)) %>%
  mutate(Rank = row_number())

p3 <- ggplot(ranking_ano, aes(x = reorder(factor(Year_Date), X_barra), y = X_barra)) +
  geom_col(aes(fill = X_barra), show.legend = FALSE) +
  geom_errorbar(aes(ymin = limite_inferior, ymax = limite_superior),
                width = 0.3, color = "black", linewidth = 0.7) +
  geom_text(aes(label = paste0("#", Rank)), hjust = -0.3, size = 3.5, fontface = "bold", color = "#48cae4") +
  scale_fill_gradient(low = "#caf0f8", high = "#0077b6") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  coord_flip() +
  labs(
    title    = "Ranking de Anos por Produtividade",
    subtitle = "Média das predições com IC 95% — Gamma identity | local + ano",
    x = "Ano",
    y = "GY (kg/ha)"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(color = "gray40", hjust = 0.5)
  )
print(p3)


# ── 4. Boxplot por Genótipo ───────────────────────────────────────
p4 <- dados_sumarizados_v3 %>%
  mutate(Predict = predict(modelo3, type = "response"),
         GEN     = reorder(GEN, Predict, median)) %>%
  ggplot(aes(x = GEN, y = Predict, fill = GEN)) +
  geom_boxplot(show.legend = FALSE, outlier.color = "#48cae4",
               outlier.shape = 18, outlier.size = 2) +
  geom_jitter(width = 0.2, alpha = 0.3, size = 1.5, color = "#0077b6") +
  scale_fill_manual(values = cores_azul) +
  coord_flip() +
  labs(
    title    = "Distribuição de Produtividade por Genótipo",
    subtitle = "Ordenado pela mediana das predições — Gamma identity | local + ano",
    x = "Genótipo",
    y = "GY (kg/ha)"
  ) +
  theme_bw(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(color = "gray40", hjust = 0.5)
  )
print(p4)


#testar NA

dados_sumarizados_v3 <- dados_sumarizados_v3 |>
  filter(n_rep > 1)

dados_sumarizados_v3 |>
  filter(is.na(GY_sd)) |>
  select(GEN, Ambiente, n_rep, GY_media, GY_mediana)

