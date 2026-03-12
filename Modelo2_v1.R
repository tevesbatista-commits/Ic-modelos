library(tidyverse)
library(glmmTMB)
library(lme4)
library(DHARMa) 
library(tsoutliers)
library(ggplot2)

#Efeito aleatório de interação entre genótipo e ambiente.

# ── Função auxiliar ───────────────────────────────────────────────────────────
#TODOS OS ARQUIVOS ESTAO NA PASTA RESULTS
# INSTRUÇÃO: selecione o arquivo ajustar_categoricamiguel.R

source(file.choose())

# ── Carregar dados ────────────────────────────────────────────────────────────
# INSTRUÇÃO: selecione o arquivo dados_sumarizados_v3.csv
dados_sumarizados_v3 <- read.csv(file.choose(), header = TRUE, sep = ",")

#------------------------
# Modelo 2
#passou no dharma 
## Gaussiano - log media
#-------------------------
modelo2 <- glmmTMB(GY_media ~ Ambiente + (1|GEN) + (1|GEN:Ambiente), data = dados_sumarizados_v3,
                   family = gaussian(link = "log"))

resultado1 <- ajustar_categorica(dados_sumarizados_v3, "Ambiente", modelo2)
modelo2 <- resultado1$modelo_atualizado
summary(modelo2)

residuos_dharma <- simulateResiduals(modelo2)
par(mar = c(3, 3, 1, 1),   # margens: baixo, esquerda, cima, direita
    oma = c(0, 0, 0, 0))   # sem margem externa

plot(residuos_dharma)
testResiduals(residuos_dharma)




testDispersion(modelo2)
testOutliers(modelo2)


#---------------------
#BOXPLOT
#---------------------

# Paleta de azuis interpolada — um tom por ambiente
ggplot(dados_sumarizados_v3, aes(x = Ambiente, y = GY_media)) +
  
  # Boxplot: mediana, quartis e outliers por ambiente
  # Outliers em azul escuro; shape = 1 = círculo vazio (menos poluído visualmente)
  geom_boxplot(aes(fill = Ambiente), outlier.colour = "#03045e", outlier.shape = 1) +
  
  # Jitter: pontos reais sobrepostos para mostrar densidade dos dados
  geom_jitter(width = 0.2, alpha = 0.3, size = 0.5, color = "#00b4d8") +
  
  # Gera a paleta dinamicamente com base no número de ambientes
  scale_fill_manual(values = colorRampPalette(
    c("#caf0f8", "#90e0ef", "#00b4d8", "#0077b6", "#48cae4")
  )(nlevels(factor(dados_sumarizados_v3$Ambiente)))) +
  
  # Tema limpo
  theme_bw() +
  
  # Rótulos do gráfico
  labs(
    title    = "Genótipos por Ambiente",
    subtitle = "Gaussian log | GEN + GEN:Ambiente",
    x        = "Ambiente",
    y        = "Produtividade Média"
  ) +
  
  theme(
    # 90° é melhor que 45° quando há muitos ambientes — ocupa menos espaço vertical
    # size = 5 reduz a fonte para os nomes não se sobreporem
    axis.text.x  = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 5),
    legend.position = "none",
    plot.title   = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40")
  )





#--------------------
## Gaussiano - log 
#passou no dharma
#mediana
#-------------------
modelo2 <- glmmTMB(GY_mediana ~ Ambiente + (1|GEN) + (1|GEN:Ambiente), data = dados_sumarizados_v3,
                   family = gaussian(link = "log"))

resultado1 <- ajustar_categorica(dados_sumarizados_v3, "Ambiente", modelo2)
modelo2 <- resultado1$modelo_atualizado
summary(modelo2)

residuos_dharma <- simulateResiduals(modelo2)
par(mar = c(3, 3, 1, 1),   # margens: baixo, esquerda, cima, direita
    oma = c(0, 0, 0, 0))   # sem margem externa

plot(residuos_dharma)
testResiduals(residuos_dharma)




testDispersion(modelo2)
testOutliers(modelo2)


#---------------------------
#BOXPLOT
#------------------------


# Paleta de azuis interpolada — um tom por ambiente
ggplot(dados_sumarizados_v3, aes(x = Ambiente, y = GY_mediana)) +
  
  # Boxplot: mediana, quartis e outliers por ambiente
  # Outliers em azul escuro; shape = 1 = círculo vazio (menos poluído visualmente)
  geom_boxplot(aes(fill = Ambiente), outlier.colour = "#03045e", outlier.shape = 1) +
  
  # Jitter: pontos reais sobrepostos para mostrar densidade dos dados
  geom_jitter(width = 0.2, alpha = 0.3, size = 0.5, color = "#00b4d8") +
  
  # Gera a paleta dinamicamente com base no número de ambientes
  scale_fill_manual(values = colorRampPalette(
    c("#caf0f8", "#90e0ef", "#00b4d8", "#0077b6", "#48cae4")
  )(nlevels(factor(dados_sumarizados_v3$Ambiente)))) +
  
  # Tema limpo
  theme_bw() +
  
  # Rótulos do gráfico
  labs(
    title    = "Genótipos por Ambiente",
    subtitle = "Gamma identity | Mediana | GEN + GEN:Ambiente",
    x        = "Ambiente",
    y        = "Mediana de Produtividade"
  ) +
  
  theme(
    # 90° evita sobreposição; size = 5 reduz fonte para caber todos os ambientes
    axis.text.x     = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 5),
    legend.position = "none",
    plot.title      = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle   = element_text(hjust = 0.5, color = "gray40")
  )


  




#---------------------------
#mediana 
#passou no dharma
## Gamma - identity
#----------------------------
modelo2 <- glmmTMB(GY_mediana ~ Ambiente + (1|GEN) + (1|GEN:Ambiente), data = dados_sumarizados_v3,
                   family = Gamma(link = "identity"))

resultado1 <- ajustar_categorica(dados_sumarizados_v3, "Ambiente", modelo2)
modelo2 <- resultado1$modelo_atualizado
summary(modelo2)

residuos_dharma <- simulateResiduals(modelo2)
par(mar = c(3, 3, 1, 1),   # margens: baixo, esquerda, cima, direita
    oma = c(0, 0, 0, 0))   # sem margem externa

plot(residuos_dharma)
testResiduals(residuos_dharma)



testDispersion(modelo2)
testOutliers(modelo2)

#-------------------
#BOXPLOT
#-------------------


# Paleta de azuis interpolada — um tom por ambiente
ggplot(dados_sumarizados_v3, aes(x = Ambiente, y = GY_mediana)) +
  
  # Boxplot: mediana, quartis e outliers por ambiente
  # Outliers em azul escuro; shape = 1 = círculo vazio (menos poluído visualmente)
  geom_boxplot(aes(fill = Ambiente), outlier.colour = "#03045e", outlier.shape = 1) +
  
  # Jitter: pontos reais sobrepostos para mostrar densidade dos dados
  geom_jitter(width = 0.2, alpha = 0.3, size = 0.5, color = "#00b4d8") +
  
  # Gera a paleta dinamicamente com base no número de ambientes
  scale_fill_manual(values = colorRampPalette(
    c("#caf0f8", "#90e0ef", "#00b4d8", "#0077b6", "#48cae4")
  )(nlevels(factor(dados_sumarizados_v3$Ambiente)))) +
  
  # Tema limpo
  theme_bw() +
  
  # Rótulos do gráfico
  labs(
    title    = "Genótipos por Ambiente",
    subtitle = "Gamma identity | Mediana | GEN + GEN:Ambiente",
    x        = "Ambiente",
    y        = "Mediana de Produtividade"
  ) +
  
  theme(
    # 90° evita sobreposição; size = 5 reduz fonte para caber todos os ambientes
    axis.text.x     = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 5),
    legend.position = "none",
    plot.title      = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle   = element_text(hjust = 0.5, color = "gray40")
  )





#---------------------------
#media 
##PIOR MODELO
#Summary- Isso não significa NA nos dados — significa que o modelo não convergiu corretamente.
#Os NA no AIC/BIC/logLik são consequência disso.
## Gamma - identity
#----------------------------

modelo2 <- glmmTMB(GY_media ~ Ambiente + (1|GEN) + (1|GEN:Ambiente), data = dados_sumarizados_v3,
                   family = Gamma(link = "identity"))

resultado1 <- ajustar_categorica(dados_sumarizados_v3, "Ambiente", modelo2)
modelo2 <- resultado1$modelo_atualizado
summary(modelo2)


residuos_dharma <- simulateResiduals(modelo2)
par(mar = c(3, 3, 1, 1),   # margens: baixo, esquerda, cima, direita
    oma = c(0, 0, 0, 0))   # sem margem externa

plot(residuos_dharma)
testResiduals(residuos_dharma)

testDispersion(modelo2)
testOutliers(modelo2)


#--------------
#BOXPLOT
#---------------

# Paleta de azuis interpolada — um tom por ambiente
ggplot(dados_sumarizados_v3, aes(x = Ambiente, y = GY_media)) +
  
  # Boxplot: mediana, quartis e outliers por ambiente
  # Outliers em azul escuro; shape = 1 = círculo vazio (menos poluído visualmente)
  geom_boxplot(aes(fill = Ambiente), outlier.colour = "#03045e", outlier.shape = 1) +
  
  # Jitter: pontos reais sobrepostos para mostrar densidade dos dados
  geom_jitter(width = 0.2, alpha = 0.3, size = 0.5, color = "#00b4d8") +
  
  # Gera a paleta dinamicamente com base no número de ambientes
  scale_fill_manual(values = colorRampPalette(
    c("#caf0f8", "#90e0ef", "#00b4d8", "#0077b6", "#48cae4")
  )(nlevels(factor(dados_sumarizados_v3$Ambiente)))) +
  
  # Tema limpo
  theme_bw() +
  
  # Rótulos do gráfico
  labs(
    title    = "Variação de Genótipos por Ambiente",
    subtitle = "Gamma identity | Média | GEN + GEN:Ambiente",
    x        = "Ambiente",
    y        = "Produtividade Média (GY_media)"
  ) +
  
  theme(
    # 90° evita sobreposição; size = 5 reduz fonte para caber todos os ambientes
    axis.text.x     = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 5),
    legend.position = "none",
    plot.title      = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle   = element_text(hjust = 0.5, color = "gray40")
  )





# ═══════════════════════════════════════════════════════════════════
#          RANKINGS — Gaussian log | GEN + GEN:Ambiente
# ═══════════════════════════════════════════════════════════════════

cores_azul <- colorRampPalette(c("#caf0f8", "#90e0ef", "#00b4d8", "#0077b6", "#48cae4"))(7)

# ── 1. Ranking de Genótipos ───────────────────────────────────────
ranking_gen <- dados_sumarizados_v3 %>%
  mutate(Predict = predict(modelo2, type = "response")) %>%
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
    subtitle = "Média das predições com IC 95% — Gaussian log | GEN + GEN:Ambiente",
    x = "Genótipo",
    y = "GY (kg/ha)"
  ) +
  theme_bw(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(color = "gray40", hjust = 0.5)
  )
print(p1)


# ── 2. Top 20 Ambientes ───────────────────────────────────────────
ranking_amb <- dados_sumarizados_v3 %>%
  mutate(Predict = predict(modelo2, type = "response")) %>%
  group_by(Ambiente) %>%
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

p2 <- ranking_amb %>%
  slice_max(X_barra, n = 20) %>%
  ggplot(aes(x = reorder(Ambiente, X_barra), y = X_barra)) +
  geom_col(aes(fill = X_barra), show.legend = FALSE) +
  geom_errorbar(aes(ymin = limite_inferior, ymax = limite_superior),
                width = 0.3, color = "black", linewidth = 0.7) +
  geom_text(aes(label = paste0("#", Rank)), hjust = -0.3, size = 3.5, fontface = "bold", color = "#48cae4") +
  scale_fill_gradient(low = "#caf0f8", high = "#0077b6") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  coord_flip() +
  labs(
    title    = "Top 20 Ambientes por Produtividade",
    subtitle = "Média das predições com IC 95% — Gaussian log | GEN + GEN:Ambiente",
    x = "Ambiente",
    y = "GY (kg/ha)"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(color = "gray40", hjust = 0.5)
  )
print(p2)


# ── 3. Boxplot por Genótipo ───────────────────────────────────────
p3 <- dados_sumarizados_v3 %>%
  mutate(Predict = predict(modelo2, type = "response"),
         GEN     = reorder(GEN, Predict, median)) %>%
  ggplot(aes(x = GEN, y = Predict, fill = GEN)) +
  geom_boxplot(show.legend = FALSE, outlier.color = "#48cae4",
               outlier.shape = 18, outlier.size = 2) +
  geom_jitter(width = 0.2, alpha = 0.3, size = 1.5, color = "#0077b6") +
  scale_fill_manual(values = cores_azul) +
  coord_flip() +
  labs(
    title    = "Distribuição de Produtividade por Genótipo",
    subtitle = "Ordenado pela mediana das predições — Gaussian log | GEN + GEN:Ambiente",
    x = "Genótipo",
    y = "GY (kg/ha)"
  ) +
  theme_bw(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(color = "gray40", hjust = 0.5)
  )
print(p3)


# ── 4. Ranking de Genótipos por Ambiente (bônus — aproveita GEN:Amb) ──
ranking_gen_amb <- dados_sumarizados_v3 %>%
  mutate(Predict = predict(modelo2, type = "response")) %>%
  group_by(Ambiente, GEN) %>%
  summarise(
    X_barra = mean(Predict, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(Ambiente) %>%
  mutate(Rank = rank(desc(X_barra))) %>%
  arrange(Ambiente, Rank)

print(ranking_gen_amb)


#-----------------
#testar NA
#Não há nenhuma linha onde GY_sd é NA após filtrar n_rep > 1
#----------------


dados_sumarizados_v3 <- dados_sumarizados_v3 |>
  filter(n_rep > 1)

dados_sumarizados_v3 |>
  filter(is.na(GY_sd)) |>
  select(GEN, Ambiente, n_rep, GY_media, GY_mediana)

