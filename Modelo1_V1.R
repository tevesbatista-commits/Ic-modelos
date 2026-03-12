library(tidyverse)
library(glmmTMB)
library(lme4)
library(DHARMa) 
library(tsoutliers)
library(ggplot2)

#Ambiente efeito fixo
# ── Função auxiliar ───────────────────────────────────────────────────────────
#TODOS OS ARQUIVOS ESTAO NA PASTA RESULTS
# INSTRUÇÃO: selecione o arquivo ajustar_categoricamiguel.R

source(file.choose())

# ── Carregar dados ────────────────────────────────────────────────────────────
# INSTRUÇÃO: selecione o arquivo dados_sumarizados_v3.csv
dados_sumarizados_v3 <- read.csv(file.choose(), header = TRUE, sep = ",")

#-----------------
#media  Tweedie
#passou
#-----------------

modelo1 <- glmmTMB(GY_media ~ Ambiente + (1|GEN),
                   data   = dados_sumarizados_v3,
                   family = tweedie(link = "log"))

resultado1      <- ajustar_categorica(dados_sumarizados_v3, "Ambiente", modelo1)
modelo1         <- resultado1$modelo_atualizado
summary(modelo1)

residuos_dharma <- simulateResiduals(modelo1)
par(mar = c(3, 3, 1, 1),   # margens: baixo, esquerda, cima, direita
    oma = c(0, 0, 0, 0))   # sem margem externa

plot(residuos_dharma)
testResiduals(residuos_dharma)


testDispersion(modelo1)
testOutliers(modelo1)


#---------------------
#BOXPLOT
#---------------------

# Cria uma paleta de azuis com um tom diferente para cada ambiente
# colorRampPalette interpola entre as cores fornecidas
cores_azul <- colorRampPalette(c("#caf0f8", "#90e0ef", "#00b4d8", "#0077b6", "#48cae4"))(
  length(unique(dados_sumarizados_v3$Ambiente)) # um tom por ambiente
)

ggplot(dados_sumarizados_v3, aes(x = Ambiente, y = GY_media, fill = Ambiente)) +
  
  # Boxplot: mostra mediana, quartis e outliers de produtividade por ambiente
  # Outliers em azul escuro para destacar observações extremas
  geom_boxplot(outlier.colour = "#03045e",
               outlier.shape = 21, outlier.size = 2) +
  
  # Jitter: plota os pontos reais sobre o boxplot
  # width = 0.2 evita sobreposição; alpha = 0.3 deixa semitransparente
  geom_jitter(width = 0.2, alpha = 0.3, size = 0.8, color = "gray30") +
  
  # Aplica a paleta de azuis criada acima a cada ambiente
  scale_fill_manual(values = cores_azul) +
  
  # Tema limpo com fonte tamanho 12
  theme_bw(base_size = 12) +
  
  # Rótulos do gráfico
  labs(
    title = "Genótipos por Ambiente",
    x     = "Ambiente",
    y     = "Produtividade Média (kg/ha)"
  ) +
  
  theme(
    # Rotaciona os nomes dos ambientes em 45° e diminui fonte para não sobrepor
    axis.text.x     = element_text(angle = 45, vjust = 1, hjust = 1, size = 5),
    # Remove legenda pois a cor já está identificada no eixo X
    legend.position = "none",
    # Título centralizado e em negrito
    plot.title      = element_text(hjust = 0.5, face = "bold")
  )






#-----------------
#mediana  Tweedie
#passou
#-----------------

modelo1 <- glmmTMB(GY_mediana ~ Ambiente + (1|GEN),
                   data   = dados_sumarizados_v3,
                   family = tweedie(link = "log"))

resultado1      <- ajustar_categorica(dados_sumarizados_v3, "Ambiente", modelo1)
modelo1         <- resultado1$modelo_atualizado
summary(modelo1)


residuos_dharma <- simulateResiduals(modelo1)
par(mar = c(3, 3, 1, 1),   # margens: baixo, esquerda, cima, direita
    oma = c(0, 0, 0, 0))   # sem margem externa

plot(residuos_dharma)
testResiduals(residuos_dharma)


testDispersion(modelo1)
testOutliers(modelo1)


#---------------------
#BOXPLOT
#---------------------


# Cria uma paleta de azuis com um tom diferente para cada ambiente
cores_azul <- colorRampPalette(c("#caf0f8", "#90e0ef", "#00b4d8", "#0077b6", "#48cae4"))(
  length(unique(dados_sumarizados_v3$Ambiente)) # um tom por ambiente
)

ggplot(dados_sumarizados_v3, aes(x = Ambiente, y = GY_mediana, fill = Ambiente)) +
  
  # Boxplot: mostra mediana, quartis e outliers da mediana de produtividade por ambiente
  # Outliers em azul escuro para destacar observações extremas
  geom_boxplot(outlier.colour = "#03045e",
               outlier.shape = 21, outlier.size = 2) +
  
  # Jitter: plota os pontos reais sobre o boxplot
  # width = 0.2 evita sobreposição; alpha = 0.3 deixa semitransparente
  geom_jitter(width = 0.2, alpha = 0.3, size = 0.8, color = "gray30") +
  
  # Aplica a paleta de azuis criada acima a cada ambiente
  scale_fill_manual(values = cores_azul) +
  
  # Tema limpo com fonte tamanho 12
  theme_bw(base_size = 12) +
  
  # Rótulos do gráfico
  labs(
    title = "Genótipos por Ambiente",
    x     = "Ambiente",
    y     = "Mediana de Produtividade (kg/ha)"
  ) +
  
  theme(
    # Rotaciona 45° e reduz fonte para os nomes dos ambientes não se sobreporem
    axis.text.x     = element_text(angle = 45, vjust = 1, hjust = 1, size = 5),
    # Remove legenda pois a cor já está identificada no eixo X
    legend.position = "none",
    # Título centralizado e em negrito
    plot.title      = element_text(hjust = 0.5, face = "bold")
  )








#_______________________________________________________________________________
#identity
#------------------
#modelo 1 media 
#------------------
modelo1 <- glmmTMB(GY_media ~ Ambiente + (1|GEN), 
                   data = dados_sumarizados_v3,
                   family = Gamma(link = "identity"))

resultado1 <- ajustar_categorica(dados_sumarizados_v3, "Ambiente", modelo1)

modelo1 <- resultado1$modelo_atualizado
summary(modelo1)

residuos_dharma <- simulateResiduals(modelo1)
par(mar = c(3, 3, 1, 1),   # margens: baixo, esquerda, cima, direita
    oma = c(0, 0, 0, 0))   # sem margem externa

plot(residuos_dharma)
testResiduals(residuos_dharma)




testDispersion(modelo1)
testOutliers(modelo1)





#---------------------
#BOXPLOT
#---------------------

# Cria uma paleta de azuis com um tom diferente para cada ambiente
cores_azul <- colorRampPalette(c("#caf0f8", "#90e0ef", "#00b4d8", "#0077b6", "#48cae4"))(
  length(unique(dados_sumarizados_v3$Ambiente)) # um tom por ambiente
)

ggplot(dados_sumarizados_v3, aes(x = Ambiente, y = GY_media, fill = Ambiente)) +
  
  # Boxplot: mostra mediana, quartis e outliers de produtividade por ambiente
  # Outliers em azul escuro para destacar observações extremas
  geom_boxplot(outlier.colour = "#03045e",
               outlier.shape = 21, outlier.size = 2) +
  
  # Jitter: plota os pontos reais sobre o boxplot
  # width = 0.2 evita sobreposição; alpha = 0.3 deixa semitransparente
  geom_jitter(width = 0.2, alpha = 0.3, size = 0.8, color = "gray30") +
  
  # Aplica a paleta de azuis criada acima a cada ambiente
  scale_fill_manual(values = cores_azul) +
  
  # Tema limpo com fonte tamanho 12
  theme_bw(base_size = 12) +
  
  # Rótulos do gráfico
  labs(
    title = "Variação de Genótipos por Ambiente (GY_media)",
    x     = "Ambiente",
    y     = "Produtividade Média (kg/ha)"
  ) +
  
  theme(
    # Rotaciona 45° e reduz fonte para os nomes dos ambientes não se sobreporem
    axis.text.x     = element_text(angle = 45, vjust = 1, hjust = 1, size = 5),
    # Remove legenda pois a cor já está identificada no eixo X
    legend.position = "none",
    # Título centralizado e em negrito
    plot.title      = element_text(hjust = 0.5, face = "bold")
  )






#------------------
#modelo 1 mediana
#------------------
modelo1 <- glmmTMB(GY_mediana ~ Ambiente + (1|GEN), 
                   data = dados_sumarizados_v3,
                   family = Gamma(link = "identity"))

resultado1 <- ajustar_categorica(dados_sumarizados_v3, "Ambiente", modelo1)

modelo1 <- resultado1$modelo_atualizado
summary(modelo1)
 
residuos_dharma <- simulateResiduals(modelo1)
par(mar = c(3, 3, 1, 1),   # margens: baixo, esquerda, cima, direita
    oma = c(0, 0, 0, 0))   # sem margem externa

plot(residuos_dharma)

testDispersion(modelo1)
testOutliers(modelo1)


#---------------------
#BOXPLOT
#---------------------


# Cria uma paleta de azuis com um tom diferente para cada ambiente
cores_azul <- colorRampPalette(c("#caf0f8", "#90e0ef", "#00b4d8", "#0077b6", "#48cae4"))(
  length(unique(dados_sumarizados_v3$Ambiente)) # um tom por ambiente
)

ggplot(dados_sumarizados_v3, aes(x = Ambiente, y = GY_mediana, fill = Ambiente)) +
  
  # Boxplot: mostra mediana, quartis e outliers da mediana de produtividade por ambiente
  # Outliers em azul escuro para destacar observações extremas
  geom_boxplot(outlier.colour = "#03045e",
               outlier.shape = 21, outlier.size = 2) +
  
  # Jitter: plota os pontos reais sobre o boxplot
  # width = 0.2 evita sobreposição; alpha = 0.3 deixa semitransparente
  geom_jitter(width = 0.2, alpha = 0.3, size = 0.8, color = "gray30") +
  
  # Aplica a paleta de azuis criada acima a cada ambiente
  scale_fill_manual(values = cores_azul) +
  
  # Tema limpo com fonte tamanho 12
  theme_bw(base_size = 12) +
  
  # Rótulos do gráfico
  labs(
    title = "Variação de Genótipos por Ambiente",
    x     = "Ambiente",
    y     = "Mediana de Produtividade (kg/ha)"
  ) +
  
  theme(
    # Rotaciona 45° e reduz fonte para os nomes dos ambientes não se sobreporem
    axis.text.x     = element_text(angle = 45, vjust = 1, hjust = 1, size = 5),
    # Remove legenda pois a cor já está identificada no eixo X
    legend.position = "none",
    # Título centralizado e em negrito
    plot.title      = element_text(hjust = 0.5, face = "bold")
  )



#_______________________________________________________________________________
#log gaussian
#------------------
#modelo 1 media 
#------------------
modelo1 <- glmmTMB(GY_media ~ Ambiente + (1|GEN), 
                   data = dados_sumarizados_v3,
                   family = gaussian())

resultado1 <- ajustar_categorica(dados_sumarizados_v3, "Ambiente", modelo1)

modelo1 <- resultado1$modelo_atualizado
summary(modelo1)

residuos_dharma <- simulateResiduals(modelo1)
par(mar = c(3, 3, 1, 1),   # margens: baixo, esquerda, cima, direita
    oma = c(0, 0, 0, 0))   # sem margem externa

plot(residuos_dharma)
testResiduals(residuos_dharma)




testDispersion(modelo1)
testOutliers(modelo1)



#---------------------
#BOXPLOT
#---------------------


# Cria uma paleta de azuis com um tom diferente para cada ambiente
cores_azul <- colorRampPalette(c("#caf0f8", "#90e0ef", "#00b4d8", "#0077b6", "#48cae4"))(
  length(unique(dados_sumarizados_v3$Ambiente)) # um tom por ambiente
)

ggplot(dados_sumarizados_v3, aes(x = Ambiente, y = GY_media, fill = Ambiente)) +
  
  # Boxplot: mostra mediana, quartis e outliers de produtividade por ambiente
  # Outliers em azul escuro para destacar observações extremas
  geom_boxplot(outlier.colour = "#03045e",
               outlier.shape = 21, outlier.size = 2) +
  
  # Jitter: plota os pontos reais sobre o boxplot
  # width = 0.2 evita sobreposição; alpha = 0.3 deixa semitransparente
  geom_jitter(width = 0.2, alpha = 0.3, size = 0.8, color = "gray30") +
  
  # Aplica a paleta de azuis criada acima a cada ambiente
  scale_fill_manual(values = cores_azul) +
  
  # Tema limpo com fonte tamanho 12
  theme_bw(base_size = 12) +
  
  # Rótulos do gráfico
  labs(
    title = "Genótipos por Ambiente",
    x     = "Ambiente",
    y     = "Produtividade Média (kg/ha)"
  ) +
  
  theme(
    # Rotaciona 45° e reduz fonte para os nomes dos ambientes não se sobreporem
    axis.text.x     = element_text(angle = 45, vjust = 1, hjust = 1, size = 5),
    # Remove legenda pois a cor já está identificada no eixo X
    legend.position = "none",
    # Título centralizado e em negrito
    plot.title      = element_text(hjust = 0.5, face = "bold")
  )



#------------------
#modelo 1 mediana
#------------------
modelo1 <- glmmTMB(GY_mediana ~ Ambiente + (1|GEN), 
                   data = dados_sumarizados_v3,
                   family = gaussian())

resultado1 <- ajustar_categorica(dados_sumarizados_v3, "Ambiente", modelo1)

modelo1 <- resultado1$modelo_atualizado
summary(modelo1)

residuos_dharma <- simulateResiduals(modelo1)
par(mar = c(3, 3, 1, 1),   # margens: baixo, esquerda, cima, direita
    oma = c(0, 0, 0, 0))   # sem margem externa

plot(residuos_dharma)



testDispersion(modelo1)
testOutliers(modelo1)



#---------------------
#BOXPLOT
#---------------------


# Cria uma paleta de azuis com um tom diferente para cada ambiente
cores_azul <- colorRampPalette(c("#caf0f8", "#90e0ef", "#00b4d8", "#0077b6", "#48cae4"))(
  length(unique(dados_sumarizados_v3$Ambiente)) # um tom por ambiente
)

ggplot(dados_sumarizados_v3, aes(x = Ambiente, y = GY_mediana, fill = Ambiente)) +
  
  # Boxplot: mostra mediana, quartis e outliers da mediana de produtividade por ambiente
  # Outliers em azul escuro para destacar observações extremas
  geom_boxplot(outlier.colour = "#03045e",
               outlier.shape = 21, outlier.size = 2) +
  
  # Jitter: plota os pontos reais sobre o boxplot
  # width = 0.2 evita sobreposição; alpha = 0.3 deixa semitransparente
  geom_jitter(width = 0.2, alpha = 0.3, size = 0.8, color = "gray30") +
  
  # Aplica a paleta de azuis criada acima a cada ambiente
  scale_fill_manual(values = cores_azul) +
  
  # Tema limpo com fonte tamanho 12
  theme_bw(base_size = 12) +
  
  # Rótulos do gráfico — título corrigido (estava faltando "Genótipos")
  labs(
    title = "Variação de Genótipos por Ambiente",
    x     = "Ambiente",
    y     = "Mediana de Produtividade (kg/ha)"
  ) +
  
  theme(
    # Rotaciona 45° e reduz fonte para os nomes dos ambientes não se sobreporem
    axis.text.x     = element_text(angle = 45, vjust = 1, hjust = 1, size = 5),
    # Remove legenda pois a cor já está identificada no eixo X
    legend.position = "none",
    # Título centralizado e em negrito
    plot.title      = element_text(hjust = 0.5, face = "bold")
  )



# ============================================================
# RANKINGS BASEADOS NAS PREDIÇÕES DO MODELO TWEEDIE (glmmTMB)
# ============================================================

# Usa o modelo ajustado para gerar os valores preditos na escala original (kg/ha)
# type = "response" desfaz o link log do Tweedie, voltando para a escala real
dados_sumarizados_v3$Predict <- predict(modelo1, type = "response")

# ── Ranking de Genótipos ──────────────────────────────────────
# Para cada genótipo, resume as predições em média, desvio padrão e IC 95%
# O IC 95% usa z = 1.96 (distribuição normal) — válido pois n é grande
# Ordena do mais produtivo para o menos produtivo e atribui posição no ranking
ranking_gen <- dados_sumarizados_v3 %>%
  group_by(GEN) %>%
  summarise(
    X_barra         = mean(Predict, na.rm = TRUE),   # média das predições do genótipo
    sigma           = sd(Predict,   na.rm = TRUE),   # variabilidade entre ambientes
    n               = n(),                            # quantos ambientes esse genótipo aparece
    erro_padrao     = sigma / sqrt(n),               # incerteza da média
    limite_inferior = X_barra - 1.96 * erro_padrao, # IC 95% inferior
    limite_superior = X_barra + 1.96 * erro_padrao, # IC 95% superior
    .groups         = "drop"
  ) %>%
  arrange(desc(X_barra)) %>%  # ordena do melhor para o pior
  mutate(Rank = row_number()) # atribui #1, #2, #3...

# ── Ranking de Ambientes ──────────────────────────────────────
# Mesma lógica do ranking de genótipos, mas agrupando por Ambiente
# Permite identificar quais locais/anos favorecem mais a produtividade
ranking_amb <- dados_sumarizados_v3 %>%
  group_by(Ambiente) %>%
  summarise(
    X_barra         = mean(Predict, na.rm = TRUE),
    sigma           = sd(Predict,   na.rm = TRUE),
    n               = n(),
    erro_padrao     = sigma / sqrt(n),
    limite_inferior = X_barra - 1.96 * erro_padrao,
    limite_superior = X_barra + 1.96 * erro_padrao,
    .groups         = "drop"
  ) %>%
  arrange(desc(X_barra)) %>%
  mutate(Rank = row_number())

# ── Ranking Genótipo × Ambiente ───────────────────────────────
# Cruza genótipo com ambiente: dentro de cada ambiente, qual genótipo foi melhor?
# Útil para identificar genótipos adaptados a ambientes específicos (interação GxE)
ranking_gen_amb <- dados_sumarizados_v3 %>%
  group_by(Ambiente, GEN) %>%
  summarise(
    X_barra = mean(Predict, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(Ambiente) %>%
  mutate(Rank = rank(desc(X_barra))) %>% # rank separado dentro de cada ambiente
  arrange(Ambiente, Rank)

# ── P1: Ranking de Genótipos ──────────────────────────────────
# Gráfico de barras horizontais ordenado pela média predita
# As barras de erro mostram o IC 95% — quanto menor, mais consistente o genótipo
# O rótulo (#1, #2...) facilita a leitura do ranking
p1 <- ggplot(ranking_gen, aes(x = reorder(GEN, X_barra), y = X_barra)) +
  geom_col(aes(fill = X_barra), show.legend = FALSE) +
  geom_errorbar(aes(ymin = limite_inferior, ymax = limite_superior),
                width = 0.3, color = "black", linewidth = 0.7) +
  geom_text(aes(label = paste0("#", Rank)), hjust = -0.3, size = 4, fontface = "bold") +
  scale_fill_gradient(low = "#caf0f8", high = "#0077b6") + # azul claro = baixo, escuro = alto
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  coord_flip() +
  labs(
    title    = "Ranking de Genótipos por Produtividade",
    subtitle = "Média das predições com IC 95% — Tweedie (Média)",
    x        = "Genótipo",
    y        = "Produtividade Predita (kg/ha)"
  ) +
  theme_bw(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(color = "gray40", hjust = 0.5)
  )
print(p1)

# ── P2: Top 20 Ambientes ──────────────────────────────────────
# Mostra apenas os 20 ambientes mais produtivos (slice_max)
# Evita poluição visual quando há muitos ambientes no dataset
p2 <- ranking_amb %>%
  slice_max(X_barra, n = 20) %>%
  ggplot(aes(x = reorder(Ambiente, X_barra), y = X_barra)) +
  geom_col(aes(fill = X_barra), show.legend = FALSE) +
  geom_errorbar(aes(ymin = limite_inferior, ymax = limite_superior),
                width = 0.3, color = "black", linewidth = 0.7) +
  geom_text(aes(label = paste0("#", Rank)), hjust = -0.3, size = 3.5, fontface = "bold") +
  scale_fill_gradient(low = "#caf0f8", high = "#0077b6") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  coord_flip() +
  labs(
    title    = "Top 20 Ambientes por Produtividade",
    subtitle = "Média das predições com IC 95% — Tweedie (Média)",
    x        = "Ambiente",
    y        = "Produtividade Predita (kg/ha)"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(color = "gray40", hjust = 0.5)
  )
print(p2)

# ── P3: Boxplot por Genótipo ──────────────────────────────────
# Diferente do P1 (média), aqui vemos a DISTRIBUIÇÃO completa das predições
# Cada ponto é um ambiente — permite ver consistência e outliers por genótipo
# Ordenado pela mediana: mais robusto que a média para distribuições assimétricas
cores_azul_gen <- colorRampPalette(c("#caf0f8", "#90e0ef", "#00b4d8", "#0077b6", "#03045e"))(
  length(unique(dados_sumarizados_v3$GEN))
)

p3 <- dados_sumarizados_v3 %>%
  mutate(GEN = reorder(GEN, Predict, median)) %>% # ordena pelo valor da mediana
  ggplot(aes(x = GEN, y = Predict, fill = GEN)) +
  geom_boxplot(show.legend = FALSE, outlier.color = "#48cae4",
               outlier.shape = 18, outlier.size = 2) +
  geom_jitter(width = 0.2, alpha = 0.3, size = 1.5, color = "#0077b6") + # pontos reais sobrepostos
  scale_fill_manual(values = cores_azul_gen) +
  coord_flip() +
  labs(
    title    = "Distribuição de Produtividade por Genótipo",
    subtitle = "Ordenado pela mediana das predições — Tweedie (Média)",
    x        = "Genótipo",
    y        = "Produtividade Predita (kg/ha)"
  ) +
  theme_bw(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(color = "gray40", hjust = 0.5)
  )
print(p3)



#-----------------
#testar NA
#Não há nenhuma linha onde GY_sd é NA após filtrar n_rep > 1
#----------------


dados_sumarizados_v3 <- dados_sumarizados_v3 |>
  filter(n_rep > 1)

dados_sumarizados_v3 |>
  filter(is.na(GY_sd)) |>
  select(GEN, Ambiente, n_rep, GY_media, GY_mediana)

