# =============================================================================
# ANÁLISE DE PRODUTIVIDADE — GLMMTMB TWEEDIE
# Efeito fixo: Ambiente | Efeito aleatório: Genótipo
# =============================================================================

library(tidyverse)
library(glmmTMB)
library(lme4)
library(DHARMa)
library(tsoutliers)
library(ggplot2)
library(emmeans)

# ── Função auxiliar : clicar em ajustar_categorica_Miguel
source(file.choose())

# ── Carregar dados clicar em: dados_sumarizados_v3
dados_sumarizados_v3 <- read.csv(file.choose(), header = TRUE, sep = ",")


# =============================================================================
# MODELO 1 — MÉDIA (Tweedie)
# =============================================================================

mod_tweedie_media <- glmmTMB(
  GY_media ~ Ambiente + (1 | GEN),
  data   = dados_sumarizados_v3,
  family = tweedie(link = "log")
)

resultado1        <- ajustar_categorica(dados_sumarizados_v3, "Ambiente", mod_tweedie_media)
mod_tweedie_media <- resultado1$modelo_atualizado

summary(mod_tweedie_media)

# ── Diagnóstico de resíduos ───────────────────────────────────────────────────
residuos_dharma_media <- simulateResiduals(mod_tweedie_media)

par(mar = c(2, 2, 1, 1), oma = c(0, 0, 0, 0))
plot(residuos_dharma_media)
testResiduals(residuos_dharma_media)
testDispersion(residuos_dharma_media)
testOutliers(residuos_dharma_media)

ranef(mod_tweedie_media)

# ── Boxplot — Média ───────────────────────────────────────────────────────────
cores_azul_media <- colorRampPalette(
  c("#caf0f8", "#90e0ef", "#00b4d8", "#0077b6", "#48cae4")
)(length(unique(dados_sumarizados_v3$Ambiente)))

ggplot(dados_sumarizados_v3, aes(x = Ambiente, y = GY_media, fill = Ambiente)) +
  geom_boxplot(outlier.colour = "#03045e", outlier.shape = 21, outlier.size = 2) +
  geom_jitter(width = 0.2, alpha = 0.3, size = 0.8, color = "gray30") +
  scale_fill_manual(values = cores_azul_media) +
  theme_bw(base_size = 12) +
  labs(
    title = "Genótipos por Ambiente",
    x     = "Ambiente",
    y     = "Produtividade Média (kg/ha)"
  ) +
  theme(
    axis.text.x     = element_text(angle = 45, vjust = 1, hjust = 1, size = 5),
    legend.position = "none",
    plot.title      = element_text(hjust = 0.5, face = "bold")
  )


# =============================================================================
# MODELO 2 — MEDIANA (Tweedie)
# =============================================================================

mod_tweedie_mediana <- glmmTMB(
  GY_mediana ~ Ambiente + (1 | GEN),
  data   = dados_sumarizados_v3,
  family = tweedie(link = "log")
)

resultado2          <- ajustar_categorica(dados_sumarizados_v3, "Ambiente", mod_tweedie_mediana)
mod_tweedie_mediana <- resultado2$modelo_atualizado

summary(mod_tweedie_mediana)

# ── Diagnóstico de resíduos ───────────────────────────────────────────────────
residuos_dharma_mediana <- simulateResiduals(mod_tweedie_mediana)

par(mar = c(2, 2, 1, 1), oma = c(0, 0, 0, 0))
plot(residuos_dharma_mediana)
testResiduals(residuos_dharma_mediana)
testDispersion(residuos_dharma_mediana) 
testOutliers(residuos_dharma_mediana)  

ranef(mod_tweedie_mediana)

# ── Boxplot — Mediana ─────────────────────────────────────────────────────────
cores_azul_mediana <- colorRampPalette(
  c("#caf0f8", "#90e0ef", "#00b4d8", "#0077b6", "#48cae4")
)(length(unique(dados_sumarizados_v3$Ambiente)))

ggplot(dados_sumarizados_v3, aes(x = Ambiente, y = GY_mediana, fill = Ambiente)) +
  geom_boxplot(outlier.colour = "#03045e", outlier.shape = 21, outlier.size = 2) +
  geom_jitter(width = 0.2, alpha = 0.3, size = 0.8, color = "gray30") +
  scale_fill_manual(values = cores_azul_mediana) +
  theme_bw(base_size = 12) +
  labs(
    title = "Genótipos por Ambiente",
    x     = "Ambiente",
    y     = "Mediana de Produtividade (kg/ha)"
  ) +
  theme(
    axis.text.x     = element_text(angle = 45, vjust = 1, hjust = 1, size = 5),
    legend.position = "none",
    plot.title      = element_text(hjust = 0.5, face = "bold")
  )


# =============================================================================
# RANKINGS DE GENÓTIPOS (BLUPs) COM BLUP_PERC
# =============================================================================

# ── Interceptos gerais extraídos do modelo ────────────────────────────────────
media_geral_media   <- fixef(mod_tweedie_media)$cond["(Intercept)"]
media_geral_mediana <- fixef(mod_tweedie_mediana)$cond["(Intercept)"]

# ── Rank Genótipos — Média ────────────────────────────────────────────────────
rank_media <- ranef(mod_tweedie_media)$cond$GEN %>%
  rownames_to_column("GEN") %>%
  rename(BLUP = `(Intercept)`) %>%
  mutate(
    # FÓRMULA EXATA SOLICITADA
    BLUP_Perc = ((media_geral_media + BLUP) / media_geral_media - 1) * 100
  ) %>%
  arrange(desc(BLUP_Perc)) %>%
  mutate(
    Rank   = row_number(),
    Modelo = "Média"
  )

# ── Rank Genótipos — Mediana ──────────────────────────────────────────────────
rank_mediana <- ranef(mod_tweedie_mediana)$cond$GEN %>%
  rownames_to_column("GEN") %>%
  rename(BLUP = `(Intercept)`) %>%
  mutate(
    # FÓRMULA EXATA SOLICITADA
    BLUP_Perc = ((media_geral_mediana + BLUP) / media_geral_mediana - 1) * 100
  ) %>%
  arrange(desc(BLUP_Perc)) %>%
  mutate(
    Rank   = row_number(),
    Modelo = "Mediana"
  )


# =============================================================================
# RANKINGS DE AMBIENTES (emmeans)
# =============================================================================

# ── Rank Ambientes — Média ────────────────────────────────────────────────────
emm_media <- as.data.frame(
  emmeans(mod_tweedie_media, specs = ~Ambiente, type = "response")
) %>%
  arrange(desc(response)) %>%
  mutate(
    Rank    = row_number(),
    Modelo  = "Média",
    Quartil = ntile(response, 4)
  )

# ── Rank Ambientes — Mediana ──────────────────────────────────────────────────
emm_mediana <- as.data.frame(
  emmeans(mod_tweedie_mediana, specs = ~Ambiente, type = "response")
) %>%
  arrange(desc(response)) %>%
  mutate(
    Rank    = row_number(),
    Modelo  = "Mediana",
    Quartil = ntile(response, 4)
  )


# =============================================================================
# PLOTS DE RANKING — GENÓTIPOS (BLUP_Perc)
# =============================================================================

# Função auxiliar para ajuste do texto
hjust_blup <- function(blup) ifelse(blup > 0, -0.2, 1.2)

# ── Plot Genótipos (Ajustado para Verde e Vermelho) ───────────────────────────
plot_genotipos_redgreen <- function(df, titulo) {
  ggplot(df, aes(x = reorder(GEN, BLUP_Perc), y = BLUP_Perc, fill = BLUP_Perc > 0)) +
    geom_col(width = 0.65) +
    geom_text(
      aes(label = paste0("#", Rank)),
      hjust = hjust_blup(df$BLUP_Perc),
      size  = 3.5
    ) +
    scale_fill_manual(
      values = c("FALSE" = "#bc4749", "TRUE" = "#386641"), 
      labels = c("FALSE" = "Abaixo da média", "TRUE" = "Acima da média")
    ) +
    coord_flip() +
    theme_bw(base_size = 12) +
    labs(
      title    = titulo,
      subtitle = "BLUPs do efeito aleatório (1|GEN)",
      x        = NULL,
      y        = "Ganho Genético (%)",
      fill     = NULL
    ) +
    theme(
      legend.position = "bottom",
      plot.title      = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle   = element_text(hjust = 0.5)
    )
}

# Gerar plots de Genótipos
plot_genotipos_redgreen(rank_media, "Ranqueamento dos Genótipos — Média Tweedie")
plot_genotipos_redgreen(rank_mediana, "Ranqueamento dos Genótipos — Mediana Tweedie")


# =============================================================================
# HEATMAP — RANK COMPARATIVO MÉDIA vs MEDIANA (TWEEDIE)
# =============================================================================

rank_comparativo <- bind_rows(rank_media, rank_mediana) %>%
  select(GEN, Modelo, BLUP_Perc)

ggplot(rank_comparativo, aes(x = Modelo, y = reorder(GEN, BLUP_Perc), fill = BLUP_Perc)) +
  geom_tile(color = "white", linewidth = 0.4) +
  geom_text(aes(label = round(BLUP_Perc, 1)), size = 3) +
  scale_fill_gradient2(
    low      = "#bc4749",
    mid      = "white",
    high     = "#386641",
    midpoint = 0,
    name     = "Ganho (%)"
  ) +
  theme_bw(base_size = 12) +
  labs(
    title = "Heatmap — Ganho Genético (%) por Genótipo (Tweedie)",
    x     = "Modelo",
    y     = "Genótipo"
  ) +
  theme(
    plot.title  = element_text(hjust = 0.5, face = "bold"),
    axis.text.y = element_text(size = 8)
  )

