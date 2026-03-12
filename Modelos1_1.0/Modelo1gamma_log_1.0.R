# =============================================================================
# ANÁLISE DE PRODUTIVIDADE — GLMMTMB GAMMA (LINK LOG)
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
# MODELO 1 — MÉDIA (Gamma Log)
# =============================================================================

mod_gamma_media <- glmmTMB(
  GY_media ~ Ambiente + (1 | GEN),
  data   = dados_sumarizados_v3,
  family = Gamma(link = "log")
)

resultado1      <- ajustar_categorica(dados_sumarizados_v3, "Ambiente", mod_gamma_media)
mod_gamma_media <- resultado1$modelo_atualizado

summary(mod_gamma_media)

# ── Diagnóstico de resíduos ───────────────────────────────────────────────────
residuos_dharma_media <- simulateResiduals(mod_gamma_media)

par(mar = c(1, 1, 1, 1), oma = c(0, 0, 0, 0))
plot(residuos_dharma_media)
testResiduals(residuos_dharma_media)
testDispersion(residuos_dharma_media)
testOutliers(residuos_dharma_media)

ranef(mod_gamma_media)


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
# MODELO 2 — MEDIANA (Gamma Log)
# =============================================================================

mod_gamma_mediana <- glmmTMB(
  GY_mediana ~ Ambiente + (1 | GEN),
  data   = dados_sumarizados_v3,
  family = Gamma(link = "log")
)

resultado2        <- ajustar_categorica(dados_sumarizados_v3, "Ambiente", mod_gamma_mediana)
mod_gamma_mediana <- resultado2$modelo_atualizado

summary(mod_gamma_mediana)

# ── Diagnóstico de resíduos ───────────────────────────────────────────────────
residuos_dharma_mediana <- simulateResiduals(mod_gamma_mediana)

par(mar = c(1, 1, 1, 1), oma = c(0, 0, 0, 0))
plot(residuos_dharma_mediana)
testResiduals(residuos_dharma_mediana)
testDispersion(residuos_dharma_mediana)
testOutliers(residuos_dharma_mediana)

ranef(mod_gamma_mediana)

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
# RANKINGS DE GENÓTIPOS (BLUPs)
# =============================================================================

rank_media <- ranef(mod_gamma_media)$cond$GEN %>%
  rownames_to_column("GEN") %>%
  rename(BLUP = `(Intercept)`) %>%
  arrange(desc(BLUP)) %>%
  mutate(
    Rank   = row_number(),
    Modelo = "Média"
  )

rank_mediana <- ranef(mod_gamma_mediana)$cond$GEN %>%
  rownames_to_column("GEN") %>%
  rename(BLUP = `(Intercept)`) %>%
  arrange(desc(BLUP)) %>%
  mutate(
    Rank   = row_number(),
    Modelo = "Mediana"
  )

# =============================================================================
# RANKINGS DE AMBIENTES (emmeans)
# — Com link log, emmeans retorna "response" + "asymp.LCL"/"asymp.UCL"
# =============================================================================

emm_raw_media <- as.data.frame(
  emmeans(mod_gamma_media, specs = ~Ambiente, type = "response")
)
names(emm_raw_media)[names(emm_raw_media) %in% c("response", "emmean")]     <- "emmean"
names(emm_raw_media)[names(emm_raw_media) %in% c("lower.CL", "asymp.LCL")] <- "lower.CL"
names(emm_raw_media)[names(emm_raw_media) %in% c("upper.CL", "asymp.UCL")] <- "upper.CL"

emm_media <- emm_raw_media %>%
  arrange(desc(emmean)) %>%
  mutate(Rank = row_number(), Modelo = "Média", Quartil = ntile(emmean, 4))


emm_raw_mediana <- as.data.frame(
  emmeans(mod_gamma_mediana, specs = ~Ambiente, type = "response")
)
names(emm_raw_mediana)[names(emm_raw_mediana) %in% c("response", "emmean")]     <- "emmean"
names(emm_raw_mediana)[names(emm_raw_mediana) %in% c("lower.CL", "asymp.LCL")] <- "lower.CL"
names(emm_raw_mediana)[names(emm_raw_mediana) %in% c("upper.CL", "asymp.UCL")] <- "upper.CL"

emm_mediana <- emm_raw_mediana %>%
  arrange(desc(emmean)) %>%
  mutate(Rank = row_number(), Modelo = "Mediana", Quartil = ntile(emmean, 4))


# =============================================================================
# PLOTS DE RANKING — GENÓTIPOS
# =============================================================================

hjust_blup <- function(blup) ifelse(blup > 0, -0.2, 1.2)

plot_genotipos_redgreen <- function(df, titulo) {
  ggplot(df, aes(x = reorder(GEN, BLUP), y = BLUP, fill = BLUP > 0)) +
    geom_col(width = 0.65) +
    geom_text(
      aes(label = paste0("#", Rank)),
      hjust = hjust_blup(df$BLUP),
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
      y        = "BLUP (escala log)",
      fill     = NULL
    ) +
    theme(
      legend.position = "bottom",
      plot.title      = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle   = element_text(hjust = 0.5)
    )
}

plot_genotipos_redgreen(rank_media,   "Ranqueamento dos Genótipos — Média Gamma Log")
plot_genotipos_redgreen(rank_mediana, "Ranqueamento dos Genótipos — Mediana Gamma Log")

