library(tidyverse)
library(here)

# =============================================================
# PRÉ-PROCESSAMENTO — V3
# Correção 1: filtro de ambientes (n >= 3), não de repetições
# Correção 2: slice_max só escolhe a data — repetições ficam
# Um experimento por local/ano por genótipo (data de plantio única)
# =============================================================

dados_raw <- read_csv(here("input", "dados_originais.csv"))

# -------------------------------------------------------------
# 1. Filtros iniciais
# -------------------------------------------------------------

dados <- dados_raw |>
  filter(TYPE == "VCU") |>
  filter(!(ST %in% c("AC", "AP", "RS", "PR", "AM", "RR", "AL"))) |>
  filter(!is.na(LOC), LOC != "") |>
  filter(!is.na(GY)) |>
  mutate(
    Year_Date    = as.integer(str_sub(DATE, 1, 4)),
    local_estado = paste(ST, LOC, sep = "-"),
    Ambiente     = paste(local_estado, Year_Date, sep = "-")
  ) |>
  filter(Year_Date >= 2013)

# -------------------------------------------------------------
# 2. Um experimento por local/ano por genótipo
# Se houver duas datas de plantio no mesmo local/ano,
# escolhe a mais recente e mantém todas as repetições dela
# -------------------------------------------------------------

data_escolhida <- dados |>
  distinct(GEN, Ambiente, DATE) |>
  group_by(GEN, Ambiente) |>
  slice_max(order_by = DATE, n = 1, with_ties = FALSE) |>
  ungroup() |>
  select(GEN, Ambiente, DATE)

dados <- dados |>
  semi_join(data_escolhida, by = c("GEN", "Ambiente", "DATE"))

# -------------------------------------------------------------
# 3. Filtro: genótipo presente em pelo menos 3 ambientes
# (repetições dentro do ambiente não têm restrição)
# -------------------------------------------------------------

ambientes_por_gen <- dados |>
  distinct(GEN, Ambiente) |>
  count(GEN, name = "n_ambientes")

gens_validos <- ambientes_por_gen |>
  filter(n_ambientes >= 3) |>
  pull(GEN)

dados_limpo <- dados |>
  filter(GEN %in% gens_validos)

# Checagem
cat("Genótipos antes do filtro:", n_distinct(dados$GEN), "\n")
cat("Genótipos após filtro:    ", n_distinct(dados_limpo$GEN), "\n")
cat("Observações finais:       ", nrow(dados_limpo), "\n")

# -------------------------------------------------------------
# 4. Sumarização por GEN x Ambiente
# Média das repetições dentro de cada local/ano
# -------------------------------------------------------------

dados_sumarizados <- dados_limpo |>
  group_by(GEN, Ambiente, local_estado, Year_Date) |>
  summarize(
    GY_media   = mean(GY,   na.rm = TRUE),
    GY_mediana = median(GY, na.rm = TRUE),
    GY_sd      = sd(GY,     na.rm = TRUE),
    n_rep      = n(),
    CV         = round((GY_sd / GY_media) * 100, 2),
    .groups    = "drop"
  ) |>
  filter(GY_media > 1500, GY_media < 7000)

# Checagem de NAs
cat("\nNAs por coluna:\n")
print(apply(is.na(dados_sumarizados), 2, sum))

cat("\nObservações sumarizadas:", nrow(dados_sumarizados), "\n")
cat("Genótipos:              ", n_distinct(dados_sumarizados$GEN), "\n")
cat("Ambientes únicos:       ", n_distinct(dados_sumarizados$Ambiente), "\n")

glimpse(dados_sumarizados)

# -------------------------------------------------------------
# 5. Salvar — versionado
# -------------------------------------------------------------

write_csv(dados_sumarizados, here("results", "dados_sumarizados_v3.csv"))
