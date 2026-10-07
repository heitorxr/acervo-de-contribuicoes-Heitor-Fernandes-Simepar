# ============================================================
# Ajuste empirico das leituras da estacao ET contra a estacao de verificacao
#
# Uso:
#   source("analise_calibracao_empirica.R")
#
# Este script calcula ajustes lineares empiricos para temperatura de bulbo
# seco e umidade relativa. Tambem avalia:
#   1) ajuste linear global no conjunto inteiro;
#   2) validacao temporal 70/30, treinando nos primeiros 70% e testando nos
#      ultimos 30%;
#   3) estabilidade dos coeficientes por janelas temporais de 3 dias.
#
# Observacao metodologica: estes ajustes sao empiricos e dependem do
# conjunto de dados atual. Eles nao substituem calibracao laboratorial.
# ============================================================

pasta_dados <- "dados/entrada"
pasta_resultados <- file.path("resultados", "calibracao_empirica")
pasta_tabelas <- file.path(pasta_resultados, "tabelas")
pasta_graficos <- file.path(pasta_resultados, "graficos")
pasta_resumos <- file.path(pasta_resultados, "resumos")

dir.create(pasta_tabelas, recursive = TRUE, showWarnings = FALSE)
dir.create(pasta_graficos, recursive = TRUE, showWarnings = FALSE)
dir.create(pasta_resumos, recursive = TRUE, showWarnings = FALSE)

arquivo_et <- file.path(pasta_dados, "Dados Sensor ET - Página1.csv")
arquivo_simepar <- file.path(pasta_dados, "dados_simepar_25264916.csv")

saida_pareada <- file.path(pasta_tabelas, "dados_calibracao_empirica_pareados.csv")
saida_coeficientes <- file.path(pasta_tabelas, "coeficientes_calibracao_empirica.csv")
saida_metricas <- file.path(pasta_tabelas, "metricas_calibracao_empirica.csv")
saida_7030 <- file.path(pasta_tabelas, "validacao_temporal_70_30.csv")
saida_janelas <- file.path(pasta_tabelas, "coeficientes_janelas_3dias.csv")
saida_resumo <- file.path(pasta_resumos, "resumo_calibracao_empirica.txt")
saida_grafico_temp <- file.path(pasta_graficos, "grafico_calibracao_temperatura.png")
saida_grafico_umidade <- file.path(pasta_graficos, "grafico_calibracao_umidade.png")
saida_grafico_ibutg <- file.path(pasta_graficos, "grafico_calibracao_ibutg.png")
saida_grafico_disp_temp <- file.path(pasta_graficos, "grafico_dispersao_calibracao_temperatura.png")
saida_grafico_disp_umidade <- file.path(pasta_graficos, "grafico_dispersao_calibracao_umidade.png")
saida_grafico_janelas <- file.path(pasta_graficos, "grafico_coeficientes_janelas_3dias.png")

# ---------- Funcoes auxiliares ----------
converter_numero <- function(x) {
  x <- trimws(as.character(x))
  x[x == ""] <- NA
  tem_virgula <- grepl(",", x, fixed = TRUE)
  x[tem_virgula] <- gsub("\\.", "", x[tem_virgula])
  x[tem_virgula] <- gsub(",", ".", x[tem_virgula], fixed = TRUE)
  as.numeric(x)
}

alinhar_15min <- function(data_hora) {
  as.POSIXct(
    floor(as.numeric(data_hora) / 900) * 900,
    origin = "1970-01-01",
    tz = "America/Sao_Paulo"
  )
}

fmt <- function(x, casas = 4) {
  format(round(x, casas), decimal.mark = ",", nsmall = casas, trim = TRUE)
}

stull_bulbo_umido <- function(T, RH) {
  RH <- pmax(pmin(RH, 100), 0)
  T * atan(0.151977 * sqrt(RH + 8.313659)) +
    atan(T + RH) - atan(RH - 1.676331) +
    0.00391838 * RH^(3 / 2) * atan(0.023101 * RH) -
    4.686035
}

calcular_ibutg <- function(tbs, ru, tg) {
  tbn <- stull_bulbo_umido(tbs, ru)
  0.7 * tbn + 0.2 * tg + 0.1 * tbs
}

calcular_metricas <- function(estimado, referencia) {
  ok <- !is.na(estimado) & !is.na(referencia)
  estimado <- estimado[ok]
  referencia <- referencia[ok]
  diferenca <- estimado - referencia
  data.frame(
    n = length(estimado),
    vies = mean(diferenca),
    mae = mean(abs(diferenca)),
    rmse = sqrt(mean(diferenca^2)),
    max_abs = max(abs(diferenca)),
    pearson = cor(estimado, referencia, method = "pearson"),
    r2 = cor(estimado, referencia, method = "pearson")^2
  )
}

ajustar_linear <- function(bruto, referencia) {
  modelo <- lm(referencia ~ bruto)
  intercepto <- unname(coef(modelo)[1])
  inclinacao <- unname(coef(modelo)[2])
  list(
    intercepto = intercepto,
    inclinacao = inclinacao,
    ajustado = intercepto + inclinacao * bruto,
    formula = sprintf("%0.6f %+0.6f*x", intercepto, inclinacao)
  )
}

aplicar_linear <- function(x, ajuste, limitar_0_100 = FALSE) {
  y <- ajuste$intercepto + ajuste$inclinacao * x
  if (limitar_0_100) y <- pmax(pmin(y, 100), 0)
  y
}

metricas_variavel <- function(nome_variavel, unidade, referencia, bruto, ajuste_global, ajuste_treino70) {
  m_bruto <- calcular_metricas(bruto, referencia)
  m_global <- calcular_metricas(ajuste_global, referencia)
  m_treino70 <- calcular_metricas(ajuste_treino70, referencia)

  rbind(
    cbind(variavel = nome_variavel, unidade = unidade, metodo = "bruto", m_bruto),
    cbind(variavel = nome_variavel, unidade = unidade, metodo = "ajuste_linear_global", m_global),
    cbind(variavel = nome_variavel, unidade = unidade, metodo = "ajuste_linear_treino70", m_treino70)
  )
}

metricas_por_subconjunto <- function(nome_variavel, unidade, conjunto, referencia, bruto, ajuste_treino70) {
  m_bruto <- calcular_metricas(bruto, referencia)
  m_ajuste <- calcular_metricas(ajuste_treino70, referencia)
  rbind(
    cbind(variavel = nome_variavel, unidade = unidade, conjunto = conjunto, metodo = "bruto", m_bruto),
    cbind(variavel = nome_variavel, unidade = unidade, conjunto = conjunto, metodo = "ajuste_linear_treino70", m_ajuste)
  )
}

plotar_series <- function(arquivo, tempo, referencia, bruto, ajuste_global, ajuste_treino70, ylab, titulo, legenda_ref) {
  png(arquivo, width = 1500, height = 850)
  plot(tempo, referencia,
       type = "l", col = "black", lwd = 2,
       xlab = "Data/hora alinhada", ylab = ylab,
       main = titulo)
  lines(tempo, bruto, col = "red", lwd = 1.7)
  lines(tempo, ajuste_global, col = "darkgreen", lwd = 1.5)
  lines(tempo, ajuste_treino70, col = "blue", lwd = 1.5, lty = 2)
  legend("topright",
         legend = c(legenda_ref, "ET bruto", "ET ajuste linear global", "ET ajuste linear 70/30"),
         col = c("black", "red", "darkgreen", "blue"),
         lwd = c(2, 1.7, 1.5, 1.5), lty = c(1, 1, 1, 2), bty = "n")
  grid()
  dev.off()
}

plotar_dispersao <- function(arquivo, referencia, bruto, ajuste_global, ajuste_treino70, xlab, ylab, titulo) {
  limites <- range(c(referencia, bruto, ajuste_global, ajuste_treino70), na.rm = TRUE)
  png(arquivo, width = 1000, height = 900)
  plot(referencia, bruto,
       pch = 19, col = rgb(1, 0, 0, 0.35),
       xlab = xlab, ylab = ylab, main = titulo,
       xlim = limites, ylim = limites)
  points(referencia, ajuste_global, pch = 19, col = rgb(0, 0.45, 0, 0.30))
  points(referencia, ajuste_treino70, pch = 19, col = rgb(0, 0, 1, 0.28))
  abline(0, 1, lwd = 2, lty = 2, col = "black")
  legend("topleft",
         legend = c("ET bruto", "ET ajuste linear global", "ET ajuste linear 70/30", "linha 1:1"),
         col = c("red", "darkgreen", "blue", "black"),
         pch = c(19, 19, 19, NA), lty = c(NA, NA, NA, 2), lwd = c(NA, NA, NA, 2), bty = "n")
  grid()
  dev.off()
}

# ---------- Leitura ----------
if (!file.exists(arquivo_et)) stop("Arquivo ET nao encontrado: ", arquivo_et)
if (!file.exists(arquivo_simepar)) stop("Arquivo Simepar nao encontrado: ", arquivo_simepar)

et <- read.csv(arquivo_et, stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8-BOM")
simepar <- read.csv(arquivo_simepar, stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8-BOM")

colunas_et <- c("Data e Horário", "RU", "Bulbo Seco", "globo", "IBUTG")
colunas_simepar <- c("datahora", "temperatura", "umidade")
faltantes_et <- setdiff(colunas_et, names(et))
faltantes_simepar <- setdiff(colunas_simepar, names(simepar))
if (length(faltantes_et) > 0) stop("Colunas faltando no ET: ", paste(faltantes_et, collapse = ", "))
if (length(faltantes_simepar) > 0) stop("Colunas faltando no Simepar: ", paste(faltantes_simepar, collapse = ", "))

# ---------- Preparacao ----------
et$data_hora_et <- as.POSIXct(et[["Data e Horário"]], format = "%d/%m/%Y %H:%M:%S", tz = "America/Sao_Paulo")
et$horario_alinhado <- alinhar_15min(et$data_hora_et)
et$status <- if ("Código de Erro (ok)" %in% names(et)) et[["Código de Erro (ok)"]] else NA
et$RU_ET <- converter_numero(et[["RU"]])
et$bulbo_seco_ET <- converter_numero(et[["Bulbo Seco"]])
et$globo_ET <- converter_numero(et[["globo"]])
et$IBUTG_ET <- converter_numero(et[["IBUTG"]])

simepar$horario_alinhado <- as.POSIXct(substr(simepar$datahora, 1, 19), format = "%Y-%m-%d %H:%M:%S", tz = "America/Sao_Paulo")
simepar$temperatura_simepar <- converter_numero(simepar$temperatura)
simepar$umidade_simepar <- converter_numero(simepar$umidade)

# Uma unica observacao ambiental e mantida por slot nominal. Duplicidades do
# arquivo bruto sao diagnosticadas em analise_transmissao_lora.R, mas nao
# recebem peso adicional no ajuste empirico.
et2 <- et[order(et$data_hora_et), c("data_hora_et", "horario_alinhado", "status", "RU_ET", "bulbo_seco_ET", "globo_ET", "IBUTG_ET")]
et2 <- et2[!duplicated(et2$horario_alinhado), ]
simepar2 <- simepar[, c("horario_alinhado", "temperatura_simepar", "umidade_simepar")]

dados <- merge(et2, simepar2, by = "horario_alinhado", all.x = TRUE, sort = TRUE)
dados <- dados[complete.cases(dados[, c("RU_ET", "bulbo_seco_ET", "globo_ET", "temperatura_simepar", "umidade_simepar")]), ]
dados <- dados[order(dados$horario_alinhado, dados$data_hora_et), ]

# ---------- Ajuste linear global ----------
ajuste_temp_global <- ajustar_linear(dados$bulbo_seco_ET, dados$temperatura_simepar)
ajuste_umidade_global <- ajustar_linear(dados$RU_ET, dados$umidade_simepar)

dados$bulbo_seco_ET_ajuste_linear_global <- aplicar_linear(dados$bulbo_seco_ET, ajuste_temp_global)
dados$RU_ET_ajuste_linear_global <- aplicar_linear(dados$RU_ET, ajuste_umidade_global, limitar_0_100 = TRUE)

# ---------- Validacao temporal 70/30 ----------
n_total <- nrow(dados)
n_treino <- floor(0.70 * n_total)
dados$conjunto_70_30 <- ifelse(seq_len(n_total) <= n_treino, "treino_70", "teste_30")
treino <- dados[dados$conjunto_70_30 == "treino_70", ]
teste <- dados[dados$conjunto_70_30 == "teste_30", ]

ajuste_temp_70 <- ajustar_linear(treino$bulbo_seco_ET, treino$temperatura_simepar)
ajuste_umidade_70 <- ajustar_linear(treino$RU_ET, treino$umidade_simepar)

dados$bulbo_seco_ET_ajuste_linear_treino70 <- aplicar_linear(dados$bulbo_seco_ET, ajuste_temp_70)
dados$RU_ET_ajuste_linear_treino70 <- aplicar_linear(dados$RU_ET, ajuste_umidade_70, limitar_0_100 = TRUE)

# Alvo de IBUTG para avaliacao: temperatura/umidade da estacao de verificacao + globo ET.
dados$IBUTG_referencia_simepar_TRH <- calcular_ibutg(dados$temperatura_simepar, dados$umidade_simepar, dados$globo_ET)
dados$IBUTG_recalculado_ET <- calcular_ibutg(dados$bulbo_seco_ET, dados$RU_ET, dados$globo_ET)
dados$IBUTG_ajuste_linear_global <- calcular_ibutg(dados$bulbo_seco_ET_ajuste_linear_global, dados$RU_ET_ajuste_linear_global, dados$globo_ET)
dados$IBUTG_ajuste_linear_treino70 <- calcular_ibutg(dados$bulbo_seco_ET_ajuste_linear_treino70, dados$RU_ET_ajuste_linear_treino70, dados$globo_ET)

coeficientes <- data.frame(
  variavel = c("temperatura_bulbo_seco", "umidade_relativa", "temperatura_bulbo_seco", "umidade_relativa"),
  referencia = c("temperatura_simepar", "umidade_simepar", "temperatura_simepar", "umidade_simepar"),
  ajuste = c("linear_global", "linear_global", "linear_treino70", "linear_treino70"),
  n_ajuste = c(nrow(dados), nrow(dados), nrow(treino), nrow(treino)),
  intercepto_linear = c(ajuste_temp_global$intercepto, ajuste_umidade_global$intercepto, ajuste_temp_70$intercepto, ajuste_umidade_70$intercepto),
  inclinacao_linear = c(ajuste_temp_global$inclinacao, ajuste_umidade_global$inclinacao, ajuste_temp_70$inclinacao, ajuste_umidade_70$inclinacao),
  formula_ajuste_linear = c(ajuste_temp_global$formula, ajuste_umidade_global$formula, ajuste_temp_70$formula, ajuste_umidade_70$formula),
  stringsAsFactors = FALSE
)

metricas <- rbind(
  metricas_variavel(
    "temperatura_bulbo_seco", "celsius", dados$temperatura_simepar,
    dados$bulbo_seco_ET, dados$bulbo_seco_ET_ajuste_linear_global, dados$bulbo_seco_ET_ajuste_linear_treino70
  ),
  metricas_variavel(
    "umidade_relativa", "pontos_percentuais", dados$umidade_simepar,
    dados$RU_ET, dados$RU_ET_ajuste_linear_global, dados$RU_ET_ajuste_linear_treino70
  ),
  metricas_variavel(
    "IBUTG", "celsius", dados$IBUTG_referencia_simepar_TRH,
    dados$IBUTG_recalculado_ET, dados$IBUTG_ajuste_linear_global, dados$IBUTG_ajuste_linear_treino70
  )
)

# Metricas 70/30 por subconjunto usando coeficientes treinados apenas no treino.
metricas_7030 <- rbind(
  metricas_por_subconjunto("temperatura_bulbo_seco", "celsius", "treino_70", treino$temperatura_simepar, treino$bulbo_seco_ET, dados$bulbo_seco_ET_ajuste_linear_treino70[dados$conjunto_70_30 == "treino_70"]),
  metricas_por_subconjunto("temperatura_bulbo_seco", "celsius", "teste_30", teste$temperatura_simepar, teste$bulbo_seco_ET, dados$bulbo_seco_ET_ajuste_linear_treino70[dados$conjunto_70_30 == "teste_30"]),
  metricas_por_subconjunto("umidade_relativa", "pontos_percentuais", "treino_70", treino$umidade_simepar, treino$RU_ET, dados$RU_ET_ajuste_linear_treino70[dados$conjunto_70_30 == "treino_70"]),
  metricas_por_subconjunto("umidade_relativa", "pontos_percentuais", "teste_30", teste$umidade_simepar, teste$RU_ET, dados$RU_ET_ajuste_linear_treino70[dados$conjunto_70_30 == "teste_30"]),
  metricas_por_subconjunto("IBUTG", "celsius", "treino_70", dados$IBUTG_referencia_simepar_TRH[dados$conjunto_70_30 == "treino_70"], dados$IBUTG_recalculado_ET[dados$conjunto_70_30 == "treino_70"], dados$IBUTG_ajuste_linear_treino70[dados$conjunto_70_30 == "treino_70"]),
  metricas_por_subconjunto("IBUTG", "celsius", "teste_30", dados$IBUTG_referencia_simepar_TRH[dados$conjunto_70_30 == "teste_30"], dados$IBUTG_recalculado_ET[dados$conjunto_70_30 == "teste_30"], dados$IBUTG_ajuste_linear_treino70[dados$conjunto_70_30 == "teste_30"])
)

# ---------- Coeficientes por janelas de 3 dias ----------
inicio <- as.POSIXct(format(min(dados$horario_alinhado), "%Y-%m-%d 00:00:00"), tz = "America/Sao_Paulo")
fim <- max(dados$horario_alinhado)
limites_janelas <- seq(from = inicio, to = fim + 3 * 24 * 3600, by = 3 * 24 * 3600)
lista_janelas <- list()

for (i in seq_len(length(limites_janelas) - 1)) {
  ini <- limites_janelas[i]
  fim_j <- limites_janelas[i + 1]
  sub <- dados[dados$horario_alinhado >= ini & dados$horario_alinhado < fim_j, ]
  if (nrow(sub) >= 20) {
    aj_t <- ajustar_linear(sub$bulbo_seco_ET, sub$temperatura_simepar)
    aj_u <- ajustar_linear(sub$RU_ET, sub$umidade_simepar)
    met_t <- calcular_metricas(aplicar_linear(sub$bulbo_seco_ET, aj_t), sub$temperatura_simepar)
    met_u <- calcular_metricas(aplicar_linear(sub$RU_ET, aj_u, limitar_0_100 = TRUE), sub$umidade_simepar)
    lista_janelas[[length(lista_janelas) + 1]] <- data.frame(
      janela_id = i,
      inicio = ini,
      fim = fim_j - 1,
      variavel = c("temperatura_bulbo_seco", "umidade_relativa"),
      n = c(nrow(sub), nrow(sub)),
      intercepto_linear = c(aj_t$intercepto, aj_u$intercepto),
      inclinacao_linear = c(aj_t$inclinacao, aj_u$inclinacao),
      mae_ajuste_linear = c(met_t$mae, met_u$mae),
      rmse_ajuste_linear = c(met_t$rmse, met_u$rmse),
      stringsAsFactors = FALSE
    )
  }
}
coeficientes_janelas <- do.call(rbind, lista_janelas)

# ---------- Saidas tabulares ----------
write.csv2(dados, saida_pareada, row.names = FALSE, fileEncoding = "UTF-8")
write.csv2(coeficientes, saida_coeficientes, row.names = FALSE, fileEncoding = "UTF-8")
write.csv2(metricas, saida_metricas, row.names = FALSE, fileEncoding = "UTF-8")
write.csv2(metricas_7030, saida_7030, row.names = FALSE, fileEncoding = "UTF-8")
write.csv2(coeficientes_janelas, saida_janelas, row.names = FALSE, fileEncoding = "UTF-8")

# ---------- Graficos ----------
plotar_series(
  saida_grafico_temp, dados$horario_alinhado, dados$temperatura_simepar,
  dados$bulbo_seco_ET, dados$bulbo_seco_ET_ajuste_linear_global, dados$bulbo_seco_ET_ajuste_linear_treino70,
  "Temperatura (°C)", "Ajuste empirico da temperatura de bulbo seco", "Estacao de verificacao"
)

plotar_series(
  saida_grafico_umidade, dados$horario_alinhado, dados$umidade_simepar,
  dados$RU_ET, dados$RU_ET_ajuste_linear_global, dados$RU_ET_ajuste_linear_treino70,
  "Umidade relativa (%)", "Ajuste empirico da umidade relativa", "Estacao de verificacao"
)

plotar_series(
  saida_grafico_ibutg, dados$horario_alinhado, dados$IBUTG_referencia_simepar_TRH,
  dados$IBUTG_recalculado_ET, dados$IBUTG_ajuste_linear_global, dados$IBUTG_ajuste_linear_treino70,
  "Indice termico estimado (°C)", "Efeito dos ajustes empiricos no indice estimado", "Estimativa com T/UR de verificacao + globo ET"
)

plotar_dispersao(
  saida_grafico_disp_temp, dados$temperatura_simepar,
  dados$bulbo_seco_ET, dados$bulbo_seco_ET_ajuste_linear_global, dados$bulbo_seco_ET_ajuste_linear_treino70,
  "Temperatura da estacao de verificacao (°C)", "Temperatura ET (°C)", "Temperatura: bruto e ajustes"
)

plotar_dispersao(
  saida_grafico_disp_umidade, dados$umidade_simepar,
  dados$RU_ET, dados$RU_ET_ajuste_linear_global, dados$RU_ET_ajuste_linear_treino70,
  "Umidade da estacao de verificacao (%)", "Umidade ET (%)", "Umidade: bruto e ajustes"
)

png(saida_grafico_janelas, width = 1400, height = 850)
par(mfrow = c(2, 2), mar = c(5, 5, 4, 2) + 0.1)
for (var in c("temperatura_bulbo_seco", "umidade_relativa")) {
  sub <- coeficientes_janelas[coeficientes_janelas$variavel == var, ]
  titulo_var <- ifelse(var == "temperatura_bulbo_seco", "Temperatura", "Umidade")
  plot(sub$janela_id, sub$intercepto_linear, type = "b", pch = 19, col = "darkgreen",
       xlab = "Janela de 3 dias", ylab = "Intercepto", main = paste(titulo_var, "- intercepto"))
  grid()
  plot(sub$janela_id, sub$inclinacao_linear, type = "b", pch = 19, col = "blue",
       xlab = "Janela de 3 dias", ylab = "Inclinação", main = paste(titulo_var, "- inclinação"))
  grid()
}
dev.off()

# ---------- Resumo organizado ----------
sink(saida_resumo, split = TRUE)
cat("\nResumo: ajuste empirico linear das leituras da estacao ET\n")
cat("==========================================================\n")
cat("Pares validos analisados: ", nrow(dados), "\n", sep = "")
cat("Periodo pareado: ", format(min(dados$horario_alinhado)), " a ", format(max(dados$horario_alinhado)), "\n", sep = "")
cat("Divisao temporal 70/30: ", nrow(treino), " pares no treino e ", nrow(teste), " pares no teste\n", sep = "")
cat("Data limite aproximada entre treino e teste: ", format(max(treino$horario_alinhado)), "\n\n", sep = "")

cat("1) Ajuste linear global calculado com todos os pares\n")
cat("----------------------------------------------------\n")
cat("Temperatura: Tbs_aj = ", ajuste_temp_global$formula, "\n", sep = "")
cat("Umidade:     RU_aj  = ", ajuste_umidade_global$formula, "\n", sep = "")
cat("Observacao: o ajuste por vies constante foi removido porque piorava ou melhorava muito pouco as metricas.\n\n")

cat("Metricas no conjunto inteiro: bruto vs ajuste linear global vs ajuste linear treinado nos 70% iniciais\n")
cat("------------------------------------------------------------------------------------------------\n")
print(metricas[, c("variavel", "metodo", "n", "vies", "mae", "rmse", "max_abs", "pearson", "r2")], row.names = FALSE)
cat("\n")

cat("2) Validacao temporal 70/30\n")
cat("---------------------------\n")
cat("Coeficientes treinados apenas nos primeiros 70% dos dados:\n")
cat("Temperatura: Tbs_aj = ", ajuste_temp_70$formula, "\n", sep = "")
cat("Umidade:     RU_aj  = ", ajuste_umidade_70$formula, "\n\n", sep = "")
cat("Metricas separadas em treino e teste usando esses coeficientes:\n")
print(metricas_7030[, c("variavel", "conjunto", "metodo", "n", "vies", "mae", "rmse", "max_abs", "pearson", "r2")], row.names = FALSE)
cat("\n")

cat("Leitura rapida do teste 30%:\n")
for (var in unique(metricas_7030$variavel)) {
  sub <- metricas_7030[metricas_7030$variavel == var & metricas_7030$conjunto == "teste_30", ]
  bruto <- sub[sub$metodo == "bruto", ]
  ajustado <- sub[sub$metodo == "ajuste_linear_treino70", ]
  cat("- ", var,
      ": MAE bruto=", fmt(bruto$mae, 3),
      ", MAE ajustado=", fmt(ajustado$mae, 3),
      "; RMSE bruto=", fmt(bruto$rmse, 3),
      ", RMSE ajustado=", fmt(ajustado$rmse, 3), "\n", sep = "")
}
cat("\n")

cat("3) Estabilidade dos coeficientes por janelas de 3 dias\n")
cat("------------------------------------------------------\n")
cat("Resumo das janelas calculadas:\n")
print(coeficientes_janelas[, c("janela_id", "inicio", "fim", "variavel", "n", "intercepto_linear", "inclinacao_linear", "mae_ajuste_linear", "rmse_ajuste_linear")], row.names = FALSE)
cat("\n")

cat("Amplitude dos coeficientes entre janelas:\n")
for (var in unique(coeficientes_janelas$variavel)) {
  sub <- coeficientes_janelas[coeficientes_janelas$variavel == var, ]
  cat("- ", var,
      ": intercepto min=", fmt(min(sub$intercepto_linear), 4),
      ", max=", fmt(max(sub$intercepto_linear), 4),
      "; inclinacao min=", fmt(min(sub$inclinacao_linear), 4),
      ", max=", fmt(max(sub$inclinacao_linear), 4), "\n", sep = "")
}
cat("\n")

cat("4) Interpretacao metodologica\n")
cat("-----------------------------\n")
cat("- O ajuste linear global mostra o melhor encaixe possivel para o conjunto atual, mas usa os mesmos dados para ajustar e avaliar.\n")
cat("- A validacao 70/30 testa se os coeficientes calculados nos primeiros 70% continuam ajudando nos 30% finais.\n")
cat("- As janelas de 3 dias mostram se intercepto e inclinacao sao estaveis ou se mudam conforme o periodo analisado.\n")
cat("- Os coeficientes sao especificos desta campanha e devem ser recalculados para outro conjunto ou periodo.\n")
cat("- Estes resultados devem ser tratados como ajuste empirico de pos-processamento, nao como calibracao laboratorial rastreavel.\n\n")

cat("Arquivos de saida:\n")
cat("- ", saida_pareada, "\n", sep = "")
cat("- ", saida_coeficientes, "\n", sep = "")
cat("- ", saida_metricas, "\n", sep = "")
cat("- ", saida_7030, "\n", sep = "")
cat("- ", saida_janelas, "\n", sep = "")
cat("- ", saida_resumo, "\n", sep = "")
cat("- ", saida_grafico_temp, "\n", sep = "")
cat("- ", saida_grafico_umidade, "\n", sep = "")
cat("- ", saida_grafico_ibutg, "\n", sep = "")
cat("- ", saida_grafico_disp_temp, "\n", sep = "")
cat("- ", saida_grafico_disp_umidade, "\n", sep = "")
cat("- ", saida_grafico_janelas, "\n", sep = "")

sink()
