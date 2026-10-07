# ============================================================
# Analise de sensibilidade do indice termico estimado com Stull
#
# Uso:
#   source("analise_ibutg_stull.R")
#
# Objetivos:
# 1) Recalcular o indice estimado usando a coluna de bulbo umido por Stull.
# 2) Quantificar o efeito de substituir T/UR ET por T/UR Simepar, mantendo o
#    mesmo globo ET em ambos os calculos.
# 3) Estimar a propagacao de diferencas de temperatura e umidade.
#
# Esta comparacao nao e validacao independente de IBUTG: os dois lados usam o
# mesmo globo e a mesma aproximacao psicrometrica de Stull, que nao mede bulbo
# umido natural.
# ============================================================

pasta_dados <- "dados/entrada"
pasta_resultados <- file.path("resultados", "ibutg_stull")
pasta_tabelas <- file.path(pasta_resultados, "tabelas")
pasta_graficos <- file.path(pasta_resultados, "graficos")
pasta_resumos <- file.path(pasta_resultados, "resumos")

dir.create(pasta_tabelas, recursive = TRUE, showWarnings = FALSE)
dir.create(pasta_graficos, recursive = TRUE, showWarnings = FALSE)
dir.create(pasta_resumos, recursive = TRUE, showWarnings = FALSE)

arquivo_et <- file.path(pasta_dados, "Dados Sensor ET - Página1.csv")
arquivo_simepar <- file.path(pasta_dados, "dados_simepar_25264916.csv")
# Resumos produzidos por outros scripts e usados para a propagacao de incerteza.
resumo_umidade <- file.path("resultados", "umidade", "resumos", "resumo_comparacao_umidade.txt")
resumo_temperatura <- file.path("resultados", "temperatura", "resumos", "resumo_comparacao_temperatura.txt")

saida_pareada <- file.path(pasta_tabelas, "comparacao_ibutg_stull.csv")
saida_intervalos <- file.path(pasta_tabelas, "intervalos_confianca_sensibilidade_ibutg_bootstrap_blocos.csv")
saida_resumo <- file.path(pasta_resumos, "resumo_ibutg_stull.txt")
saida_grafico_series <- file.path(pasta_graficos, "grafico_ibutg_stull_series.png")
saida_grafico_diferenca <- file.path(pasta_graficos, "grafico_diferenca_ibutg_stull.png")
saida_grafico_incerteza <- file.path(pasta_graficos, "grafico_incerteza_ibutg_stull.png")

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

fmt <- function(x, casas = 2) {
  format(round(x, casas), decimal.mark = ",", nsmall = casas, trim = TRUE)
}

extrair_metrica_txt <- function(arquivo, nome) {
  if (!file.exists(arquivo)) return(NA_real_)
  linhas <- readLines(arquivo, encoding = "UTF-8", warn = FALSE)
  linha <- grep(paste0("^- ", nome, ":"), linhas, value = TRUE)
  if (length(linha) == 0) return(NA_real_)
  valor <- sub(paste0("^- ", nome, ":\\s*"), "", linha[1])
  valor <- sub(" .*", "", valor)
  valor <- gsub(",", ".", valor, fixed = TRUE)
  as.numeric(valor)
}

# Formula de Stull (2011) para estimar temperatura de bulbo umido em graus Celsius.
# T: temperatura do ar em °C; RH: umidade relativa em %.
stull_bulbo_umido <- function(T, RH) {
  RH <- pmax(pmin(RH, 100), 0)
  T * atan(0.151977 * sqrt(RH + 8.313659)) +
    atan(T + RH) - atan(RH - 1.676331) +
    0.00391838 * RH^(3 / 2) * atan(0.023101 * RH) -
    4.686035
}

calcular_metricas_diferenca <- function(diferenca) {
  diferenca <- diferenca[!is.na(diferenca)]
  c(
    vies = mean(diferenca),
    mae = mean(abs(diferenca)),
    rmse = sqrt(mean(diferenca^2)),
    sd = sd(diferenca),
    max_abs = max(abs(diferenca))
  )
}

# ---------- Leitura ----------
if (!file.exists(arquivo_et)) stop("Arquivo ET nao encontrado: ", arquivo_et)
if (!file.exists(arquivo_simepar)) stop("Arquivo Simepar nao encontrado: ", arquivo_simepar)

et <- read.csv(arquivo_et, stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8-BOM")
simepar <- read.csv(arquivo_simepar, stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8-BOM")

colunas_et <- c("Data e Horário", "RU", "Bulbo Umido (stull)", "globo", "Bulbo Seco", "IBUTG")
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
et$bulbo_umido_stull_ET <- converter_numero(et[["Bulbo Umido (stull)"]])
et$globo_ET <- converter_numero(et[["globo"]])
et$bulbo_seco_ET <- converter_numero(et[["Bulbo Seco"]])
et$IBUTG_ET <- converter_numero(et[["IBUTG"]])

simepar$horario_alinhado <- as.POSIXct(substr(simepar$datahora, 1, 19), format = "%Y-%m-%d %H:%M:%S", tz = "America/Sao_Paulo")
simepar$temperatura_simepar <- converter_numero(simepar$temperatura)
simepar$umidade_simepar <- converter_numero(simepar$umidade)

# Uma unica observacao ambiental e mantida por slot nominal. Duplicidades do
# arquivo bruto sao diagnosticadas em analise_transmissao_lora.R, mas nao
# recebem peso adicional nas metricas ambientais.
et2 <- et[order(et$data_hora_et), c("data_hora_et", "horario_alinhado", "status", "RU_ET", "bulbo_umido_stull_ET", "globo_ET", "bulbo_seco_ET", "IBUTG_ET")]
et2 <- et2[!duplicated(et2$horario_alinhado), ]
simepar2 <- simepar[, c("horario_alinhado", "temperatura_simepar", "umidade_simepar")]
comparacao <- merge(et2, simepar2, by = "horario_alinhado", all.x = TRUE, sort = TRUE)

# ---------- Calculos de IBUTG ----------
# Formula ponderada usada como indice termico estimado para ambiente externo:
# I_est = 0,7*Tbu,est + 0,2*Tg + 0,1*Tbs.
# Tbu,est e a aproximacao psicrometrica de Stull, nao bulbo umido natural.
comparacao$bulbo_umido_stull_recalc_ET <- stull_bulbo_umido(comparacao$bulbo_seco_ET, comparacao$RU_ET)
comparacao$IBUTG_recalc_coluna_ET <- 0.7 * comparacao$bulbo_umido_stull_ET + 0.2 * comparacao$globo_ET + 0.1 * comparacao$bulbo_seco_ET
comparacao$IBUTG_recalc_stull_ET <- 0.7 * comparacao$bulbo_umido_stull_recalc_ET + 0.2 * comparacao$globo_ET + 0.1 * comparacao$bulbo_seco_ET

# Estimativa alternativa usando temperatura e umidade Simepar na formula de Stull,
# mantendo globo ET por nao haver globo no Simepar.
comparacao$bulbo_umido_stull_simepar <- stull_bulbo_umido(comparacao$temperatura_simepar, comparacao$umidade_simepar)
comparacao$IBUTG_stull_simepar_T_RH <- 0.7 * comparacao$bulbo_umido_stull_simepar + 0.2 * comparacao$globo_ET + 0.1 * comparacao$temperatura_simepar

comparacao$dif_ibutg_coluna_menos_original <- comparacao$IBUTG_recalc_coluna_ET - comparacao$IBUTG_ET
comparacao$dif_ibutg_stull_ET_menos_original <- comparacao$IBUTG_recalc_stull_ET - comparacao$IBUTG_ET
comparacao$dif_ibutg_simepar_menos_ET <- comparacao$IBUTG_stull_simepar_T_RH - comparacao$IBUTG_ET
comparacao$dif_bulbo_umido_recalc_menos_coluna <- comparacao$bulbo_umido_stull_recalc_ET - comparacao$bulbo_umido_stull_ET

validos <- comparacao[!is.na(comparacao$IBUTG_ET) & !is.na(comparacao$IBUTG_stull_simepar_T_RH), ]

# ---------- Incerteza aproximada usando resumos TXT ----------
mae_umidade <- extrair_metrica_txt(resumo_umidade, "MAE")
rmse_umidade <- extrair_metrica_txt(resumo_umidade, "RMSE")
mae_temperatura <- extrair_metrica_txt(resumo_temperatura, "MAE")
rmse_temperatura <- extrair_metrica_txt(resumo_temperatura, "RMSE")

# Sensibilidade numerica da formula de Stull em torno dos dados ET.
eps_T <- 0.1
eps_RH <- 0.1
dTw_dT <- (stull_bulbo_umido(validos$bulbo_seco_ET + eps_T, validos$RU_ET) -
             stull_bulbo_umido(validos$bulbo_seco_ET - eps_T, validos$RU_ET)) / (2 * eps_T)
dTw_dRH <- (stull_bulbo_umido(validos$bulbo_seco_ET, validos$RU_ET + eps_RH) -
              stull_bulbo_umido(validos$bulbo_seco_ET, validos$RU_ET - eps_RH)) / (2 * eps_RH)

# Derivadas aproximadas do IBUTG em relacao a Tbs e RH, sem incluir incerteza do globo.
dIBUTG_dT <- 0.7 * dTw_dT + 0.1
dIBUTG_dRH <- 0.7 * dTw_dRH

incerteza_mae_aprox <- mean(abs(dIBUTG_dT)) * mae_temperatura + mean(abs(dIBUTG_dRH)) * mae_umidade
incerteza_rmse_aprox <- sqrt((mean(abs(dIBUTG_dT)) * rmse_temperatura)^2 +
                               (mean(abs(dIBUTG_dRH)) * rmse_umidade)^2)

# ---------- Metricas ----------
metricas_coluna <- calcular_metricas_diferenca(validos$dif_ibutg_coluna_menos_original)
metricas_stull_ET <- calcular_metricas_diferenca(validos$dif_ibutg_stull_ET_menos_original)
metricas_simepar <- calcular_metricas_diferenca(validos$dif_ibutg_simepar_menos_ET)
metricas_tbu <- calcular_metricas_diferenca(validos$dif_bulbo_umido_recalc_menos_coluna)

source("funcoes_estatisticas.R")
metricas_concordancia <- metricas_pareadas(validos$IBUTG_stull_simepar_T_RH, validos$IBUTG_ET)
intervalos <- bootstrap_blocos_pareados(validos$IBUTG_stull_simepar_T_RH, validos$IBUTG_ET)
cor_ibutg <- metricas_concordancia$pearson
r2_ibutg <- metricas_concordancia$r2
limite_concordancia_inferior <- metricas_concordancia$limite_concordancia_inferior
limite_concordancia_superior <- metricas_concordancia$limite_concordancia_superior

# ---------- Saidas ----------
write.csv2(comparacao, saida_pareada, row.names = FALSE, fileEncoding = "UTF-8")
write.csv2(intervalos, saida_intervalos, row.names = FALSE, fileEncoding = "UTF-8")

png(saida_grafico_series, width = 1400, height = 800)
plot(validos$horario_alinhado, validos$IBUTG_ET,
     type = "l", col = "#2166AC", lwd = 2,
     xlab = "Data/hora alinhada", ylab = "Indice termico estimado (°C)",
     main = "Estimativas com T/UR ET e T/UR Simepar")
lines(validos$horario_alinhado, validos$IBUTG_stull_simepar_T_RH, col = "#B2182B", lwd = 2, lty = 2)
legend("topright", legend = c("Estimativa com T/UR ET", "Estimativa com T/UR Simepar"),
       col = c("#2166AC", "#B2182B"), lwd = 2, lty = c(1, 2), bty = "n")
grid()
dev.off()

png(saida_grafico_diferenca, width = 1400, height = 800)
plot(validos$horario_alinhado, validos$dif_ibutg_simepar_menos_ET,
     type = "h", col = "#1B7837", lwd = 2,
     xlab = "Data/hora alinhada", ylab = "Estimativa T/UR Simepar - estimativa ET (°C)",
     main = "Sensibilidade a substituicao de temperatura e umidade")
abline(h = 0, col = "black", lwd = 2)
abline(h = c(-1, 1), col = "orange", lty = 2)
grid()
dev.off()

png(saida_grafico_incerteza, width = 1000, height = 800)
plot(validos$IBUTG_ET, validos$IBUTG_stull_simepar_T_RH,
     pch = 19, col = rgb(0.13, 0.40, 0.67, 0.45),
     xlab = "Estimativa com T/UR ET (°C)", ylab = "Estimativa com T/UR Simepar (°C)",
     main = "Sensibilidade do indice termico estimado")
abline(lm(IBUTG_stull_simepar_T_RH ~ IBUTG_ET, data = validos), col = "red", lwd = 2)
abline(a = 0, b = 1, col = "gray40", lwd = 2, lty = 2)
legend("topleft", legend = c("Regressao", "Linha 1:1"), col = c("red", "gray40"), lwd = 2, lty = c(1, 2), bty = "n")
grid()
dev.off()

sink(saida_resumo, split = TRUE)
cat("\nResumo estatistico: sensibilidade do indice estimado por Stull\n")
cat("===========================================================\n")
cat("Pares validos analisados: ", nrow(validos), "\n\n", sep = "")

cat("Escopo da comparacao:\n")
cat("- Tw e uma estimativa psicrometrica de Stull, nao bulbo umido natural medido.\n")
cat("- As duas series reutilizam o mesmo globo ET; portanto, a comparacao nao valida o indice completo.\n")
cat("- Formula ponderada: I_est = 0,7*Tw_est + 0,2*Tg + 0,1*Tbs.\n\n")

cat("Verificacao interna da planilha ET:\n")
cat("- IBUTG recalculado com coluna 'Bulbo Umido (stull)' menos IBUTG original:\n")
cat("  Vies:    ", fmt(metricas_coluna['vies'], 3), " °C\n", sep = "")
cat("  MAE:     ", fmt(metricas_coluna['mae'], 3), " °C\n", sep = "")
cat("  RMSE:    ", fmt(metricas_coluna['rmse'], 3), " °C\n\n", sep = "")

cat("- Bulbo umido Stull recalculado no R menos coluna 'Bulbo Umido (stull)':\n")
cat("  Vies:    ", fmt(metricas_tbu['vies'], 3), " °C\n", sep = "")
cat("  MAE:     ", fmt(metricas_tbu['mae'], 3), " °C\n", sep = "")
cat("  RMSE:    ", fmt(metricas_tbu['rmse'], 3), " °C\n\n", sep = "")

cat("Sensibilidade a substituicao de T/UR ET por T/UR Simepar, com o mesmo globo:\n")
cat("- Pearson entre as duas estimativas:             ", fmt(cor_ibutg, 4), "\n", sep = "")
cat("- R2:                                            ", fmt(r2_ibutg, 4), "\n", sep = "")
cat("- Diferenca media:                               ", fmt(metricas_simepar['vies'], 3), " °C\n", sep = "")
cat("- MAE:                                           ", fmt(metricas_simepar['mae'], 3), " °C\n", sep = "")
cat("- RMSE:                                          ", fmt(metricas_simepar['rmse'], 3), " °C\n", sep = "")
cat("- Maior diferenca absoluta:                      ", fmt(metricas_simepar['max_abs'], 3), " °C\n", sep = "")
cat("- Limites de concordancia:                       ", fmt(limite_concordancia_inferior, 3), " a ", fmt(limite_concordancia_superior, 3), " °C\n", sep = "")
cat("- IC95% por bootstrap em blocos de 24 h: consultar ", saida_intervalos, "\n\n", sep = "")

cat("Propagacao aproximada das diferencas de T/UR na estimativa:\n")
cat("- MAE temperatura usado: ", fmt(mae_temperatura, 2), " °C\n", sep = "")
cat("- RMSE temperatura usado: ", fmt(rmse_temperatura, 2), " °C\n", sep = "")
cat("- MAE umidade usado: ", fmt(mae_umidade, 2), " p.p.\n", sep = "")
cat("- RMSE umidade usado: ", fmt(rmse_umidade, 2), " p.p.\n", sep = "")
cat("- Incerteza aproximada por MAE em IBUTG: ", fmt(incerteza_mae_aprox, 3), " °C\n", sep = "")
cat("- Incerteza aproximada por RMSE em IBUTG: ", fmt(incerteza_rmse_aprox, 3), " °C\n\n", sep = "")

cat("O que as estatisticas medem:\n")
cat("- Vies: erro medio com sinal; positivo indica estimativa maior que o IBUTG ET.\n")
cat("- MAE: erro medio absoluto em °C; indica o erro tipico sem considerar sinal.\n")
cat("- RMSE: erro quadratico medio; penaliza mais erros grandes.\n")
cat("- Pearson/R2: indicam se a estimativa acompanha a variacao temporal do IBUTG ET.\n")
cat("- Propagacao: estima como diferencas de temperatura e umidade afetam o indice calculado; nao inclui erro do globo nem converte Stull em bulbo umido natural.\n\n")

cat("Arquivos de saida:\n")
cat("- ", saida_pareada, "\n", sep = "")
cat("- ", saida_intervalos, "\n", sep = "")
cat("- ", saida_resumo, "\n", sep = "")
cat("- ", saida_grafico_series, "\n", sep = "")
cat("- ", saida_grafico_diferenca, "\n", sep = "")
cat("- ", saida_grafico_incerteza, "\n", sep = "")
sink()
