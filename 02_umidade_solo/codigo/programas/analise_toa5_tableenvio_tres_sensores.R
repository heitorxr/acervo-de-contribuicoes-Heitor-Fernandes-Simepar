# Comparação de umidade a partir do TOA5 TableEnvio do CR350
#
# Fonte: dados/cr350_cs625/brutos/CR350Series_TableEnvio-2.dat
# UmidSolo       = umidade do CS/CR350, usada como série de referência de campo
# PeriodoCS625   = período bruto do CS625, mantido para diagnóstico
# UmidPlug       = umidade do plugfild
# UmidAzul       = umidade do azul/PC03
#
# Esta análise é de comparação de séries de campo. Não substitui a calibração
# laboratorial gravimétrica e não transforma a série do CS.

options(stringsAsFactors = FALSE)

args <- commandArgs(trailingOnly = FALSE)
script_arg <- args[grep("^--file=", args)]
base_dir <- if (length(script_arg)) {
  normalizePath(file.path(dirname(sub("^--file=", "", script_arg[1])), ".."), winslash = "/")
} else normalizePath(getwd(), winslash = "/")

entrada <- file.path(base_dir, "dados", "cr350_cs625", "brutos", "CR350Series_TableEnvio-2.dat")
saida <- file.path(base_dir, "resultados", "analise_toa5_tableenvio_tres_sensores")
tabelas <- file.path(saida, "tabelas")
graficos <- file.path(saida, "graficos")
resumos <- file.path(saida, "resumos")
for (d in c(tabelas, graficos, resumos)) dir.create(d, recursive = TRUE, showWarnings = FALSE)

ler_num <- function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("", "NAN", "NaN", "NA")] <- NA_character_
  suppressWarnings(as.numeric(gsub(",", ".", x, fixed = TRUE)))
}

metricas <- function(obs, ref) {
  ok <- is.finite(obs) & is.finite(ref)
  obs <- obs[ok]; ref <- ref[ok]
  erro <- obs - ref
  p <- if (length(obs) >= 3 && sd(obs) > 0 && sd(ref) > 0) cor(obs, ref) else NA_real_
  s <- if (length(obs) >= 3 && sd(obs) > 0 && sd(ref) > 0) cor(obs, ref, method = "spearman") else NA_real_
  data.frame(
    n = length(obs), bias_sensor_menos_cs_pct = mean(erro),
    mae_pct = mean(abs(erro)), rmse_pct = sqrt(mean(erro^2)),
    sd_erro_pct = if (length(erro) > 1) sd(erro) else NA_real_,
    pearson = p, spearman = s, r2_associacao = p^2
  )
}

if (!file.exists(entrada)) stop("TOA5 não encontrado: ", entrada)
# TOA5: linha 1 = metadados; linha 2 = nomes; linhas 3-4 = unidades/processamento.
d <- read.csv(entrada, skip = 1, header = TRUE, check.names = FALSE,
               na.strings = c("", "NAN", "NaN", "NA"),
               fileEncoding = "UTF-8-BOM", quote = "\"")
obrigatorias <- c("TIMESTAMP", "RECORD", "UmidSolo", "PeriodoCS625", "UmidPlug", "UmidAzul")
if (length(setdiff(obrigatorias, names(d)))) stop("Colunas ausentes: ", paste(setdiff(obrigatorias, names(d)), collapse = ", "))

# As duas linhas descritivas do TOA5 não têm timestamp ISO válido e são removidas.
d$datahora <- as.POSIXct(trimws(d$TIMESTAMP), format = "%Y-%m-%d %H:%M:%S", tz = "America/Sao_Paulo")
for (nm in c("RECORD", "UmidSolo", "PeriodoCS625", "UmidPlug", "TempSoloPlug", "UmidAzul", "TempSoloAzul", "BatteryV")) {
  if (nm %in% names(d)) d[[nm]] <- ler_num(d[[nm]])
}
d <- d[!is.na(d$datahora) & is.finite(d$RECORD), ]
d <- d[order(d$datahora), ]

serie <- data.frame(
  datahora_local = format(d$datahora, "%Y-%m-%d %H:%M:%S%z"),
  record = d$RECORD,
  cs_umidsolo_pct = d$UmidSolo,
  cs_periodo_us = d$PeriodoCS625,
  plugfild_pct = d$UmidPlug,
  temp_solo_plug_c = d$TempSoloPlug,
  azul_pc03_pct = d$UmidAzul,
  temp_solo_azul_c = d$TempSoloAzul,
  bateria_v = d$BatteryV
)

parear <- function(sensor_col, nome) {
  z <- data.frame(datahora = d$datahora, cs_pct = d$UmidSolo, sensor_pct = d[[sensor_col]])
  z <- z[is.finite(z$cs_pct) & is.finite(z$sensor_pct), ]
  z <- z[order(z$datahora), ]
  modelo <- lm(cs_pct ~ sensor_pct, data = z)
  z$predicao_linear_cs_pct <- predict(modelo, newdata = z)
  z$erro_original_pct <- z$sensor_pct - z$cs_pct
  z$erro_ajustado_pct <- z$predicao_linear_cs_pct - z$cs_pct
  m_original <- cbind(sensor = nome, modelo = "original", metricas(z$sensor_pct, z$cs_pct))
  m_ajustado <- cbind(sensor = nome, modelo = "linear_ajustado_no_mesmo_conjunto", metricas(z$predicao_linear_cs_pct, z$cs_pct))
  co <- data.frame(sensor = nome, referencia = "CS_UmidSolo", intercepto = coef(modelo)[1], inclinacao = coef(modelo)[2], r2_ajuste = summary(modelo)$r.squared, n = nrow(z))
  list(dados = z, metricas = rbind(m_original, m_ajustado), coeficientes = co)
}

plug <- parear("UmidPlug", "plugfild")
azul <- parear("UmidAzul", "azul_pc03")

# Comparação direta dos dois sensores de campo somente nos timestamps completos.
triplo <- data.frame(datahora = d$datahora, cs_pct = d$UmidSolo, plugfild_pct = d$UmidPlug, azul_pct = d$UmidAzul)
triplo <- triplo[complete.cases(triplo), ]
comparacao_direta <- cbind(comparacao = "azul_pc03_menos_plugfild", metricas(triplo$azul_pct, triplo$plugfild_pct))

metricas_todas <- rbind(plug$metricas, azul$metricas)
coeficientes <- rbind(plug$coeficientes, azul$coeficientes)
disponibilidade <- data.frame(
  serie = c("CS_UmidSolo", "CS_PeriodoCS625", "plugfild", "azul_pc03"),
  validos = c(sum(is.finite(d$UmidSolo)), sum(is.finite(d$PeriodoCS625)), sum(is.finite(d$UmidPlug)), sum(is.finite(d$UmidAzul))),
  total_registros = nrow(d)
)
disponibilidade$ausentes <- disponibilidade$total_registros - disponibilidade$validos
disponibilidade$disponibilidade_pct <- 100 * disponibilidade$validos / disponibilidade$total_registros

write.csv(serie, file.path(tabelas, "tableenvio_normalizado.csv"), row.names = FALSE)
write.csv(metricas_todas, file.path(tabelas, "metricas_sensores_vs_cs.csv"), row.names = FALSE)
write.csv(coeficientes, file.path(tabelas, "ajustes_lineares_sensores_vs_cs.csv"), row.names = FALSE)
write.csv(comparacao_direta, file.path(tabelas, "comparacao_direta_azul_vs_plugfild.csv"), row.names = FALSE)
write.csv(disponibilidade, file.path(tabelas, "disponibilidade_tableenvio.csv"), row.names = FALSE)
write.csv(plug$dados, file.path(tabelas, "pares_cs_plugfild.csv"), row.names = FALSE)
write.csv(azul$dados, file.path(tabelas, "pares_cs_azul.csv"), row.names = FALSE)
write.csv(triplo, file.path(tabelas, "timestamps_completos_tres_sensores.csv"), row.names = FALSE)

# Gráfico temporal: valores brutos, sem suavização.
png(file.path(graficos, "series_tableenvio_tres_sensores.png"), width = 2200, height = 1200, res = 150)
par(mar = c(5.2, 5.5, 4.0, 1.2), las = 1)
lim <- range(c(d$UmidSolo, d$UmidPlug, d$UmidAzul), finite = TRUE)
plot(d$datahora, d$UmidSolo, type = "n", ylim = lim, xlab = "Data/hora local", ylab = "Umidade do solo (%)", main = "TOA5 TableEnvio: três sensores")
grid(col = "gray88")
lines(d$datahora, d$UmidSolo, col = "#1F77B4", lwd = 2.2)
lines(d$datahora, d$UmidPlug, col = "#D62728", lwd = 1.5)
lines(d$datahora, d$UmidAzul, col = "#17A2B8", lwd = 1.5)
legend("topleft", bty = "n", col = c("#1F77B4", "#D62728", "#17A2B8"), lwd = c(2.2, 1.5, 1.5), legend = c("CS — UmidSolo", "plugfild — UmidPlug", "azul/PC03 — UmidAzul"))
dev.off()

# Gráficos de ajuste, uma comparação por sensor.
png(file.path(graficos, "ajustes_sensores_vs_cs.png"), width = 2200, height = 1050, res = 150)
par(mfrow = c(1, 2), mar = c(5.2, 5.2, 4.0, 1.0), las = 1)
for (obj in list(plug, azul)) {
  z <- obj$dados
  plot(z$sensor_pct, z$cs_pct, pch = 16, cex = .45, col = adjustcolor("#555555", .35), xlab = "Sensor de campo (%)", ylab = "CS — UmidSolo (%)", main = unique(obj$coeficientes$sensor))
  grid(col = "gray88"); abline(0, 1, lty = 2, col = "gray40")
  o <- order(z$sensor_pct); lines(z$sensor_pct[o], z$predicao_linear_cs_pct[o], col = "#2CA02C", lwd = 2.5)
  legend("topleft", bty = "n", legend = c("pares", "linha 1:1", "ajuste linear"), pch = c(16, NA, NA), lty = c(NA, 2, 1), col = c("#555555", "gray40", "#2CA02C"), lwd = c(NA, 1, 2.5), cex = .82)
}
dev.off()

resumo <- c(
  "COMPARAÇÃO TOA5 TABLEENVIO — CS, PLUGFILD E AZUL/PC03", "",
  paste0("Registros válidos após remover as linhas descritivas: ", nrow(d)),
  paste0("Período: ", format(min(d$datahora), "%Y-%m-%d %H:%M:%S%z"), " a ", format(max(d$datahora), "%Y-%m-%d %H:%M:%S%z")),
  paste0("CS: UmidSolo; período bruto: PeriodoCS625; plugfild: UmidPlug; azul: UmidAzul."), "",
  "A série CS — UmidSolo foi mantida como referência de campo e não foi recalibrada neste script.",
  "A calibração laboratorial gravimétrica permanece uma análise separada.", "",
  sprintf("Pares CS × plugfild: %d; RMSE original %.3f%%; RMSE após ajuste no mesmo conjunto %.3f%%.", nrow(plug$dados), plug$metricas$rmse_pct[1], plug$metricas$rmse_pct[2]),
  sprintf("Pares CS × azul: %d; RMSE original %.3f%%; RMSE após ajuste no mesmo conjunto %.3f%%.", nrow(azul$dados), azul$metricas$rmse_pct[1], azul$metricas$rmse_pct[2]),
  sprintf("Timestamps completos dos três sensores: %d.", nrow(triplo)),
  "Os ajustes são empíricos, calculados no mesmo conjunto, e não constituem validação independente.",
  "Não houve interpolação nem suavização.",
  "",
  "Arquivos de entrada e saída:", paste0("Entrada: ", entrada), paste0("Resultados: ", saida)
)
writeLines(resumo, file.path(resumos, "resumo_tableenvio_tres_sensores.txt"), useBytes = TRUE)
message("Concluído. Resultados em: ", saida)
