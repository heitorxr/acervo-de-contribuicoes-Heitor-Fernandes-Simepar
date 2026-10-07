# Comparação exclusiva do fim de semana: CS625 (TOA5) × PC03/azul (SIMEPAR).
# Fontes:
# - UmidSolo e PeriodoCS625: CR350Series_TableEnvio_20260921_1320.dat
# - azul/PC03: download SIMEPAR 19/09/2026 00:00 a 20/09/2026 23:55
# Não usa UmidAzul interno do TOA5 e não mistura campanhas anteriores.

options(stringsAsFactors = FALSE)
args <- commandArgs(trailingOnly = FALSE)
arg_script <- args[grep("^--file=", args)]
base_dir <- if (length(arg_script)) normalizePath(file.path(dirname(sub("^--file=", "", arg_script[1])), ".."), winslash = "/") else normalizePath(getwd(), winslash = "/")
tz_local <- "America/Sao_Paulo"
arq_toa5 <- file.path(base_dir, "dados", "cr350_cs625", "brutos", "CR350Series_TableEnvio_20260921_1320.dat")
arq_pc03 <- file.path(base_dir, "dados", "pc03_azul", "simepar", "pc03_azul_25354889_soil_moisture_238_20260919_0000_a_20260920_2359_local.csv")
saida <- file.path(base_dir, "resultados", "comparacao_cs625_pc03_fim_de_semana")
tabelas <- file.path(saida, "tabelas"); graficos <- file.path(saida, "graficos"); resumos <- file.path(saida, "resumos")
for (d in c(tabelas, graficos, resumos)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
for (f in list.files(saida, recursive = TRUE, full.names = TRUE)) if (grepl("\\.(csv|txt|png)$", f, ignore.case = TRUE)) unlink(f)

ler_num <- function(x) {
  x <- trimws(as.character(x)); x[x %in% c("", "NAN", "NaN", "NA")] <- NA_character_
  suppressWarnings(as.numeric(gsub(",", ".", x, fixed = TRUE)))
}
metricas <- function(est, ref) {
  ok <- is.finite(est) & is.finite(ref); est <- est[ok]; ref <- ref[ok]; e <- est - ref
  p <- if (length(est) >= 3 && sd(est) > 0 && sd(ref) > 0) cor(est, ref) else NA_real_
  s <- if (length(est) >= 3 && sd(est) > 0 && sd(ref) > 0) cor(est, ref, method = "spearman") else NA_real_
  data.frame(n = length(est), bias_pp = mean(e), mae_pp = mean(abs(e)), rmse_pp = sqrt(mean(e^2)), sd_erro_pp = sd(e), pearson = p, spearman = s, r2_associacao = p^2)
}
resumo_serie <- function(nome, x) {
  x <- x[is.finite(x)]
  data.frame(sensor = nome, n = length(x), minimo_pct = min(x), q05_pct = unname(quantile(x, .05)), mediana_pct = median(x), media_pct = mean(x), q95_pct = unname(quantile(x, .95)), maximo_pct = max(x), desvio_padrao_pct = sd(x), amplitude_pp = diff(range(x)))
}
if (!file.exists(arq_toa5) || !file.exists(arq_pc03)) stop("Entrada ausente: ", paste(c(arq_toa5, arq_pc03)[!file.exists(c(arq_toa5, arq_pc03))], collapse = "; "))

# CS625 no TOA5.
toa5 <- read.csv(arq_toa5, skip = 1, header = TRUE, check.names = FALSE, na.strings = c("", "NAN", "NaN", "NA"), quote = "\"", fileEncoding = "UTF-8-BOM")
if (length(setdiff(c("TIMESTAMP", "UmidSolo", "PeriodoCS625"), names(toa5)))) stop("TOA5 sem as colunas exigidas UmidSolo/PeriodoCS625.")
toa5$datahora <- as.POSIXct(trimws(toa5$TIMESTAMP), format = "%Y-%m-%d %H:%M:%S", tz = tz_local)
toa5$cs_umidsolo_pct <- ler_num(toa5$UmidSolo); toa5$cs_periodo_us <- ler_num(toa5$PeriodoCS625)
cs <- toa5[!is.na(toa5$datahora) & is.finite(toa5$cs_umidsolo_pct), c("datahora", "cs_umidsolo_pct", "cs_periodo_us")]
cs <- aggregate(cbind(cs_umidsolo_pct, cs_periodo_us) ~ datahora, data = cs, FUN = mean, na.rm = TRUE)

# PC03/azul externo da SIMEPAR, somente o fim de semana solicitado.
pc03 <- read.csv(arq_pc03, check.names = FALSE, fileEncoding = "UTF-8-BOM")
if (length(setdiff(c("datahora_local", "leitura", "estacaoId", "sensorId", "qualidadeId"), names(pc03)))) stop("CSV PC03 não tem as colunas esperadas.")
pc03$datahora <- as.POSIXct(substr(pc03$datahora_local, 1, 19), format = "%Y-%m-%d %H:%M:%S", tz = tz_local)
pc03$azul_pc03_pct <- ler_num(pc03$leitura); pc03$qualidadeId <- suppressWarnings(as.integer(pc03$qualidadeId))
pc03 <- pc03[!is.na(pc03$datahora) & is.finite(pc03$azul_pc03_pct), c("datahora", "azul_pc03_pct", "qualidadeId")]
pc03 <- aggregate(cbind(azul_pc03_pct, qualidadeId) ~ datahora, data = pc03, FUN = mean, na.rm = TRUE)

inicio <- min(pc03$datahora); fim <- max(pc03$datahora)
cs_intervalo <- cs[cs$datahora >= inicio & cs$datahora <= fim, ]
alinhados <- merge(cs_intervalo, pc03, by = "datahora", all = TRUE)
alinhados <- alinhados[order(alinhados$datahora), ]
pares <- alinhados[is.finite(alinhados$cs_umidsolo_pct) & is.finite(alinhados$azul_pc03_pct), ]
if (nrow(pares) < 20) stop("Menos de 20 pares exatos CS × PC03 no fim de semana.")

# Ajustes descritivos no conjunto completo e validação temporal 70/30.
mod_lin <- lm(cs_umidsolo_pct ~ azul_pc03_pct, data = pares)
mod_quad <- lm(cs_umidsolo_pct ~ azul_pc03_pct + I(azul_pc03_pct^2), data = pares)
pares$azul_linear_cs_pct <- predict(mod_lin, pares); pares$azul_quadratico_cs_pct <- predict(mod_quad, pares)
pares$residuo_original_pp <- pares$azul_pc03_pct - pares$cs_umidsolo_pct
pares$residuo_linear_pp <- pares$azul_linear_cs_pct - pares$cs_umidsolo_pct
pares$residuo_quadratico_pp <- pares$azul_quadratico_cs_pct - pares$cs_umidsolo_pct
n <- nrow(pares); n_treino <- floor(.7 * n); treino <- pares[seq_len(n_treino), ]; validacao <- pares[(n_treino + 1):n, ]; inicio_validacao <- validacao$datahora[1]
mod_lin_70 <- lm(cs_umidsolo_pct ~ azul_pc03_pct, data = treino)
mod_quad_70 <- lm(cs_umidsolo_pct ~ azul_pc03_pct + I(azul_pc03_pct^2), data = treino)
pares$azul_linear_treino70_cs_pct <- predict(mod_lin_70, pares); pares$azul_quadratico_treino70_cs_pct <- predict(mod_quad_70, pares)
pares$conjunto_70_30 <- ifelse(seq_len(n) <= n_treino, "treino_70", "validacao_30")

# Lag apenas diagnóstico, sobre os pares exatos de 5 min.
lag_df <- do.call(rbind, lapply(seq(-180, 180, by = 5), function(lag_min) {
  z <- pares[, c("datahora", "azul_pc03_pct")]; z$datahora <- z$datahora - lag_min * 60
  m <- merge(pares[, c("datahora", "cs_umidsolo_pct")], z, by = "datahora", all = FALSE)
  p <- if (nrow(m) >= 3 && sd(m$cs_umidsolo_pct) > 0 && sd(m$azul_pc03_pct) > 0) cor(m$azul_pc03_pct, m$cs_umidsolo_pct) else NA_real_
  data.frame(lag_min = lag_min, n = nrow(m), pearson = p, r2 = p^2)
}))
melhor_lag <- lag_df[which.max(abs(lag_df$pearson)), ]

# Tabelas.
meta <- data.frame(chave = c("entrada_toa5_cs625", "entrada_simepar_pc03", "periodo", "pareamento", "pares"), valor = c(normalizePath(arq_toa5, winslash = "/"), normalizePath(arq_pc03, winslash = "/"), paste(format(inicio, "%Y-%m-%d %H:%M:%S%z"), "a", format(fim, "%Y-%m-%d %H:%M:%S%z")), "timestamp exato de 5 min", nrow(pares)))
esperados_5min <- floor(as.numeric(difftime(fim, inicio, units = "mins")) / 5) + 1
disp <- data.frame(sensor = c("CS625 — UmidSolo", "PC03/azul — SIMEPAR"), registros_validos = c(sum(is.finite(cs_intervalo$cs_umidsolo_pct)), nrow(pc03)), esperados_5min = esperados_5min)
disp$ausentes <- disp$esperados_5min - disp$registros_validos; disp$disponibilidade_pct <- 100 * disp$registros_validos / disp$esperados_5min
estat <- rbind(resumo_serie("CS625 — UmidSolo", pares$cs_umidsolo_pct), resumo_serie("PC03/azul — SIMEPAR", pares$azul_pc03_pct))
met_completo <- rbind(cbind(sensor = "PC03/azul", modelo = "original", metricas(pares$azul_pc03_pct, pares$cs_umidsolo_pct)), cbind(sensor = "PC03/azul", modelo = "linear_ajustado_mesmo_conjunto", metricas(pares$azul_linear_cs_pct, pares$cs_umidsolo_pct)), cbind(sensor = "PC03/azul", modelo = "quadratico_ajustado_mesmo_conjunto", metricas(pares$azul_quadratico_cs_pct, pares$cs_umidsolo_pct)))
met_validacao <- rbind(cbind(sensor = "PC03/azul", modelo = "original", conjunto = "validacao_30", metricas(validacao$azul_pc03_pct, validacao$cs_umidsolo_pct)), cbind(sensor = "PC03/azul", modelo = "linear_treino70", conjunto = "validacao_30", metricas(predict(mod_lin_70, validacao), validacao$cs_umidsolo_pct)), cbind(sensor = "PC03/azul", modelo = "quadratico_treino70", conjunto = "validacao_30", metricas(predict(mod_quad_70, validacao), validacao$cs_umidsolo_pct)))
coeficientes <- data.frame(modelo = c("linear", "quadratico"), intercepto = c(coef(mod_lin)[1], coef(mod_quad)[1]), coeficiente_linear = c(coef(mod_lin)[2], coef(mod_quad)[2]), coeficiente_quadratico = c(NA_real_, coef(mod_quad)[3]), r2_ajuste = c(summary(mod_lin)$r.squared, summary(mod_quad)$r.squared), n = n)
predicoes <- data.frame(datahora_local = format(pares$datahora, "%Y-%m-%d %H:%M:%S%z"), conjunto_70_30 = pares$conjunto_70_30, cs_umidsolo_referencia_pct = pares$cs_umidsolo_pct, cs_periodo_us = pares$cs_periodo_us, azul_simepar_pct = pares$azul_pc03_pct, qualidadeId_simepar = pares$qualidadeId, azul_linear_completo_pct = pares$azul_linear_cs_pct, azul_quadratico_completo_pct = pares$azul_quadratico_cs_pct, residuo_original_pp = pares$residuo_original_pp, residuo_linear_pp = pares$residuo_linear_pp, residuo_quadratico_pp = pares$residuo_quadratico_pp, azul_linear_treino70_pct = pares$azul_linear_treino70_cs_pct, azul_quadratico_treino70_pct = pares$azul_quadratico_treino70_cs_pct)
alinhados_saida <- alinhados; alinhados_saida$datahora <- format(alinhados_saida$datahora, "%Y-%m-%d %H:%M:%S%z")
write.csv(meta, file.path(tabelas, "metadados_fontes_e_periodo.csv"), row.names = FALSE)
write.csv(alinhados_saida, file.path(tabelas, "dados_cs625_pc03_alinhados.csv"), row.names = FALSE)
write.csv(disp, file.path(tabelas, "disponibilidade_fim_de_semana.csv"), row.names = FALSE)
write.csv(estat, file.path(tabelas, "estatisticas_descritivas.csv"), row.names = FALSE)
write.csv(met_completo, file.path(tabelas, "metricas_conjunto_completo.csv"), row.names = FALSE)
write.csv(coeficientes, file.path(tabelas, "modelos_empiricos_conjunto_completo.csv"), row.names = FALSE)
write.csv(met_validacao, file.path(tabelas, "validacao_temporal_70_30.csv"), row.names = FALSE)
write.csv(predicoes, file.path(tabelas, "predicoes_e_residuos.csv"), row.names = FALSE)
write.csv(lag_df, file.path(tabelas, "lag_correlacoes.csv"), row.names = FALSE)
write.csv(melhor_lag, file.path(tabelas, "melhor_lag.csv"), row.names = FALSE)

# Gráficos sem suavização.
cores <- c(cs = "#1F77B4", azul = "#17A2B8", linear = "#2CA02C", quadratico = "#FF7F0E")
ticks <- pretty(pares$datahora, n = 7)
png(file.path(graficos, "01_series_brutas_fim_de_semana.png"), width = 2200, height = 1200, res = 150)
par(mar = c(5.8, 5.8, 4.2, 1), las = 1); yl <- range(c(pares$cs_umidsolo_pct, pares$azul_pc03_pct))
plot(pares$datahora, pares$cs_umidsolo_pct, type = "n", xaxt = "n", ylim = yl, xlab = "Data/hora local", ylab = "Umidade do solo (%)", main = "Fim de semana: CS625 e azul/PC03 (SIMEPAR)")
axis.POSIXct(1, at = ticks, format = "%d/%m\n%H:%M"); grid(col = "gray88"); lines(pares$datahora, pares$cs_umidsolo_pct, col = cores["cs"], lwd = 2.6); lines(pares$datahora, pares$azul_pc03_pct, col = cores["azul"], lwd = 2)
legend("topleft", bty = "n", lwd = c(2.6, 2), col = cores[c("cs", "azul")], legend = c("CS625 — UmidSolo (TOA5)", "azul/PC03 (SIMEPAR)")); dev.off()

png(file.path(graficos, "02_ajustes_pc03_vs_cs625.png"), width = 1900, height = 1200, res = 150)
par(mar = c(5.8, 5.8, 4.2, 1), las = 1); plot(pares$azul_pc03_pct, pares$cs_umidsolo_pct, pch = 16, cex = .65, col = adjustcolor(cores["azul"], .45), xlab = "PC03/azul — SIMEPAR (%)", ylab = "CS625 — UmidSolo (%)", main = "Ajustes empíricos no fim de semana")
grid(col = "gray88"); abline(0, 1, lty = 2, col = "gray45"); o <- order(pares$azul_pc03_pct); lines(pares$azul_pc03_pct[o], pares$azul_linear_cs_pct[o], col = cores["linear"], lwd = 2.5); lines(pares$azul_pc03_pct[o], pares$azul_quadratico_cs_pct[o], col = cores["quadratico"], lwd = 2.5); legend("topleft", bty = "n", pch = c(16, NA, NA, NA), lty = c(NA, 2, 1, 1), col = c(cores["azul"], "gray45", cores["linear"], cores["quadratico"]), legend = c("pares", "linha 1:1", "linear", "quadrático")); dev.off()

png(file.path(graficos, "03_series_ajustadas_quadraticas.png"), width = 2200, height = 1200, res = 150)
par(mar = c(5.8, 5.8, 4.2, 1), las = 1); yl <- range(c(pares$cs_umidsolo_pct, pares$azul_quadratico_cs_pct)); plot(pares$datahora, pares$cs_umidsolo_pct, type = "n", xaxt = "n", ylim = yl, xlab = "Data/hora local", ylab = "Umidade do solo (%)", main = "Fim de semana: ajuste quadrático do PC03")
axis.POSIXct(1, at = ticks, format = "%d/%m\n%H:%M"); grid(col = "gray88"); lines(pares$datahora, pares$cs_umidsolo_pct, col = cores["cs"], lwd = 2.6); lines(pares$datahora, pares$azul_quadratico_cs_pct, col = cores["quadratico"], lwd = 2); legend("topleft", bty = "n", lwd = c(2.6, 2), col = cores[c("cs", "quadratico")], legend = c("CS625 — referência", "PC03 quadrático (mesmo conjunto)")); dev.off()

png(file.path(graficos, "04_residuos_no_tempo.png"), width = 2200, height = 1400, res = 150)
par(mfrow = c(2, 1), mar = c(3.5, 5.8, 3.5, 1), las = 1); yr <- range(c(pares$residuo_linear_pp, pares$residuo_quadratico_pp)); plot(pares$datahora, pares$residuo_linear_pp, type = "l", xaxt = "n", ylim = yr, xlab = "", ylab = "Resíduo (p.p.)", main = "A) PC03 linear − CS625"); grid(col = "gray88"); abline(h = 0, lty = 2); plot(pares$datahora, pares$residuo_quadratico_pp, type = "l", ylim = yr, xlab = "Data/hora local", ylab = "Resíduo (p.p.)", main = "B) PC03 quadrático − CS625"); grid(col = "gray88"); abline(h = 0, lty = 2); dev.off()

png(file.path(graficos, "05_validacao_temporal_70_30.png"), width = 2200, height = 1200, res = 150)
par(mar = c(5.8, 5.8, 4.2, 1), las = 1); yl <- range(c(pares$cs_umidsolo_pct, pares$azul_quadratico_treino70_cs_pct)); plot(pares$datahora, pares$cs_umidsolo_pct, type = "n", xaxt = "n", ylim = yl, xlab = "Data/hora local", ylab = "Umidade do solo (%)", main = "Validação cronológica 70/30 — fim de semana")
u <- par("usr"); rect(inicio_validacao, u[3], max(pares$datahora), u[4], col = "gray92", border = NA); grid(col = "gray85"); lines(pares$datahora, pares$cs_umidsolo_pct, col = cores["cs"], lwd = 2.6); lines(pares$datahora, pares$azul_quadratico_treino70_cs_pct, col = cores["quadratico"], lwd = 2); abline(v = inicio_validacao, lty = 2, lwd = 2); axis.POSIXct(1, at = ticks, format = "%d/%m\n%H:%M"); legend("topleft", bty = "n", lwd = c(2.6, 2, 2), lty = c(1, 1, 2), col = c(cores["cs"], cores["quadratico"], "gray30"), legend = c("CS625", "PC03 quadrático treinado nos 70%", "início dos 30% finais")); dev.off()

png(file.path(graficos, "06_lag_correlacao.png"), width = 1900, height = 1100, res = 150)
par(mar = c(5.8, 5.8, 4.2, 1), las = 1); plot(lag_df$lag_min, lag_df$pearson, type = "l", lwd = 2.6, col = cores["azul"], xlab = "Lag aplicado ao PC03 (min)", ylab = "Correlação de Pearson", main = "Lag diagnóstico no fim de semana"); grid(col = "gray88"); abline(v = 0, lty = 2); abline(v = melhor_lag$lag_min, lty = 3, lwd = 2, col = "#D62728"); dev.off()

png(file.path(graficos, "07_metricas_validacao_70_30.png"), width = 1800, height = 1100, res = 150)
par(mar = c(7, 5.8, 4.2, 1), las = 1); mat <- rbind(MAE = met_validacao$mae_pp, RMSE = met_validacao$rmse_pp); b <- barplot(mat, beside = TRUE, names.arg = c("Original", "Linear", "Quadrático"), col = c("#6BAED6", "#FD8D3C"), ylab = "Erro (p.p.)", main = "Validação 70/30 — fim de semana"); grid(nx = NA, ny = NULL, col = "gray88"); legend("topright", bty = "n", fill = c("#6BAED6", "#FD8D3C"), legend = c("MAE", "RMSE")); text(b, mat, labels = sprintf("%.3f", mat), pos = 3, cex = .8); dev.off()

png(file.path(graficos, "08_disponibilidade_fim_de_semana.png"), width = 1800, height = 1100, res = 150)
par(mar = c(7, 5.8, 4.2, 1), las = 1); b <- barplot(disp$disponibilidade_pct, names.arg = disp$sensor, ylim = c(0, 108), col = cores[c("cs", "azul")], ylab = "Disponibilidade no intervalo (%)", main = "Disponibilidade: fim de semana"); grid(nx = NA, ny = NULL, col = "gray88"); text(b, disp$disponibilidade_pct, labels = sprintf("%.2f%%\n%d/%d", disp$disponibilidade_pct, disp$registros_validos, disp$esperados_5min), pos = 3); dev.off()

m0 <- met_completo[met_completo$modelo == "original", ]; mv <- met_validacao[met_validacao$modelo == "quadratico_treino70", ]
resumo <- c("COMPARAÇÃO EXCLUSIVA DO FIM DE SEMANA: CS625 × PC03/AZUL", "", paste0("Período: ", format(inicio, "%Y-%m-%d %H:%M:%S%z"), " a ", format(fim, "%Y-%m-%d %H:%M:%S%z")), "Fontes: CS — TOA5/UmidSolo; PC03 — SIMEPAR/sensor 238.", paste0("Pares exatos de 5 min: ", n, "; treino: ", n_treino, "; validação: ", n - n_treino, "."), "", sprintf("PC03 original: Pearson %.4f; Spearman %.4f; viés %.4f p.p.; MAE %.4f p.p.; RMSE %.4f p.p.", m0$pearson, m0$spearman, m0$bias_pp, m0$mae_pp, m0$rmse_pp), sprintf("Validação temporal, quadrático treinado nos 70%%: viés %.4f p.p.; MAE %.4f p.p.; RMSE %.4f p.p.", mv$bias_pp, mv$mae_pp, mv$rmse_pp), sprintf("Lag diagnóstico: %d min; Pearson %.4f; n %d.", melhor_lag$lag_min, melhor_lag$pearson, melhor_lag$n), "", "Ajustes no conjunto completo são descritivos e não constituem validação independente.", "Não houve interpolação nem suavização.", paste0("Resultados: ", saida))
writeLines(resumo, file.path(resumos, "resumo_cs625_pc03_fim_de_semana.txt"), useBytes = TRUE)
message("Concluído: ", saida)
