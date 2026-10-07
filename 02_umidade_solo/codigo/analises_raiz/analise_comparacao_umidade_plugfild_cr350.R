# Analise comparativa, lag, suavizacao visual e ajuste empirico: plugfild x CR350/CS625
#
# Sentido definido pelo usuario:
#   - plugfild = sensor a calibrar
#   - CR350/CS625 = sensor de verificacao/referencia
#
# Este script usa somente funcoes base do R.

options(stringsAsFactors = FALSE)

base_dir <- getwd()
entrada_dir <- file.path(base_dir, "dados", "entradas_analise", "plugfild_pc01")
saida_dir <- file.path(base_dir, "resultados", "comparacao_umidade_plugfild_cr350")
tabelas_dir <- file.path(saida_dir, "tabelas")
graficos_dir <- file.path(saida_dir, "graficos")
resumos_dir <- file.path(saida_dir, "resumos")
dir.create(tabelas_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(graficos_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(resumos_dir, recursive = TRUE, showWarnings = FALSE)

tz_local <- "America/Sao_Paulo"
arq_cr350 <- file.path(entrada_dir, "CR350Series_TabelaSolo_limpo.csv")
arq_plugfild  <- file.path(entrada_dir, "plugfild_25334889_soil_moisture_238_intervalo_TabelaSolo_local.csv")

ler_num <- function(x) suppressWarnings(as.numeric(gsub(",", ".", as.character(x), fixed = TRUE)))
media_movel <- function(x, k = 5) {
  n <- length(x); h <- floor(k / 2); y <- rep(NA_real_, n)
  for (i in seq_len(n)) {
    idx <- max(1, i - h):min(n, i + h)
    y[i] <- mean(x[idx], na.rm = TRUE)
  }
  y
}
metricas_erro <- function(obs, ref) {
  ok <- is.finite(obs) & is.finite(ref)
  d <- obs[ok] - ref[ok]
  p <- cor(obs[ok], ref[ok], method = "pearson")
  data.frame(
    n = sum(ok),
    bias_pp = mean(d),
    mae_pp = mean(abs(d)),
    rmse_pp = sqrt(mean(d^2)),
    sd_diferenca_pp = sd(d),
    pearson = p,
    spearman = cor(obs[ok], ref[ok], method = "spearman"),
    r2 = p^2
  )
}

# -----------------------------------------------------------------------------
# 1. Leitura e alinhamento por timestamp exato
# -----------------------------------------------------------------------------
cr350 <- read.csv(arq_cr350, check.names = FALSE, fileEncoding = "UTF-8-BOM")
plugfild <- read.csv(arq_plugfild, check.names = FALSE, fileEncoding = "UTF-8-BOM")
cr350$datahora <- as.POSIXct(cr350$TIMESTAMP, format = "%Y-%m-%d %H:%M:%S", tz = tz_local)
cr350$periodo_cs625_us <- ler_num(cr350$Periodo_CS625_us)
cr350$umidade_cr350_pct <- ler_num(cr350$VWC_m3_m3) * 100
plugfild$datahora <- as.POSIXct(plugfild$datahora_local, format = "%Y-%m-%d %H:%M:%S%z", tz = tz_local)
plugfild$umidade_plugfild_pct <- ler_num(plugfild$leitura)
plugfild$qualidadeId <- suppressWarnings(as.integer(plugfild$qualidadeId))

cr350_cmp <- aggregate(cbind(umidade_cr350_pct, periodo_cs625_us) ~ datahora, data = cr350, FUN = mean, na.rm = TRUE)
plugfild_cmp <- aggregate(cbind(umidade_plugfild_pct, qualidadeId) ~ datahora, data = plugfild, FUN = mean, na.rm = TRUE)
pares <- merge(cr350_cmp, plugfild_cmp, by = "datahora", all = FALSE)
pares <- pares[order(pares$datahora), ]
pares <- pares[is.finite(pares$umidade_cr350_pct) & is.finite(pares$umidade_plugfild_pct), ]
pares$erro_plugfild_original_pp <- pares$umidade_plugfild_pct - pares$umidade_cr350_pct

# -----------------------------------------------------------------------------
# 2. Ajustes empiricos aplicados ao plugfild; CR350 permanece referencia
# -----------------------------------------------------------------------------
modelo_linear <- lm(umidade_cr350_pct ~ umidade_plugfild_pct, data = pares)
modelo_quadratico <- lm(umidade_cr350_pct ~ umidade_plugfild_pct + I(umidade_plugfild_pct^2), data = pares)
pares$umidade_plugfild_calibrada_linear_pct <- predict(modelo_linear, newdata = pares)
pares$umidade_plugfild_calibrada_quadratica_pct <- predict(modelo_quadratico, newdata = pares)
pares$erro_plugfild_linear_pp <- pares$umidade_plugfild_calibrada_linear_pct - pares$umidade_cr350_pct
pares$erro_plugfild_quadratico_pp <- pares$umidade_plugfild_calibrada_quadratica_pct - pares$umidade_cr350_pct

# -----------------------------------------------------------------------------
# 3. Lag: plugfild deslocado contra CR350
# -----------------------------------------------------------------------------
lags_min <- seq(-180, 180, by = 5)
lag_df <- do.call(rbind, lapply(lags_min, function(lag_min) {
  pc_lag <- plugfild_cmp[, c("datahora", "umidade_plugfild_pct")]
  pc_lag$datahora <- pc_lag$datahora - lag_min * 60
  m <- merge(cr350_cmp[, c("datahora", "umidade_cr350_pct")], pc_lag, by = "datahora", all = FALSE)
  p <- if (nrow(m) >= 3) cor(m$umidade_plugfild_pct, m$umidade_cr350_pct, use = "complete.obs") else NA_real_
  data.frame(lag_min = lag_min, n = nrow(m), pearson = p, r2 = p^2)
}))
melhor_lag <- lag_df[which.max(abs(lag_df$pearson)), ]

pc_lag <- plugfild_cmp[, c("datahora", "umidade_plugfild_pct")]
pc_lag$datahora_plugfild_original <- pc_lag$datahora
pc_lag$datahora <- pc_lag$datahora - melhor_lag$lag_min * 60
pares_lag <- merge(cr350_cmp[, c("datahora", "umidade_cr350_pct")], pc_lag, by = "datahora", all = FALSE)
modelo_lag_linear <- lm(umidade_cr350_pct ~ umidade_plugfild_pct, data = pares_lag)
pares_lag$umidade_plugfild_lag_calibrada_linear_pct <- predict(modelo_lag_linear, newdata = pares_lag)
pares_lag$erro_plugfild_lag_calibrado_pp <- pares_lag$umidade_plugfild_lag_calibrada_linear_pct - pares_lag$umidade_cr350_pct

# Suavizacao apenas para visualizacao das curvas do plugfild
pares$umidade_plugfild_pct_suavizada <- media_movel(pares$umidade_plugfild_pct, 5)
pares$umidade_plugfild_calibrada_linear_pct_suavizada <- media_movel(pares$umidade_plugfild_calibrada_linear_pct, 5)
pares$umidade_plugfild_calibrada_quadratica_pct_suavizada <- media_movel(pares$umidade_plugfild_calibrada_quadratica_pct, 5)
pares_lag$umidade_plugfild_lag_calibrada_linear_pct_suavizada <- media_movel(pares_lag$umidade_plugfild_lag_calibrada_linear_pct, 5)

# -----------------------------------------------------------------------------
# 4. Tabelas
# -----------------------------------------------------------------------------
m_original <- metricas_erro(pares$umidade_plugfild_pct, pares$umidade_cr350_pct)
m_linear <- metricas_erro(pares$umidade_plugfild_calibrada_linear_pct, pares$umidade_cr350_pct)
m_quadratico <- metricas_erro(pares$umidade_plugfild_calibrada_quadratica_pct, pares$umidade_cr350_pct)
m_lag_linear <- metricas_erro(pares_lag$umidade_plugfild_lag_calibrada_linear_pct, pares_lag$umidade_cr350_pct)

coef_lin <- coef(modelo_linear)
coef_quad <- coef(modelo_quadratico)
coef_lag <- coef(modelo_lag_linear)

curvas <- data.frame(
  modelo = c("linear_sem_lag", "quadratico_sem_lag", paste0("linear_com_lag_", melhor_lag$lag_min, "_min")),
  sensor_calibrado = "plugfild",
  sensor_referencia = "CR350_CS625",
  formula = c(
    sprintf("y = %.8f + %.8f * plugfild", coef_lin[1], coef_lin[2]),
    sprintf("y = %.8f + %.8f * plugfild + %.8f * plugfild^2", coef_quad[1], coef_quad[2], coef_quad[3]),
    sprintf("y = %.8f + %.8f * plugfild_deslocado", coef_lag[1], coef_lag[2])
  ),
  r2_modelo = c(summary(modelo_linear)$r.squared, summary(modelo_quadratico)$r.squared, summary(modelo_lag_linear)$r.squared),
  n = c(nrow(pares), nrow(pares), nrow(pares_lag))
)

minimos_maximos <- data.frame(
  serie = c("CR350/CS625 referencia", "plugfild original", "plugfild calibrado linear", "plugfild calibrado quadratico", "plugfild lag + calibracao linear"),
  min_pct = c(min(pares$umidade_cr350_pct), min(pares$umidade_plugfild_pct), min(pares$umidade_plugfild_calibrada_linear_pct), min(pares$umidade_plugfild_calibrada_quadratica_pct), min(pares_lag$umidade_plugfild_lag_calibrada_linear_pct)),
  max_pct = c(max(pares$umidade_cr350_pct), max(pares$umidade_plugfild_pct), max(pares$umidade_plugfild_calibrada_linear_pct), max(pares$umidade_plugfild_calibrada_quadratica_pct), max(pares_lag$umidade_plugfild_lag_calibrada_linear_pct)),
  n = c(nrow(pares), nrow(pares), nrow(pares), nrow(pares), nrow(pares_lag))
)
minimos_maximos$amplitude_pct <- minimos_maximos$max_pct - minimos_maximos$min_pct

metricas <- rbind(
  cbind(serie = "plugfild_original", referencia = "CR350_CS625", m_original),
  cbind(serie = "plugfild_calibrado_linear", referencia = "CR350_CS625", m_linear),
  cbind(serie = "plugfild_calibrado_quadratico", referencia = "CR350_CS625", m_quadratico),
  cbind(serie = "plugfild_lag_calibrado_linear", referencia = "CR350_CS625", m_lag_linear)
)

write.csv(pares, file.path(tabelas_dir, "pares_alinhados_umidade_com_calibracao.csv"), row.names = FALSE)
write.csv(pares_lag, file.path(tabelas_dir, "pares_lag_calibracao_linear.csv"), row.names = FALSE)
write.csv(curvas, file.path(tabelas_dir, "curvas_ajuste_plugfild_para_cr350.csv"), row.names = FALSE)
write.csv(metricas, file.path(tabelas_dir, "metricas_antes_depois_calibracao.csv"), row.names = FALSE)
write.csv(lag_df, file.path(tabelas_dir, "lag_correlacoes_umidade.csv"), row.names = FALSE)
write.csv(minimos_maximos, file.path(tabelas_dir, "minimos_maximos_umidade.csv"), row.names = FALSE)
write.csv(data.frame(parametro = c("sensor_calibrado", "sensor_referencia", "intercepto", "inclinacao", "r2_modelo", "n_pares", "formula"), valor = c("plugfild", "CR350_CS625", coef_lin[1], coef_lin[2], summary(modelo_linear)$r.squared, nrow(pares), sprintf("umidade_plugfild_calibrada_pct = %.8f + %.8f * umidade_plugfild_pct", coef_lin[1], coef_lin[2]))), file.path(tabelas_dir, "calibracao_empirica_linear.csv"), row.names = FALSE)

# -----------------------------------------------------------------------------
# 5. Graficos PNG com painel lateral e escalas legiveis
# -----------------------------------------------------------------------------
for (f in list.files(graficos_dir, pattern = "\\.(png|svg)$", full.names = TRUE)) unlink(f)

cores <- c(cr350 = "#1f77b4", original = "#d62728", linear = "#2ca02c", quadratico = "#ff7f0e", lag = "#9467bd")

png(file.path(graficos_dir, "serie_temporal_melhorada_plugfild_lag_calibracao.png"), width = 2500, height = 1400, res = 150)
layout(matrix(c(1, 2), nrow = 1), widths = c(4.6, 1.5))
par(mar = c(5.5, 5.6, 4.2, 1.0), las = 1)
ylim_serie <- range(c(pares$umidade_cr350_pct, pares$umidade_plugfild_pct_suavizada, pares_lag$umidade_plugfild_lag_calibrada_linear_pct_suavizada), finite = TRUE)
plot(pares$datahora, pares$umidade_cr350_pct, type = "n", ylim = ylim_serie, xlab = "Data/hora local", ylab = "Umidade volumétrica do solo (%)", main = "CR350 e plugfild: calibração e lag")
grid(col = "gray88")
lines(pares$datahora, pares$umidade_cr350_pct, lwd = 3, col = cores["cr350"])
lines(pares$datahora, pares$umidade_plugfild_pct_suavizada, lwd = 2.5, col = cores["original"])
lines(pares_lag$datahora, pares_lag$umidade_plugfild_lag_calibrada_linear_pct_suavizada, lwd = 3, col = cores["lag"])
par(mar = c(5.5, 0.4, 4.2, 1.0))
plot.new()
legend("topleft", bty = "n", lwd = c(3, 2.5, 3), col = cores[c("cr350", "original", "lag")], legend = c("CR350/CS625 referência", "plugfild original suavizado", paste0("plugfild calibrado + lag ", melhor_lag$lag_min, " min")), cex = 0.95)
text(0, 0.48, adj = c(0, 1), cex = 0.82, col = "gray35", labels = "Suavização: média móvel\ncentrada de 5 pontos, apenas\npara visualização. Métricas\ncalculadas sem suavização.")
layout(1)
dev.off()

png(file.path(graficos_dir, "curva_ajuste_plugfild_para_cr350.png"), width = 2100, height = 1400, res = 150)
layout(matrix(c(1, 2), nrow = 1), widths = c(4.3, 1.5))
par(mar = c(5.5, 5.6, 4.2, 1.0), las = 1)
plot(pares$umidade_plugfild_pct, pares$umidade_cr350_pct, type = "n", xlab = "Umidade do plugfild (%)", ylab = "CR350/CS625 referência (%)", main = "Ajustes do plugfild para o CR350")
grid(col = "gray88")
points(pares$umidade_plugfild_pct, pares$umidade_cr350_pct, pch = 19, col = rgb(214/255, 39/255, 40/255, 0.35), cex = 0.7)
abline(0, 1, col = "gray35", lwd = 2, lty = 2)
ord <- order(pares$umidade_plugfild_pct)
lines(pares$umidade_plugfild_pct[ord], predict(modelo_linear)[ord], col = cores["linear"], lwd = 3)
lines(pares$umidade_plugfild_pct[ord], predict(modelo_quadratico)[ord], col = cores["quadratico"], lwd = 3)
par(mar = c(5.5, 0.4, 4.2, 1.0))
plot.new()
legend("topleft", bty = "n", lwd = c(NA, 2, 3, 3), pch = c(19, NA, NA, NA), col = c(cores["original"], "gray35", cores["linear"], cores["quadratico"]), legend = c("Pares observados", "Linha 1:1", "Ajuste linear", "Ajuste quadrático"), cex = 0.95)
text(0, 0.48, adj = c(0, 1), cex = 0.82, col = "gray35", labels = paste0("Linear: R² = ", sprintf("%.3f", summary(modelo_linear)$r.squared), "\nQuadrático: R² = ", sprintf("%.3f", summary(modelo_quadratico)$r.squared)))
layout(1)
dev.off()

png(file.path(graficos_dir, "erro_melhorado_plugfild_calibracao_lag.png"), width = 2500, height = 1600, res = 150)
layout(matrix(c(1, 3, 2, 3), nrow = 2, byrow = TRUE), widths = c(4.6, 1.5), heights = c(1, 1))
erro_original_s <- media_movel(pares$erro_plugfild_original_pp, 5)
erro_linear_s <- media_movel(pares$erro_plugfild_linear_pp, 5)
erro_lag_s <- media_movel(pares_lag$erro_plugfild_lag_calibrado_pp, 5)
par(mar = c(2.2, 5.6, 4.0, 1.0), las = 1)
plot(pares$datahora, erro_original_s, type = "n", xaxt = "n", xlab = "", ylab = "Erro original (p.p.)", main = "A) plugfild original − CR350")
grid(col = "gray88"); lines(pares$datahora, erro_original_s, lwd = 2.5, col = cores["original"]); abline(h = 0, col = "gray35", lty = 2)
par(mar = c(5.5, 5.6, 3.0, 1.0), las = 1)
ylim_cal <- range(c(erro_linear_s, erro_lag_s), finite = TRUE)
plot(pares$datahora, erro_linear_s, type = "n", ylim = ylim_cal, xlab = "Data/hora local", ylab = "Erro calibrado (p.p.)", main = "B) calibração sem e com lag")
grid(col = "gray88"); lines(pares$datahora, erro_linear_s, lwd = 2.5, col = cores["linear"]); lines(pares_lag$datahora, erro_lag_s, lwd = 2.5, col = cores["lag"]); abline(h = 0, col = "gray35", lty = 2)
par(mar = c(5.5, 0.4, 4.0, 1.0))
plot.new()
legend("topleft", bty = "n", lwd = c(2.5, 2.5, 2.5, 2), col = c(cores["original"], cores["linear"], cores["lag"], "gray35"), legend = c("plugfild original", "plugfild calibrado", paste0("calibrado + lag ", melhor_lag$lag_min, " min"), "zero = igual ao CR350"), cex = 0.95)
text(0, 0.48, adj = c(0, 1), cex = 0.82, col = "gray35", labels = "Painéis com escalas próprias.\nCurvas suavizadas apenas para\nvisualização; métricas usam\ndados não suavizados.")
layout(1)
dev.off()

png(file.path(graficos_dir, "lag_correlacao_umidade.png"), width = 2100, height = 1300, res = 150)
layout(matrix(c(1, 2), nrow = 1), widths = c(4.3, 1.4))
par(mar = c(5.5, 5.8, 4.2, 1.0), las = 1)
yticks <- pretty(range(lag_df$pearson, finite = TRUE), n = 7)
plot(lag_df$lag_min, lag_df$pearson, type = "n", yaxt = "n", xlab = "Lag aplicado ao plugfild (min)", ylab = "Correlação de Pearson (adimensional)", main = "Correlação por lag")
axis(2, at = yticks, labels = sprintf("%.3f", yticks), las = 1)
grid(col = "gray88")
lines(lag_df$lag_min, lag_df$pearson, lwd = 2.5, col = cores["original"])
points(lag_df$lag_min, lag_df$pearson, pch = 19, cex = 0.75, col = cores["original"])
abline(v = 0, col = "gray50", lty = 2)
abline(v = melhor_lag$lag_min, col = cores["cr350"], lty = 3, lwd = 2)
par(mar = c(5.5, 0.4, 4.2, 1.0))
plot.new()
legend("topleft", bty = "n", lwd = c(2.5, 2), col = c(cores["original"], cores["cr350"]), legend = c("Pearson", paste0("Melhor lag: ", melhor_lag$lag_min, " min")), cex = 0.95)
text(0, 0.50, adj = c(0, 1), cex = 0.82, col = "gray35", labels = paste0("Pearson máximo: ", sprintf("%.4f", melhor_lag$pearson), "\nn = ", melhor_lag$n, " pares"))
layout(1)
dev.off()

# -----------------------------------------------------------------------------
# 6. Resumo TXT
# -----------------------------------------------------------------------------
resumo <- file.path(resumos_dir, "resumo_comparacao_umidade.txt")
cat(
  "Comparação de umidade do solo: plugfild calibrado contra CR350/CS625 de verificação\n",
  "===============================================================================\n\n",
  "Arquivos usados:\n",
  paste0("- CR350/CS625 de verificação: ", normalizePath(arq_cr350, winslash = "\\", mustWork = FALSE), "\n"),
  paste0("- plugfild a calibrar:            ", normalizePath(arq_plugfild, winslash = "\\", mustWork = FALSE), "\n\n"),
  "Período comum e disponibilidade:\n",
  paste0("- CR350: ", format(min(cr350$datahora), "%Y-%m-%d %H:%M:%S%z"), " a ", format(max(cr350$datahora), "%Y-%m-%d %H:%M:%S%z"), "\n"),
  paste0("- plugfild:  ", format(min(plugfild$datahora), "%Y-%m-%d %H:%M:%S%z"), " a ", format(max(plugfild$datahora), "%Y-%m-%d %H:%M:%S%z"), "\n"),
  paste0("- Pares válidos por timestamp exato: ", nrow(pares), "\n\n"),
  "Mínimos e máximos no período comum:\n",
  paste0("- CR350/CS625: mínimo ", sprintf("%.4f", min(pares$umidade_cr350_pct)), "%, máximo ", sprintf("%.4f", max(pares$umidade_cr350_pct)), "%, amplitude ", sprintf("%.4f", diff(range(pares$umidade_cr350_pct))), " p.p.\n"),
  paste0("- plugfild original: mínimo ", sprintf("%.4f", min(pares$umidade_plugfild_pct)), "%, máximo ", sprintf("%.4f", max(pares$umidade_plugfild_pct)), "%, amplitude ", sprintf("%.4f", diff(range(pares$umidade_plugfild_pct))), " p.p.\n"),
  "- Observação: a amplitude observada é limitada; portanto estes coeficientes são empíricos para este intervalo e não devem ser extrapolados sem novos dados.\n\n",
  "Estatísticas antes da calibração do plugfild:\n",
  paste0("- Pearson plugfild vs CR350: ", sprintf("%.4f", m_original$pearson), "\n"),
  paste0("- Spearman plugfild vs CR350: ", sprintf("%.4f", m_original$spearman), "\n"),
  paste0("- R²: ", sprintf("%.4f", m_original$r2), "\n"),
  paste0("- Bias plugfild - CR350: ", sprintf("%.4f", m_original$bias_pp), " pontos percentuais\n"),
  paste0("- MAE: ", sprintf("%.4f", m_original$mae_pp), " pontos percentuais\n"),
  paste0("- RMSE: ", sprintf("%.4f", m_original$rmse_pp), " pontos percentuais\n\n"),
  "Calibração empírica linear aplicada ao plugfild:\n",
  paste0("- Fórmula: umidade_plugfild_calibrada_pct = ", sprintf("%.6f", coef_lin[1]), " + ", sprintf("%.6f", coef_lin[2]), " × umidade_plugfild_pct\n"),
  paste0("- R² do modelo linear: ", sprintf("%.4f", summary(modelo_linear)$r.squared), "\n\n"),
  "Curva de ajuste adicional testada:\n",
  paste0("- Fórmula quadrática: umidade_plugfild_ajustada_pct = ", sprintf("%.6f", coef_quad[1]), " + ", sprintf("%.6f", coef_quad[2]), " × plugfild + ", sprintf("%.6f", coef_quad[3]), " × plugfild²\n"),
  paste0("- R² do modelo quadrático: ", sprintf("%.4f", summary(modelo_quadratico)$r.squared), "\n"),
  paste0("- RMSE quadrático no mesmo conjunto: ", sprintf("%.4f", m_quadratico$rmse_pp), " pontos percentuais\n\n"),
  "Estatísticas depois da calibração do plugfild:\n",
  paste0("- Linear sem lag: Bias ", sprintf("%.4f", m_linear$bias_pp), " p.p.; MAE ", sprintf("%.4f", m_linear$mae_pp), " p.p.; RMSE ", sprintf("%.4f", m_linear$rmse_pp), " p.p.\n"),
  paste0("- Quadrática sem lag: Bias ", sprintf("%.4f", m_quadratico$bias_pp), " p.p.; MAE ", sprintf("%.4f", m_quadratico$mae_pp), " p.p.; RMSE ", sprintf("%.4f", m_quadratico$rmse_pp), " p.p.\n"),
  paste0("- Linear com melhor lag: Bias ", sprintf("%.4f", m_lag_linear$bias_pp), " p.p.; MAE ", sprintf("%.4f", m_lag_linear$mae_pp), " p.p.; RMSE ", sprintf("%.4f", m_lag_linear$rmse_pp), " p.p.\n\n"),
  "Análise de lag:\n",
  paste0("- Melhor lag por |Pearson|: ", melhor_lag$lag_min, " min; Pearson=", sprintf("%.4f", melhor_lag$pearson), "; n=", melhor_lag$n, "\n"),
  "- Na série temporal melhorada, a curva 'plugfild calibrado + lag' usa esse deslocamento temporal e depois aplica a calibração linear ajustada nesse conjunto com lag.\n\n",
  "Arquivos gerados:\n",
  paste0("- Tabelas: ", normalizePath(tabelas_dir, winslash = "\\", mustWork = FALSE), "\n"),
  paste0("- Gráficos PNG: ", normalizePath(graficos_dir, winslash = "\\", mustWork = FALSE), "\n"),
  paste0("- Resumos: ", normalizePath(resumos_dir, winslash = "\\", mustWork = FALSE), "\n"),
  file = resumo, sep = ""
)

cat("OK - análise concluída\n")
cat("Melhor lag:", melhor_lag$lag_min, "min\n")
cat("Resumo:", resumo, "\n")
