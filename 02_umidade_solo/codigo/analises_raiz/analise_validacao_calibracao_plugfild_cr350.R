# Validação temporal e diagnósticos: plugfild x CR350/CS625
# plugfild = sensor a calibrar; CR350/CS625 = referência sem transformação.
# Usa somente base R e grava resultados/validacao_calibracao_plugfild_cr350.

options(stringsAsFactors = FALSE)
base_dir <- getwd()
entrada_dir <- file.path(base_dir, "dados", "entradas_analise", "plugfild_pc01")
saida_dir <- file.path(base_dir, "resultados", "validacao_calibracao_plugfild_cr350")
tabelas_dir <- file.path(saida_dir, "tabelas")
graficos_dir <- file.path(saida_dir, "graficos")
resumos_dir <- file.path(saida_dir, "resumos")
dir.create(tabelas_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(graficos_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(resumos_dir, recursive = TRUE, showWarnings = FALSE)
for (f in c(list.files(tabelas_dir, full.names = TRUE), list.files(graficos_dir, full.names = TRUE), list.files(resumos_dir, full.names = TRUE))) {
  if (file.exists(f)) unlink(f)
}

tz_local <- "America/Sao_Paulo"
ler_num <- function(x) suppressWarnings(as.numeric(gsub(",", ".", as.character(x), fixed = TRUE)))
cor_segura <- function(x, y, metodo = "pearson") {
  ok <- is.finite(x) & is.finite(y)
  if (sum(ok) < 3 || sd(x[ok]) == 0 || sd(y[ok]) == 0) return(NA_real_)
  cor(x[ok], y[ok], method = metodo)
}
metricas <- function(obs, ref) {
  ok <- is.finite(obs) & is.finite(ref)
  d <- obs[ok] - ref[ok]
  p <- cor_segura(obs[ok], ref[ok], "pearson")
  data.frame(
    n = sum(ok), bias_pp = mean(d), mae_pp = mean(abs(d)),
    rmse_pp = sqrt(mean(d^2)), sd_diferenca_pp = sd(d),
    pearson = p, spearman = cor_segura(obs[ok], ref[ok], "spearman"),
    r2 = ifelse(is.finite(p), p^2, NA_real_)
  )
}

# 1. Leitura e pareamento exato ------------------------------------------------
arq_cr <- file.path(entrada_dir, "CR350Series_TabelaSolo_limpo.csv")
arq_plugfild <- file.path(entrada_dir, "plugfild_25334889_soil_moisture_238_intervalo_TabelaSolo_local.csv")
cr <- read.csv(arq_cr, check.names = FALSE, fileEncoding = "UTF-8-BOM")
plugfild <- read.csv(arq_plugfild, check.names = FALSE, fileEncoding = "UTF-8-BOM")
cr$datahora <- as.POSIXct(cr$TIMESTAMP, format = "%Y-%m-%d %H:%M:%S", tz = tz_local)
cr$umidade_cr350_pct <- ler_num(cr$VWC_m3_m3) * 100
plugfild$datahora <- as.POSIXct(plugfild$datahora_local, format = "%Y-%m-%d %H:%M:%S%z", tz = tz_local)
plugfild$umidade_plugfild_pct <- ler_num(plugfild$leitura)
cr_cmp <- aggregate(umidade_cr350_pct ~ datahora, cr, mean, na.rm = TRUE)
plugfild_cmp <- aggregate(umidade_plugfild_pct ~ datahora, plugfild, mean, na.rm = TRUE)
pares <- merge(cr_cmp, plugfild_cmp, by = "datahora", all = FALSE)
pares <- pares[order(pares$datahora), ]
pares <- pares[is.finite(pares$umidade_cr350_pct) & is.finite(pares$umidade_plugfild_pct), ]
if (nrow(pares) < 20) stop("Pares insuficientes para validação temporal.")

# 2. Validação temporal 70/30 --------------------------------------------------
corte_indice <- floor(nrow(pares) * 0.70)
treino <- pares[seq_len(corte_indice), ]
teste <- pares[(corte_indice + 1):nrow(pares), ]
inicio_validacao <- teste$datahora[1]
mod_lin_treino <- lm(umidade_cr350_pct ~ umidade_plugfild_pct, data = treino)
mod_quad_treino <- lm(umidade_cr350_pct ~ umidade_plugfild_pct + I(umidade_plugfild_pct^2), data = treino)
pares$estimativa_linear_treino70_pct <- predict(mod_lin_treino, newdata = pares)
pares$estimativa_quadratica_treino70_pct <- predict(mod_quad_treino, newdata = pares)
pares$conjunto <- ifelse(seq_len(nrow(pares)) <= corte_indice, "treino_70", "validacao_30")
pares$residuo_linear_treino70_pp <- pares$estimativa_linear_treino70_pct - pares$umidade_cr350_pct
pares$residuo_quadratico_treino70_pp <- pares$estimativa_quadratica_treino70_pct - pares$umidade_cr350_pct

validacao <- rbind(
  cbind(conjunto = "treino_70", modelo = "plugfild_original", metricas(treino$umidade_plugfild_pct, treino$umidade_cr350_pct)),
  cbind(conjunto = "validacao_30", modelo = "plugfild_original", metricas(teste$umidade_plugfild_pct, teste$umidade_cr350_pct)),
  cbind(conjunto = "validacao_30", modelo = "linear_treino70", metricas(predict(mod_lin_treino, newdata = teste), teste$umidade_cr350_pct)),
  cbind(conjunto = "validacao_30", modelo = "quadratico_treino70", metricas(predict(mod_quad_treino, newdata = teste), teste$umidade_cr350_pct))
)
write.csv(validacao, file.path(tabelas_dir, "validacao_temporal_70_30.csv"), row.names = FALSE)
write.csv(pares, file.path(tabelas_dir, "pares_validacao_residuos.csv"), row.names = FALSE)

# 3. Ajuste completo para diagnósticos -----------------------------------------
mod_lin_completo <- lm(umidade_cr350_pct ~ umidade_plugfild_pct, data = pares)
pares$plugfild_calibrado_linear_pct <- predict(mod_lin_completo, newdata = pares)
pares$residuo_linear_completo_pp <- pares$plugfild_calibrado_linear_pct - pares$umidade_cr350_pct

# 4. CR350 médio em janela de ±2 min -------------------------------------------
janela <- lapply(seq_len(nrow(plugfild_cmp)), function(i) {
  t <- plugfild_cmp$datahora[i]
  idx <- which(cr_cmp$datahora >= t - 120 & cr_cmp$datahora <= t + 120)
  if (!length(idx)) return(NULL)
  data.frame(
    datahora = t,
    umidade_plugfild_pct = plugfild_cmp$umidade_plugfild_pct[i],
    umidade_cr350_media_mais_menos_2min_pct = mean(cr_cmp$umidade_cr350_pct[idx]),
    n_cr350_na_janela = length(idx)
  )
})
pares_janela <- do.call(rbind, janela)
mod_janela <- lm(umidade_cr350_media_mais_menos_2min_pct ~ umidade_plugfild_pct, data = pares_janela)
pares_janela$plugfild_calibrado_linear_pct <- predict(mod_janela, newdata = pares_janela)
comp_janela <- rbind(
  cbind(comparacao = "timestamp_exato", modelo = "plugfild_original", metricas(pares$umidade_plugfild_pct, pares$umidade_cr350_pct)),
  cbind(comparacao = "timestamp_exato", modelo = "plugfild_calibrado_linear", metricas(pares$plugfild_calibrado_linear_pct, pares$umidade_cr350_pct)),
  cbind(comparacao = "CR350_media_mais_menos_2min", modelo = "plugfild_original", metricas(pares_janela$umidade_plugfild_pct, pares_janela$umidade_cr350_media_mais_menos_2min_pct)),
  cbind(comparacao = "CR350_media_mais_menos_2min", modelo = "plugfild_calibrado_linear", metricas(pares_janela$plugfild_calibrado_linear_pct, pares_janela$umidade_cr350_media_mais_menos_2min_pct))
)
write.csv(pares_janela, file.path(tabelas_dir, "pares_cr350_janela_mais_menos_2min.csv"), row.names = FALSE)
write.csv(comp_janela, file.path(tabelas_dir, "comparacao_janela_cr350_mais_menos_2min.csv"), row.names = FALSE)

# 5. Estabilidade dos coeficientes em janelas de 12 h e 24 h ------------------
ajustar_janelas <- function(horas) {
  inicios <- seq(min(pares$datahora), max(pares$datahora), by = horas * 3600)
  saida <- lapply(inicios, function(inicio) {
    fim <- inicio + horas * 3600
    d <- pares[pares$datahora >= inicio & pares$datahora < fim, ]
    if (nrow(d) < 6 || sd(d$umidade_plugfild_pct) == 0) {
      return(data.frame(janela_horas = horas, inicio = format(inicio, "%Y-%m-%d %H:%M:%S%z"), fim = format(fim, "%Y-%m-%d %H:%M:%S%z"), n = nrow(d), intercepto_pct = NA_real_, inclinacao = NA_real_, rmse_pp = NA_real_, r2 = NA_real_))
    }
    m <- lm(umidade_cr350_pct ~ umidade_plugfild_pct, data = d)
    pred <- predict(m, newdata = d)
    data.frame(
      janela_horas = horas, inicio = format(inicio, "%Y-%m-%d %H:%M:%S%z"),
      fim = format(fim, "%Y-%m-%d %H:%M:%S%z"), n = nrow(d),
      intercepto_pct = coef(m)[1], inclinacao = coef(m)[2],
      rmse_pp = sqrt(mean((pred - d$umidade_cr350_pct)^2)), r2 = summary(m)$r.squared
    )
  })
  do.call(rbind, saida)
}
estabilidade <- rbind(ajustar_janelas(12), ajustar_janelas(24))
write.csv(estabilidade, file.path(tabelas_dir, "estabilidade_coeficientes_janelas.csv"), row.names = FALSE)

# 6. Fases de umedecimento/secagem --------------------------------------------
delta_cr350 <- c(NA_real_, diff(pares$umidade_cr350_pct))
limiar_fase_pp <- 0.05
pares$fase <- ifelse(delta_cr350 > limiar_fase_pp, "umedecendo_CR350_subindo", ifelse(delta_cr350 < -limiar_fase_pp, "secando_CR350_descendo", "estavel"))
fases <- do.call(rbind, lapply(c("umedecendo_CR350_subindo", "secando_CR350_descendo", "estavel"), function(fase) {
  d <- pares[pares$fase == fase, ]
  cbind(fase = fase, serie = "plugfild_calibrado_linear", metricas(d$plugfild_calibrado_linear_pct, d$umidade_cr350_pct))
}))
write.csv(fases, file.path(tabelas_dir, "metricas_por_fase_secagem_umedecimento.csv"), row.names = FALSE)

# 7. Quantis e disponibilidade -------------------------------------------------
resumo_quantis <- function(nome, x) {
  q <- quantile(x, probs = c(0, .05, .25, .5, .75, .95, 1), na.rm = TRUE, names = FALSE)
  data.frame(serie = nome, min = q[1], q05 = q[2], q25 = q[3], mediana = q[4], q75 = q[5], q95 = q[6], max = q[7], amplitude = q[7] - q[1])
}
quantis <- rbind(
  resumo_quantis("CR350/CS625", pares$umidade_cr350_pct),
  resumo_quantis("plugfild_original", pares$umidade_plugfild_pct),
  resumo_quantis("plugfild_calibrado_linear", pares$plugfild_calibrado_linear_pct)
)
write.csv(quantis, file.path(tabelas_dir, "quantis_faixa_umidade.csv"), row.names = FALSE)

inicio_total <- min(c(cr_cmp$datahora, plugfild_cmp$datahora))
fim_total <- max(c(cr_cmp$datahora, plugfild_cmp$datahora))
esperados_5min <- floor(as.numeric(difftime(fim_total, inicio_total, units = "mins")) / 5) + 1
disponibilidade <- data.frame(
  serie = c("CR350/CS625", "plugfild"), periodo = "intervalo_completo_novo_dat",
  intervalo_esperado_min = 5, esperados = esperados_5min,
  observados = c(length(unique(cr_cmp$datahora)), length(unique(plugfild_cmp$datahora)))
)
disponibilidade$ausentes <- disponibilidade$esperados - disponibilidade$observados
disponibilidade$disponibilidade_pct <- 100 * disponibilidade$observados / disponibilidade$esperados
write.csv(disponibilidade, file.path(tabelas_dir, "disponibilidade_temporal.csv"), row.names = FALSE)

# 8. Gráficos ------------------------------------------------------------------
cores <- c(cr350 = "#1f77b4", plugfild = "#ff7f0e", linear = "#2ca02c", residuo = "#7b3294")

png(file.path(graficos_dir, "validacao_temporal_70_30.png"), width = 2500, height = 1400, res = 150)
layout(matrix(c(1, 2), nrow = 1), widths = c(4.6, 1.5))
par(mar = c(5.5, 5.6, 4.2, 1.0), las = 1)
ylim_val <- range(c(pares$umidade_cr350_pct, pares$estimativa_quadratica_treino70_pct), finite = TRUE)
plot(pares$datahora, pares$umidade_cr350_pct, type = "n", ylim = ylim_val, xlab = "Data/hora local", ylab = "Umidade volumétrica do solo (%)", main = "Validação temporal 70/30")
usr <- par("usr")
rect(inicio_validacao, usr[3], max(pares$datahora), usr[4], col = "gray92", border = NA)
grid(col = "gray84")
lines(pares$datahora, pares$umidade_cr350_pct, col = cores["cr350"], lwd = 3)
lines(pares$datahora, pares$estimativa_quadratica_treino70_pct, col = cores["plugfild"], lwd = 2.7)
abline(v = inicio_validacao, lty = 2, col = "gray35", lwd = 2)
par(mar = c(5.5, 0.4, 4.2, 1.0))
plot.new()
legend("topleft", bty = "n", col = c(cores["cr350"], cores["plugfild"], "gray35", "gray80"), lwd = c(3, 2.7, 2, 8), lty = c(1, 1, 2, 1), legend = c("CR350/CS625 referência", "plugfild com ajuste quadrático", "corte 70/30", "validação: 30% final"), cex = 0.92)
text(0, 0.46, adj = c(0, 1), cex = 0.82, col = "gray35", labels = paste0("Treino: ", nrow(treino), " pares\nValidação: ", nrow(teste), " pares\nInício da validação:\n", format(inicio_validacao, "%Y-%m-%d %H:%M%z")))
layout(1)
dev.off()

png(file.path(graficos_dir, "residuos_tempo_plugfild_calibrado.png"), width = 2200, height = 1200, res = 150)
par(mar = c(5.5, 5.8, 4.2, 2.0), las = 1)
plot(pares$datahora, pares$residuo_linear_completo_pp, type = "n", xlab = "Data/hora local", ylab = "Resíduo: plugfild calibrado − CR350 (p.p.)", main = "Resíduos da calibração linear no tempo")
grid(col = "gray88"); points(pares$datahora, pares$residuo_linear_completo_pp, pch = 19, cex = 0.45, col = rgb(123/255, 50/255, 148/255, 0.45)); abline(h = 0, lty = 2, lwd = 2, col = "gray35")
dev.off()

png(file.path(graficos_dir, "residuos_vs_umidade_cr350.png"), width = 1900, height = 1300, res = 150)
par(mar = c(5.5, 5.8, 4.2, 2.0), las = 1)
plot(pares$umidade_cr350_pct, pares$residuo_linear_completo_pp, type = "n", xlab = "CR350/CS625 referência (%)", ylab = "Resíduo: plugfild calibrado − CR350 (p.p.)", main = "Resíduos por umidade de referência")
grid(col = "gray88"); points(pares$umidade_cr350_pct, pares$residuo_linear_completo_pp, pch = 19, cex = 0.55, col = rgb(123/255, 50/255, 148/255, 0.40)); abline(h = 0, lty = 2, lwd = 2, col = "gray35")
dev.off()

est24 <- estabilidade[estabilidade$janela_horas == 24, ]
datas24 <- as.POSIXct(est24$inicio, format = "%Y-%m-%d %H:%M:%S%z", tz = tz_local)
png(file.path(graficos_dir, "estabilidade_coeficientes_24h.png"), width = 2200, height = 1500, res = 150)
par(mfrow = c(2, 1), mar = c(2.2, 5.8, 4.0, 2.0), las = 1)
plot(datas24, est24$intercepto_pct, type = "n", xaxt = "n", xlab = "", ylab = "Intercepto (%)", main = "A) Intercepto em janelas de 24 h")
grid(col = "gray88"); lines(datas24, est24$intercepto_pct, type = "b", pch = 19, lwd = 2.5, col = cores["cr350"])
par(mar = c(5.5, 5.8, 3.0, 2.0), las = 1)
plot(datas24, est24$inclinacao, type = "n", xlab = "Início da janela (data/hora local)", ylab = "Inclinação (adimensional)", main = "B) Inclinação em janelas de 24 h")
grid(col = "gray88"); lines(datas24, est24$inclinacao, type = "b", pch = 19, lwd = 2.5, col = cores["linear"])
dev.off()

png(file.path(graficos_dir, "disponibilidade_temporal.png"), width = 1700, height = 1100, res = 150)
par(mar = c(5.5, 6.0, 4.2, 2.0), las = 1)
bp <- barplot(disponibilidade$disponibilidade_pct, names.arg = disponibilidade$serie, ylim = c(0, 112), col = c(cores["cr350"], cores["linear"]), ylab = "Disponibilidade no intervalo (%)", main = "Disponibilidade temporal")
grid(nx = NA, ny = NULL, col = "gray88"); abline(h = 100, lty = 2, col = "gray35")
text(bp, 106, labels = sprintf("%.2f%%", disponibilidade$disponibilidade_pct), cex = 0.9)
text(bp, 103, labels = sprintf("%d/%d registros", disponibilidade$observados, disponibilidade$esperados), cex = 0.82)
dev.off()

# 9. Resumo --------------------------------------------------------------------
m_original_val <- metricas(teste$umidade_plugfild_pct, teste$umidade_cr350_pct)
m_lin_val <- metricas(predict(mod_lin_treino, newdata = teste), teste$umidade_cr350_pct)
m_quad_val <- metricas(predict(mod_quad_treino, newdata = teste), teste$umidade_cr350_pct)
coef_lin <- coef(mod_lin_completo)
resumo <- file.path(resumos_dir, "resumo_validacao_calibracao_plugfild.txt")
cat(
  "Validação e diagnósticos da calibração plugfild → CR350/CS625\n",
  "================================================================\n\n",
  paste0("Período comum: ", format(min(pares$datahora), "%Y-%m-%d %H:%M:%S%z"), " a ", format(max(pares$datahora), "%Y-%m-%d %H:%M:%S%z"), "\n"),
  paste0("Pares exatos: ", nrow(pares), "\n"),
  paste0("Treino inicial (70%): ", nrow(treino), " pares\n"),
  paste0("Validação final (30%): ", nrow(teste), " pares; início em ", format(inicio_validacao, "%Y-%m-%d %H:%M:%S%z"), "\n\n"),
  "Validação nos 30% finais:\n",
  paste0("- plugfild original: MAE=", sprintf("%.4f", m_original_val$mae_pp), " p.p.; RMSE=", sprintf("%.4f", m_original_val$rmse_pp), " p.p.; bias=", sprintf("%.4f", m_original_val$bias_pp), " p.p.\n"),
  paste0("- ajuste linear do treino: MAE=", sprintf("%.4f", m_lin_val$mae_pp), " p.p.; RMSE=", sprintf("%.4f", m_lin_val$rmse_pp), " p.p.; bias=", sprintf("%.4f", m_lin_val$bias_pp), " p.p.\n"),
  paste0("- ajuste quadrático do treino: MAE=", sprintf("%.4f", m_quad_val$mae_pp), " p.p.; RMSE=", sprintf("%.4f", m_quad_val$rmse_pp), " p.p.; bias=", sprintf("%.4f", m_quad_val$bias_pp), " p.p.\n\n"),
  "Ajuste linear no conjunto completo para diagnósticos:\n",
  paste0("- CR350_estimado = ", sprintf("%.6f", coef_lin[1]), " + ", sprintf("%.6f", coef_lin[2]), " × plugfild_original\n"),
  paste0("- disponibilidade CR350: ", sprintf("%.2f", disponibilidade$disponibilidade_pct[1]), "%\n"),
  paste0("- disponibilidade plugfild: ", sprintf("%.2f", disponibilidade$disponibilidade_pct[2]), "% (", disponibilidade$ausentes[2], " timestamps ausentes)\n"),
  paste0("- faixa CR350: ", sprintf("%.4f", quantis$min[1]), "% a ", sprintf("%.4f", quantis$max[1]), "%\n"),
  paste0("- faixa plugfild: ", sprintf("%.4f", quantis$min[2]), "% a ", sprintf("%.4f", quantis$max[2]), "%\n\n"),
  "Observações:\n",
  "- O CR350/CS625 permanece como referência; somente o plugfild é transformado.\n",
  "- A faixa cinza do gráfico 70/30 representa os 30% finais usados na validação.\n",
  "- Coeficientes são empíricos para este período e não devem ser extrapolados sem novos dados.\n",
  "- Métricas utilizam valores não suavizados.\n",
  file = resumo, sep = ""
)

cat("OK - validação R concluída\n")
cat("Pares:", nrow(pares), "| treino:", nrow(treino), "| validação:", nrow(teste), "\n")
cat("Resumo:", resumo, "\n")
