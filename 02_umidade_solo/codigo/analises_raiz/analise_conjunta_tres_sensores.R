# Análise conjunta de umidade do solo: CS625, plugfild e PC03/azul
#
# Fonte única: TOA5 TableEnvio do CR350.
#   UmidSolo     = VWC calculada pelo CS/CR350, referência de campo desta análise.
#   PeriodoCS625 = período bruto em µs, preservado como diagnóstico.
#   UmidPlug     = plugfild; UmidAzul = azul/PC03.
#
# Corte operacional: a análise começa no primeiro UmidPlug válido após a primeira
# lacuna contínua de pelo menos 60 min. Isso remove o trecho inicial anterior ao
# início contínuo do plugfild de TODAS as saídas, modelos, lags e gráficos.
# A calibração laboratorial gravimétrica permanece separada.

options(stringsAsFactors = FALSE)

args <- commandArgs(trailingOnly = FALSE)
script_arg <- args[grep("^--file=", args)]
base_dir <- if (length(script_arg)) {
  normalizePath(file.path(dirname(sub("^--file=", "", script_arg[1])), "."), winslash = "/")
} else normalizePath(getwd(), winslash = "/")
tz_local <- "America/Sao_Paulo"
arq_toa5 <- file.path(base_dir, "dados", "cr350_cs625", "brutos", "CR350Series_TableEnvio-2.dat")

saida_dir <- file.path(base_dir, "resultados", "analise_conjunta_tres_sensores")
tabelas_dir <- file.path(saida_dir, "tabelas")
graficos_dir <- file.path(saida_dir, "graficos")
resumos_dir <- file.path(saida_dir, "resumos")
for (d in c(tabelas_dir, graficos_dir, resumos_dir)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
# Evita mistura com artefatos de execuções anteriores; a árvore histórica fica em resultados_antigos/.
for (f in list.files(saida_dir, recursive = TRUE, full.names = TRUE)) {
  if (grepl("\\.(csv|txt|png)$", f, ignore.case = TRUE)) unlink(f)
}

ler_num <- function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("", "NAN", "NaN", "NA")] <- NA_character_
  suppressWarnings(as.numeric(gsub(",", ".", x, fixed = TRUE)))
}
metricas <- function(est, ref) {
  ok <- is.finite(est) & is.finite(ref)
  est <- est[ok]; ref <- ref[ok]
  erro <- est - ref
  p <- if (length(est) >= 3 && sd(est) > 0 && sd(ref) > 0) cor(est, ref, method = "pearson") else NA_real_
  s <- if (length(est) >= 3 && sd(est) > 0 && sd(ref) > 0) cor(est, ref, method = "spearman") else NA_real_
  data.frame(
    n = length(est), bias_pp = mean(erro), mae_pp = mean(abs(erro)),
    rmse_pp = sqrt(mean(erro^2)), sd_erro_pp = sd(erro),
    pearson = p, spearman = s, r2_associacao = p^2
  )
}
resumo_serie <- function(nome, x) {
  x <- x[is.finite(x)]
  data.frame(
    sensor = nome, n = length(x), minimo_pct = min(x), q05_pct = unname(quantile(x, .05)),
    mediana_pct = median(x), media_pct = mean(x), q95_pct = unname(quantile(x, .95)),
    maximo_pct = max(x), desvio_padrao_pct = sd(x), amplitude_pp = diff(range(x))
  )
}

detectar_inicio_plug <- function(x, tempos, lacuna_minima = 60L) {
  valido <- is.finite(x)
  i <- 1L
  while (i <= length(valido)) {
    if (!valido[i]) {
      ini <- i
      while (i < length(valido) && !valido[i + 1L]) i <- i + 1L
      fim <- i
      if ((fim - ini + 1L) >= lacuna_minima && fim < length(valido)) {
        prox <- fim + 1L
        if (valido[prox]) return(list(indice = prox, inicio = tempos[prox], lacuna_registros = fim - ini + 1L))
      }
    }
    i <- i + 1L
  }
  primeiro <- which(valido)[1]
  if (is.na(primeiro)) stop("UmidPlug não tem nenhuma leitura válida.")
  list(indice = primeiro, inicio = tempos[primeiro], lacuna_registros = 0L)
}

# 1. Leitura TOA5, corte operacional e pareamento exato ------------------------
if (!file.exists(arq_toa5)) stop("TOA5 não encontrado: ", arq_toa5)
toa5 <- read.csv(arq_toa5, skip = 1, header = TRUE, check.names = FALSE,
                 na.strings = c("", "NAN", "NaN", "NA"), quote = "\"", fileEncoding = "UTF-8-BOM")
necessarias <- c("TIMESTAMP", "RECORD", "UmidSolo", "PeriodoCS625", "UmidPlug", "UmidAzul")
faltantes <- setdiff(necessarias, names(toa5))
if (length(faltantes)) stop("Colunas ausentes no TOA5: ", paste(faltantes, collapse = ", "))
toa5$datahora <- as.POSIXct(trimws(toa5$TIMESTAMP), format = "%Y-%m-%d %H:%M:%S", tz = tz_local)
for (nm in c("RECORD", "UmidSolo", "PeriodoCS625", "UmidPlug", "TempSoloPlug", "UmidAzul", "TempSoloAzul", "BatteryV")) {
  if (nm %in% names(toa5)) toa5[[nm]] <- ler_num(toa5[[nm]])
}
toa5 <- toa5[!is.na(toa5$datahora) & is.finite(toa5$RECORD), ]
toa5 <- toa5[order(toa5$datahora), ]
if (!nrow(toa5)) stop("Nenhum registro TOA5 válido após remover linhas descritivas.")

corte <- detectar_inicio_plug(toa5$UmidPlug, toa5$datahora, lacuna_minima = 60L)
inicio_analise <- corte$inicio
toa5 <- toa5[toa5$datahora >= inicio_analise, ]
if (!nrow(toa5)) stop("O corte do plugfild eliminou todos os registros.")

cr <- data.frame(datahora = toa5$datahora, umidade_cr350_cs625_pct = toa5$UmidSolo, periodo_cs625_us = toa5$PeriodoCS625)
plug <- data.frame(datahora = toa5$datahora, umidade_plugfild_pct = toa5$UmidPlug)
azul <- data.frame(datahora = toa5$datahora, umidade_azul_pc03_pct = toa5$UmidAzul)
cr <- cr[is.finite(cr$umidade_cr350_cs625_pct), c("datahora", "umidade_cr350_cs625_pct", "periodo_cs625_us")]
plug <- plug[is.finite(plug$umidade_plugfild_pct), ]
azul <- azul[is.finite(azul$umidade_azul_pc03_pct), ]
cr <- aggregate(cbind(umidade_cr350_cs625_pct, periodo_cs625_us) ~ datahora, data = cr, FUN = mean, na.rm = TRUE)
plug <- aggregate(umidade_plugfild_pct ~ datahora, data = plug, FUN = mean, na.rm = TRUE)
azul <- aggregate(umidade_azul_pc03_pct ~ datahora, data = azul, FUN = mean, na.rm = TRUE)

alinhados <- Reduce(function(x, y) merge(x, y, by = "datahora", all = TRUE), list(cr, plug, azul))
alinhados <- alinhados[order(alinhados$datahora), ]
pares <- alinhados[complete.cases(alinhados[, c("umidade_cr350_cs625_pct", "umidade_plugfild_pct", "umidade_azul_pc03_pct")]), ]
if (nrow(pares) < 20) stop("Menos de 20 pares completos após o corte do plugfild.")

fim_analise <- max(alinhados$datahora)
esperados <- floor(as.numeric(difftime(fim_analise, inicio_analise, units = "mins"))) + 1
corte_df <- data.frame(
  criterio = "primeiro UmidPlug válido após lacuna contínua >= 60 min",
  inicio_analise_local = format(inicio_analise, "%Y-%m-%d %H:%M:%S%z"),
  lacuna_anterior_minutos = corte$lacuna_registros,
  registros_toa5_apos_corte = nrow(toa5),
  stringsAsFactors = FALSE
)
disponibilidade <- data.frame(
  sensor = c("CS — UmidSolo", "plugfild — UmidPlug", "PC03/azul — UmidAzul"),
  registros = c(nrow(cr), nrow(plug), nrow(azul)),
  esperados_1min = esperados,
  disponibilidade_pct = 100 * c(nrow(cr), nrow(plug), nrow(azul)) / esperados,
  ausentes = esperados - c(nrow(cr), nrow(plug), nrow(azul))
)

saida_alinhados <- data.frame(
  datahora_local = format(alinhados$datahora, "%Y-%m-%d %H:%M:%S%z"),
  cs_umidsolo_pct = alinhados$umidade_cr350_cs625_pct,
  cs_periodo_us = alinhados$periodo_cs625_us,
  umidade_plugfild_pct = alinhados$umidade_plugfild_pct,
  umidade_azul_pc03_pct = alinhados$umidade_azul_pc03_pct
)
write.csv(saida_alinhados, file.path(tabelas_dir, "dados_tres_sensores_alinhados.csv"), row.names = FALSE)
write.csv(disponibilidade, file.path(tabelas_dir, "disponibilidade.csv"), row.names = FALSE)
write.csv(corte_df, file.path(tabelas_dir, "corte_inicio_plugfild.csv"), row.names = FALSE)

estatisticas <- rbind(
  resumo_serie("CS — UmidSolo", pares$umidade_cr350_cs625_pct),
  resumo_serie("plugfild — UmidPlug", pares$umidade_plugfild_pct),
  resumo_serie("PC03/azul — UmidAzul", pares$umidade_azul_pc03_pct)
)
write.csv(estatisticas, file.path(tabelas_dir, "estatisticas_descritivas.csv"), row.names = FALSE)

# 2. Associação, erro e ajustes empíricos ------------------------------------
mod_plug_lin <- lm(umidade_cr350_cs625_pct ~ umidade_plugfild_pct, data = pares)
mod_plug_quad <- lm(umidade_cr350_cs625_pct ~ umidade_plugfild_pct + I(umidade_plugfild_pct^2), data = pares)
mod_azul_lin <- lm(umidade_cr350_cs625_pct ~ umidade_azul_pc03_pct, data = pares)
mod_azul_quad <- lm(umidade_cr350_cs625_pct ~ umidade_azul_pc03_pct + I(umidade_azul_pc03_pct^2), data = pares)

pares$plugfild_linear_pct <- predict(mod_plug_lin, newdata = pares)
pares$plugfild_quadratico_pct <- predict(mod_plug_quad, newdata = pares)
pares$azul_linear_pct <- predict(mod_azul_lin, newdata = pares)
pares$azul_quadratico_pct <- predict(mod_azul_quad, newdata = pares)

metricas_completas <- rbind(
  cbind(sensor = "plugfild", modelo = "original", metricas(pares$umidade_plugfild_pct, pares$umidade_cr350_cs625_pct)),
  cbind(sensor = "plugfild", modelo = "linear_ajustado_mesmo_conjunto", metricas(pares$plugfild_linear_pct, pares$umidade_cr350_cs625_pct)),
  cbind(sensor = "plugfild", modelo = "quadratico_ajustado_mesmo_conjunto", metricas(pares$plugfild_quadratico_pct, pares$umidade_cr350_cs625_pct)),
  cbind(sensor = "PC03/azul", modelo = "original", metricas(pares$umidade_azul_pc03_pct, pares$umidade_cr350_cs625_pct)),
  cbind(sensor = "PC03/azul", modelo = "linear_ajustado_mesmo_conjunto", metricas(pares$azul_linear_pct, pares$umidade_cr350_cs625_pct)),
  cbind(sensor = "PC03/azul", modelo = "quadratico_ajustado_mesmo_conjunto", metricas(pares$azul_quadratico_pct, pares$umidade_cr350_cs625_pct))
)
write.csv(metricas_completas, file.path(tabelas_dir, "metricas_conjunto_completo.csv"), row.names = FALSE)

cp <- coef(mod_plug_lin); cpq <- coef(mod_plug_quad); ca <- coef(mod_azul_lin); caq <- coef(mod_azul_quad)
modelos <- data.frame(
  sensor = c("plugfild", "plugfild", "PC03/azul", "PC03/azul"),
  modelo = c("linear", "quadratico", "linear", "quadratico"),
  intercepto = c(cp[1], cpq[1], ca[1], caq[1]),
  coeficiente_linear = c(cp[2], cpq[2], ca[2], caq[2]),
  coeficiente_quadratico = c(NA_real_, cpq[3], NA_real_, caq[3]),
  r2_ajuste = c(summary(mod_plug_lin)$r.squared, summary(mod_plug_quad)$r.squared, summary(mod_azul_lin)$r.squared, summary(mod_azul_quad)$r.squared),
  n = nrow(pares)
)
write.csv(modelos, file.path(tabelas_dir, "modelos_empiricos_conjunto_completo.csv"), row.names = FALSE)

# Comparação direta adicional entre os dois sensores de campo.
comparacao_campo <- cbind(
  comparacao = "PC03_azul_menos_plugfild",
  metricas(pares$umidade_azul_pc03_pct, pares$umidade_plugfild_pct)
)
write.csv(comparacao_campo, file.path(tabelas_dir, "comparacao_direta_azul_plugfild.csv"), row.names = FALSE)

# 3. Validação cronológica 70/30 ---------------------------------------------
n <- nrow(pares)
n_treino <- floor(n * 0.70)
idx_treino <- seq_len(n_treino)
idx_validacao <- (n_treino + 1):n
treino <- pares[idx_treino, ]
validacao <- pares[idx_validacao, ]
inicio_validacao <- validacao$datahora[1]

vp_lin <- lm(umidade_cr350_cs625_pct ~ umidade_plugfild_pct, data = treino)
vp_quad <- lm(umidade_cr350_cs625_pct ~ umidade_plugfild_pct + I(umidade_plugfild_pct^2), data = treino)
va_lin <- lm(umidade_cr350_cs625_pct ~ umidade_azul_pc03_pct, data = treino)
va_quad <- lm(umidade_cr350_cs625_pct ~ umidade_azul_pc03_pct + I(umidade_azul_pc03_pct^2), data = treino)

pares$plugfild_validacao_linear_pct <- predict(vp_lin, newdata = pares)
pares$plugfild_validacao_quadratico_pct <- predict(vp_quad, newdata = pares)
pares$azul_validacao_linear_pct <- predict(va_lin, newdata = pares)
pares$azul_validacao_quadratico_pct <- predict(va_quad, newdata = pares)
pares$conjunto_70_30 <- ifelse(seq_len(n) <= n_treino, "treino_70", "validacao_30")

metricas_validacao <- rbind(
  cbind(sensor = "plugfild", modelo = "original", conjunto = "validacao_30", metricas(validacao$umidade_plugfild_pct, validacao$umidade_cr350_cs625_pct)),
  cbind(sensor = "plugfild", modelo = "linear_treino70", conjunto = "validacao_30", metricas(predict(vp_lin, newdata = validacao), validacao$umidade_cr350_cs625_pct)),
  cbind(sensor = "plugfild", modelo = "quadratico_treino70", conjunto = "validacao_30", metricas(predict(vp_quad, newdata = validacao), validacao$umidade_cr350_cs625_pct)),
  cbind(sensor = "PC03/azul", modelo = "original", conjunto = "validacao_30", metricas(validacao$umidade_azul_pc03_pct, validacao$umidade_cr350_cs625_pct)),
  cbind(sensor = "PC03/azul", modelo = "linear_treino70", conjunto = "validacao_30", metricas(predict(va_lin, newdata = validacao), validacao$umidade_cr350_cs625_pct)),
  cbind(sensor = "PC03/azul", modelo = "quadratico_treino70", conjunto = "validacao_30", metricas(predict(va_quad, newdata = validacao), validacao$umidade_cr350_cs625_pct))
)
write.csv(metricas_validacao, file.path(tabelas_dir, "validacao_temporal_70_30.csv"), row.names = FALSE)

saida_predicoes <- data.frame(
  datahora_local = format(pares$datahora, "%Y-%m-%d %H:%M:%S%z"),
  conjunto_70_30 = pares$conjunto_70_30,
  cs_umidsolo_referencia_pct = pares$umidade_cr350_cs625_pct,
  plugfild_original_pct = pares$umidade_plugfild_pct,
  plugfild_quadratico_completo_pct = pares$plugfild_quadratico_pct,
  residuo_plugfild_quadratico_completo_pp = pares$plugfild_quadratico_pct - pares$umidade_cr350_cs625_pct,
  plugfild_quadratico_treino70_pct = pares$plugfild_validacao_quadratico_pct,
  residuo_plugfild_quadratico_treino70_pp = pares$plugfild_validacao_quadratico_pct - pares$umidade_cr350_cs625_pct,
  azul_original_pct = pares$umidade_azul_pc03_pct,
  azul_quadratico_completo_pct = pares$azul_quadratico_pct,
  residuo_azul_quadratico_completo_pp = pares$azul_quadratico_pct - pares$umidade_cr350_cs625_pct,
  azul_quadratico_treino70_pct = pares$azul_validacao_quadratico_pct,
  residuo_azul_quadratico_treino70_pp = pares$azul_validacao_quadratico_pct - pares$umidade_cr350_cs625_pct
)
write.csv(saida_predicoes, file.path(tabelas_dir, "predicoes_e_residuos.csv"), row.names = FALSE)

# 4. Lag diagnóstico ----------------------------------------------------------
calcular_lag <- function(sensor_nome, sensor_df, coluna) {
  lags <- seq(-180, 180, by = 5)
  do.call(rbind, lapply(lags, function(lag_min) {
    s <- sensor_df[, c("datahora", coluna)]
    s$datahora <- s$datahora - lag_min * 60
    m <- merge(cr, s, by = "datahora", all = FALSE)
    nomes <- names(m)
    ref <- m$umidade_cr350_cs625_pct
    val <- m[[coluna]]
    p <- if (nrow(m) >= 3 && sd(ref) > 0 && sd(val) > 0) cor(val, ref) else NA_real_
    data.frame(sensor = sensor_nome, lag_min = lag_min, n = nrow(m), pearson = p, r2 = p^2)
  }))
}
lag_df <- rbind(
  calcular_lag("plugfild", plug, "umidade_plugfild_pct"),
  calcular_lag("PC03/azul", azul, "umidade_azul_pc03_pct")
)
melhor_lag <- do.call(rbind, lapply(split(lag_df, lag_df$sensor), function(d) d[which.max(abs(d$pearson)), ]))
write.csv(lag_df, file.path(tabelas_dir, "lag_correlacoes.csv"), row.names = FALSE)
write.csv(melhor_lag, file.path(tabelas_dir, "melhores_lags.csv"), row.names = FALSE)

# 5. Gráficos -----------------------------------------------------------------
cores <- c(ref = "#1F77B4", plug = "#D62728", azul = "#17A2B8", plug_cal = "#FF7F0E", azul_cal = "#9467BD")
data_ticks <- pretty(pares$datahora, n = 7)

# 01 - Séries brutas juntas: mesma grandeza e unidade.
png(file.path(graficos_dir, "01_series_brutas_tres_sensores.png"), width = 2500, height = 1400, res = 150)
layout(matrix(c(1, 2), 1), widths = c(4.7, 1.5))
par(mar = c(5.6, 5.8, 4.2, 1), las = 1)
y_raw <- range(c(pares$umidade_cr350_cs625_pct, pares$umidade_plugfild_pct, pares$umidade_azul_pc03_pct))
plot(pares$datahora, pares$umidade_cr350_cs625_pct, type = "n", xaxt = "n", ylim = y_raw,
     xlab = "Data/hora local", ylab = "Umidade volumétrica do solo (%)", main = "Séries brutas dos três sensores")
axis.POSIXct(1, at = data_ticks, format = "%d/%m\n%H:%M")
grid(col = "gray88")
lines(pares$datahora, pares$umidade_cr350_cs625_pct, col = cores["ref"], lwd = 3)
lines(pares$datahora, pares$umidade_plugfild_pct, col = cores["plug"], lwd = 2.4)
lines(pares$datahora, pares$umidade_azul_pc03_pct, col = cores["azul"], lwd = 2.4)
par(mar = c(5.6, .4, 4.2, 1)); plot.new()
legend("topleft", bty = "n", lwd = c(3, 2.4, 2.4), col = cores[c("ref", "plug", "azul")],
       legend = c("CS — UmidSolo referência", "plugfild original", "PC03/azul original"), cex = .95)
text(0, .48, adj = c(0, 1), cex = .82, col = "gray35", labels = "Mesmo período e unidade.\nDados não suavizados.\nPareamento por timestamp exato.")
layout(1); dev.off()

# 02 - Ajustes separados, pois as faixas dos sensores são diferentes.
png(file.path(graficos_dir, "02_ajustes_dos_sensores_vs_referencia.png"), width = 2800, height = 1400, res = 150)
layout(matrix(c(1, 2, 3), 1), widths = c(3.4, 3.4, 1.5))
plot_ajuste <- function(x, ref, mod_lin, mod_quad, titulo, xlab, cor) {
  par(mar = c(5.5, 5.5, 4.2, 1), las = 1)
  plot(x, ref, pch = 19, cex = .65, col = adjustcolor(cor, .32), xlab = xlab,
       ylab = "CS — UmidSolo referência (%)", main = titulo)
  grid(col = "gray88")
  abline(0, 1, lty = 2, lwd = 2, col = "gray45")
  ord <- order(x)
  lines(x[ord], predict(mod_lin)[ord], lwd = 3, col = "#2CA02C")
  lines(x[ord], predict(mod_quad)[ord], lwd = 3, col = "#FF7F0E")
}
plot_ajuste(pares$umidade_plugfild_pct, pares$umidade_cr350_cs625_pct, mod_plug_lin, mod_plug_quad, "plugfild vs referência", "plugfild (%)", cores["plug"])
plot_ajuste(pares$umidade_azul_pc03_pct, pares$umidade_cr350_cs625_pct, mod_azul_lin, mod_azul_quad, "PC03/azul vs referência", "PC03/azul (%)", cores["azul"])
par(mar = c(5.5, .4, 4.2, 1)); plot.new()
legend("topleft", bty = "n", pch = c(19, NA, NA, NA), lty = c(NA, 2, 1, 1), lwd = c(NA, 2, 3, 3),
       col = c("gray45", "gray45", "#2CA02C", "#FF7F0E"), legend = c("Pares observados", "Linha 1:1", "Ajuste linear", "Ajuste quadrático"), cex = .92)
text(0, .45, adj = c(0, 1), cex = .80, col = "gray35", labels = "Painéis separados porque\nas faixas originais diferem.\nA referência não é transformada.")
layout(1); dev.off()

# 03 - Predições quadráticas no mesmo eixo após transformação.
png(file.path(graficos_dir, "03_series_ajustadas_quadraticas.png"), width = 2500, height = 1400, res = 150)
layout(matrix(c(1, 2), 1), widths = c(4.7, 1.5))
par(mar = c(5.6, 5.8, 4.2, 1), las = 1)
y_cal <- range(c(pares$umidade_cr350_cs625_pct, pares$plugfild_quadratico_pct, pares$azul_quadratico_pct))
plot(pares$datahora, pares$umidade_cr350_cs625_pct, type = "n", xaxt = "n", ylim = y_cal,
     xlab = "Data/hora local", ylab = "Umidade volumétrica do solo (%)", main = "Ajustes quadráticos no conjunto completo")
axis.POSIXct(1, at = data_ticks, format = "%d/%m\n%H:%M"); grid(col = "gray88")
lines(pares$datahora, pares$umidade_cr350_cs625_pct, col = cores["ref"], lwd = 3)
lines(pares$datahora, pares$plugfild_quadratico_pct, col = cores["plug_cal"], lwd = 2.5)
lines(pares$datahora, pares$azul_quadratico_pct, col = cores["azul_cal"], lwd = 2.5)
par(mar = c(5.6, .4, 4.2, 1)); plot.new()
legend("topleft", bty = "n", lwd = c(3, 2.5, 2.5), col = cores[c("ref", "plug_cal", "azul_cal")],
       legend = c("CS — UmidSolo referência", "plugfild ajustado", "PC03/azul ajustado"), cex = .95)
text(0, .45, adj = c(0, 1), cex = .80, col = "gray35", labels = "Ajustes no mesmo conjunto:\ndescrição, não validação externa.\nSéries não suavizadas.")
layout(1); dev.off()

# 04 - Resíduos separados para evitar sobreposição.
png(file.path(graficos_dir, "04_residuos_quadraticos_no_tempo.png"), width = 2500, height = 1600, res = 150)
res_plug <- pares$plugfild_quadratico_pct - pares$umidade_cr350_cs625_pct
res_azul <- pares$azul_quadratico_pct - pares$umidade_cr350_cs625_pct
y_res <- range(c(res_plug, res_azul))
layout(matrix(c(1, 3, 2, 3), 2, byrow = TRUE), widths = c(4.7, 1.5))
par(mar = c(2.2, 5.8, 4, 1), las = 1)
plot(pares$datahora, res_plug, type = "l", xaxt = "n", ylim = y_res, col = cores["plug_cal"], lwd = 2.3,
     xlab = "", ylab = "Resíduo (p.p.)", main = "A) plugfild ajustado − referência"); grid(col = "gray88"); abline(h = 0, lty = 2)
par(mar = c(5.6, 5.8, 3.2, 1), las = 1)
plot(pares$datahora, res_azul, type = "l", xaxt = "n", ylim = y_res, col = cores["azul_cal"], lwd = 2.3,
     xlab = "Data/hora local", ylab = "Resíduo (p.p.)", main = "B) PC03/azul ajustado − referência"); axis.POSIXct(1, at = data_ticks, format = "%d/%m\n%H:%M"); grid(col = "gray88"); abline(h = 0, lty = 2)
par(mar = c(5.6, .4, 4, 1)); plot.new(); legend("topleft", bty = "n", lwd = c(2.3, 2.3, 2), lty = c(1, 1, 2),
       col = c(cores["plug_cal"], cores["azul_cal"], "gray35"), legend = c("plugfild", "PC03/azul", "resíduo zero"), cex = .95)
text(0, .45, adj = c(0, 1), cex = .80, col = "gray35", labels = "Mesma escala vertical.\nDados não suavizados.")
layout(1); dev.off()

# 05 - Validação 70/30 única: as duas predições quadráticas compartilham a referência.
png(file.path(graficos_dir, "05_validacao_temporal_70_30.png"), width = 2600, height = 1400, res = 150)
layout(matrix(c(1, 2), 1), widths = c(4.7, 1.5))
y_val <- range(c(pares$umidade_cr350_cs625_pct, pares$plugfild_validacao_quadratico_pct, pares$azul_validacao_quadratico_pct))
par(mar = c(5.6, 5.8, 4.2, 1), las = 1)
plot(pares$datahora, pares$umidade_cr350_cs625_pct, type = "n", xaxt = "n", ylim = y_val,
     xlab = "Data/hora local", ylab = "Umidade volumétrica do solo (%)", main = "Validação temporal 70/30: plugfild e PC03/azul")
usr <- par("usr")
rect(inicio_validacao, usr[3], max(pares$datahora), usr[4], col = "gray92", border = NA)
grid(col = "gray85")
lines(pares$datahora, pares$umidade_cr350_cs625_pct, col = cores["ref"], lwd = 3)
lines(pares$datahora, pares$plugfild_validacao_quadratico_pct, col = cores["plug_cal"], lwd = 2.5)
lines(pares$datahora, pares$azul_validacao_quadratico_pct, col = cores["azul_cal"], lwd = 2.5)
abline(v = inicio_validacao, lty = 2, lwd = 2, col = "gray35")
axis.POSIXct(1, at = data_ticks, format = "%d/%m\n%H:%M")
par(mar = c(5.6, .4, 4.2, 1)); plot.new()
legend("topleft", bty = "n", lwd = c(3, 2.5, 2.5, 2, 8), lty = c(1, 1, 1, 2, 1),
       col = c(cores["ref"], cores["plug_cal"], cores["azul_cal"], "gray35", "gray92"),
       legend = c("CS — UmidSolo referência", "plugfild: predição quadrática", "PC03/azul: predição quadrática", "corte 70/30", "30% final"), cex = .90)
text(0, .36, adj = c(0, 1), cex = .80, col = "gray35", labels = paste0("Treino: ", n_treino, " pares\nValidação: ", n - n_treino, " pares\nSem curva linear ou suavização."))
layout(1); dev.off()

# 06 - Lag conjunto, pois o eixo e a estatística são comuns.
png(file.path(graficos_dir, "06_lag_correlacao_dois_sensores.png"), width = 2300, height = 1350, res = 150)
layout(matrix(c(1, 2), 1), widths = c(4.5, 1.6))
par(mar = c(5.5, 5.8, 4.2, 1), las = 1)
dp <- lag_df[lag_df$sensor == "plugfild", ]; da <- lag_df[lag_df$sensor == "PC03/azul", ]
y_lag <- range(lag_df$pearson, na.rm = TRUE)
plot(dp$lag_min, dp$pearson, type = "n", ylim = y_lag, xlab = "Lag aplicado ao sensor de campo (min)",
     ylab = "Correlação de Pearson", main = "Lag diagnóstico contra CS — UmidSolo")
grid(col = "gray88"); lines(dp$lag_min, dp$pearson, col = cores["plug"], lwd = 2.7); lines(da$lag_min, da$pearson, col = cores["azul"], lwd = 2.7)
abline(v = 0, lty = 2, col = "gray50")
for (i in seq_len(nrow(melhor_lag))) abline(v = melhor_lag$lag_min[i], lty = 3, lwd = 1.8, col = ifelse(melhor_lag$sensor[i] == "plugfild", cores["plug"], cores["azul"]))
par(mar = c(5.5, .4, 4.2, 1)); plot.new()
legend("topleft", bty = "n", lwd = c(2.7, 2.7), col = cores[c("plug", "azul")], legend = c("plugfild", "PC03/azul"), cex = .95)
text(0, .55, adj = c(0, 1), cex = .82, col = "gray35", labels = paste(apply(melhor_lag, 1, function(z) sprintf("%s: %s min; r=%0.4f", z[["sensor"]], as.numeric(z[["lag_min"]]), as.numeric(z[["pearson"]]))), collapse = "\n"))
text(0, .33, adj = c(0, 1), cex = .78, col = "gray35", labels = "Lag é diagnóstico temporal,\nnão correção física automática.")
layout(1); dev.off()

# 07 - Métricas da validação, em painéis por sensor devido às escalas.
png(file.path(graficos_dir, "07_metricas_validacao_70_30.png"), width = 2400, height = 1350, res = 150)
par(mfrow = c(1, 2), mar = c(7, 5.5, 4.2, 1), las = 1)
for (sensor_atual in c("plugfild", "PC03/azul")) {
  d <- metricas_validacao[metricas_validacao$sensor == sensor_atual, ]
  nomes <- c("Original", "Linear", "Quadrático")
  mat <- rbind(MAE = d$mae_pp, RMSE = d$rmse_pp)
  b <- barplot(mat, beside = TRUE, names.arg = nomes, col = c("#6BAED6", "#FD8D3C"),
               ylab = "Erro (p.p.)", main = sensor_atual, ylim = c(0, max(mat) * 1.18), las = 2)
  grid(nx = NA, ny = NULL, col = "gray88"); legend("topright", bty = "n", fill = c("#6BAED6", "#FD8D3C"), legend = c("MAE", "RMSE"))
  text(b, mat, labels = sprintf("%.3f", mat), pos = 3, cex = .78)
}
dev.off()

# 08 - Disponibilidade conjunta.
png(file.path(graficos_dir, "08_disponibilidade_tres_sensores.png"), width = 1800, height = 1200, res = 150)
par(mar = c(6, 5.5, 4.2, 1), las = 1)
b <- barplot(disponibilidade$disponibilidade_pct, names.arg = disponibilidade$sensor,
             col = cores[c("ref", "plug", "azul")], ylim = c(0, 108), ylab = "Disponibilidade no intervalo (%)", main = "Disponibilidade temporal")
grid(nx = NA, ny = NULL, col = "gray88")
text(b, disponibilidade$disponibilidade_pct, labels = sprintf("%.2f%%\n%d/%d", disponibilidade$disponibilidade_pct, disponibilidade$registros, disponibilidade$esperados_1min), pos = 3, cex = .9)
dev.off()

# 6. Resumo conciso -----------------------------------------------------------
mc_plug <- metricas_completas[metricas_completas$sensor == "plugfild" & metricas_completas$modelo == "original", ]
mc_azul <- metricas_completas[metricas_completas$sensor == "PC03/azul" & metricas_completas$modelo == "original", ]
val_plug_q <- metricas_validacao[metricas_validacao$sensor == "plugfild" & metricas_validacao$modelo == "quadratico_treino70", ]
val_azul_q <- metricas_validacao[metricas_validacao$sensor == "PC03/azul" & metricas_validacao$modelo == "quadratico_treino70", ]
lag_plug <- melhor_lag[melhor_lag$sensor == "plugfild", ]
lag_azul <- melhor_lag[melhor_lag$sensor == "PC03/azul", ]

resumo <- file.path(resumos_dir, "resumo_analise_conjunta_tres_sensores.txt")
cat(
  "Análise conjunta: CS — UmidSolo, plugfild e PC03/azul\n",
  "===================================================\n\n",
  "Direção: CS — UmidSolo é a referência de campo; plugfild e PC03/azul são avaliados e ajustados.\n",
  "Fonte: TOA5 TableEnvio; UmidSolo (CS), PeriodoCS625, UmidPlug e UmidAzul.\n",
  paste0("Corte aplicado: ", format(inicio_analise, "%Y-%m-%d %H:%M:%S%z"), " — primeiro UmidPlug válido após lacuna contínua de ", corte$lacuna_registros, " min.\n"),
  paste0("Período: ", format(min(pares$datahora), "%Y-%m-%d %H:%M:%S%z"), " a ", format(max(pares$datahora), "%Y-%m-%d %H:%M:%S%z"), "\n"),
  paste0("Pares exatos dos três sensores: ", nrow(pares), "\n"),
  paste0("Treino 70%: ", n_treino, "; validação 30%: ", n - n_treino, "; início da validação: ", format(inicio_validacao, "%Y-%m-%d %H:%M:%S%z"), "\n\n"),
  "Dados brutos contra CS — UmidSolo:\n",
  sprintf("- plugfild: Pearson=%.4f; Spearman=%.4f; bias=%.4f p.p.; MAE=%.4f p.p.; RMSE=%.4f p.p.\n", mc_plug$pearson, mc_plug$spearman, mc_plug$bias_pp, mc_plug$mae_pp, mc_plug$rmse_pp),
  sprintf("- PC03/azul: Pearson=%.4f; Spearman=%.4f; bias=%.4f p.p.; MAE=%.4f p.p.; RMSE=%.4f p.p.\n\n", mc_azul$pearson, mc_azul$spearman, mc_azul$bias_pp, mc_azul$mae_pp, mc_azul$rmse_pp),
  "Validação temporal nos 30% finais — ajuste quadrático treinado nos 70% iniciais:\n",
  sprintf("- plugfild: bias=%.4f p.p.; MAE=%.4f p.p.; RMSE=%.4f p.p.\n", val_plug_q$bias_pp, val_plug_q$mae_pp, val_plug_q$rmse_pp),
  sprintf("- PC03/azul: bias=%.4f p.p.; MAE=%.4f p.p.; RMSE=%.4f p.p.\n\n", val_azul_q$bias_pp, val_azul_q$mae_pp, val_azul_q$rmse_pp),
  "Lag diagnóstico:\n",
  sprintf("- plugfild: %d min; Pearson=%.4f; n=%d\n", lag_plug$lag_min, lag_plug$pearson, lag_plug$n),
  sprintf("- PC03/azul: %d min; Pearson=%.4f; n=%d\n\n", lag_azul$lag_min, lag_azul$pearson, lag_azul$n),
  "Observações:\n",
  "- Não houve interpolação nem suavização.\n",
  "- Os gráficos conjuntos foram usados quando unidade, escala e objetivo permitiram comparação direta.\n",
  "- Ajustes e resíduos com faixas distintas foram separados em painéis.\n",
  "- Coeficientes são empíricos para este período curto e não devem ser extrapolados.\n",
  file = resumo, sep = ""
)

cat("OK - análise conjunta concluída\n")
cat("Período:", format(min(pares$datahora), "%Y-%m-%d %H:%M:%S%z"), "a", format(max(pares$datahora), "%Y-%m-%d %H:%M:%S%z"), "\n")
cat("Pares:", nrow(pares), "| treino:", n_treino, "| validação:", n - n_treino, "\n")
cat("Resumo:", resumo, "\n")
