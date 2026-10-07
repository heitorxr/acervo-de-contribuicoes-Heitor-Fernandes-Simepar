# Funcoes estatisticas comuns para series ambientais de 15 minutos.
# O bootstrap amostra blocos temporais contiguos para preservar parte da
# autocorrelacao intradiaria. O tamanho padrao de 96 observacoes corresponde
# a 24 horas em uma serie regular de 15 minutos.

metricas_pareadas <- function(estimado, referencia) {
  ok <- is.finite(estimado) & is.finite(referencia)
  estimado <- estimado[ok]
  referencia <- referencia[ok]
  diferenca <- estimado - referencia
  r_pearson <- cor(estimado, referencia, method = "pearson")
  data.frame(
    n = length(estimado),
    vies = mean(diferenca),
    mae = mean(abs(diferenca)),
    rmse = sqrt(mean(diferenca^2)),
    pearson = r_pearson,
    spearman = cor(estimado, referencia, method = "spearman"),
    r2 = r_pearson^2,
    limite_concordancia_inferior = mean(diferenca) - 1.96 * sd(diferenca),
    limite_concordancia_superior = mean(diferenca) + 1.96 * sd(diferenca)
  )
}

bootstrap_blocos_associacao <- function(
  x,
  y,
  tamanho_bloco = 96,
  repeticoes = 2000,
  semente = 20260716
) {
  ok <- is.finite(x) & is.finite(y)
  x <- x[ok]
  y <- y[ok]
  n <- length(x)
  if (n < 3) stop("Sao necessarios ao menos tres pares validos.")

  tamanho_bloco <- min(as.integer(tamanho_bloco), n)
  inicios_possiveis <- seq_len(n - tamanho_bloco + 1)
  n_blocos <- ceiling(n / tamanho_bloco)
  amostras <- matrix(NA_real_, nrow = repeticoes, ncol = 3)
  colnames(amostras) <- c("pearson", "spearman", "r2")

  set.seed(semente)
  for (b in seq_len(repeticoes)) {
    inicios <- sample(inicios_possiveis, n_blocos, replace = TRUE)
    indices <- unlist(lapply(inicios, function(i) i:(i + tamanho_bloco - 1)), use.names = FALSE)
    indices <- indices[seq_len(n)]
    pearson <- cor(x[indices], y[indices], method = "pearson")
    amostras[b, ] <- c(
      pearson = pearson,
      spearman = cor(x[indices], y[indices], method = "spearman"),
      r2 = pearson^2
    )
  }

  pearson <- cor(x, y, method = "pearson")
  pontual <- c(
    pearson = pearson,
    spearman = cor(x, y, method = "spearman"),
    r2 = pearson^2
  )
  data.frame(
    metrica = names(pontual),
    estimativa = unname(pontual),
    limite_inferior_95 = apply(amostras, 2, quantile, probs = 0.025, na.rm = TRUE),
    limite_superior_95 = apply(amostras, 2, quantile, probs = 0.975, na.rm = TRUE),
    tamanho_bloco = tamanho_bloco,
    repeticoes = repeticoes,
    semente = semente,
    stringsAsFactors = FALSE
  )
}

bootstrap_blocos_pareados <- function(
  estimado,
  referencia,
  tamanho_bloco = 96,
  repeticoes = 2000,
  semente = 20260716
) {
  ok <- is.finite(estimado) & is.finite(referencia)
  estimado <- estimado[ok]
  referencia <- referencia[ok]
  n <- length(estimado)
  if (n < 3) stop("Sao necessarios ao menos tres pares validos.")

  tamanho_bloco <- min(as.integer(tamanho_bloco), n)
  inicios_possiveis <- seq_len(n - tamanho_bloco + 1)
  n_blocos <- ceiling(n / tamanho_bloco)
  nomes_metricas <- c("vies", "mae", "rmse", "pearson", "spearman", "r2")
  amostras <- matrix(NA_real_, nrow = repeticoes, ncol = length(nomes_metricas))
  colnames(amostras) <- nomes_metricas

  set.seed(semente)
  for (b in seq_len(repeticoes)) {
    inicios <- sample(inicios_possiveis, n_blocos, replace = TRUE)
    indices <- unlist(lapply(inicios, function(i) i:(i + tamanho_bloco - 1)), use.names = FALSE)
    indices <- indices[seq_len(n)]
    m <- metricas_pareadas(estimado[indices], referencia[indices])
    amostras[b, ] <- unlist(m[1, nomes_metricas], use.names = FALSE)
  }

  pontual <- metricas_pareadas(estimado, referencia)
  data.frame(
    metrica = nomes_metricas,
    estimativa = unlist(pontual[1, nomes_metricas], use.names = FALSE),
    limite_inferior_95 = apply(amostras, 2, quantile, probs = 0.025, na.rm = TRUE),
    limite_superior_95 = apply(amostras, 2, quantile, probs = 0.975, na.rm = TRUE),
    tamanho_bloco = tamanho_bloco,
    repeticoes = repeticoes,
    semente = semente,
    stringsAsFactors = FALSE
  )
}
