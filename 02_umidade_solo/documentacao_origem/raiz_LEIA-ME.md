# Projeto — sensores de umidade do solo

Este projeto reúne aquisições do CR350/CS625, sensores de campo (plugfild e azul/PC03), calibração laboratorial e análises históricas.

## Onde encontrar cada coisa

| Pasta | Conteúdo | Uso atual |
|---|---|---|
| `dados/` | Entradas atuais, brutas e processadas, organizadas por sensor/ensaio. | Fonte dos scripts e análises vigentes. |
| `programas/` | Programas CRBasic do CR350 e análises R reutilizáveis. | Código principal de aquisição/calibração. |
| `scripts/` | Rotinas Python de download SIMEPAR, atualização e execução. | Automação de dados. |
| `resultados/` | Resultados ativos, uma subpasta por análise. | Consultar primeiro. |
| `dados_antigos/` | Arquivos de entrada preservados de campanhas anteriores. | Histórico; não usar sem confirmar o período. |
| `resultados_antigos/` | Resultados preservados antes de reanálises. | Histórico; não é saída atual. |
| `docs/` | Protocolos e índice das análises. | Orientação metodológica e de navegação. |

## Resultado mais recente: CS625 × azul/PC03 — fim de semana

```text
resultados/comparacao_cs625_pc03_fim_de_semana/
```

Entradas correspondentes:

```text
dados/cr350_cs625/brutos/CR350Series_TableEnvio_20260921_1320.dat
dados/pc03_azul/simepar/pc03_azul_25354889_soil_moisture_238_20260919_0000_a_20260920_2359_local.csv
```

Script correspondente:

```text
programas/analise_cs625_pc03_fim_de_semana.R
```

Os diretórios históricos foram mantidos nos mesmos caminhos para preservar a reprodutibilidade de scripts anteriores.
