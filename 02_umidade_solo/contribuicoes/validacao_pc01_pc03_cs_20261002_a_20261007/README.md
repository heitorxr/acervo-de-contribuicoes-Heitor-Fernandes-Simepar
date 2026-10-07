# Comparação de umidade — CS × PC01 × PC03

**Período:** 02/10/2026 10:10 a 07/10/2026 15:00, America/Sao_Paulo. Foram pareados 1.499 timestamps exatos, em intervalos de cinco minutos.

## Fontes e interpretação

- Referência operacional: `UmidSolo` e diagnóstico `PeriodoCS625`, do TOA5 CR350.
- Sensores de campo: PC01/plugfild e PC03/azul, CSVs SIMEPAR do sensor 238.
- O início foi escolhido após o último trecho zerado do PC03. É um critério operacional de recorte, não comprovação metrológica de adequação dos sensores.
- Ajustes no conjunto completo descrevem esta campanha. O teste 70/30 é cronológico: 1.049 pares de treino e 450 de validação, sem embaralhamento.
- **PC03:** 74 das 450 leituras de validação estão fora da faixa de treino (23–29%); parte dos erros publicados corresponde a extrapolação. O quadrático do conjunto completo não é crescente em toda a faixa observada. Portanto, não deve ser adotado como curva física ou correção universal.
- O diagnóstico de lag usa amostras com tamanho variável entre candidatos; os melhores lags são exploratórios, não estimativas de tempo físico de resposta.

## Conteúdo público

Somente código, resumo e tabelas agregadas. Não estão incluídos TOA5, downloads/JSON, entradas CSV, pares individuais, séries alinhadas, predições/resíduos por observação nem imagens das séries. Resultados agregados ainda descrevem informações derivadas das fontes originais.

## Reexecução local

Forneça três arquivos privados e um diretório novo de saída **fora do acervo**:

```bash
Rscript codigo/analise_conjunta_tres_sensores.R TOA5.dat PC01.csv PC03.csv DIRETORIO_PRIVADO_NOVO
```

O TOA5 deve conter `TIMESTAMP`, `RECORD`, `UmidSolo`, `PeriodoCS625`, `UmidPlug` e `UmidAzul` (as duas últimas são substituídas pelos CSVs externos). Os CSVs devem conter `datahora_local`, `estacaoId`, `sensorId`, `leitura` e `qualidadeId`; as estações verificadas são 25334889 e 25354889. Timestamps inválidos ou duplicados interrompem a execução.

O script usa o recorte desta campanha, não escolhe automaticamente o início de outras campanhas. A execução produz tabelas individuais e gráficos apenas no diretório privado; esses arquivos não devem ser copiados para o acervo público.
