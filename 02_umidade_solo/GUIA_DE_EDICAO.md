# Guia — Umidade do solo

## Conteúdo do acervo público

- `codigo/analises_raiz/`, `codigo/programas/` e `codigo/scripts/`: análises e automações históricas. Caminhos de entrada e datas pertencem às campanhas de origem; examine-os antes de executar.
- `contribuicoes/`: análises posteriores, com documentação própria. A contribuição de outubro compara CS/CR350, PC01 e PC03, de 02 a 07/10/2026.
- `dados/`: somente documentação; não há TOA5, downloads SIMEPAR nem leituras de laboratório disponíveis aqui.
- `resultados/<analise>/`: estatísticas, métricas, modelos e diagnósticos agregados. Tabelas com leituras individuais e imagens dessas séries foram excluídas da versão pública.
- `documentacao_origem/`: documentos do ambiente privado. Seus exemplos de arquivos de entrada são referências de procedimento, não arquivos distribuídos no acervo.

## Reexecução e privacidade

Mantenha entradas e saídas por observação fora do repositório público. Para a contribuição de outubro, use o comando e os cabeçalhos documentados no README correspondente; o código exige uma pasta nova de saída e recusa gerar resultados dentro do acervo.

Revisões de outros scripts requerem verificação dos caminhos antes da execução: os geradores históricos podem produzir dados individuais. Só publique métricas e resultados agregados depois de conferir o conteúdo, não apenas o nome do arquivo.
