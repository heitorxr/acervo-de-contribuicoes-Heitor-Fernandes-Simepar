# Caixa sonar 18650 — revisões v04 e v05

`cad_e_exportacoes/` mantém CAD e STLs por revisão. **Para a v05, fatiar somente o STL conjunto**; os STLs separados v04 continuam como histórico e não devem ser misturados à v05. As versões v01/v02 e os modelos históricos também foram preservados.

- `caixa_sonar_18650_v05.fcstd`: montagem FreeCAD com base e tampa separadas, componentes de referência e objeto `Conjunto_Impressao` (oculto por padrão).
- `caixa_sonar_base_tampa_juntas_v05.stl`: arranjo único com piso da base e face externa da tampa no leito, **seis mouse ears**, das quais **duas compartilhadas** por pontes finas removíveis. Depois da impressão, cortar as duas pontes ligadas à tampa e remover as ears. Ocupa **148 × 216 × 40 mm**; confirmar área útil, margem e primeira camada no fatiador antes de imprimir.
- A saia da tampa v05 tem recortes retangulares nos quatro bosses para retirar os filetes finos residuais junto à parede, preservando os quatro furos de fixação.
- `scripts/generate_caixa_sonar.py`: gerador da v05. O caminho `PROJECT` refere-se ao projeto local de origem; ajustar antes de regenerar fora dele. `scripts/validar_v05.py`: reabre o STL e verifica malha e dimensões.
- `scripts/alternar_montagem_explodida.FCMacro`: alterna montagem e vista explodida no FreeCAD; requer documento v04 ou v05 aberto. O objeto de impressão continua independente da vista.

A v05 foi reaberta como **uma malha estanque e conectada**, com normais consistentes; o teste CAD registrou interseção zero entre base e tampa na posição fechada e confirmou a remoção da saia nas quatro zonas de canto. **Não houve teste no fatiador nem peça física.** Diâmetro/passo da rosca, porca do sonar, altura/posição real dos conectores, comprimento das células e tolerância dos parafusos ainda exigem medição/prova. As fendas das abraçadeiras atravessam o piso: a caixa não é estanque à água.

## Revisão v04 (histórico)

- `caixa_sonar_18650_v04.fcstd`: montagem para inspeção com referências indicativas.
- `caixa_sonar_base_v04.stl`: base com quatro ears removíveis e furo do sonar na **face menor (82 mm)**.
- `caixa_sonar_tampa_v04.stl`: tampa separada, **sem as ears da v05**.
