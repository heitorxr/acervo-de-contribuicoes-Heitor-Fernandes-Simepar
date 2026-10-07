# -*- coding: utf-8 -*-
"""Caixa sonar v05: base+tampa num STL com duas mouse ears compartilhadas."""
import FreeCAD as App
import FreeCADGui as Gui
import Part, Mesh, os

PROJECT = r"C:/Users/VANT/Documents/Projetos/caixa-sonar-18650"
OUT_FC, OUT_STL = (os.path.join(PROJECT, n) for n in ('freecad', 'stl'))
for directory in (OUT_FC, OUT_STL): os.makedirs(directory, exist_ok=True)
doc = App.getDocument('Caixa_Sonar_18650')
if doc is None: raise RuntimeError('Criar documento via MCP antes de executar')
for obj in list(doc.Objects): doc.removeObject(obj.Name)
App.setActiveDocument(doc.Name)

L, W, H = 112.0, 82.0, 40.0
WALL, FLOOR = 2.6, 3.0
LID_T, SKIRT_H, FIT = 3.2, 6.0, 0.35
CORNER_R = 7.0
BAT_D, BAT_L = 18.8, 64.0
BOARD_X, BOARD_Y, BOARD_T = 30.0, 70.0, 1.6
BOARD_X0, BOARD_Y0, BOARD_Z = 73.5, 6.0, 7.5
SONAR_THREAD_D, SONAR_HOLE_D = 30.0, 32.5 # provisórios: medir rosca e porca reais
SONAR_Y, SONAR_Z = W/2, H/2
LID_BOSSES = ((6.5, 7.0), (6.5, W-7.0), (L-4.0, 7.0), (L-4.0, W-7.0))

params = doc.addObject('App::FeaturePython', 'PARAMETROS')
params.Label = 'Dimensões v05 (referência; regenerar pelo script para alterar geometria)'
for name, value in dict(Comprimento=L, Largura=W, Altura_Base=H, Parede=WALL,
                        Piso=FLOOR, Comprimento_18650=BAT_L, Diametro_18650=BAT_D,
                        Placa_X=BOARD_X, Placa_Y=BOARD_Y, Furo_Sonar=SONAR_HOLE_D).items():
    params.addProperty('App::PropertyLength', name, 'Dimensões')
    setattr(params, name, value)
params.addProperty('App::PropertyString', 'Aviso', 'Dimensões')
params.Aviso = 'Propriedades indicativas: alterar no gerador e regenerar. Medir rosca/porca sonar e terminais das células antes de imprimir.'
printable = doc.addObject('App::DocumentObjectGroup', 'PECAS_IMPRIMIVEIS')
components = doc.addObject('App::DocumentObjectGroup', 'MODELOS_DE_FIT')

def rounded_prism(length, width, height, radius):
    s = Part.makeBox(length-2*radius, width, height, App.Vector(radius,0,0))
    s = s.fuse(Part.makeBox(length, width-2*radius, height, App.Vector(0,radius,0)))
    for x in (radius, length-radius):
        for y in (radius, width-radius):
            s = s.fuse(Part.makeCylinder(radius,height,App.Vector(x,y,0)))
    return s.removeSplitter()

base_shape = rounded_prism(L,W,H,CORNER_R).cut(
    Part.makeBox(L-2*WALL,W-2*WALL,H,App.Vector(WALL,WALL,FLOOR)))
# Aba fina sacrificial em cada canto, conectada à caixa pela área de sobreposição.
for x,y,bx,by in ((-8,-8,-1,-1),(L+8,-8,L-5,-1),(-8,W+8,-1,W-5),(L+8,W+8,L-5,W-5)):
    ear = Part.makeCylinder(10,0.55,App.Vector(x,y,0))
    bridge = Part.makeBox(6,6,0.55,App.Vector(bx,by,0))
    base_shape = base_shape.fuse(ear.fuse(bridge))

# Sonar atravessa especificamente a face x=0 (largura W=82, lado menor).
# Porca fica do lado interno; as células começam em x=11,6 para dar acesso à rosca.
base_shape = base_shape.cut(Part.makeCylinder(SONAR_HOLE_D/2,WALL+2,
                         App.Vector(-1,SONAR_Y,SONAR_Z),App.Vector(1,0,0)))

# Duas baterias paralelas ao eixo Y, presas por trilhos baixos e duas cintas cada.
# Fendas pequenas ficam sob cada célula; não considerar a caixa estanque.
BAT_YS = 9.0
BAT_XS = (21.0,46.5)
for cx in BAT_XS:
    for x in (cx-BAT_D/2-2.4, cx+BAT_D/2+0.4):
        base_shape = base_shape.fuse(Part.makeBox(2.0,BAT_L+0.8,5.5,
                                     App.Vector(x,BAT_YS-0.4,FLOOR-0.2)))
    for y in (BAT_YS-3.0, BAT_YS+BAT_L+0.6):
        base_shape = base_shape.fuse(Part.makeBox(BAT_D+4.8,2.4,4.4,
                                     App.Vector(cx-BAT_D/2-2.4,y,FLOOR-0.2)))
    for y in (BAT_YS+17, BAT_YS+48):
        base_shape = base_shape.cut(Part.makeBox(10.0,3.2,FLOOR+2,
                                    App.Vector(cx-5,y,-1)))

# Apoios da placa: furo Ø3; borda do furo a 1 mm de cada lado adjacente
# -> centros a 2,5 mm das bordas. Pilotos Ø2,1 para parafuso M2,5 autorroscante.
BOARD_HOLES = tuple((BOARD_X0+dx,BOARD_Y0+dy)
                    for dx in (2.5,BOARD_X-2.5) for dy in (2.5,BOARD_Y-2.5))
for x,y in BOARD_HOLES:
    post = Part.makeCylinder(2.25,BOARD_Z-FLOOR+0.2,App.Vector(x,y,FLOOR-0.2))
    base_shape = base_shape.fuse(post)
    # Furo cego: não perfura o piso da caixa.
    base_shape = base_shape.cut(Part.makeCylinder(1.05,BOARD_Z-FLOOR+0.8,
                                     App.Vector(x,y,FLOOR+0.2)))

# Quatro bosses realmente alinhados aos furos da tampa. Os da direita ficam junto à
# parede x=112; a placa termina em x=103,5 sem tocar a zona dos bosses.
for x,y in LID_BOSSES:
    boss = Part.makeCylinder(3.7,H-FLOOR+0.2,App.Vector(x,y,FLOOR-0.2))
    base_shape = base_shape.fuse(boss)
    # Piloto cego Ø2,5 para M3; fundo fechado.
    base_shape = base_shape.cut(Part.makeCylinder(1.25,10.0,App.Vector(x,y,H-9)))
base_shape = base_shape.removeSplitter()
base = doc.addObject('PartDesign::Feature','Base_Caixa')
base.Label='Base — sonar no lado de 82 mm; apoios da placa e da tampa'
base.Shape=base_shape
base.ViewObject.ShapeColor=(0.82,0.82,0.86)
base.addProperty('App::PropertyString','Orientacao_Impressao','Impressão')
base.Orientacao_Impressao='Piso em Z=0; mouse ears removíveis; furo lateral Ø32,5 requer suporte localizado.'
printable.addObject(base)

# Tampa: saia com alívio local para cada boss e quatro furos passantes M3.
plate = rounded_prism(L,W,LID_T,CORNER_R)
rim_outer = Part.makeBox(L-2*WALL-2*FIT,W-2*WALL-2*FIT,SKIRT_H,
                         App.Vector(WALL+FIT,WALL+FIT,-SKIRT_H))
rim_inner = Part.makeBox(L-2*WALL-2*FIT-2.2,W-2*WALL-2*FIT-2.2,SKIRT_H+0.2,
                         App.Vector(WALL+FIT+1.1,WALL+FIT+1.1,-SKIRT_H-0.1))
rim = rim_outer.cut(rim_inner)
# Alívio da saia na parede do sonar para não tocar a rosca, flange ou futura porca.
rim = rim.cut(Part.makeBox(10,46,SKIRT_H+0.4,App.Vector(0,SONAR_Y-23,-SKIRT_H-0.2)))
# Zonas dos bosses: eliminar toda a pequena faixa da saia nos cantos.
# O alívio retangular envolve o antigo recorte circular Ø8,3; assim não
# sobra filete fino entre boss, saia e parede externa da tampa.
for x,y in LID_BOSSES:
    x0 = 0 if x < L/2 else L-12
    y0 = 0 if y < W/2 else W-12
    rim = rim.cut(Part.makeBox(12,12,SKIRT_H+0.4,
                              App.Vector(x0,y0,-SKIRT_H-0.2)))
lid_shape = plate.fuse(rim)
for x,y in LID_BOSSES:
    lid_shape = lid_shape.cut(Part.makeCylinder(1.65,LID_T+SKIRT_H+2,
                              App.Vector(x,y,-SKIRT_H-1)))
lid = doc.addObject('PartDesign::Feature','Tampa_Caixa')
lid.Label='Tampa — quatro furos M3, sem furo do sonar'
lid.Shape=lid_shape.removeSplitter()
lid.ViewObject.ShapeColor=(0.72,0.76,0.84)
lid.addProperty('App::PropertyString','Orientacao_Impressao','Impressão')
lid.Orientacao_Impressao='Face externa no leito; saia acima. Sem junta: não é estanque.'
printable.addObject(lid)

def ref(name,label,shape,color,alpha=0):
    o=doc.addObject('PartDesign::Feature',name); o.Label=label; o.Shape=shape
    o.ViewObject.ShapeColor=color; o.ViewObject.Transparency=alpha
    components.addObject(o); return o

for n,cx in enumerate(BAT_XS,1):
    ref('Bateria_18650_'+str(n),'Modelo de fit — bateria 64 × Ø18,8 #'+str(n),
        Part.makeCylinder(BAT_D/2,BAT_L,App.Vector(cx,BAT_YS,FLOOR+BAT_D/2),App.Vector(0,1,0)),
        (0.20,0.55,0.20))
board_shape=Part.makeBox(BOARD_X,BOARD_Y,BOARD_T,App.Vector(BOARD_X0,BOARD_Y0,BOARD_Z))
for x,y in BOARD_HOLES:
    board_shape=board_shape.cut(Part.makeCylinder(1.5,BOARD_T+2,App.Vector(x,y,BOARD_Z-1)))
ref('Placa_Eletronica_30x70','Placa 30×70, 4 furos Ø3 (borda a 1 mm)',board_shape,(0.10,0.35,0.60))
ref('Borne_Placa','Envelope indicativo dos conectores; posição real a conferir',
    Part.makeBox(20,12,12,App.Vector(BOARD_X0+5,BOARD_Y0+18,BOARD_Z+BOARD_T)),
    (0.05,0.40,0.10),35)
# Rosca/corpo meramente ilustrativos. Porca, comprimento da rosca e envelope reais pendentes.
thread=Part.makeCylinder(SONAR_THREAD_D/2,27,App.Vector(-21,SONAR_Y,SONAR_Z),App.Vector(1,0,0))
body=Part.makeCone(23,18,38,App.Vector(-21,SONAR_Y,SONAR_Z),App.Vector(-1,0,0))
flange=Part.makeCylinder(21,5,App.Vector(-3,SONAR_Y,SONAR_Z),App.Vector(-1,0,0))
sonar=ref('Sonar_Externo','Sonar — montado em x=0 (lado de 82 mm)',thread.fuse(body).fuse(flange),(0.12,0.12,0.12))

control=doc.addObject('App::FeaturePython','Controle_Visualizacao')
control.Label='CONTROLE — executar macro para alternar montagem/explodida'
control.addProperty('App::PropertyEnumeration','Modo','Visualização')
control.Modo=['Montagem fechada','Explodida']; control.Modo='Explodida'
control.addProperty('App::PropertyString','Instrucao','Visualização')
control.Instrucao='Executar scripts/alternar_montagem_explodida.FCMacro'
lid.Placement.Base=App.Vector(0,0,H+20)
for name,offset in {
    'Bateria_18650_1': App.Vector(0,-83,14),
    'Bateria_18650_2': App.Vector(0,83,14),
    'Placa_Eletronica_30x70': App.Vector(42,0,15),
    'Borne_Placa': App.Vector(42,0,15),
}.items(): doc.getObject(name).Placement.Base=offset

# Layout único: tampa acima da base, 16 mm de distância entre bordas.
# As duas ears superiores da base ancoram também os dois cantos inferiores
# da tampa; uma ear compartilhada deve ter pontes sacrificiais para ambas.
# A peça contínua se separa cortando as duas pontes voltadas à tampa.
from math import hypot

def diagonal_bridge(start, end, thickness=0.55, width=2.0):
    sx, sy = start; ex, ey = end
    dx, dy = ex-sx, ey-sy
    norm = hypot(dx,dy)
    nx, ny = -dy*width/(2*norm), dx*width/(2*norm)
    points = [App.Vector(sx+nx,sy+ny,0),App.Vector(ex+nx,ey+ny,0),
              App.Vector(ex-nx,ey-ny,0),App.Vector(sx-nx,sy-ny,0)]
    wire = Part.makePolygon(points+[points[0]])
    return Part.Face(wire).extrude(App.Vector(0,0,thickness))

# Face externa apoiada em Z=0; girar 180° sobre X põe a saia para cima.
lid_print = lid.Shape.copy()
lid_print.Placement = App.Placement(App.Vector(0,2*W+16,LID_T),
                                    App.Rotation(App.Vector(1,0,0),180))
layout_shape = base.Shape.copy().fuse(lid_print)
shared_centers = ((-8,W+8),(L+8,W+8))
for inner,center in [((4,W+20),shared_centers[0]),
                     ((L-4,W+20),shared_centers[1])]:
    layout_shape = layout_shape.fuse(diagonal_bridge(inner,center))
# Apenas duas ears novas, nos cantos externos superiores da tampa.
for inner,center in [((4,2*W+12),(-8,2*W+24)),
                     ((L-4,2*W+12),(L+8,2*W+24))]:
    ear = Part.makeCylinder(10,0.55,App.Vector(center[0],center[1],0))
    layout_shape = layout_shape.fuse(ear.fuse(diagonal_bridge(inner,center)))
# Z=0 em ambas as faces externas; origem positiva para fatiamento.
layout_shape = layout_shape.removeSplitter()
layout_shape.translate(App.Vector(18,18,0))
layout = doc.addObject('PartDesign::Feature','Conjunto_Impressao')
layout.Label='Impressão conjunta — base + tampa, 6 orelhas (2 compartilhadas)'
layout.Shape=layout_shape
layout.ViewObject.ShapeColor=(0.90,0.67,0.25)
layout.addProperty('App::PropertyString','Aviso_Impressao','Impressão')
layout.Aviso_Impressao='STL único; área de 148 x 216 mm, conferir área útil da GTmax H4. Cortar duas pontes após imprimir.'
printable.addObject(layout)
layout.ViewObject.Visibility=False

# Exportar apenas a geometria de impressão conjunta, nunca a montagem explodida.
doc.recompute()
layout_path=os.path.join(OUT_STL,'caixa_sonar_base_tampa_juntas_v05.stl')
Mesh.export([layout],layout_path)
fc_path=os.path.join(OUT_FC,'caixa_sonar_18650_v05.FCStd')
doc.saveAs(fc_path)
Gui.activeDocument().activeView().viewAxonometric(); Gui.activeDocument().activeView().fitAll()
print({'fcstd':fc_path,'stl_conjunto':layout_path,
       'layout_valid':layout.Shape.isValid(),'layout_solids':len(layout.Shape.Solids),
       'layout_bounds':str(layout.Shape.BoundBox),
       'lid_valid':lid.Shape.isValid(),'base_valid':base.Shape.isValid(),
       'ear_count':6,'shared_ear_centers_xy':shared_centers,
       'sonar_face':'x=0, largura 82 mm','sonar_thread_estimated':True})
