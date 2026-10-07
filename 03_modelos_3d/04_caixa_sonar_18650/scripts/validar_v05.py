"""Checa a malha do layout conjunto da revisão v05."""
import json
from pathlib import Path
import trimesh

root = Path(__file__).resolve().parents[1]
p = root / 'cad_e_exportacoes' / 'caixa_sonar_base_tampa_juntas_v05.stl'
m = trimesh.load_mesh(p, force='mesh')
parts = m.split(only_watertight=False)
report = {
    'arquivo': str(p), 'bytes': p.stat().st_size,
    'estanque': bool(m.is_watertight), 'normais_consistentes': bool(m.is_winding_consistent),
    'componentes_conectados': len(parts), 'volume_mm3': float(m.volume),
    'limites_mm': m.bounds.tolist(), 'dimensoes_mm': m.extents.tolist(),
    'faces': len(m.faces), 'euler': int(m.euler_number),
    'z_min_mm': float(m.bounds[0, 2]),
}
print(json.dumps(report, ensure_ascii=False, indent=2))
assert m.is_watertight and m.is_winding_consistent and len(parts) == 1
assert abs(m.extents[0]-148) < 0.02 and abs(m.extents[1]-216) < 0.02
assert abs(m.bounds[0, 2]) < 0.01
assert m.volume > 0
