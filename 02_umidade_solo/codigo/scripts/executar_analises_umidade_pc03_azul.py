#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""Baixa PC03/azul e executa as análises equivalentes contra CR350/CS625."""
from __future__ import annotations

import subprocess
from pathlib import Path

from executar_analises_umidade import localizar_rscript


def main() -> int:
    projeto = Path(__file__).resolve().parent.parent
    subprocess.run(["python", str(projeto / "scripts" / "baixar_dados_pc03_azul.py")], cwd=projeto, check=True)
    rscript = localizar_rscript()
    for nome in ("analise_comparacao_umidade_pc03_azul_cr350.R", "analise_validacao_calibracao_pc03_azul_cr350.R"):
        print(f"Executando: {nome}")
        subprocess.run([str(rscript), str(projeto / nome)], cwd=projeto, check=True)
    print("OK - PC03/azul baixado e analisado.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
