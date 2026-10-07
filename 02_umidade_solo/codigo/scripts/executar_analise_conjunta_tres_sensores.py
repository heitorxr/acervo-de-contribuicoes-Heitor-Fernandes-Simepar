#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""Executa a análise conjunta dos três sensores de umidade do solo."""
from __future__ import annotations

import subprocess
from pathlib import Path

from executar_analises_umidade import localizar_rscript


def main() -> int:
    projeto = Path(__file__).resolve().parent.parent
    rscript = localizar_rscript()
    script = projeto / "analise_conjunta_tres_sensores.R"
    print(f"Rscript: {rscript}")
    print(f"Executando: {script.name}")
    subprocess.run([str(rscript), str(script)], cwd=projeto, check=True)
    print("OK - análise conjunta dos três sensores executada.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
