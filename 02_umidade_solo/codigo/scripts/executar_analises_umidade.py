#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""Executa, em sequência, as duas análises R de umidade do solo."""
from __future__ import annotations

import os
import shutil
import subprocess
from pathlib import Path


def localizar_rscript() -> Path:
    no_path = shutil.which("Rscript")
    if no_path:
        return Path(no_path)
    local_appdata = Path(os.environ.get("LOCALAPPDATA", Path.home() / "AppData" / "Local"))
    candidatos = sorted(
        (local_appdata / "Programs" / "R").glob("R-*\\bin\\Rscript.exe"),
        reverse=True,
    )
    if not candidatos:
        candidatos = sorted(
            Path("C:/Program Files/R").glob("R-*\\bin\\Rscript.exe"),
            reverse=True,
        )
    if not candidatos:
        raise FileNotFoundError("Rscript não foi encontrado no PATH nem nas instalações usuais do R.")
    return candidatos[0]


def main() -> int:
    projeto = Path(__file__).resolve().parent.parent
    rscript = localizar_rscript()
    codigos = [
        projeto / "analise_comparacao_umidade_plugfild_cr350.R",
        projeto / "analise_validacao_calibracao_plugfild_cr350.R",
    ]
    print(f"Rscript: {rscript}")
    for codigo in codigos:
        print(f"Executando: {codigo.name}")
        subprocess.run([str(rscript), str(codigo)], cwd=projeto, check=True)
    print("OK - todas as análises R foram executadas.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
