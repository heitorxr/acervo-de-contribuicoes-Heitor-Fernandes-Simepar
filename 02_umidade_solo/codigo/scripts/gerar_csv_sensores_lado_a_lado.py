#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""Gera um CSV lado a lado de CR350/CS625, plugfild e PC03/azul.

Não interpola nem altera as fontes. O recorte começa no mesmo limite aplicado
à PC03/azul: 2026-08-04 11:20 (America/Sao_Paulo). Execute a qualquer momento:

    python scripts/gerar_csv_sensores_lado_a_lado.py
"""
from __future__ import annotations

import csv
from datetime import datetime
from pathlib import Path
from zoneinfo import ZoneInfo

TZ = ZoneInfo("America/Sao_Paulo")
INICIO = datetime(2026, 8, 4, 11, 20, tzinfo=TZ)


def ler_simepar(caminho: Path, coluna_saida: str) -> dict[datetime, tuple[float, str]]:
    """Lê uma fonte SIMEPAR já normalizada e mantém um valor por timestamp."""
    dados: dict[datetime, tuple[float, str]] = {}
    with caminho.open("r", encoding="utf-8", newline="") as arquivo:
        for linha in csv.DictReader(arquivo):
            try:
                instante = datetime.fromisoformat(linha["datahora_local"])
                valor = float(linha["leitura"].replace(",", "."))
            except (KeyError, TypeError, ValueError):
                continue
            if instante >= INICIO:
                dados[instante] = (valor, str(linha.get("qualidadeId", "")))
    if not dados:
        raise RuntimeError(f"Nenhuma leitura válida após o corte em {caminho}")
    return dados


def ler_cr350(caminho: Path) -> dict[datetime, float]:
    """Lê CR350/CS625; VWC_m3_m3 é convertido para porcentagem."""
    dados: dict[datetime, float] = {}
    with caminho.open("r", encoding="utf-8", newline="") as arquivo:
        for linha in csv.DictReader(arquivo):
            try:
                instante = datetime.strptime(linha["TIMESTAMP"], "%Y-%m-%d %H:%M:%S").replace(tzinfo=TZ)
                valor_pct = float(linha["VWC_m3_m3"].replace(",", ".")) * 100
            except (KeyError, TypeError, ValueError):
                continue
            if instante >= INICIO:
                dados[instante] = valor_pct
    if not dados:
        raise RuntimeError(f"Nenhuma leitura CR350 válida após o corte em {caminho}")
    return dados


def valor(numero: float | None, casas: int = 4) -> str:
    return "" if numero is None else f"{numero:.{casas}f}"


def main() -> int:
    projeto = Path(__file__).resolve().parent.parent
    arq_cr350 = projeto / "dados" / "entradas_analise" / "plugfild_pc01" / "CR350Series_TabelaSolo_limpo.csv"
    arq_plugfild = projeto / "dados" / "plugfild_pc01" / "simepar" / "plugfild_25334889_soil_moisture_238_intervalo_TabelaSolo_local.csv"
    arq_azul = projeto / "dados" / "pc03_azul" / "simepar" / "pc03_azul_25354889_soil_moisture_238_intervalo_TabelaSolo_local.csv"

    for caminho in (arq_cr350, arq_plugfild, arq_azul):
        if not caminho.is_file():
            raise FileNotFoundError(caminho)

    cr350 = ler_cr350(arq_cr350)
    plugfild = ler_simepar(arq_plugfild, "plugfild")
    azul = ler_simepar(arq_azul, "azul")

    # União preserva explicitamente uma eventual lacuna: valores ausentes ficam vazios.
    instantes = sorted(set(cr350) | set(plugfild) | set(azul))
    destino = projeto / "resultados" / "comparacao_sensores_azul_plugfild" / "tabelas"
    destino.mkdir(parents=True, exist_ok=True)
    saida = destino / "dados_sensores_lado_a_lado.csv"
    # CSV enxuto: data/hora para alinhamento e uma coluna para cada sensor.
    campos = [
        "datahora_local",
        "umidade_cr350_cs625_pct",
        "umidade_plugfild_pct",
        "umidade_azul_pc03_pct",
    ]
    triplos = 0
    with saida.open("w", encoding="utf-8", newline="") as arquivo:
        escritor = csv.DictWriter(arquivo, fieldnames=campos)
        escritor.writeheader()
        for instante in instantes:
            p = plugfild.get(instante)
            a = azul.get(instante)
            c = cr350.get(instante)
            completo = c is not None and p is not None and a is not None
            triplos += int(completo)
            escritor.writerow({
                "datahora_local": instante.strftime("%Y-%m-%d %H:%M:%S%z"),
                "umidade_cr350_cs625_pct": valor(c),
                "umidade_plugfild_pct": valor(p[0] if p else None),
                "umidade_azul_pc03_pct": valor(a[0] if a else None),
            })

    resumo = destino.parent / "resumos" / "resumo_dados_sensores_lado_a_lado.txt"
    resumo.parent.mkdir(parents=True, exist_ok=True)
    resumo.write_text(
        "CSV lado a lado: CR350/CS625, plugfild e PC03/azul\n"
        "====================================================\n\n"
        f"Corte inicial: {INICIO.isoformat()}\n"
        f"CR350/CS625 após o corte: {len(cr350)} registros\n"
        f"plugfild após o corte: {len(plugfild)} registros\n"
        f"PC03/azul após o corte: {len(azul)} registros\n"
        f"Linhas na união de timestamps: {len(instantes)}\n"
        f"Pares exatos dos três sensores: {triplos}\n"
        "Valores ausentes permanecem vazios; não houve interpolação.\n"
        f"CSV: {saida}\n",
        encoding="utf-8",
    )
    print(f"INICIO={INICIO.isoformat()}")
    print(f"CR350={len(cr350)} PLUGFILD={len(plugfild)} AZUL={len(azul)} UNIAO={len(instantes)} TRIPLOS={triplos}")
    print(f"CSV={saida}")
    print(f"RESUMO={resumo}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
