#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""Baixa o sensor de umidade do solo "azul" da estação PC03 e prepara a análise.

Usa o .dat mais recente em Downloads como referência temporal, mas restringe
PC03 e CR350/CS625 ao período a partir de 2026-08-04 11:20 (America/Sao_Paulo).
Nenhum arquivo de Downloads é alterado.
"""
from __future__ import annotations

import csv
import json
from datetime import datetime, timezone
from pathlib import Path
from urllib.parse import urlencode
from urllib.request import urlopen
from zoneinfo import ZoneInfo

API_URL = "https://cluster.simepar.br/simepar-services/horaria"
ESTACAO_PC03 = 25354889
SENSOR_AZUL = 238
NOME_SENSOR = "azul"
TZ = ZoneInfo("America/Sao_Paulo")
INICIO_PC03 = datetime(2026, 8, 4, 11, 20, tzinfo=TZ)


def localizar_dat(downloads: Path) -> Path:
    arquivos = sorted(downloads.glob("*.dat"), key=lambda p: p.stat().st_mtime, reverse=True)
    if not arquivos:
        raise FileNotFoundError(f"Nenhum .dat em {downloads}")
    return arquivos[0]


def ler_toa5(caminho: Path) -> list[dict]:
    with caminho.open("r", encoding="utf-8-sig", newline="") as f:
        linhas = list(csv.reader(f))
    if len(linhas) < 5 or linhas[0][0] != "TOA5":
        raise ValueError(f"TOA5 inválido: {caminho}")
    campos = linhas[1]
    saida: list[dict] = []
    for valores in linhas[4:]:
        if len(valores) != len(campos):
            continue
        r = dict(zip(campos, valores))
        try:
            instante = datetime.strptime(r["TIMESTAMP"], "%Y-%m-%d %H:%M:%S").replace(tzinfo=TZ)
            saida.append({
                "TIMESTAMP": instante.strftime("%Y-%m-%d %H:%M:%S"),
                "RECORD": int(r["RECORD"]),
                "Periodo_CS625_us": float(r["Periodo_CS625_us"]),
                "VWC_m3_m3": float(r["VWC_m3_m3"]),
                "_datahora": instante,
            })
        except (KeyError, ValueError):
            continue
    saida.sort(key=lambda x: x["_datahora"])
    if not saida:
        raise ValueError("Nenhum registro CR350 válido")
    return saida


def parse_utc(texto: str) -> datetime:
    dt = datetime.fromisoformat(texto.replace("Z", "+00:00"))
    return (dt if dt.tzinfo else dt.replace(tzinfo=timezone.utc)).astimezone(timezone.utc)


def baixar_pc03(inicio: datetime, fim: datetime) -> tuple[list[dict], str]:
    params = {
        "datahorai": inicio.astimezone(timezone.utc).strftime("%Y-%m-%dT%H:%M:%S"),
        "datahoraf": fim.astimezone(timezone.utc).strftime("%Y-%m-%dT%H:%M:%S"),
        "tipos": "M,P,H",
        "estacoes": str(ESTACAO_PC03),
        "sensores": str(SENSOR_AZUL),
    }
    url = API_URL + "?" + urlencode(params)
    with urlopen(url, timeout=120) as resposta:
        if resposta.status != 200:
            raise RuntimeError(f"HTTP {resposta.status}: {url}")
        bruto = json.loads(resposta.read().decode("utf-8"))
    if not isinstance(bruto, list):
        raise RuntimeError(f"Resposta inesperada: {bruto!r}")
    unicos: dict[datetime, dict] = {}
    for item in bruto:
        try:
            if int(item["estacaoId"]) != ESTACAO_PC03 or int(item["sensorId"]) != SENSOR_AZUL:
                continue
            local = parse_utc(str(item["datahora"])).astimezone(TZ)
            if inicio <= local <= fim:
                unicos[local] = item
        except (KeyError, TypeError, ValueError):
            continue
    return [unicos[k] for k in sorted(unicos)], url


def escrever_cr350(registros: list[dict], caminho: Path) -> None:
    caminho.parent.mkdir(parents=True, exist_ok=True)
    campos = ["TIMESTAMP", "RECORD", "Periodo_CS625_us", "VWC_m3_m3"]
    with caminho.open("w", encoding="utf-8", newline="") as f:
        w = csv.DictWriter(f, fieldnames=campos)
        w.writeheader()
        for r in registros:
            w.writerow({k: r[k] for k in campos})


def escrever_pc03(registros: list[dict], csv_saida: Path, json_saida: Path) -> list[datetime]:
    csv_saida.parent.mkdir(parents=True, exist_ok=True)
    json_saida.parent.mkdir(parents=True, exist_ok=True)
    with json_saida.open("w", encoding="utf-8") as f:
        json.dump(registros, f, ensure_ascii=False, separators=(",", ":"))
    campos = ["datahora_local", "datahora_utc", "estacaoId", "sensorId", "sensor", "leitura", "qualidadeId"]
    instantes: list[datetime] = []
    with csv_saida.open("w", encoding="utf-8", newline="") as f:
        w = csv.DictWriter(f, fieldnames=campos)
        w.writeheader()
        for item in registros:
            utc = parse_utc(str(item["datahora"]))
            local = utc.astimezone(TZ)
            instantes.append(local)
            w.writerow({
                "datahora_local": local.strftime("%Y-%m-%d %H:%M:%S%z"),
                "datahora_utc": utc.strftime("%Y-%m-%dT%H:%M:%S%z"),
                "estacaoId": item.get("estacaoId"),
                "sensorId": item.get("sensorId"),
                "sensor": NOME_SENSOR,
                "leitura": item.get("leitura"),
                "qualidadeId": item.get("qualidadeId"),
            })
    return instantes


def main() -> int:
    projeto = Path(__file__).resolve().parent.parent
    downloads = Path.home() / "Downloads"
    dat = localizar_dat(downloads)
    cr350_todos = ler_toa5(dat)
    fim = cr350_todos[-1]["_datahora"]
    cr350 = [r for r in cr350_todos if r["_datahora"] >= INICIO_PC03]
    if not cr350:
        raise RuntimeError("O .dat não cobre o início definido para PC03")
    pc03, url = baixar_pc03(INICIO_PC03, fim)
    if not pc03:
        raise RuntimeError("A API não retornou o sensor 238 (azul) da PC03")

    dados_pc03 = projeto / "dados" / "pc03_azul"
    csv_cr350 = dados_pc03 / "processados" / "CR350Series_TabelaSolo_limpo_pc03_azul.csv"
    escrever_cr350(cr350, csv_cr350)
    (dados_pc03 / "brutos").mkdir(parents=True, exist_ok=True)
    (dados_pc03 / "brutos" / "origem_CR350Series_TabelaSolo.dat.txt").write_text(str(dat), encoding="utf-8")

    base = f"pc03_azul_{ESTACAO_PC03}_soil_moisture_{SENSOR_AZUL}_20260804_1120_a_{fim:%Y%m%d_%H%M}"
    destino_api = dados_pc03 / "simepar"
    csv_pc03 = destino_api / f"{base}_local.csv"
    json_pc03 = destino_api / f"{base}_raw_utc.json"
    instantes_pc03 = escrever_pc03(pc03, csv_pc03, json_pc03)
    csv_estavel = destino_api / "pc03_azul_25354889_soil_moisture_238_intervalo_TabelaSolo_local.csv"
    json_estavel = destino_api / "pc03_azul_25354889_soil_moisture_238_raw_utc_interval.json"
    csv_estavel.write_bytes(csv_pc03.read_bytes())
    json_estavel.write_bytes(json_pc03.read_bytes())

    entrada = projeto / "dados" / "entradas_analise" / "pc03_azul"
    entrada.mkdir(parents=True, exist_ok=True)
    (entrada / csv_cr350.name).write_bytes(csv_cr350.read_bytes())
    (entrada / csv_estavel.name).write_bytes(csv_estavel.read_bytes())

    grade = {r["_datahora"] for r in cr350}
    presentes = set(instantes_pc03)
    ausentes = sorted(grade - presentes)
    lacunas = destino_api / "lacunas_pc03_azul_no_intervalo_cr350.csv"
    with lacunas.open("w", encoding="utf-8", newline="") as f:
        w = csv.DictWriter(f, fieldnames=["datahora_local_ausente", "datahora_utc_ausente"])
        w.writeheader()
        for t in ausentes:
            w.writerow({"datahora_local_ausente": t.isoformat(), "datahora_utc_ausente": t.astimezone(timezone.utc).isoformat()})

    resumo = dados_pc03 / "resumos" / "resumo_download_pc03_azul.txt"
    resumo.parent.mkdir(parents=True, exist_ok=True)
    resumo.write_text(
        "Download PC03 — sensor azul\n===========================\n\n"
        f"Arquivo CR350 de Downloads: {dat}\n"
        f"Estação PC03: {ESTACAO_PC03}\nSensor azul: {SENSOR_AZUL} (umidade do solo instantânea)\n"
        f"Período aplicado: {INICIO_PC03.isoformat()} a {fim.isoformat()}\n"
        f"CR350/CS625: {len(cr350)} registros\nPC03 azul: {len(pc03)} registros\n"
        f"Timestamps comuns: {len(grade.intersection(presentes))}\nAusências PC03: {len(ausentes)}\n"
        f"URL: {url}\n",
        encoding="utf-8",
    )
    print(f"DAT={dat}")
    print(f"INICIO={INICIO_PC03.isoformat()}")
    print(f"FIM={fim.isoformat()}")
    print(f"CR350={len(cr350)} PC03_AZUL={len(pc03)} COMUNS={len(grade.intersection(presentes))} AUSENTES={len(ausentes)}")
    print(f"CSV_ANALISE_CR350={entrada / csv_cr350.name}")
    print(f"CSV_ANALISE_PC03={entrada / csv_estavel.name}")
    print(f"RESUMO={resumo}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
