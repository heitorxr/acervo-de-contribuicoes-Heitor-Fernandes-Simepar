#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""Baixa dados horários da estação de verificação SIMEPAR para o projeto Analise_ET.

Saída esperada pelos scripts R:
    dados/entrada/dados_simepar_25264916.csv

Colunas:
    datahora,temperatura,umidade,6

A coluna "6" mantém o nome usado historicamente pelos scripts R para radiação solar.
"""

from __future__ import annotations

import argparse
import csv
import json
import sys
from collections import defaultdict
from datetime import datetime, timedelta, timezone
from pathlib import Path
from urllib.parse import urlencode
from urllib.request import urlopen
from zoneinfo import ZoneInfo

API_URL = "https://cluster.simepar.br/simepar-services/horaria"
POSTO_CODIGO = 25264916
SENSORES = [2, 6, 1]  # 1=temperatura, 2=umidade, 6=radiacao solar
SENSOR_COLUNAS = {1: "temperatura", 2: "umidade", 6: "6"}
TZ_LOCAL = ZoneInfo("America/Sao_Paulo")


def parse_local_datetime(texto: str) -> datetime:
    """Aceita 'YYYY-MM-DD' ou 'YYYY-MM-DD HH:MM' em America/Sao_Paulo."""
    texto = texto.strip()
    for fmt in ("%Y-%m-%d %H:%M:%S", "%Y-%m-%d %H:%M", "%Y-%m-%d"):
        try:
            dt = datetime.strptime(texto, fmt)
            return dt.replace(tzinfo=TZ_LOCAL)
        except ValueError:
            pass
    raise ValueError(f"Data local inválida: {texto!r}")


def parse_utc_datetime(texto: str) -> datetime:
    texto = texto.replace("Z", "+00:00")
    dt = datetime.fromisoformat(texto)
    if dt.tzinfo is None:
        dt = dt.replace(tzinfo=timezone.utc)
    return dt.astimezone(timezone.utc)


def montar_url(inicio_utc: datetime, fim_utc: datetime, posto_codigo: int, sensores: list[int]) -> str:
    params = {
        "datahorai": inicio_utc.strftime("%Y-%m-%dT%H:00:00"),
        "datahoraf": fim_utc.strftime("%Y-%m-%dT%H:00:00"),
        "tipos": "M,P,H",
        "estacoes": str(posto_codigo),
        "sensores": ",".join(map(str, sensores)),
    }
    return API_URL + "?" + urlencode(params)


def baixar_intervalo(inicio_utc: datetime, fim_utc: datetime, posto_codigo: int, sensores: list[int]) -> list[dict]:
    url = montar_url(inicio_utc, fim_utc, posto_codigo, sensores)
    with urlopen(url, timeout=90) as response:
        if response.status != 200:
            raise RuntimeError(f"Erro HTTP {response.status} ao baixar {url}")
        payload = response.read().decode("utf-8")
    dados = json.loads(payload)
    if not isinstance(dados, list):
        raise RuntimeError(f"Resposta inesperada da API: {type(dados).__name__}")
    return dados


def baixar_periodo(inicio_utc: datetime, fim_utc: datetime, posto_codigo: int, sensores: list[int], dias_por_lote: int = 10) -> list[dict]:
    todos: list[dict] = []
    atual = inicio_utc
    delta = timedelta(days=dias_por_lote)
    while atual < fim_utc:
        prox = min(atual + delta, fim_utc)
        print(f"Coletando {atual.isoformat()} até {prox.isoformat()}...", flush=True)
        todos.extend(baixar_intervalo(atual, prox, posto_codigo, sensores))
        atual = prox
    return todos


def pivotar_registros(registros: list[dict]) -> list[dict]:
    por_hora: dict[str, dict] = defaultdict(dict)
    for item in registros:
        try:
            sensor_id = int(item.get("sensorId"))
        except Exception:
            continue
        coluna = SENSOR_COLUNAS.get(sensor_id)
        if coluna is None:
            continue
        datahora_raw = item.get("datahora")
        if not datahora_raw:
            continue
        datahora_local = parse_utc_datetime(str(datahora_raw)).astimezone(TZ_LOCAL)
        chave = datahora_local.strftime("%Y-%m-%d %H:%M:%S%z")
        chave = chave[:-2] + ":" + chave[-2:]  # -0300 -> -03:00
        por_hora[chave]["datahora"] = chave
        por_hora[chave][coluna] = item.get("leitura")

    linhas = []
    for chave in sorted(por_hora):
        linha = {"datahora": chave, "temperatura": "", "umidade": "", "6": ""}
        linha.update(por_hora[chave])
        linhas.append(linha)
    return linhas


def escrever_csv(linhas: list[dict], saida: Path) -> None:
    saida.parent.mkdir(parents=True, exist_ok=True)
    with saida.open("w", encoding="utf-8", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=["datahora", "temperatura", "umidade", "6"])
        writer.writeheader()
        writer.writerows(linhas)


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description="Baixa dados SIMEPAR para Analise_ET.")
    parser.add_argument("--inicio-local", default="2026-06-18 00:00", help="Início em America/Sao_Paulo. Ex.: '2026-06-18 00:00'.")
    parser.add_argument("--fim-local", default=None, help="Fim em America/Sao_Paulo. Padrão: agora.")
    parser.add_argument("--saida", default="dados/entrada/dados_simepar_25264916.csv", help="CSV de saída relativo ao diretório do projeto.")
    parser.add_argument("--posto", type=int, default=POSTO_CODIGO)
    args = parser.parse_args(argv)

    inicio_local = parse_local_datetime(args.inicio_local)
    fim_local = parse_local_datetime(args.fim_local) if args.fim_local else datetime.now(TZ_LOCAL)
    inicio_utc = inicio_local.astimezone(timezone.utc)
    fim_utc = fim_local.astimezone(timezone.utc)

    registros = baixar_periodo(inicio_utc, fim_utc, args.posto, SENSORES)
    linhas = pivotar_registros(registros)
    saida = Path(args.saida)
    escrever_csv(linhas, saida)

    print(f"Registros brutos baixados: {len(registros)}")
    print(f"Linhas horárias gravadas: {len(linhas)}")
    print(f"Arquivo: {saida}")
    if linhas:
        print(f"Primeira linha: {linhas[0]['datahora']}")
        print(f"Última linha: {linhas[-1]['datahora']}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
