#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""Atualiza os dados da comparação CR350/CS625 x plugfild (PC01).

Fluxo:
1. localiza o .dat mais recente em Downloads;
2. arquiva os dados ativos anteriores em dados_antigos/<timestamp>/;
3. copia e converte o novo .dat;
4. baixa da SIMEPAR a estação 25334889, sensor 238, no intervalo exato
   coberto pelo .dat;
5. atualiza as cópias isoladas em dados/entradas_analise/plugfild_pc01/ e grava um resumo.

O acesso à SIMEPAR segue o mesmo endpoint e as mesmas funções auxiliares de
Documents/Analise_ET/scripts/baixar_dados_simepar.py, mas preserva minutos e
segundos nos limites para corresponder exatamente ao novo arquivo do CR350.
"""
from __future__ import annotations

import argparse
import csv
import importlib.util
import json
import shutil
from datetime import datetime, timezone
from pathlib import Path
from urllib.parse import urlencode
from urllib.request import urlopen
from zoneinfo import ZoneInfo

ESTACAO_PLUGFILD = 25334889
SENSOR_PLUGFILD = 238
SENSOR_NOME = "soil_moisture_instantaneous"
TZ_LOCAL = ZoneInfo("America/Sao_Paulo")


def carregar_downloader_existente(caminho: Path):
    spec = importlib.util.spec_from_file_location("baixar_dados_simepar_existente", caminho)
    if spec is None or spec.loader is None:
        raise RuntimeError(f"Não foi possível carregar {caminho}")
    modulo = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(modulo)
    return modulo


def localizar_dat_mais_recente(downloads: Path) -> Path:
    arquivos = sorted(downloads.glob("*.dat"), key=lambda p: p.stat().st_mtime, reverse=True)
    if not arquivos:
        raise FileNotFoundError(f"Nenhum arquivo .dat encontrado em {downloads}")
    return arquivos[0]


def ler_dat_toa5(caminho: Path) -> tuple[list[str], list[dict], dict]:
    with caminho.open("r", encoding="utf-8-sig", newline="") as f:
        linhas = list(csv.reader(f))
    if len(linhas) < 5 or not linhas[0] or linhas[0][0] != "TOA5":
        raise ValueError(f"Arquivo não parece TOA5 válido: {caminho}")
    cabecalho = linhas[1]
    registros: list[dict] = []
    for numero, valores in enumerate(linhas[4:], start=5):
        if len(valores) != len(cabecalho):
            continue
        linha = dict(zip(cabecalho, valores))
        try:
            datahora = datetime.strptime(linha["TIMESTAMP"], "%Y-%m-%d %H:%M:%S").replace(tzinfo=TZ_LOCAL)
            periodo = float(linha["Periodo_CS625_us"])
            vwc = float(linha["VWC_m3_m3"])
            record = int(linha["RECORD"])
        except (KeyError, TypeError, ValueError):
            continue
        registros.append({
            "TIMESTAMP": datahora.strftime("%Y-%m-%d %H:%M:%S"),
            "RECORD": record,
            "Periodo_CS625_us": periodo,
            "VWC_m3_m3": vwc,
            "_datahora": datahora,
        })
    if not registros:
        raise ValueError(f"Nenhum registro válido no .dat: {caminho}")
    registros.sort(key=lambda r: r["_datahora"])
    meta = {
        "formato": linhas[0][0],
        "estacao": linhas[0][1],
        "modelo": linhas[0][2],
        "serial": linhas[0][3],
        "programa": linhas[0][5],
        "tabela": linhas[0][7],
    }
    return cabecalho, registros, meta


def arquivar_dados_ativos(projeto: Path, destino: Path) -> list[Path]:
    origens = [
        projeto / "dados" / "cr350_cs625" / "brutos",
        projeto / "dados" / "cr350_cs625" / "processados",
        projeto / "dados" / "cr350_cs625" / "resumos",
        projeto / "dados" / "plugfild_pc01" / "simepar",
        projeto / "dados" / "entradas_analise" / "plugfild_pc01",
    ]
    movidos: list[Path] = []
    for origem in origens:
        if not origem.exists():
            continue
        rel_dir = origem.relative_to(projeto)
        for arquivo in sorted(origem.iterdir()):
            if not arquivo.is_file():
                continue
            alvo = destino / rel_dir / arquivo.name
            alvo.parent.mkdir(parents=True, exist_ok=True)
            shutil.move(str(arquivo), str(alvo))
            movidos.append(alvo)
    return movidos


def escrever_csv_cr350(registros: list[dict], caminho: Path) -> None:
    caminho.parent.mkdir(parents=True, exist_ok=True)
    campos = ["TIMESTAMP", "RECORD", "Periodo_CS625_us", "VWC_m3_m3"]
    with caminho.open("w", encoding="utf-8", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=campos)
        writer.writeheader()
        for registro in registros:
            writer.writerow({campo: registro[campo] for campo in campos})


def montar_url_exata(api_url: str, inicio_utc: datetime, fim_utc: datetime) -> str:
    parametros = {
        "datahorai": inicio_utc.strftime("%Y-%m-%dT%H:%M:%S"),
        "datahoraf": fim_utc.strftime("%Y-%m-%dT%H:%M:%S"),
        "tipos": "M,P,H",
        "estacoes": str(ESTACAO_PLUGFILD),
        "sensores": str(SENSOR_PLUGFILD),
    }
    return api_url + "?" + urlencode(parametros)


def baixar_plugfild(api_url: str, parser_utc, inicio_local: datetime, fim_local: datetime) -> tuple[list[dict], str]:
    inicio_utc = inicio_local.astimezone(timezone.utc)
    fim_utc = fim_local.astimezone(timezone.utc)
    url = montar_url_exata(api_url, inicio_utc, fim_utc)
    with urlopen(url, timeout=120) as resposta:
        if resposta.status != 200:
            raise RuntimeError(f"Erro HTTP {resposta.status} ao baixar {url}")
        dados = json.loads(resposta.read().decode("utf-8"))
    if not isinstance(dados, list):
        raise RuntimeError(f"Resposta inesperada da API: {type(dados).__name__}")
    unicos: dict[datetime, dict] = {}
    for item in dados:
        try:
            if int(item.get("estacaoId")) != ESTACAO_PLUGFILD or int(item.get("sensorId")) != SENSOR_PLUGFILD:
                continue
            instante_utc = parser_utc(str(item["datahora"]))
        except Exception:
            continue
        instante_local = instante_utc.astimezone(TZ_LOCAL)
        if inicio_local <= instante_local <= fim_local:
            unicos[instante_local] = item
    ordenados = [unicos[chave] for chave in sorted(unicos)]
    return ordenados, url


def escrever_plugfild(registros: list[dict], csv_saida: Path, json_saida: Path, parser_utc) -> None:
    csv_saida.parent.mkdir(parents=True, exist_ok=True)
    json_saida.parent.mkdir(parents=True, exist_ok=True)
    with json_saida.open("w", encoding="utf-8") as f:
        json.dump(registros, f, ensure_ascii=False, separators=(",", ":"))
    campos = ["datahora_local", "datahora_utc", "estacaoId", "sensorId", "sensor", "leitura", "qualidadeId"]
    with csv_saida.open("w", encoding="utf-8", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=campos)
        writer.writeheader()
        for item in registros:
            instante_utc = parser_utc(str(item["datahora"]))
            instante_local = instante_utc.astimezone(TZ_LOCAL)
            writer.writerow({
                "datahora_local": instante_local.strftime("%Y-%m-%d %H:%M:%S%z"),
                "datahora_utc": instante_utc.strftime("%Y-%m-%dT%H:%M:%S%z"),
                "estacaoId": item.get("estacaoId"),
                "sensorId": item.get("sensorId"),
                "sensor": SENSOR_NOME,
                "leitura": item.get("leitura"),
                "qualidadeId": item.get("qualidadeId"),
            })


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--projeto", type=Path, default=Path(__file__).resolve().parent.parent)
    parser.add_argument("--downloads", type=Path, default=Path.home() / "Downloads")
    parser.add_argument("--downloader-existente", type=Path, default=Path.home() / "Documents" / "Analise_ET" / "scripts" / "baixar_dados_simepar.py")
    args = parser.parse_args()

    projeto = args.projeto.resolve()
    dat_origem = localizar_dat_mais_recente(args.downloads)
    _, cr350, meta = ler_dat_toa5(dat_origem)
    inicio_local = cr350[0]["_datahora"]
    fim_local = cr350[-1]["_datahora"]

    downloader = carregar_downloader_existente(args.downloader_existente)
    plugfild, url = baixar_plugfild(downloader.API_URL, downloader.parse_utc_datetime, inicio_local, fim_local)
    if not plugfild:
        raise RuntimeError("A API não retornou dados do plugfild no intervalo do .dat; nada foi arquivado.")

    carimbo = datetime.now(TZ_LOCAL).strftime("%Y%m%d_%H%M%S")
    arquivo_antigo = projeto / "dados_antigos" / carimbo
    movidos = arquivar_dados_ativos(projeto, arquivo_antigo)

    bruto_destino = projeto / "dados" / "cr350_cs625" / "brutos" / "CR350Series_TabelaSolo.dat"
    bruto_destino.parent.mkdir(parents=True, exist_ok=True)
    shutil.copy2(dat_origem, bruto_destino)

    convertido = projeto / "dados" / "cr350_cs625" / "processados" / "CR350Series_TabelaSolo_convertido.csv"
    limpo = projeto / "dados" / "cr350_cs625" / "processados" / "CR350Series_TabelaSolo_limpo.csv"
    escrever_csv_cr350(cr350, convertido)
    shutil.copy2(convertido, limpo)

    prefixo = (
        f"plugfild_{ESTACAO_PLUGFILD}_soil_moisture_{SENSOR_PLUGFILD}_"
        f"{inicio_local:%Y%m%d_%H%M}_a_{fim_local:%Y%m%d_%H%M}"
    )
    pasta_plugfild = projeto / "dados" / "plugfild_pc01" / "simepar"
    csv_intervalo = pasta_plugfild / f"{prefixo}_local.csv"
    json_intervalo = pasta_plugfild / f"{prefixo}_raw_utc.json"
    escrever_plugfild(plugfild, csv_intervalo, json_intervalo, downloader.parse_utc_datetime)

    csv_estavel = pasta_plugfild / "plugfild_25334889_soil_moisture_238_intervalo_TabelaSolo_local.csv"
    json_estavel = pasta_plugfild / "plugfild_25334889_soil_moisture_238_raw_utc_interval.json"
    shutil.copy2(csv_intervalo, csv_estavel)
    shutil.copy2(json_intervalo, json_estavel)

    dados_analise = projeto / "dados" / "entradas_analise" / "plugfild_pc01"
    dados_analise.mkdir(parents=True, exist_ok=True)
    shutil.copy2(limpo, dados_analise / limpo.name)
    shutil.copy2(csv_estavel, dados_analise / csv_estavel.name)

    parser_utc = downloader.parse_utc_datetime
    plug_datas = [parser_utc(str(item["datahora"])).astimezone(TZ_LOCAL) for item in plugfild]
    cr_datas = {r["_datahora"] for r in cr350}
    plug_datas_conjunto = set(plug_datas)
    comuns = cr_datas.intersection(plug_datas_conjunto)
    ausentes_plugfild = sorted(cr_datas - plug_datas_conjunto)
    esperados = int((fim_local - inicio_local).total_seconds() // 300) + 1

    lacunas_csv = pasta_plugfild / "lacunas_plugfild_no_intervalo_cr350.csv"
    with lacunas_csv.open("w", encoding="utf-8", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=["datahora_local_ausente", "datahora_utc_ausente"])
        writer.writeheader()
        for instante in ausentes_plugfild:
            writer.writerow({
                "datahora_local_ausente": instante.isoformat(),
                "datahora_utc_ausente": instante.astimezone(timezone.utc).isoformat(),
            })

    resumo = projeto / "dados" / "cr350_cs625" / "resumos" / "resumo_TabelaSolo_e_plugfild.txt"
    resumo.parent.mkdir(parents=True, exist_ok=True)
    resumo.write_text(
        "Atualização dos dados de umidade do solo\n"
        "========================================\n\n"
        f"Arquivo .dat de origem: {dat_origem}\n"
        f"Arquivo .dat ativo: {bruto_destino}\n"
        f"Programa CRBasic: {meta['programa']}\n"
        f"Tabela: {meta['tabela']}\n"
        f"Intervalo local: {inicio_local.isoformat()} a {fim_local.isoformat()}\n"
        f"Intervalo UTC: {inicio_local.astimezone(timezone.utc).isoformat()} a {fim_local.astimezone(timezone.utc).isoformat()}\n"
        f"Registros CR350/CS625 válidos: {len(cr350)}\n"
        f"Registros plugfild/PC01 baixados: {len(plugfild)}\n"
        f"Registros esperados a cada 5 min: {esperados}\n"
        f"Timestamps comuns exatos: {len(comuns)}\n"
        f"Timestamps ausentes no plugfild: {len(ausentes_plugfild)}\n"
        f"Arquivo de lacunas: {lacunas_csv}\n"
        f"Primeiro plugfild local: {min(plug_datas).isoformat()}\n"
        f"Último plugfild local: {max(plug_datas).isoformat()}\n"
        f"URL consultada: {url}\n"
        f"Dados antigos arquivados em: {arquivo_antigo}\n"
        f"Arquivos antigos movidos: {len(movidos)}\n",
        encoding="utf-8",
    )

    print(f"DAT_ORIGEM={dat_origem}")
    print(f"ARQUIVO_ANTIGO={arquivo_antigo}")
    print(f"ANTIGOS_MOVIDOS={len(movidos)}")
    print(f"INICIO_LOCAL={inicio_local.isoformat()}")
    print(f"FIM_LOCAL={fim_local.isoformat()}")
    print(f"CR350_REGISTROS={len(cr350)}")
    print(f"PLUGFILD_REGISTROS={len(plugfild)}")
    print(f"ESPERADOS_5MIN={esperados}")
    print(f"TIMESTAMPS_COMUNS={len(comuns)}")
    print(f"TIMESTAMPS_AUSENTES_PLUGFILD={len(ausentes_plugfild)}")
    print(f"LACUNAS_CSV={lacunas_csv}")
    print(f"CSV_PLUGFILD={csv_intervalo}")
    print(f"CSV_ANALISE={dados_analise / csv_estavel.name}")
    print(f"RESUMO={resumo}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
