# Painel de Estações Meteorológicas | OCS

Dashboard web para visualização e monitoramento de dados climáticos provenientes de sensores meteorológicos instalados em sítios sentinela no Brasil, desenvolvido pelo **Observatório de Clima e Saúde — ICICT/Fiocruz**.

---

## Estrutura do Projeto

```
painel_estacoes/
├── ui.R                   # Interface do usuário
├── server.R               # Lógica do servidor
├── data/
│   └── estacoes.duckdb    # Banco de dados reduzido para testes locais (não utilizado em produção) 
├── www/
    └── logo_ocs.png       # Logos, ícones, etc.
```
---

## Ferramentas 

- **R** ≥ 4.2
- **RStudio** 
- Principais pacotes R:
  - `shiny`, `bslib`, `DBI`
  - `plotly`, `leaflet`, `RPostgres`
  - `dplyr`, `duckdb`
  - `lubridate`, `fontawesome`

---

## Acesso ao Dashboard
O Dashboard está disponível online, no seguinte link: 
[https://shiny.icict.fiocruz.br/painel_estacoes/](https://shiny.icict.fiocruz.br/painel_estacoes/)

---

## Funcionalidades do Dashboard

| Componente | Descrição |
|-----|-----------|
| **KPIs** | Indicadores resumidos com valores mais recentes |
| **Gráficos** | Séries temporais interativas por sensor |
| **Mapa** | Visualização geográfica das estações |
| **Filtros** | Seleção por estação, data e sensor meteorológico |

---

## Dados

Os dados são coletados por sensores em estações meteorológicas distribuídas pelo Brasil, armazenados em banco de dados do Icict/Fiocruz e disponibilizados ao dashboard por meio de uma API.

- **Intervalo de atualização** — Tempo real 
- **Principais variáveis** — Temperatura, sensação térmica, qualidade do ar, umidade, pressão atmosférica, chuva

---

## Arquitetura

O Dashboard é composto por duas partes principais:

### Dashboard (Frontend)
- Desenvolvido em **R Shiny**
- Responsável pela interface do usuário, pela reatividade e pelas visualizações de dados.

### API de Dados (Backend)
- Responsável por coletar dados das estações meteorológicas, processá-los, armazená-los no banco de dados do ICICT/Fiocruz e disponibilizá-los para consumo pelo dashboard.

---

## Equipe de Desenvolvimento

### Dashboard (Frontend - R Shiny)
- **Maira Alejandra Moreno**  
  [![GitHub](https://img.shields.io/badge/GitHub-Perfil-181717?style=for-the-badge&logo=github)](https://github.com/mairamorenoc)  

### API de Dados (Backend)
- **Raphael Saldanha**  
  [![GitHub](https://img.shields.io/badge/GitHub-Perfil-181717?style=for-the-badge&logo=github)](https://github.com/rfsaldanha)

---

## Contato

Observatório de Clima e Saúde — ICICT/Fiocruz  
[https://climaesaude.icict.fiocruz.br](https://climaesaude.icict.fiocruz.br)
[![Email](https://img.shields.io/badge/Email-Contato-D14836?style=for-the-badge&logo=gmail&logoColor=white)](mailto:
obs.climaesaude@fiocruz.br)