# OncoDatasusR

Este pacote R apresenta funções para download e processamento dos arquivos de dados de tratamento oncológico ambulatorial e hospitalar do DATASUS, para contagem de casos de câncer no Brasil.

## Instalação

Ao iniciar a instalação, o R solicitará a instalação de pacotes de dependências que ainda não estejam instalados. Se o usuário aceitar, esses pacotes serão instalados automaticamente junto com o  **OncoDatasusR** .

Você pode instalar o pacote com o comando:

```R
if (!require("devtools")) install.packages("devtools")
devtools::install_github("PatriciaKZ/OncoDatasusR", build_vignettes = TRUE)
```

Para compilar o pacote, é necessário instalar o [Rtools](https://cran.r-project.org/bin/windows/Rtools/) em seu computador.

## Utilização

A utilização do pacote consiste em três etapas: download dos arquivos, processamento dos arquivos e contagem de casos.
Para mais detalhes consulte os vignettes do pacote.

```R
# Análise de câncer de colo de útero
vignette("cervical_cancer_apac_example", package = "OncoDatasusR")
vignette("cervical_cancer_aih_example", package = "OncoDatasusR")

# Análises de câncer de pâncreas
vignette("pancreatic_cancer_apac_example", package = "OncoDatasusR")
vignette("pancreatic_cancer_aih_example", package = "OncoDatasusR")
```

Veja também as sugestões de relatórios automatizados para apresentação dos resultados da análise:

```R
# Relatórios de câncer de colo de útero
vignette("cervical_cancer_apac_report", package = "OncoDatasusR")
vignette("cervical_cancer_aih_report", package = "OncoDatasusR")

# Relatórios de câncer de pâncreas
vignette("pancreatic_cancer_apac_report", package = "OncoDatasusR")
vignette("pancreatic_cancer_aih_report", package = "OncoDatasusR")
```

## Fontes dos dados

O pacote realiza o dowload e processa os registros dos Banco de Dados das APACs de Quimioterapia e Radioterapia do Sistema de Informações Ambulatoriais do SUS (SIA-SUS) e do banco de dados das Autorizações de Internação Hospitalar (AIH) - Reduzida (RD) do Sistema de Informações Hospitalares do SUS (SIH-SUS)

## Apoio financeiro

Este pacote foi desenvolvido com o apoio financeiro da F. Hoffmann-La Roche Ltd - Brasil.

## Agradecimentos:

Agradecemos a todos os colabores deste projeto que contribuiram com o desenvolvimento do pacote.

## Como citar

Para citar o pacote OncoDatasusR em publicações, utilize:

```R
citation("OncoDatasusR")
```

O artigo de referência do pacote está processo de revisão por pares e será disponibilizado em breve.

## Dúvidas e sugestões

Crie uma [issue](https://github.com/PatriciaKZ/OncoDatasusR/issues) no projeto ou envie um email para patriciakz99@gmail.com.
