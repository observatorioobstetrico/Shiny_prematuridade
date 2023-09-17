
# Bibliotecas ---------------

#shiny
library(shiny)
library(shinythemes)
library(shinycssloaders)
library(shinydashboard)
library(shinyjs)
library(shinyWidgets)
library(markdown)
#dados
library(dplyr)
library(janitor)
library(tidyr)
#graficos
library(ggplot2)
library(ggpubr)
library(patchwork)


# UI -----------------------

ui <- navbarPage(
  theme = "meu_cerulean.css",  
  title = span(
    img(
      src = 'logo.png',
      style = "height: 30px; margin: auto;"
    ),
    "NASCIMENTO PREMATURO NO BRASIL"
  ),
  # menu analise descritiva -----
  navbarMenu(
    "Descritiva", 
    icon = icon("chart-line"),
    # painel de graficos de dispersao -----
    tabPanel(
      "Gráficos de dispersão", 
      fluidPage(
        sidebarLayout(
          sidebarPanel(
            print(h3("Gráficos de dispersão")),
            hr(),
            width = 4,
            radioButtons(
              inputId = "RadioBase1Vars", 
              label = h4("Selecione a base de variáveis:"),
              choices = c("SINASC" = "sinasc", "Atlas Brasil" = "atlbr"),
              selected = "sinasc"
            ),
            radioButtons(
              inputId = "RadioSUS", 
              label = h4("Considerar vínculo com o SUS?"),
              choices = c("Não" = 'nao', "Sim" = 'sim'),
              selected = "nao"
            ),
            shiny::conditionalPanel(
              condition = "input.RadioSUS == 'sim'",
              selectInput(
                inputId = "SelectSUS",
                label = h4("Estabelecimentos de saúde:"),
                choices = c("Com vínculo com o SUS" = 'sim', "Sem vínculo com o SUS" = 'nao'),
                selected = "sim",
                multiple = FALSE
              )
            ),
            tags$div(submitButton("Atualizar Pesquisa", icon = icon("arrows-rotate")), `align` = "center"),
            hr(),
            shiny::conditionalPanel(
              condition = "input.RadioBase1Vars == 'sinasc'",
              selectInput(
                inputId = "SelectVarSINASC",
                label = h4("Selecione a variável de interesse:"),
                choices = c(
                  "APGAR1" = "apgar1",
                  "APGAR 5" = "apgar5",
                  "N° de consultas de pré-natal" = "consprenat",
                  "Escolaridade da mãe" = "escmae",
                  "Estado civil da mãe" = "estcivmae",
                  "Tipo de gravidez" = "tipograv",
                  "Idade da mãe" = "idademae",
                  "Local de nascimento" = "locnasc",
                  "Mês que iniciou o pré-natal" = "mesprenat",
                  "Tipo de parto" = "tipoparto",
                  "Quantidade de filhos vivos" = "qtdfilvivo",
                  "Quantidade de gestações anteriores" = "qtdgest",
                  "Raça/cor da mãe" = "racacormae",
                  "Raça/cor do RN" = "racacor",
                  "Sexo do RN" = "sexo"
                ),
                selected = "apgar1",
                multiple = FALSE
              )
            ),
            shiny::conditionalPanel(
              condition = "input.RadioBase1Vars == 'atlbr'",
              selectInput(
                inputId = "SelectVarAtlasBR",
                label = h4("Selecione a variável de interesse:"),
                choices = c("IDHM 2010 - Estado" = "idhm_2010_estado",
                            "Índice de Gini 2010 - Estado" = "indice_de_gini_2010_estado"),
                selected = "indice_de_gini_2010_estado",
                multiple = FALSE
              )
            ),
            tags$div(submitButton("Atualizar Variável", icon = icon("arrows-rotate")), `align` = "center")
          ),
          # tela de visualizacao 1 -----
          mainPanel(
            width = 8,
            shinycssloaders::withSpinner(
              plotOutput("Plot1", height = "700px"), 
              color = getOption("spinner.color", "#32a0ff"), 
              type = getOption("spinner.type", 1)
            ),
            hr()
          )
        )
      )
    ),
    # painel de graficos de boxplot -----
    tabPanel(
      "Gráficos de boxplot",
      fluidPage(
        sidebarLayout(
          sidebarPanel(
            print(h3("Gráficos de boxplot")),
            hr(),
            width = 4,
            radioButtons(
              inputId = "RadioBase2Vars", 
              label = h4("Selecione a base de variáveis:"),
              choices = c("CNES" = "cnes"),
              selected = "cnes"
            ),
            selectInput(
              inputId = "SelectVarCNES",
              label = h4("Selecione a variável de interesse:"),
              choices = c("Vínculo com o SUS" = "vinc_sus"),
              selected = "vinc_sus",
              multiple = FALSE
            ),
            tags$div(submitButton("Atualizar Variável", icon = icon("arrows-rotate")), `align` = "center"),
          ),
          # tela de visualizacao 2 -----
          mainPanel(
            width = 8,
            shinycssloaders::withSpinner(
              plotOutput("Plot2", height = "700px"),
              color = getOption("spinner.color", "#32a0ff"),
              type = getOption("spinner.type", 1)
            ),
            hr()
          )
        )
      )
    )
  ),
  # menu mmca -----
  tabPanel(
    "MMCA", icon = icon("th", lib = "glyphicon")
  ),
  # menu sobre -----
  tabPanel(
    "Sobre", icon = icon("info-sign", lib = "glyphicon"),
    includeMarkdown("sobre.md")
  )
)


# Dados ------------------

dados <- readr::read_csv("7_base_final_premat_14-20[2023-08-23].csv.gz") |> 
  select(-consprenat_na) |> 
  # incluindo zero em informacoes na nas covariaveis do sinasc
  mutate_at(c(10:65), ~replace_na(., 0)) |> 
  # selecionando somente o ultimo ano da analise
  filter(ano == 2020)


# Server -----------------

server <- function(input, output){
  # graficos de dispersao - sinasc ----
  output$Plot1 <- renderPlot({
    # apgar1
    if (input$RadioBase1Vars == "sinasc" & input$RadioSUS == "nao" & 
          input$SelectVarSINASC == 'apgar1'){
      a <- dados |> 
        ggplot(aes(x = (apgar1_menor_que_4/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson", 
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      b <- dados |> 
        ggplot(aes(x = (apgar1_menor_que_4/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson", 
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      c <- dados |> 
        ggplot(aes(x = (apgar1_de_4_a_7/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson", 
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      d <- dados |> 
        ggplot(aes(x = (apgar1_de_4_a_7/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson", 
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      e <- dados |> 
        ggplot(aes(x = (apgar1_maior_que_7/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson", 
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      f <- dados |> 
        ggplot(aes(x = (apgar1_maior_que_7/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson", 
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
     return(
       ggarrange(
        a, b, c, d, e, f,
        labels = c("<4", "", "4-7", "", ">7", ""), hjust = 0,
        ncol = 2, nrow = 3
        )
      )
    } 
    # apgar5
    else if (input$RadioBase1Vars == "sinasc" & input$RadioSUS == "nao" & 
               input$SelectVarSINASC == 'apgar5'){
      a <- dados |> 
        ggplot(aes(x = (apgar5_menor_que_4/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson", 
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      b <- dados |> 
        ggplot(aes(x = (apgar5_menor_que_4/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson", 
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      c <- dados |> 
        ggplot(aes(x = (apgar5_de_4_a_7/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson", 
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      d <- dados |> 
        ggplot(aes(x = (apgar5_de_4_a_7/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson", 
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      e <- dados |> 
        ggplot(aes(x = (apgar5_maior_que_7/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson", 
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      f <- dados |> 
        ggplot(aes(x = (apgar5_maior_que_7/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson", 
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      return(
        ggarrange(
          a, b, c, d, e, f,
          labels = c("<4", "", "4-7", "", ">7", ""), hjust = 0,
          ncol = 2, nrow = 3       
        )
      )
    } 
    # numero de consultas de pre-natal
    else if (input$RadioBase1Vars == "sinasc" & input$RadioSUS == "nao" & 
               input$SelectVarSINASC == 'consprenat'){
      a <- dados |> 
        ggplot(aes(x = (consprenat_menor_que_6_consultas/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson", 
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      b <- dados |> 
        ggplot(aes(x = (consprenat_menor_que_6_consultas/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson", 
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      c <- dados |> 
        ggplot(aes(x = (consprenat_maior_ou_igual_a_6_consultas/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson", 
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      d <- dados |> 
        ggplot(aes(x = (consprenat_maior_ou_igual_a_6_consultas/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson", 
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      return(
        ggarrange(
          a, b, c, d,
          labels = c("<6", "", ">=6", ""), hjust = 0,
          ncol = 2, nrow = 2      
        )
      )
    } 
    # escolaridade da mae
    else if (input$RadioBase1Vars == "sinasc" & input$RadioSUS == "nao" & 
               input$SelectVarSINASC == 'escmae'){
      a <- dados |> 
        ggplot(aes(x = (escmae2010_sem_escolaridade/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson", 
                 label.x = 50, label.y = 100, size = 4.5, color = "red")
      
      b <- dados |> 
        ggplot(aes(x = (escmae2010_sem_escolaridade/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson", 
                 label.x = 50, label.y = 100, size = 4.5, color = "red")
      
      c <- dados |> 
        ggplot(aes(x = (escmae2010_fundamental_1_1_a_4_serie/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson", 
                 label.x = 50, label.y = 100, size = 4.5, color = "red")
      
      d <- dados |> 
        ggplot(aes(x = (escmae2010_fundamental_1_1_a_4_serie/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson", 
                 label.x = 50, label.y = 100, size = 4.5, color = "red")
      
      e <- dados |> 
        ggplot(aes(x = (escmae2010_fundamental_2_5_a_8_serie/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson", 
                 label.x = 50, label.y = 100, size = 4.5, color = "red")
      
      f <- dados |> 
        ggplot(aes(x = (escmae2010_fundamental_2_5_a_8_serie/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson", 
                 label.x = 50, label.y = 100, size = 4.5, color = "red")
      
      g <- dados |> 
        ggplot(aes(x = (escmae2010_medio/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson", 
                 label.x = 50, label.y = 100, size = 4.5, color = "red")
      
      h <- dados |> 
        ggplot(aes(x = (escmae2010_medio/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson", 
                 label.x = 50, label.y = 100, size = 4.5, color = "red")
      
      i <- dados |> 
        ggplot(aes(x = (escmae2010_superior_incompleto/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson", 
                 label.x = 50, label.y = 100, size = 4.5, color = "red")
      
      j <- dados |> 
        ggplot(aes(x = (escmae2010_superior_incompleto/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson", 
                 label.x = 50, label.y = 100, size = 4.5, color = "red")
      
      k <- dados |> 
        ggplot(aes(x = (escmae2010_superior_completo/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson", 
                 label.x = 50, label.y = 100, size = 4.5, color = "red")
      
      l <- dados |> 
        ggplot(aes(x = (escmae2010_superior_completo/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson", 
                 label.x = 50, label.y = 100, size = 4.5, color = "red")
      
      return(
        ggarrange(
          a, b, c, d, e, f, g, h, i, j, k, l,
          labels = c(
            "Sem escolaridade", "", "Ens. Fund.: 1ª a 4ª série", "", "Ens. Fund.: 5ª a 8ª série", "", 
            "Ens. Médio", "", "Ens. Sup. Incompleto", "", "Ens. Sup. Incompleto", ""), 
          hjust = 0,
          ncol = 4, nrow = 3       
        )
      )
    }
    # estado civil da mae
    else if (input$RadioBase1Vars == "sinasc" & input$RadioSUS == "nao" & 
               input$SelectVarSINASC == 'estcivmae'){
      a <- dados |> 
        ggplot(aes(x = (estcivmae_casada_uniao_estavel/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson", 
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      b <- dados |> 
        ggplot(aes(x = (estcivmae_casada_uniao_estavel/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson", 
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      c <- dados |> 
        ggplot(aes(x = (estcivmae_outros/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson", 
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      d <- dados |> 
        ggplot(aes(x = (estcivmae_outros/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson", 
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      return(
        ggarrange(
          a, b, c, d,
          labels = c("Casada/Un. Estável", "", "Outro", ""), hjust = 0,
          ncol = 2, nrow = 2      
        )
      )
    } 
    # tipo de gravidez
    else if (input$RadioBase1Vars == "sinasc" & input$RadioSUS == "nao" & 
               input$SelectVarSINASC == 'tipograv'){
      a <- dados |> 
        ggplot(aes(x = (gravidez_unica/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      b <- dados |> 
        ggplot(aes(x = (gravidez_unica/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",             
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      c <- dados |> 
        ggplot(aes(x = (gravidez_multipla/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",             
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      d <- dados |> 
        ggplot(aes(x = (gravidez_multipla/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",             
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      return(
        ggarrange(
          a, b, c, d,
          labels = c("Única", "", "Mútipla", ""), hjust = 0,
          ncol = 2, nrow = 2      
        )
      )
    }
    # idade da mae
    else if (input$RadioBase1Vars == "sinasc" & input$RadioSUS == "nao" & 
               input$SelectVarSINASC == 'idademae'){
      a <- dados |> 
        ggplot(aes(x = (idade_menor_que_19_anos/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",             
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      b <- dados |> 
        ggplot(aes(x = (idade_menor_que_19_anos/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",             
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      c <- dados |>  
        ggplot(aes(x = (idade_de_19_a_35_anos/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",             
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      d <- dados |> 
        ggplot(aes(x = (idade_de_19_a_35_anos/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",           
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      e <- dados |> 
        ggplot(aes(x = (idade_maior_que_35_anos/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",             
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      f <- dados |> 
        ggplot(aes(x = (idade_maior_que_35_anos/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",    
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      return(
        ggarrange(
          a, b, c, d, e, f,
          labels = c("<19", "", "19-35", "", ">35", ""), hjust = 0,
          ncol = 2, nrow = 3      
        )
      )
    }
    # local de nascimento
    else if (input$RadioBase1Vars == "sinasc" & input$RadioSUS == "nao" & 
               input$SelectVarSINASC == 'locnasc'){
      a <- dados |> 
        ggplot(aes(x = (locnasc_hospital/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      b <- dados |> 
        ggplot(aes(x = (locnasc_hospital/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",             
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      c <- dados |> 
        ggplot(aes(x = (locnasc_outros_estabelecimentos_de_saude/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",             
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      d <- dados |> 
        ggplot(aes(x = (locnasc_outros_estabelecimentos_de_saude/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",             
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      return(
        ggarrange(
          a, b, c, d,
          labels = c("Hospital", "", "Outros", ""), hjust = 0,
          ncol = 2, nrow = 2      
        )
      )
    }
    # mes que iniciou o pre-natal
    else if (input$RadioBase1Vars == "sinasc" & input$RadioSUS == "nao" & 
               input$SelectVarSINASC == 'mesprenat'){
      a <- dados |> 
        ggplot(aes(x = (mesprenat_do_1_ao_3_mes/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      b <- dados |> 
        ggplot(aes(x = (mesprenat_do_1_ao_3_mes/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",             
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      c <- dados |> 
        ggplot(aes(x = (mesprenat_do_4_ao_9_mes/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",             
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      d <- dados |> 
        ggplot(aes(x = (mesprenat_do_4_ao_9_mes/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",             
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      return(
        ggarrange(
          a, b, c, d,
          labels = c("1°-3°", "", "4°-9°", ""), hjust = 0,
          ncol = 2, nrow = 2      
        )
      )
    }
    # tipo de parto
    else if (input$RadioBase1Vars == "sinasc" & input$RadioSUS == "nao" & 
               input$SelectVarSINASC == 'tipoparto'){
      a <- dados |> 
        ggplot(aes(x = (parto_cesareo/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      b <- dados |> 
        ggplot(aes(x = (parto_cesareo/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",             
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      c <- dados |> 
        ggplot(aes(x = (parto_vaginal/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",             
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      d <- dados |> 
        ggplot(aes(x = (parto_vaginal/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",             
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      return(
        ggarrange(
          a, b, c, d,
          labels = c("Cesáreo", "", "Vaginal", ""), hjust = 0,
          ncol = 2, nrow = 2      
        )
      )
    }
    # quantidade de filhos vivos
    else if (input$RadioBase1Vars == "sinasc" & input$RadioSUS == "nao" & 
               input$SelectVarSINASC == 'qtdfilvivo'){
      a <- dados |> 
        ggplot(aes(x = (qtdfilvivo_nenhum/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",             
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      b <- dados |> 
        ggplot(aes(x = (qtdfilvivo_nenhum/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",             
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      c <- dados |>  
        ggplot(aes(x = (qtdfilvivo_de_1_a_3_filhos/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",             
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      d <- dados |> 
        ggplot(aes(x = (qtdfilvivo_de_1_a_3_filhos/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",           
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      e <- dados |> 
        ggplot(aes(x = (qtdfilvivo_mais_que_3_filhos/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",             
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      f <- dados |> 
        ggplot(aes(x = (qtdfilvivo_mais_que_3_filhos/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",    
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      return(
        ggarrange(
          a, b, c, d, e, f,
          labels = c("0", "", "1-3", "", ">3", ""), hjust = 0,
          ncol = 2, nrow = 3      
        )
      )
    }
    # quantidade de gestacoes anteriores
    else if (input$RadioBase1Vars == "sinasc" & input$RadioSUS == "nao" & 
               input$SelectVarSINASC == 'qtdgest'){
      a <- dados |> 
        ggplot(aes(x = (qtdgestant_nenhuma/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",             
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      b <- dados |> 
        ggplot(aes(x = (qtdgestant_nenhuma/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",             
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      c <- dados |>  
        ggplot(aes(x = (qtdgestant_de_1_a_3_gestacoes/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",             
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      d <- dados |> 
        ggplot(aes(x = (qtdgestant_de_1_a_3_gestacoes/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",           
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      e <- dados |> 
        ggplot(aes(x = (qtdgestant_mais_que_3_gestacoes/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",             
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      f <- dados |> 
        ggplot(aes(x = (qtdgestant_mais_que_3_gestacoes/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",    
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      return(
        ggarrange(
          a, b, c, d, e, f,
          labels = c("0", "", "1-3", "", ">3", ""), hjust = 0,
          ncol = 2, nrow = 3      
        )
      )
    }
    # raca/cor da mae
    else if (input$RadioBase1Vars == "sinasc" & input$RadioSUS == "nao" & 
               input$SelectVarSINASC == 'racacormae'){
      a <- dados |> 
        ggplot(aes(x = (racacormae_branca_amarela/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",             
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      b <- dados |> 
        ggplot(aes(x = (racacormae_branca_amarela/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",             
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      c <- dados |>  
        ggplot(aes(x = (racacormae_preta_parda/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",             
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      d <- dados |> 
        ggplot(aes(x = (racacormae_preta_parda/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",           
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      e <- dados |> 
        ggplot(aes(x = (racacormae_indigena/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",             
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      f <- dados |> 
        ggplot(aes(x = (racacormae_indigena/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",    
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      return(
        ggarrange(
          a, b, c, d, e, f,
          labels = c("Branca/Amarela", "", "Preta/Parda", "", "Indígena", ""), hjust = 0,
          ncol = 2, nrow = 3      
        )
      )
    }
    # raca/cor do rn
    else if (input$RadioBase1Vars == "sinasc" & input$RadioSUS == "nao" & 
               input$SelectVarSINASC == 'racacor'){
      a <- dados |> 
        ggplot(aes(x = (racacor_branca_amarela/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",             
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      b <- dados |> 
        ggplot(aes(x = (racacor_branca_amarela/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",             
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      c <- dados |>  
        ggplot(aes(x = (racacor_preta_parda/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",             
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      d <- dados |> 
        ggplot(aes(x = (racacor_preta_parda/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",           
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      e <- dados |> 
        ggplot(aes(x = (racacor_indigena/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",             
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      f <- dados |> 
        ggplot(aes(x = (racacor_indigena/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",    
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      return(
        ggarrange(
          a, b, c, d, e, f,
          labels = c("Branca/Amarela", "", "Preta/Parda", "", "Indígena", ""), hjust = 0,
          ncol = 2, nrow = 3      
        )
      )
    }
    # sexo do rs
    else if (input$RadioBase1Vars == "sinasc" & input$RadioSUS == "nao" & 
               input$SelectVarSINASC == 'sexo'){
      a <- dados |> 
        ggplot(aes(x = (sexo_feminino/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      b <- dados |> 
        ggplot(aes(x = (sexo_feminino/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",             
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      c <- dados |> 
        ggplot(aes(x = (sexo_masculino/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",             
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      d <- dados |> 
        ggplot(aes(x = (sexo_masculino/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",             
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      return(
        ggarrange(
          a, b, c, d,
          labels = c("Feminino", "", "Masculino", ""), hjust = 0,
          ncol = 2, nrow = 2      
        )
      )
    }
    # graficos de dispersao - sinasc (c/ vinculo com o sus) ----
    # apgar1
    else if (input$RadioBase1Vars == "sinasc" & input$RadioSUS == "sim" & input$SelectSUS == "sim" & 
               input$SelectVarSINASC == 'apgar1'){
      a <- dados |> 
        filter(vinc_sus == 1) |> 
        ggplot(aes(x = (apgar1_menor_que_4/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson", 
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      b <- dados |> 
        filter(vinc_sus == 1) |> 
        ggplot(aes(x = (apgar1_menor_que_4/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson", 
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      c <- dados |> 
        filter(vinc_sus == 1) |> 
        ggplot(aes(x = (apgar1_de_4_a_7/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson", 
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      d <- dados |> 
        filter(vinc_sus == 1) |> 
        ggplot(aes(x = (apgar1_de_4_a_7/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson", 
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      e <- dados |> 
        filter(vinc_sus == 1) |> 
        ggplot(aes(x = (apgar1_maior_que_7/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson", 
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      f <- dados |> 
        filter(vinc_sus == 1) |> 
        ggplot(aes(x = (apgar1_maior_que_7/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson", 
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      return(
        ggarrange(
          a, b, c, d, e, f,
          labels = c("<4", "", "4-7", "", ">7", ""), hjust = 0,
          ncol = 2, nrow = 3
        )
      )
    } 
    # apgar5
    else if (input$RadioBase1Vars == "sinasc" & input$RadioSUS == "sim" & input$SelectSUS == "sim" & 
               input$SelectVarSINASC == 'apgar5'){
      a <- dados |> 
        filter(vinc_sus == 1) |> 
        ggplot(aes(x = (apgar5_menor_que_4/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson", 
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      b <- dados |> 
        filter(vinc_sus == 1) |> 
        ggplot(aes(x = (apgar5_menor_que_4/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson", 
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      c <- dados |>
        filter(vinc_sus == 1) |> 
        ggplot(aes(x = (apgar5_de_4_a_7/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson", 
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      d <- dados |>
        filter(vinc_sus == 1) |> 
        ggplot(aes(x = (apgar5_de_4_a_7/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson", 
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      e <- dados |> 
        filter(vinc_sus == 1) |> 
        ggplot(aes(x = (apgar5_maior_que_7/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson", 
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      f <- dados |> 
        filter(vinc_sus == 1) |> 
        ggplot(aes(x = (apgar5_maior_que_7/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson", 
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      return(
        ggarrange(
          a, b, c, d, e, f,
          labels = c("<4", "", "4-7", "", ">7", ""), hjust = 0,
          ncol = 2, nrow = 3       
        )
      )
    } 
    # numero de consultas de pre-natal
    else if (input$RadioBase1Vars == "sinasc" & input$RadioSUS == "sim" & input$SelectSUS == "sim" & 
               input$SelectVarSINASC == 'consprenat'){
      a <- dados |> 
        filter(vinc_sus == 1) |> 
        ggplot(aes(x = (consprenat_menor_que_6_consultas/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson", 
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      b <- dados |>
        filter(vinc_sus == 1) |> 
        ggplot(aes(x = (consprenat_menor_que_6_consultas/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson", 
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      c <- dados |> 
        filter(vinc_sus == 1) |> 
        ggplot(aes(x = (consprenat_maior_ou_igual_a_6_consultas/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson", 
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      d <- dados |> 
        filter(vinc_sus == 1) |> 
        ggplot(aes(x = (consprenat_maior_ou_igual_a_6_consultas/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson", 
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      return(
        ggarrange(
          a, b, c, d,
          labels = c("<6", "", ">=6", ""), hjust = 0,
          ncol = 2, nrow = 2      
        )
      )
    } 
    # escolaridade da mae
    else if (input$RadioBase1Vars == "sinasc" & input$RadioSUS == "sim" & input$SelectSUS == "sim" & 
               input$SelectVarSINASC == 'escmae'){
      a <- dados |> 
        filter(vinc_sus == 1) |> 
        ggplot(aes(x = (escmae2010_sem_escolaridade/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson", 
                 label.x = 50, label.y = 100, size = 4.5, color = "red")
      
      b <- dados |> 
        filter(vinc_sus == 1) |> 
        ggplot(aes(x = (escmae2010_sem_escolaridade/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson", 
                 label.x = 50, label.y = 100, size = 4.5, color = "red")
      
      c <- dados |> 
        filter(vinc_sus == 1) |> 
        ggplot(aes(x = (escmae2010_fundamental_1_1_a_4_serie/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson", 
                 label.x = 50, label.y = 100, size = 4.5, color = "red")
      
      d <- dados |> 
        filter(vinc_sus == 1) |> 
        ggplot(aes(x = (escmae2010_fundamental_1_1_a_4_serie/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson", 
                 label.x = 50, label.y = 100, size = 4.5, color = "red")
      
      e <- dados |>
        filter(vinc_sus == 1) |> 
        ggplot(aes(x = (escmae2010_fundamental_2_5_a_8_serie/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson", 
                 label.x = 50, label.y = 100, size = 4.5, color = "red")
      
      f <- dados |> 
        filter(vinc_sus == 1) |> 
        ggplot(aes(x = (escmae2010_fundamental_2_5_a_8_serie/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson", 
                 label.x = 50, label.y = 100, size = 4.5, color = "red")
      
      g <- dados |>
        filter(vinc_sus == 1) |> 
        ggplot(aes(x = (escmae2010_medio/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson", 
                 label.x = 50, label.y = 100, size = 4.5, color = "red")
      
      h <- dados |> 
        filter(vinc_sus == 1) |> 
        ggplot(aes(x = (escmae2010_medio/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson", 
                 label.x = 50, label.y = 100, size = 4.5, color = "red")
      
      i <- dados |> 
        filter(vinc_sus == 1) |> 
        ggplot(aes(x = (escmae2010_superior_incompleto/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson", 
                 label.x = 50, label.y = 100, size = 4.5, color = "red")
      
      j <- dados |> 
        filter(vinc_sus == 1) |> 
        ggplot(aes(x = (escmae2010_superior_incompleto/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson", 
                 label.x = 50, label.y = 100, size = 4.5, color = "red")
      
      k <- dados |>
        filter(vinc_sus == 1) |> 
        ggplot(aes(x = (escmae2010_superior_completo/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson", 
                 label.x = 50, label.y = 100, size = 4.5, color = "red")
      
      l <- dados |> 
        filter(vinc_sus == 1) |> 
        ggplot(aes(x = (escmae2010_superior_completo/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson", 
                 label.x = 50, label.y = 100, size = 4.5, color = "red")
      
      return(
        ggarrange(
          a, b, c, d, e, f, g, h, i, j, k, l,
          labels = c(
            "Sem escolaridade", "", "Ens. Fund.: 1ª a 4ª série", "", "Ens. Fund.: 5ª a 8ª série", "", 
            "Ens. Médio", "", "Ens. Sup. Incompleto", "", "Ens. Sup. Incompleto", ""), 
          hjust = 0,
          ncol = 4, nrow = 3       
        )
      )
    }
    # estado civil da mae
    else if (input$RadioBase1Vars == "sinasc" &  input$RadioSUS == "sim" & input$SelectSUS == "sim" & 
               input$SelectVarSINASC == 'estcivmae'){
      a <- dados |> 
        filter(vinc_sus == 1) |> 
        ggplot(aes(x = (estcivmae_casada_uniao_estavel/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson", 
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      b <- dados |> 
        filter(vinc_sus == 1) |> 
        ggplot(aes(x = (estcivmae_casada_uniao_estavel/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson", 
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      c <- dados |> 
        filter(vinc_sus == 1) |> 
        ggplot(aes(x = (estcivmae_outros/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson", 
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      d <- dados |> 
        filter(vinc_sus == 1) |> 
        ggplot(aes(x = (estcivmae_outros/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson", 
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      return(
        ggarrange(
          a, b, c, d,
          labels = c("Casada/Un. Estável", "", "Outro", ""), hjust = 0,
          ncol = 2, nrow = 2      
        )
      )
    } 
    # tipo de gravidez
    else if (input$RadioBase1Vars == "sinasc" &  input$RadioSUS == "sim" & input$SelectSUS == "sim" & 
               input$SelectVarSINASC == 'tipograv'){
      a <- dados |> 
        filter(vinc_sus == 1) |> 
        ggplot(aes(x = (gravidez_unica/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      b <- dados |> 
        filter(vinc_sus == 1) |> 
        ggplot(aes(x = (gravidez_unica/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",             
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      c <- dados |> 
        filter(vinc_sus == 1) |> 
        ggplot(aes(x = (gravidez_multipla/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",             
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      d <- dados |> 
        filter(vinc_sus == 1) |> 
        ggplot(aes(x = (gravidez_multipla/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",             
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      return(
        ggarrange(
          a, b, c, d,
          labels = c("Única", "", "Mútipla", ""), hjust = 0,
          ncol = 2, nrow = 2      
        )
      )
    }
    # idade da mae
    else if (input$RadioBase1Vars == "sinasc" & input$RadioSUS == "sim" & input$SelectSUS == "sim" & 
               input$SelectVarSINASC == 'idademae'){
      a <- dados |> 
        filter(vinc_sus == 1) |> 
        ggplot(aes(x = (idade_menor_que_19_anos/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",             
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      b <- dados |> 
        filter(vinc_sus == 1) |> 
        ggplot(aes(x = (idade_menor_que_19_anos/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",             
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      c <- dados |>
        filter(vinc_sus == 1) |> 
        ggplot(aes(x = (idade_de_19_a_35_anos/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",             
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      d <- dados |> 
        filter(vinc_sus == 1) |> 
        ggplot(aes(x = (idade_de_19_a_35_anos/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",           
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      e <- dados |>
        filter(vinc_sus == 1) |> 
        ggplot(aes(x = (idade_maior_que_35_anos/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",             
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      f <- dados |> 
        filter(vinc_sus == 1) |> 
        ggplot(aes(x = (idade_maior_que_35_anos/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",    
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      return(
        ggarrange(
          a, b, c, d, e, f,
          labels = c("<19", "", "19-35", "", ">35", ""), hjust = 0,
          ncol = 2, nrow = 3      
        )
      )
    }
    # local de nascimento
    else if (input$RadioBase1Vars == "sinasc" & input$RadioSUS == "sim" & input$SelectSUS == "sim" & 
               input$SelectVarSINASC == 'locnasc'){
      a <- dados |> 
        filter(vinc_sus == 1) |> 
        ggplot(aes(x = (locnasc_hospital/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      b <- dados |> 
        filter(vinc_sus == 1) |> 
        ggplot(aes(x = (locnasc_hospital/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",             
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      c <- dados |> 
        filter(vinc_sus == 1) |> 
        ggplot(aes(x = (locnasc_outros_estabelecimentos_de_saude/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",             
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      d <- dados |> 
        filter(vinc_sus == 1) |> 
        ggplot(aes(x = (locnasc_outros_estabelecimentos_de_saude/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",             
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      return(
        ggarrange(
          a, b, c, d,
          labels = c("Hospital", "", "Outros", ""), hjust = 0,
          ncol = 2, nrow = 2      
        )
      )
    }
    # mes que iniciou o pre-natal
    else if (input$RadioBase1Vars == "sinasc" &  input$RadioSUS == "sim" & input$SelectSUS == "sim" & 
             input$SelectVarSINASC == 'mesprenat'){
      a <- dados |> 
        filter(vinc_sus == 1) |> 
        ggplot(aes(x = (mesprenat_do_1_ao_3_mes/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      b <- dados |> 
        filter(vinc_sus == 1) |> 
        ggplot(aes(x = (mesprenat_do_1_ao_3_mes/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",             
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      c <- dados |> 
        filter(vinc_sus == 1) |> 
        ggplot(aes(x = (mesprenat_do_4_ao_9_mes/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",             
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      d <- dados |> 
        filter(vinc_sus == 1) |> 
        ggplot(aes(x = (mesprenat_do_4_ao_9_mes/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",             
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      return(
        ggarrange(
          a, b, c, d,
          labels = c("1°-3°", "", "4°-9°", ""), hjust = 0,
          ncol = 2, nrow = 2      
        )
      )
    }
    # tipo de parto
    else if (input$RadioBase1Vars == "sinasc" & input$RadioSUS == "sim" & input$SelectSUS == "sim" 
             & input$SelectVarSINASC == 'tipoparto'){
      a <- dados |> 
        filter(vinc_sus == 1) |> 
        ggplot(aes(x = (parto_cesareo/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      b <- dados |> 
        filter(vinc_sus == 1) |> 
        ggplot(aes(x = (parto_cesareo/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",             
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      c <- dados |> 
        filter(vinc_sus == 1) |> 
        ggplot(aes(x = (parto_vaginal/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",             
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      d <- dados |> 
        filter(vinc_sus == 1) |> 
        ggplot(aes(x = (parto_vaginal/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",             
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      return(
        ggarrange(
          a, b, c, d,
          labels = c("Cesáreo", "", "Vaginal", ""), hjust = 0,
          ncol = 2, nrow = 2      
        )
      )
    }
    # quantidade de filhos vivos
    else if (input$RadioBase1Vars == "sinasc" & input$RadioSUS == "sim" & input$SelectSUS == "sim" 
               & input$SelectVarSINASC == 'qtdfilvivo'){
      a <- dados |> 
        filter(vinc_sus == 1) |> 
        ggplot(aes(x = (qtdfilvivo_nenhum/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",             
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      b <- dados |> 
        filter(vinc_sus == 1) |> 
        ggplot(aes(x = (qtdfilvivo_nenhum/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",             
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      c <- dados |>  
        filter(vinc_sus == 1) |> 
        ggplot(aes(x = (qtdfilvivo_de_1_a_3_filhos/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",             
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      d <- dados |>
        filter(vinc_sus == 1) |> 
        ggplot(aes(x = (qtdfilvivo_de_1_a_3_filhos/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",           
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      e <- dados |> 
        filter(vinc_sus == 1) |> 
        ggplot(aes(x = (qtdfilvivo_mais_que_3_filhos/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",             
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      f <- dados |> 
        filter(vinc_sus == 1) |> 
        ggplot(aes(x = (qtdfilvivo_mais_que_3_filhos/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",    
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      return(
        ggarrange(
          a, b, c, d, e, f,
          labels = c("0", "", "1-3", "", ">3", ""), hjust = 0,
          ncol = 2, nrow = 3      
        )
      )
    }
    # quantidade de gestacoes anteriores
    else if (input$RadioBase1Vars == "sinasc" & input$RadioSUS == "sim" & input$SelectSUS == "sim" 
               & input$SelectVarSINASC == 'qtdgest'){
      a <- dados |> 
        filter(vinc_sus == 1) |> 
        ggplot(aes(x = (qtdgestant_nenhuma/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",             
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      b <- dados |> 
        filter(vinc_sus == 1) |> 
        ggplot(aes(x = (qtdgestant_nenhuma/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",             
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      c <- dados |>  
        filter(vinc_sus == 1) |> 
        ggplot(aes(x = (qtdgestant_de_1_a_3_gestacoes/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",             
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      d <- dados |> 
        filter(vinc_sus == 1) |> 
        ggplot(aes(x = (qtdgestant_de_1_a_3_gestacoes/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",           
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      e <- dados |> 
        filter(vinc_sus == 1) |> 
        ggplot(aes(x = (qtdgestant_mais_que_3_gestacoes/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",             
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      f <- dados |> 
        filter(vinc_sus == 1) |> 
        ggplot(aes(x = (qtdgestant_mais_que_3_gestacoes/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",    
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      return(
        ggarrange(
          a, b, c, d, e, f,
          labels = c("0", "", "1-3", "", ">3", ""), hjust = 0,
          ncol = 2, nrow = 3      
        )
      )
    }
    # raca/cor da mae
    else if (input$RadioBase1Vars == "sinasc" & input$RadioSUS == "sim" & input$SelectSUS == "sim" & 
               input$SelectVarSINASC == 'racacormae'){
      a <- dados |> 
        filter(vinc_sus == 1) |> 
        ggplot(aes(x = (racacormae_branca_amarela/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",             
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      b <- dados |> 
        filter(vinc_sus == 1) |> 
        ggplot(aes(x = (racacormae_branca_amarela/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",             
                 label.x = 75, label.y = 100, size = 4.5, color = "red")

      c <- dados |> 
        filter(vinc_sus == 1) |> 
        ggplot(aes(x = (racacormae_preta_parda/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",             
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      d <- dados |> 
        filter(vinc_sus == 1) |> 
        ggplot(aes(x = (racacormae_preta_parda/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",           
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      e <- dados |> 
        filter(vinc_sus == 1) |> 
        ggplot(aes(x = (racacormae_indigena/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",             
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      f <- dados |> 
        filter(vinc_sus == 1) |> 
        ggplot(aes(x = (racacormae_indigena/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",    
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      return(
        ggarrange(
          a, b, c, d, e, f,
          labels = c("Branca/Amarela", "", "Preta/Parda", "", "Indígena", ""), hjust = 0,
          ncol = 2, nrow = 3      
        )
      )
    }
    # raca/cor do rn
    else if (input$RadioBase1Vars == "sinasc" & input$RadioSUS == "sim" & input$SelectSUS == "sim" 
               & input$SelectVarSINASC == 'racacor'){
      a <- dados |> 
        filter(vinc_sus == 1) |> 
        ggplot(aes(x = (racacor_branca_amarela/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",             
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      b <- dados |> 
        filter(vinc_sus == 1) |> 
        ggplot(aes(x = (racacor_branca_amarela/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",             
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      c <- dados |>  
        filter(vinc_sus == 1) |> 
        ggplot(aes(x = (racacor_preta_parda/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",             
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      d <- dados |> 
        filter(vinc_sus == 1) |> 
        ggplot(aes(x = (racacor_preta_parda/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",           
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      e <- dados |> 
        filter(vinc_sus == 1) |> 
        ggplot(aes(x = (racacor_indigena/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",             
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      f <- dados |> 
        filter(vinc_sus == 1) |> 
        ggplot(aes(x = (racacor_indigena/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",    
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      return(
        ggarrange(
          a, b, c, d, e, f,
          labels = c("Branca/Amarela", "", "Preta/Parda", "", "Indígena", ""), hjust = 0,
          ncol = 2, nrow = 3      
        )
      )
    }
    # sexo do rs
    else if (input$RadioBase1Vars == "sinasc" & input$RadioSUS == "sim" & input$SelectSUS == "sim" &
               input$SelectVarSINASC == 'sexo'){
      a <- dados |> 
        filter(vinc_sus == 1) |> 
        ggplot(aes(x = (sexo_feminino/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      b <- dados |> 
        filter(vinc_sus == 1) |> 
        ggplot(aes(x = (sexo_feminino/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",             
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      c <- dados |> 
        filter(vinc_sus == 1) |> 
        ggplot(aes(x = (sexo_masculino/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",             
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      d <- dados |> 
        filter(vinc_sus == 1) |> 
        ggplot(aes(x = (sexo_masculino/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",             
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      return(
        ggarrange(
          a, b, c, d,
          labels = c("Feminino", "", "Masculino", ""), hjust = 0,
          ncol = 2, nrow = 2      
        )
      )
    }
    # graficos de dispersao - sinasc (s/ vinculo com o sus) ----
    # apgar1
    else if (input$RadioBase1Vars == "sinasc" & input$RadioSUS == "sim" & input$SelectSUS == "nao" & 
             input$SelectVarSINASC == 'apgar1'){
      a <- dados |> 
        filter(vinc_sus == 0) |> 
        ggplot(aes(x = (apgar1_menor_que_4/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson", 
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      b <- dados |> 
        filter(vinc_sus == 0) |> 
        ggplot(aes(x = (apgar1_menor_que_4/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson", 
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      c <- dados |> 
        filter(vinc_sus == 0) |> 
        ggplot(aes(x = (apgar1_de_4_a_7/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson", 
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      d <- dados |> 
        filter(vinc_sus == 0) |> 
        ggplot(aes(x = (apgar1_de_4_a_7/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson", 
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      e <- dados |> 
        filter(vinc_sus == 0) |> 
        ggplot(aes(x = (apgar1_maior_que_7/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson", 
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      f <- dados |> 
        filter(vinc_sus == 0) |> 
        ggplot(aes(x = (apgar1_maior_que_7/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson", 
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      return(
        ggarrange(
          a, b, c, d, e, f,
          labels = c("<4", "", "4-7", "", ">7", ""), hjust = 0,
          ncol = 2, nrow = 3
        )
      )
    } 
    # apgar5
    else if (input$RadioBase1Vars == "sinasc" & input$RadioSUS == "sim" & input$SelectSUS == "nao" & 
             input$SelectVarSINASC == 'apgar5'){
      a <- dados |> 
        filter(vinc_sus == 0) |> 
        ggplot(aes(x = (apgar5_menor_que_4/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson", 
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      b <- dados |> 
        filter(vinc_sus == 0) |> 
        ggplot(aes(x = (apgar5_menor_que_4/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson", 
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      c <- dados |>
        filter(vinc_sus == 0) |> 
        ggplot(aes(x = (apgar5_de_4_a_7/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson", 
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      d <- dados |>
        filter(vinc_sus == 0) |> 
        ggplot(aes(x = (apgar5_de_4_a_7/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson", 
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      e <- dados |> 
        filter(vinc_sus == 0) |> 
        ggplot(aes(x = (apgar5_maior_que_7/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson", 
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      f <- dados |> 
        filter(vinc_sus == 0) |> 
        ggplot(aes(x = (apgar5_maior_que_7/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson", 
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      return(
        ggarrange(
          a, b, c, d, e, f,
          labels = c("<4", "", "4-7", "", ">7", ""), hjust = 0,
          ncol = 2, nrow = 3       
        )
      )
    } 
    # numero de consultas de pre-natal
    else if (input$RadioBase1Vars == "sinasc" & input$RadioSUS == "sim" & input$SelectSUS == "nao" & 
             input$SelectVarSINASC == 'consprenat'){
      a <- dados |> 
        filter(vinc_sus == 0) |> 
        ggplot(aes(x = (consprenat_menor_que_6_consultas/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson", 
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      b <- dados |>
        filter(vinc_sus == 0) |> 
        ggplot(aes(x = (consprenat_menor_que_6_consultas/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson", 
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      c <- dados |> 
        filter(vinc_sus == 0) |> 
        ggplot(aes(x = (consprenat_maior_ou_igual_a_6_consultas/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson", 
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      d <- dados |> 
        filter(vinc_sus == 0) |> 
        ggplot(aes(x = (consprenat_maior_ou_igual_a_6_consultas/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson", 
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      return(
        ggarrange(
          a, b, c, d,
          labels = c("<6", "", ">=6", ""), hjust = 0,
          ncol = 2, nrow = 2      
        )
      )
    } 
    # escolaridade da mae
    else if (input$RadioBase1Vars == "sinasc" & input$RadioSUS == "sim" & input$SelectSUS == "nao" & 
             input$SelectVarSINASC == 'escmae'){
      a <- dados |> 
        filter(vinc_sus == 0) |> 
        ggplot(aes(x = (escmae2010_sem_escolaridade/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson", 
                 label.x = 50, label.y = 100, size = 4.5, color = "red")
      
      b <- dados |> 
        filter(vinc_sus == 0) |> 
        ggplot(aes(x = (escmae2010_sem_escolaridade/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson", 
                 label.x = 50, label.y = 100, size = 4.5, color = "red")
      
      c <- dados |> 
        filter(vinc_sus == 0) |> 
        ggplot(aes(x = (escmae2010_fundamental_1_1_a_4_serie/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson", 
                 label.x = 50, label.y = 100, size = 4.5, color = "red")
      
      d <- dados |> 
        filter(vinc_sus == 0) |> 
        ggplot(aes(x = (escmae2010_fundamental_1_1_a_4_serie/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson", 
                 label.x = 50, label.y = 100, size = 4.5, color = "red")
      
      e <- dados |>
        filter(vinc_sus == 0) |> 
        ggplot(aes(x = (escmae2010_fundamental_2_5_a_8_serie/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson", 
                 label.x = 50, label.y = 100, size = 4.5, color = "red")
      
      f <- dados |> 
        filter(vinc_sus == 0) |> 
        ggplot(aes(x = (escmae2010_fundamental_2_5_a_8_serie/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson", 
                 label.x = 50, label.y = 100, size = 4.5, color = "red")
      
      g <- dados |>
        filter(vinc_sus == 0) |> 
        ggplot(aes(x = (escmae2010_medio/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson", 
                 label.x = 50, label.y = 100, size = 4.5, color = "red")
      
      h <- dados |> 
        filter(vinc_sus == 0) |> 
        ggplot(aes(x = (escmae2010_medio/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson", 
                 label.x = 50, label.y = 100, size = 4.5, color = "red")
      
      i <- dados |> 
        filter(vinc_sus == 0) |> 
        ggplot(aes(x = (escmae2010_superior_incompleto/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson", 
                 label.x = 50, label.y = 100, size = 4.5, color = "red")
      
      j <- dados |> 
        filter(vinc_sus == 0) |> 
        ggplot(aes(x = (escmae2010_superior_incompleto/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson", 
                 label.x = 50, label.y = 100, size = 4.5, color = "red")
      
      k <- dados |>
        filter(vinc_sus == 0) |> 
        ggplot(aes(x = (escmae2010_superior_completo/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson", 
                 label.x = 50, label.y = 100, size = 4.5, color = "red")
      
      l <- dados |> 
        filter(vinc_sus == 0) |> 
        ggplot(aes(x = (escmae2010_superior_completo/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson", 
                 label.x = 50, label.y = 100, size = 4.5, color = "red")
      
      return(
        ggarrange(
          a, b, c, d, e, f, g, h, i, j, k, l,
          labels = c(
            "Sem escolaridade", "", "Ens. Fund.: 1ª a 4ª série", "", "Ens. Fund.: 5ª a 8ª série", "", 
            "Ens. Médio", "", "Ens. Sup. Incompleto", "", "Ens. Sup. Incompleto", ""), 
          hjust = 0,
          ncol = 4, nrow = 3       
        )
      )
    }
    # estado civil da mae
    else if (input$RadioBase1Vars == "sinasc" &  input$RadioSUS == "sim" & input$SelectSUS == "nao" & 
             input$SelectVarSINASC == 'estcivmae'){
      a <- dados |> 
        filter(vinc_sus == 0) |> 
        ggplot(aes(x = (estcivmae_casada_uniao_estavel/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson", 
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      b <- dados |> 
        filter(vinc_sus == 0) |> 
        ggplot(aes(x = (estcivmae_casada_uniao_estavel/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson", 
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      c <- dados |> 
        filter(vinc_sus == 0) |> 
        ggplot(aes(x = (estcivmae_outros/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson", 
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      d <- dados |> 
        filter(vinc_sus == 0) |> 
        ggplot(aes(x = (estcivmae_outros/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson", 
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      return(
        ggarrange(
          a, b, c, d,
          labels = c("Casada/Un. Estável", "", "Outro", ""), hjust = 0,
          ncol = 2, nrow = 2      
        )
      )
    } 
    # tipo de gravidez
    else if (input$RadioBase1Vars == "sinasc" &  input$RadioSUS == "sim" & input$SelectSUS == "nao" & 
             input$SelectVarSINASC == 'tipograv'){
      a <- dados |> 
        filter(vinc_sus == 0) |> 
        ggplot(aes(x = (gravidez_unica/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      b <- dados |> 
        filter(vinc_sus == 0) |> 
        ggplot(aes(x = (gravidez_unica/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",             
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      c <- dados |> 
        filter(vinc_sus == 0) |> 
        ggplot(aes(x = (gravidez_multipla/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",             
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      d <- dados |> 
        filter(vinc_sus == 0) |> 
        ggplot(aes(x = (gravidez_multipla/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",             
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      return(
        ggarrange(
          a, b, c, d,
          labels = c("Única", "", "Mútipla", ""), hjust = 0,
          ncol = 2, nrow = 2      
        )
      )
    }
    # idade da mae
    else if (input$RadioBase1Vars == "sinasc" & input$RadioSUS == "sim" & input$SelectSUS == "nao" & 
             input$SelectVarSINASC == 'idademae'){
      a <- dados |> 
        filter(vinc_sus == 0) |> 
        ggplot(aes(x = (idade_menor_que_19_anos/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",             
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      b <- dados |> 
        filter(vinc_sus == 0) |> 
        ggplot(aes(x = (idade_menor_que_19_anos/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",             
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      c <- dados |>
        filter(vinc_sus == 0) |> 
        ggplot(aes(x = (idade_de_19_a_35_anos/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",             
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      d <- dados |> 
        filter(vinc_sus == 0) |> 
        ggplot(aes(x = (idade_de_19_a_35_anos/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",           
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      e <- dados |>
        filter(vinc_sus == 0) |> 
        ggplot(aes(x = (idade_maior_que_35_anos/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",             
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      f <- dados |> 
        filter(vinc_sus == 0) |> 
        ggplot(aes(x = (idade_maior_que_35_anos/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",    
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      return(
        ggarrange(
          a, b, c, d, e, f,
          labels = c("<19", "", "19-35", "", ">35", ""), hjust = 0,
          ncol = 2, nrow = 3      
        )
      )
    }
    # local de nascimento
    else if (input$RadioBase1Vars == "sinasc" & input$RadioSUS == "sim" & input$SelectSUS == "nao" & 
             input$SelectVarSINASC == 'locnasc'){
      a <- dados |> 
        filter(vinc_sus == 0) |> 
        ggplot(aes(x = (locnasc_hospital/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      b <- dados |> 
        filter(vinc_sus == 0) |> 
        ggplot(aes(x = (locnasc_hospital/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",             
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      c <- dados |> 
        filter(vinc_sus == 0) |> 
        ggplot(aes(x = (locnasc_outros_estabelecimentos_de_saude/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",             
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      d <- dados |> 
        filter(vinc_sus == 0) |> 
        ggplot(aes(x = (locnasc_outros_estabelecimentos_de_saude/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",             
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      return(
        ggarrange(
          a, b, c, d,
          labels = c("Hospital", "", "Outros", ""), hjust = 0,
          ncol = 2, nrow = 2      
        )
      )
    }
    # mes que iniciou o pre-natal
    else if (input$RadioBase1Vars == "sinasc" &  input$RadioSUS == "sim" & input$SelectSUS == "nao" & 
             input$SelectVarSINASC == 'mesprenat'){
      a <- dados |> 
        filter(vinc_sus == 0) |> 
        ggplot(aes(x = (mesprenat_do_1_ao_3_mes/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      b <- dados |> 
        filter(vinc_sus == 0) |> 
        ggplot(aes(x = (mesprenat_do_1_ao_3_mes/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",             
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      c <- dados |> 
        filter(vinc_sus == 0) |> 
        ggplot(aes(x = (mesprenat_do_4_ao_9_mes/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",             
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      d <- dados |> 
        filter(vinc_sus == 0) |> 
        ggplot(aes(x = (mesprenat_do_4_ao_9_mes/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",             
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      return(
        ggarrange(
          a, b, c, d,
          labels = c("1°-3°", "", "4°-9°", ""), hjust = 0,
          ncol = 2, nrow = 2      
        )
      )
    }
    # tipo de parto
    else if (input$RadioBase1Vars == "sinasc" & input$RadioSUS == "sim" & input$SelectSUS == "nao" & 
               input$SelectVarSINASC == 'tipoparto'){
      a <- dados |> 
        filter(vinc_sus == 0) |> 
        ggplot(aes(x = (parto_cesareo/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      b <- dados |> 
        filter(vinc_sus == 0) |> 
        ggplot(aes(x = (parto_cesareo/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",             
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      c <- dados |> 
        filter(vinc_sus == 0) |> 
        ggplot(aes(x = (parto_vaginal/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",             
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      d <- dados |> 
        filter(vinc_sus == 0) |> 
        ggplot(aes(x = (parto_vaginal/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",             
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      return(
        ggarrange(
          a, b, c, d,
          labels = c("Cesáreo", "", "Vaginal", ""), hjust = 0,
          ncol = 2, nrow = 2      
        )
      )
    }
    # quantidade de filhos vivos
    else if (input$RadioBase1Vars == "sinasc" & input$RadioSUS == "sim" & input$SelectSUS == "nao" &
               input$SelectVarSINASC == 'qtdfilvivo'){
      a <- dados |> 
        filter(vinc_sus == 0) |> 
        ggplot(aes(x = (qtdfilvivo_nenhum/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",             
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      b <- dados |> 
        filter(vinc_sus == 0) |> 
        ggplot(aes(x = (qtdfilvivo_nenhum/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",             
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      c <- dados |>  
        filter(vinc_sus == 0) |> 
        ggplot(aes(x = (qtdfilvivo_de_1_a_3_filhos/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",             
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      d <- dados |>
        filter(vinc_sus == 0) |> 
        ggplot(aes(x = (qtdfilvivo_de_1_a_3_filhos/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",           
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      e <- dados |> 
        filter(vinc_sus == 0) |> 
        ggplot(aes(x = (qtdfilvivo_mais_que_3_filhos/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",             
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      f <- dados |> 
        filter(vinc_sus == 0) |> 
        ggplot(aes(x = (qtdfilvivo_mais_que_3_filhos/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",    
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      return(
        ggarrange(
          a, b, c, d, e, f,
          labels = c("0", "", "1-3", "", ">3", ""), hjust = 0,
          ncol = 2, nrow = 3      
        )
      )
    }
    # quantidade de gestacoes anteriores
    else if (input$RadioBase1Vars == "sinasc" & input$RadioSUS == "sim" & input$SelectSUS == "nao" &
               input$SelectVarSINASC == 'qtdgest'){
      a <- dados |> 
        filter(vinc_sus == 0) |> 
        ggplot(aes(x = (qtdgestant_nenhuma/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",             
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      b <- dados |> 
        filter(vinc_sus == 0) |> 
        ggplot(aes(x = (qtdgestant_nenhuma/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",             
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      c <- dados |>  
        filter(vinc_sus == 0) |> 
        ggplot(aes(x = (qtdgestant_de_1_a_3_gestacoes/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",             
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      d <- dados |> 
        filter(vinc_sus == 0) |> 
        ggplot(aes(x = (qtdgestant_de_1_a_3_gestacoes/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",           
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      e <- dados |> 
        filter(vinc_sus == 0) |> 
        ggplot(aes(x = (qtdgestant_mais_que_3_gestacoes/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",             
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      f <- dados |> 
        filter(vinc_sus == 0) |> 
        ggplot(aes(x = (qtdgestant_mais_que_3_gestacoes/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",    
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      return(
        ggarrange(
          a, b, c, d, e, f,
          labels = c("0", "", "1-3", "", ">3", ""), hjust = 0,
          ncol = 2, nrow = 3      
        )
      )
    }
    # raca/cor da mae
    else if (input$RadioBase1Vars == "sinasc" & input$RadioSUS == "sim" & input$SelectSUS == "nao" & 
             input$SelectVarSINASC == 'racacormae'){
      a <- dados |> 
        filter(vinc_sus == 0) |> 
        ggplot(aes(x = (racacormae_branca_amarela/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",             
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      b <- dados |> 
        filter(vinc_sus == 0) |> 
        ggplot(aes(x = (racacormae_branca_amarela/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",             
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      c <- dados |> 
        filter(vinc_sus == 0) |> 
        ggplot(aes(x = (racacormae_preta_parda/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",             
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      d <- dados |> 
        filter(vinc_sus == 0) |> 
        ggplot(aes(x = (racacormae_preta_parda/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",           
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      e <- dados |> 
        filter(vinc_sus == 0) |> 
        ggplot(aes(x = (racacormae_indigena/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",             
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      f <- dados |> 
        filter(vinc_sus == 0) |> 
        ggplot(aes(x = (racacormae_indigena/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",    
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      return(
        ggarrange(
          a, b, c, d, e, f,
          labels = c("Branca/Amarela", "", "Preta/Parda", "", "Indígena", ""), hjust = 0,
          ncol = 2, nrow = 3      
        )
      )
    }
    # raca/cor do rn
    else if (input$RadioBase1Vars == "sinasc" & input$RadioSUS == "sim" & input$SelectSUS == "nao" & 
               input$SelectVarSINASC == 'racacor'){
      a <- dados |> 
        filter(vinc_sus == 0) |> 
        ggplot(aes(x = (racacor_branca_amarela/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",             
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      b <- dados |> 
        filter(vinc_sus == 0) |> 
        ggplot(aes(x = (racacor_branca_amarela/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",             
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      c <- dados |>  
        filter(vinc_sus == 0) |> 
        ggplot(aes(x = (racacor_preta_parda/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",             
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      d <- dados |> 
        filter(vinc_sus == 0) |> 
        ggplot(aes(x = (racacor_preta_parda/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",           
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      e <- dados |> 
        filter(vinc_sus == 0) |> 
        ggplot(aes(x = (racacor_indigena/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",             
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      f <- dados |> 
        filter(vinc_sus == 0) |> 
        ggplot(aes(x = (racacor_indigena/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",    
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      return(
        ggarrange(
          a, b, c, d, e, f,
          labels = c("Branca/Amarela", "", "Preta/Parda", "", "Indígena", ""), hjust = 0,
          ncol = 2, nrow = 3      
        )
      )
    }
    # sexo do rs
    else if (input$RadioBase1Vars == "sinasc" & input$RadioSUS == "sim" & input$SelectSUS == "nao" &
             input$SelectVarSINASC == 'sexo'){
      a <- dados |> 
        filter(vinc_sus == 0) |> 
        ggplot(aes(x = (sexo_feminino/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      b <- dados |> 
        filter(vinc_sus == 0) |> 
        ggplot(aes(x = (sexo_feminino/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",             
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      c <- dados |> 
        filter(vinc_sus == 0) |> 
        ggplot(aes(x = (sexo_masculino/n_cnes) * 100, y = (tipopremat_eletivo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() + 
        labs(x = "",
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",             
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      d <- dados |> 
        filter(vinc_sus == 0) |> 
        ggplot(aes(x = (sexo_masculino/n_cnes) * 100, y = (tipopremat_espontaneo/n_cnes) * 100)) + 
        geom_point(alpha = 0.5) + 
        theme_bw() +
        labs(x = "",
             y = "Prematuridade espontânea (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson",             
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      return(
        ggarrange(
          a, b, c, d,
          labels = c("Feminino", "", "Masculino", ""), hjust = 0,
          ncol = 2, nrow = 2      
        )
      )
    }
    # grafico de dispersao - atlas brasil ----
    else if (input$RadioBase1Vars == "atlbr" & input$RadioSUS == "nao"){
      dados_mod <- dados |>
        filter(!is.na(sigla_uf)) |> 
        group_by(sigla_uf) |> 
        summarise(
          eletivo = sum(tipopremat_eletivo),
          espontaneo = sum(tipopremat_espontaneo),
          n_cnes = sum(n_cnes), 
          var = mean((get(input$SelectVarAtlasBR)), na.rm = TRUE)
        ) |> 
        mutate(p_eletivo = (eletivo/n_cnes) * 100) |> 
        mutate(p_espontaneo = (espontaneo/n_cnes) * 100) |> 
        select(sigla_uf, p_eletivo, p_espontaneo, var) |> 
        mutate(var = ifelse(sigla_uf == "DF", 0.824, var))
      
      a <- dados_mod |>
        ggplot(aes(var, p_eletivo, label = sigla_uf)) +
        geom_point() +
        labs(x = input$SelectVarAtlasBR,
             y = "Prematuridade eletiva (%)") +
        scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
        geom_text(hjust = 0, vjust = 0) +
        theme_bw() +
        theme(title = element_text(face = "bold"),
              axis.text = element_text(face = "bold"),
              panel.grid.minor.y = element_blank(),
              plot.title = element_text(size = 15, hjust = .5))
      
      b <- dados_mod |>
        ggplot(aes(var, p_espontaneo, label = sigla_uf)) +
        geom_point() +
        labs(x = input$SelectVarAtlasBR,
             y = "Prematuridade espontânea (%)") +
        scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
        geom_text(hjust = 0, vjust = 0) +
        theme_bw() +
        theme(title = element_text(face = "bold"),
              axis.text = element_text(face = "bold"),
              panel.grid.minor.y = element_blank(),
              plot.title = element_text(size = 15, hjust = .5))
    }
    # grafico de dispersao - atlas brasil (c/ vinculo com o sus) ----
    else if (input$RadioBase1Vars == "atlbr" & input$RadioSUS == "sim" & input$SelectSUS == "sim"){
      dados_mod <- dados |>
        filter(vinc_sus == 1) |> 
        filter(!is.na(sigla_uf)) |> 
        group_by(sigla_uf) |> 
        summarise(
          eletivo = sum(tipopremat_eletivo),
          espontaneo = sum(tipopremat_espontaneo),
          n_cnes = sum(n_cnes), 
          var = mean((get(input$SelectVarAtlasBR)), na.rm = TRUE)
        ) |> 
        mutate(p_eletivo = (eletivo/n_cnes) * 100) |> 
        mutate(p_espontaneo = (espontaneo/n_cnes) * 100) |> 
        select(sigla_uf, p_eletivo, p_espontaneo, var) |> 
        mutate(var = ifelse(sigla_uf == "DF", 0.824, var))
      
      a <- dados_mod |>
        ggplot(aes(var, p_eletivo, label = sigla_uf)) +
        geom_point() +
        labs(x = input$SelectVarAtlasBR,
             y = "Prematuridade eletiva (%)") +
        scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
        geom_text(hjust = 0, vjust = 0) +
        theme_bw() +
        theme(title = element_text(face = "bold"),
              axis.text = element_text(face = "bold"),
              panel.grid.minor.y = element_blank(),
              plot.title = element_text(size = 15, hjust = .5))
      
      b <- dados_mod |>
        ggplot(aes(var, p_espontaneo, label = sigla_uf)) +
        geom_point() +
        labs(x = paste0(input$SelectVarAtlasBR, " ", "(%)"),
             y = "Prematuridade espontânea (%)") +
        scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
        geom_text(hjust = 0, vjust = 0) +
        theme_bw() +
        theme(title = element_text(face = "bold"),
              axis.text = element_text(face = "bold"),
              panel.grid.minor.y = element_blank(),
              plot.title = element_text(size = 15, hjust = .5))
    }
    # grafico de dispersao - atlas brasil (s/ vinculo com o sus) ----
    else if (input$RadioBase1Vars == "atlbr" & input$RadioSUS == "sim" & input$SelectSUS == "nao"){
      dados_mod <- dados |>
        filter(vinc_sus == 0) |> 
        filter(!is.na(sigla_uf)) |> 
        group_by(sigla_uf) |> 
        summarise(
          eletivo = sum(tipopremat_eletivo),
          espontaneo = sum(tipopremat_espontaneo),
          n_cnes = sum(n_cnes), 
          var = mean((get(input$SelectVarAtlasBR)), na.rm = TRUE)
        ) |> 
        mutate(p_eletivo = (eletivo/n_cnes) * 100) |> 
        mutate(p_espontaneo = (espontaneo/n_cnes) * 100) |> 
        select(sigla_uf, p_eletivo, p_espontaneo, var) |> 
        mutate(var = ifelse(sigla_uf == "DF", 0.824, var))
      
      a <- dados_mod |>
        ggplot(aes(var, p_eletivo, label = sigla_uf)) +
        geom_point() +
        labs(x = input$SelectVarAtlasBR,
             y = "Prematuridade eletiva (%)") +
        scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
        geom_text(hjust = 0, vjust = 0) +
        theme_bw() +
        theme(title = element_text(face = "bold"),
              axis.text = element_text(face = "bold"),
              panel.grid.minor.y = element_blank(),
              plot.title = element_text(size = 15, hjust = .5))
      
      b <- dados_mod |>
        ggplot(aes(var, p_espontaneo, label = sigla_uf)) +
        geom_point() +
        labs(x = paste0(input$SelectVarAtlasBR, " ", "(%)"),
             y = "Prematuridade espontânea (%)") +
        scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
        geom_text(hjust = 0, vjust = 0) +
        theme_bw() +
        theme(title = element_text(face = "bold"),
              axis.text = element_text(face = "bold"),
              panel.grid.minor.y = element_blank(),
              plot.title = element_text(size = 15, hjust = .5))
    }
    a + b
  })
  # grafico de boxplot - cnes ----
  output$Plot2 <- renderPlot({
    # cnes
    if (input$RadioBase2Vars == "cnes"){
      a <- dados |> 
        mutate(vinc_sus_recod = ifelse(vinc_sus == 0, "Não", "Sim")) |> 
        dplyr::filter(!is.na(vinc_sus)) |>
        ggplot(aes(x = as.factor(vinc_sus_recod), y = (tipopremat_eletivo/n_cnes) * 100, fill = vinc_sus_recod)) + 
        geom_boxplot(outlier.alpha = .5) +
        scale_fill_brewer(palette = "Blues") +
        theme_bw() + 
        labs(x = "Vínculo com o SUS",
             y = "Prematuridade eletiva (%)") +
        theme(legend.position = "none")
      
      b <- dados |>
        mutate(vinc_sus_recod = ifelse(vinc_sus == 0, "Não", "Sim")) |> 
        dplyr::filter(!is.na(vinc_sus)) |> 
        ggplot(aes(x = as.factor(vinc_sus_recod), y = (tipopremat_espontaneo/n_cnes) * 100, fill = vinc_sus_recod)) + 
        geom_boxplot(outlier.alpha = .5) + 
        scale_fill_brewer(palette = "Blues") +
        theme_bw() +
        labs(x = "Vínculo com o SUS",
             y = "Prematuridade espontânea (%)") +
        theme(legend.position = "none")
    }
    a + b
  })
}

shinyApp(ui, server)
