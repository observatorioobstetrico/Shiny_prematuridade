# UI -----------------------

ui <- dashboardPage(
  title = "NASCIMENTO PREMATURO NO BRASIL",
  dashboardHeader(
    title = 'Nascimento prematuro no Brasil',
    titleWidth = 325
  ),
  dashboardSidebar(
    ## Menu ----
    sidebarMenu(
      menuItem("Sobre", tabName = "Sobre"),
      menuItem("Exploratória", tabName = "Exploratória"),
      menuItem("MMCA", tabName = "MMCA")
    )
  ),
  dashboardBody(
    tags$head(tags$style(HTML('
        /* logo */
        .skin-blue .main-header .logo {
                              background-color: #0A1E3C;
                              }

        /* logo when hovered */
        .skin-blue .main-header .logo:hover {
                              background-color: #0A1E3C;
                              }

        /* navbar (rest of the header) */
        .skin-blue .main-header .navbar {
                              background-color: #0A1E3C;
                              }

        /* main sidebar */
        .skin-blue .main-sidebar {
                              background-color: #0A1E3C;
                              }

        /* active selected tab in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                              background-color: #32A0FF;
                              }

        /* other links in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                              background-color: #0A1E3C;
                              color: #FFFFFF;
                              }

        /* other links in the sidebarmenu when hovered */
         .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                              background-color: #32A0FF;
                              }
        /* toggle button when hovered  */
         .skin-blue .main-header .navbar .sidebar-toggle:hover{
                              background-color: #32A0FF;
                              }
      '))),
  
  
  # menu analise descritiva -----
  tabItems(
    tabItem(tabName = "Exploratória",
    icon = icon("chart-line"),
    # painel de graficos de dispersao -----
    tabPanel(
      "Gráficos de dispersão", 
      fluidPage(
        theme = shinythemes::shinytheme("cerulean"),
        sidebarLayout(
          sidebarPanel(
            hr(),
            print(h3("Gráfico de dispersão")),
            hr(),
            width = 4,
            radioButtons(inputId = "RadioBase1Vars", 
                         label = h4("Selecione a base de variáveis:"),
                         choices = c("SINASC" = "sinasc",
                                     "Atlas Brasil" = "atlbr"),
                         selected = "sinasc"),
            radioButtons(inputId = "RadioSUS", 
                         label = h4("Considerar vínculo com o SUS?"),
                         choices = c("Não" = 'nao', "Sim" = 'sim'),
                         selected = "nao"),
            shiny::conditionalPanel(
              condition = "input.RadioSUS == 'sim'",
              selectInput(inputId = "SelectSUS",
                          label = h4("Estabelecimentos de saúde com vínculo com o SUS:"),
                          choices = c("Sim" = 'sim', "Não" = 'nao'),
                          selected = "sim",
                          multiple = FALSE)
            ),
            tags$div(submitButton("Atualizar Pesquisa", icon = icon("arrows-rotate")), `align` = "center"),
            hr(),
            shiny::conditionalPanel(
              condition = "input.RadioBase1Vars == 'sinasc'",
              selectInput(inputId = "SelectVarSINASC",
                          label = h4("Selecione a variável de interesse:"),
                          choices = c("APGAR1 < 4" = "apgar1_menor_que_4",
                                      "APGAR1 4-7" = "apgar1_de_4_a_7",
                                      "APGAR1 > 7" = "apgar1_maior_que_7",
                                      "APGAR5 < 4" = "apgar5_menor_que_4",
                                      "APGAR5 4-7" = "apgar5_de_4_a_7",
                                      "APGAR5 > 7" = "apgar5_maior_que_7",
                                      "N° de consultas de pré-natal < 6" = "consprenat_menor_que_6_consultas",
                                      "N° de consultas de pré-natal >= 6" = "consprenat_maior_ou_igual_a_6_consultas",
                                      "Escolaridade da mãe - Sem escolaridade" = "escmae2010_sem_escolaridade",
                                      "Escolaridade da mãe - Ens. Fund. I (1ª a 4ª série)" = "escmae2010_fundamental_1_1_a_4_serie" ,
                                      "Escolaridade da mãe - Ens. Fund. II (5ª a 8ª série)" = "escmae2010_fundamental_2_5_a_8_serie",
                                      "Escolaridade da mãe - Ens. Médio" = "escmae2010_medio",
                                      "Escolaridade da mãe - Ens. Sup. Incompleto" = "escmae2010_superior_incompleto",
                                      "Escolaridade da mãe - Ens. Sup. Completo" = "escmae2010_superior_completo",
                                      "Estado civil da mãe - Casada/União estável" = "estcivmae_casada_uniao_estavel",
                                      "Estado civil da mãe - Outros" = "estcivmae_outros",
                                      "Tipo de gravidez - Única" = "gravidez_unica",
                                      "Tipo de gravidez - Múltipla" = "gravidez_multipla",
                                      "Idade da mãe < 19" = "idade_menor_que_19_anos" ,
                                      "Idade da mãe 19-35" = "idade_de_19_a_35_anos",
                                      "Idade da mãe > 35" = "idade_maior_que_35_anos" ,
                                      "Local de nascimento - Hospital" = "locnasc_hospital",
                                      "Local de nascimento - Outros" = "locnasc_outros_estabelecimentos_de_saude",
                                      "Mês que iniciou o pré-natal - 1° ao 3°" = "mesprenat_do_1_ao_3_mes",
                                      "Mês que iniciou o pré-natal - 4° ao 9°" = "mesprenat_do_4_ao_9_mes",
                                      "Tipo de parto - Cesáreo" = "parto_cesareo",
                                      "Tipo de parto - Vaginal" = "parto_vaginal",
                                      "Quantidade de filhos vivos - Nenhum" = "qtdfilvivo_nenhum",
                                      "Quantidade de filhos vivos - 1 a 3" = "qtdfilvivo_de_1_a_3_filhos" ,
                                      "Quantidade de filhos vivos - > 3" = "qtdfilvivo_mais_que_3_filhos",
                                      "Quantidade de gestações anteriores - Nenhuma" = "qtdgestant_nenhuma",
                                      "Quantidade de gestações anteriores - 1 a 3" = "qtdgestant_de_1_a_3_gestacoes",
                                      "Quantidade de gestações anteriores - > 3" = "qtdgestant_mais_que_3_gestacoes",
                                      "Raça/cor da mãe - Branca/Amarela" = "racacormae_branca_amarela",
                                      "Raça/cor da mãe - Preta/Parda" = "racacormae_preta_parda",
                                      "Raça/cor da mãe - Indígena" = "racacormae_indigena" ,
                                      "Raça/cor do RN - Branca/Amarela" = "racacor_branca_amarela",
                                      "Raça/cor do RN - Preta/Parda" = "racacor_preta_parda" ,
                                      "Raça/cor do RN - Indígena" = "racacor_indigena",
                                      "Sexo do RN - Feminino" = "sexo_feminino",
                                      "Sexo do RN - Masculino" = "sexo_masculino"),
                          selected = "apgar1_menor_que_4",
                          multiple = FALSE)
            ),
            shiny::conditionalPanel(
              condition = "input.RadioBase1Vars == 'atlbr'",
              selectInput(inputId = "SelectVarAtlasBR",
                          label = h4("Selecione a variável de interesse:"),
                          choices = c("IDHM 2010 - Estado" = "idhm_2010_estado",
                                      "Índice de Gini 2010 - Estado" = "indice_de_gini_2010_estado"),
                          selected = "indice_de_gini_2010_estado",
                          multiple = FALSE)
            ),
            tags$div(submitButton("Atualizar Variável", icon = icon("arrows-rotate")), `align` = "center")
          ),
          # tela de visualizacao 1 -----
          mainPanel(
            hr(),
            width = 8,
            shinycssloaders::withSpinner(
              plotOutput("Plot1"), 
              color = getOption("spinner.color", "#40cfff"), 
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
        theme = shinythemes::shinytheme("cerulean"),
        sidebarLayout(
          sidebarPanel(
            hr(),
            print(h3("Gráfico de boxplot")),
            hr(),
            width = 4,
            radioButtons(inputId = "RadioBase2Vars", 
                         label = h4("Selecione a base de variáveis:"),
                         choices = c("CNES" = "cnes"),
                         selected = "cnes"),
            selectInput(inputId = "SelectVarCNES",
                        label = h4("Selecione a variável de interesse:"),
                        choices = c("Vínculo com o SUS" = "vinc_sus"),
                        selected = "vinc_sus",
                        multiple = FALSE),
            tags$div(submitButton("Atualizar Variável", icon = icon("arrows-rotate")), `align` = "center"),
          ),
          # tela de visualizacao 2 -----
          mainPanel(
            hr(),
            width = 8,
            shinycssloaders::withSpinner(
              plotOutput("Plot2"),
              color = getOption("spinner.color", "#40cfff"),
              type = getOption("spinner.type", 1)
            ),
            hr()
          )
        )
      )
    )
  ),
  # menu mmca -----
  tabItem(
    tabName = "MMCA", icon = icon("th", lib = "glyphicon")
  ),
  # menu sobre -----
  tabItem(
   tabName = "Sobre", 
   div(img(
     src = "logo2.png",
     height = 100,
     width = 175
   ), style = "text-align: center;"),
   icon = icon("info-sign", lib = "glyphicon"),
    includeMarkdown("sobre.md")
  )))
)


# Bibliotecas ------------

#dados
library(dplyr)
library(janitor)
library(tidyr)
#shiny
library(shiny)
library(shinythemes)
library(shinycssloaders)
library(shinydashboard)
library(shinyjs)
library(shinyWidgets)
library(markdown)
#graficos
library(ggplot2)
library(ggpubr)
library(patchwork)


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
    # sinasc
    if (input$RadioBase1Vars == "sinasc" & input$RadioSUS == "nao"){
     a <- dados |> 
        ggplot(aes(x = ((get(input$SelectVarSINASC)/n_cnes) * 100), y = ((tipopremat_eletivo/n_cnes) * 100))) + 
        geom_point(alpha = .5) + 
        theme_bw() + 
        labs(x = paste0(input$SelectVarSINASC, " ", "(%)"),
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson", 
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
        
     b <- dados |>
       ggplot(aes(x = ((get(input$SelectVarSINASC)/n_cnes) * 100), y = ((tipopremat_espontaneo/n_cnes) * 100))) +
       geom_point(alpha = .5) +
       theme_bw() +
       labs(x = paste0(input$SelectVarSINASC, " ", "(%)"),
            y = "Prematuridade espontânea (%)") +
       stat_cor(aes(label = ..r.label..), method = "pearson",
                label.x = 75, label.y = 100, size = 4.5, color = "red")
     
    } 
    # graficos de dispersao - sinasc (c/ vinculo com o sus) ----
    else if (input$RadioBase1Vars == "sinasc" & input$RadioSUS == "sim" & input$SelectSUS == "sim"){
      a <- dados |> 
        filter(vinc_sus == 1) |> 
        ggplot(aes(x = ((get(input$SelectVarSINASC)/n_cnes) * 100), y = ((tipopremat_eletivo/n_cnes) * 100))) + 
        geom_point(alpha = .5) + 
        theme_bw() + 
        labs(x = paste0(input$SelectVarSINASC, " ", "(%)"),
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson", 
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      b <- dados |>
        filter(vinc_sus == 1) |> 
        ggplot(aes(x = ((get(input$SelectVarSINASC)/n_cnes) * 100), y = ((tipopremat_espontaneo/n_cnes) * 100))) +
        geom_point(alpha = .5) +
        theme_bw() +
        labs(x = paste0(input$SelectVarSINASC, " ", "(%)"),
             y = "Prematuridade espontânea (%)") +
        stat_cor(aes(label = ..r.label..), method = "pearson",
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
    }
    # graficos de dispersao - sinasc (s/ vinculo com o sus) ----
    else if (input$RadioBase1Vars == "sinasc" & input$RadioSUS == "sim" & input$SelectSUS == "nao"){
      a <- dados |> 
        filter(vinc_sus == 0) |> 
        ggplot(aes(x = ((get(input$SelectVarSINASC)/n_cnes) * 100), y = ((tipopremat_eletivo/n_cnes) * 100))) + 
        geom_point(alpha = .5) + 
        theme_bw() + 
        labs(x = paste0(input$SelectVarSINASC, " ", "(%)"),
             y = "Prematuridade eletiva (%)") + 
        stat_cor(aes(label = ..r.label..), method = "pearson", 
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
      
      b <- dados |>
        filter(vinc_sus == 0) |> 
        ggplot(aes(x = ((get(input$SelectVarSINASC)/n_cnes) * 100), y = ((tipopremat_espontaneo/n_cnes) * 100))) +
        geom_point(alpha = .5) +
        theme_bw() +
        labs(x = paste0(input$SelectVarSINASC, " ", "(%)"),
             y = "Prematuridade espontânea (%)") +
        stat_cor(aes(label = ..r.label..), method = "pearson",
                 label.x = 75, label.y = 100, size = 4.5, color = "red")
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
