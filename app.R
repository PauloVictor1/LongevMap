library(shiny)
library(bslib)
library(leaflet)
library(dplyr)
library(sf)
library(DT)
library(plotly)

# Definindo a Interface do Usuário (UI)
ui <- page_navbar(
  title = "LongevMap",
  bg = "#2D89C8",
  inverse = TRUE,
  
  #------------#------------#------------#------------------------------
  #-------------- PAIEL 1 - Mapa Interativo LADO INTERFACE ------------- 
  #------------#------------#------------#------------------------------
  
  nav_panel(
    title = "Mapa Interativo do Brasil",
    layout_sidebar(
      sidebar = sidebar(
        width = 250,
        selectInput("data_type", "Selecione a Base de Dados:", 
                    choices = c("Dados Brutos", "Dados Per Capita"),
                    selected = "Dados Brutos"),
        
        uiOutput("variavel_mapa_ui"),  # A lista de variáveis será gerada dinamicamente
        
        selectInput("estado_mapa", "Selecione o Estado:", 
                    choices = c("Todos os estados", "AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES", 
                                "GO", "MA", "MG", "MS", "MT", "PA", "PB", "PE", "PI", "PR", "RJ", 
                                "RN", "RO", "RR", "RS", "SC", "SE", "SP", "TO"),
                    selected = "SP"),
        
        checkboxInput("show_legend", "Exibir Legenda", value = TRUE)
      ),
      
      mainPanel(
        width = 9,  # Aumentando a largura do mainPanel para ocupar o restante do espaço
        leafletOutput("mapa_interativo", height = "100vh")
      )
    )
  ),
  
  #------------#------------#------------#------------------------------
  #-------------- PAIEL 2 - DATA EXPLORER LADO INTERFACE --------------- 
  #------------#------------#------------#------------------------------
  
  nav_panel(
    title = "Tabela de Dados",
    layout_sidebar(
      sidebar = sidebar(
        width = 250,
        selectInput("data_type_DE", 
                    "Selecione a Base de Dados para a tabela ao lado:", 
                    choices = c("Escolha uma opção...", "Dados_Brutos", "Dados_Per_Capita"), 
                    selected = "Escolha uma opção..."),  
        
        uiOutput("variavel_mapa_ui_DE"),  
        
        selectInput("estado_mapa_DE", "Selecione o Estado:", 
                    choices = c("AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES", 
                                "GO", "MA", "MG", "MS", "MT", "PA", "PB", "PE", "PI", "PR", "RJ", 
                                "RN", "RO", "RR", "RS", "SC", "SE", "SP", "TO"),
                    selected = NULL,
                    multiple = TRUE),
        conditionalPanel(condition = "input.estado_mapa_DE != ''", 
                         selectizeInput("cidades_DE", 
                                        label = "Selecione o município", 
                                        choices = NULL, 
                                        multiple = TRUE))
      ),
      mainPanel(
        width = 9,
        
        # Primeiro card: DataTable_DE_UI
        card(
          full_screen = TRUE,
          uiOutput("DataTable_DE_UI")
        ),
        
        # Segundo card: plot_DE
        card(
          full_screen = TRUE,
          plotly::plotlyOutput("plot_DE")
        )
      )
    )
  ),
  
  #------------#------------#------------#------------------------------ 
  #------------ PAIEL 3 - RAIO X MUNICIPAL  LADO INTERFACE ------------- 
  #------------#------------#------------#------------------------------ 
  
  nav_panel(
    title = "Raio-X Municipal",
    layout_sidebar(
      sidebar = sidebar(
        width = 250,
        selectInput("data_type_RM", 
                    "Selecione a Base de Dados para a tabela ao lado:", 
                    choices = c("Escolha uma opção...", "Dados Brutos", "Dados Per Capita"), 
                    selected = "Escolha uma opção..."),  
        
        uiOutput("variavel_mapa_ui_RM"),  
        
        selectInput("estado_mapa_RM", "Selecione o Estado:", 
                    choices = c("AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES", 
                                "GO", "MA", "MG", "MS", "MT", "PA", "PB", "PE", "PI", "PR", "RJ", 
                                "RN", "RO", "RR", "RS", "SC", "SE", "SP", "TO"),
                    selected = NULL,
                    multiple = TRUE),
        conditionalPanel(condition = "input.estado_mapa_RM != ''", 
                         selectizeInput("cidades_RM", 
                                        label = "Selecione o município", 
                                        choices = NULL, 
                                        multiple = TRUE))
      ),
      
      mainPanel(
        width = 9,
        
        # Card para o gráfico
        card(
          full_screen = TRUE,
          plotly::plotlyOutput("scatterpolar_rm")
        ),
        
        # Card para a tabela de informações de valores originais, mínimo e máximo
        card(
          full_screen = TRUE,
          DT::dataTableOutput("informacao_tabela_RM")
        )
      )
    )
  ),
  
  
  #------------#------------#------------#------------------------------ 
  #------------ PAIEL 4 - Modelos Regressão LADO INTERFACE ------------- 
  #------------#------------#------------#------------------------------ 
  
  # Painel 4: Modelos - Configuração da Interface
  nav_panel(
    title = "Modelos",
    layout_sidebar(
      sidebar = sidebar(
        width = 250,
        selectInput("var_dependente", "Selecione a Variável Dependente:", 
                    choices = c("Total de Idosos per capita (TIpc)", "Mediana de Idade dos Idosos (MII)")),
        
        selectInput("modelo", "Selecione o Modelo:", 
                    choices = c("Regressão Linear", "Regressão SAR", "GWR Multiscale")),
        
        selectInput("estado_mapa_modelos", "Selecione o Estado:", 
                    choices = c("Todos os estados", "AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES", 
                                "GO", "MA", "MG", "MS", "MT", "PA", "PB", "PE", "PI", "PR", "RJ", 
                                "RN", "RO", "RR", "RS", "SC", "SE", "SP", "TO"),
                    selected = "SP"),
        
        checkboxInput("exibir_legenda_modelos", "Exibir Legenda", value = TRUE)
        
      ),
      
      mainPanel(
        width = 9,
        
        # Card para o título do mapa e o mapa interativo
        card(
          full_screen = TRUE,
          # Título dinâmico do mapa
          uiOutput("titulo_mapa_residuos"),
          leafletOutput("mapa_residuos", height = "80vh")
        ),
        
        # Card para o título do sumário e o sumário do modelo
        card(
          full_screen = TRUE,
          # Título dinâmico para o sumário do modelo
          uiOutput("titulo_summary_modelos"),
          verbatimTextOutput("summary_output_modelos")
        )
      )
      
      
    )
  ),
  
  # Espaçador e menu de links
  nav_spacer(),
  
  nav_menu(
    title = "Links",
    align = "right",
    nav_item(tags$a("Posit", href = "https://posit.co")),
    nav_item(tags$a("Shiny", href = "https://shiny.posit.co"))
  )
)


#------------#------------#------------#------------------------------ 
#--------------------- INÍCIO DO LADO SERVIDOR ----------------------- 
#------------#------------#------------#------------------------------ 

server <- function(input, output, session) {
  
  #------------#------------#------------#------------------------------ 
  #------------- CARREGANDO AS BASES DE DADOS -------------------------- 
  #------------#------------#------------#------------------------------
  
  # Carregar as bases de dados
  dados_brutos <- readRDS("data/Mapa_Final3.rds")
  dados_per_capita <- readRDS("data/Mapa_Final_PC.rds")
  
  # Carregar os modelos salvos
  model_linear_TIpc <- readRDS("models/model_linear_TIpc.rds")
  model_SAR_TIpc <- readRDS("models/model_SAR_TIpc.rds")
  model_GWR_TIpc <- readRDS("models/model_GWR_TIpc.rds")
  
  model_linear_MII <- readRDS("models/model_linear_MII.rds")
  model_SAR_MII <- readRDS("models/model_SAR_MII.rds")
  model_GWR_MII <- readRDS("models/model_GWR_MII.rds")
  
  # Carregar o objeto sf para o mapa do modelo GWR Multiscale
  sf_objeto_GWR <- readRDS("data/sf_objeto_Dados_Painel_4.rds")
  # Organizar em uma lista para facilitar o acesso
  models <- list(
    "TIpc" = list(
      "Regressão Linear" = model_linear_TIpc,
      "Regressão SAR" = model_SAR_TIpc,
      "GWR Multiscale" = model_GWR_TIpc
    ),
    "MII" = list(
      "Regressão Linear" = model_linear_MII,
      "Regressão SAR" = model_SAR_MII,
      "GWR Multiscale" = model_GWR_MII
    )
  )
  
  #------------#------------#------------#------------------------------
  #-------------- PAIEL 1 - Mapa Interativo LADO SERVIDOR -------------- 
  #------------#------------#------------#------------------------------
  
  
  # Observador para atualizar as variáveis de acordo com a base selecionada
  observe({
    dados <- if (input$data_type == "Dados Brutos") dados_brutos else dados_per_capita
    variaveis <- names(dados)[sapply(dados, is.numeric)]
    variaveis <- variaveis[!variaveis %in% c("Código UF", "Código Região")]
    variaveis <- sort(variaveis)
    updateSelectInput(session, "variavel_mapa", choices = variaveis, selected = variaveis[1])
  })
  
  # Interface UI para o seletor de variáveis
  output$variavel_mapa_ui <- renderUI({
    selectInput("variavel_mapa", "Selecione a Variável para o Mapa:", choices = NULL)
  })
  
  # Renderiza o mapa interativo no Painel 1
  output$mapa_interativo <- renderLeaflet({
    req(input$variavel_mapa)  # Garante que input$variavel_mapa está disponível
    
    dados <- if (input$data_type == "Dados Brutos") dados_brutos else dados_per_capita
    if (input$estado_mapa != "Todos os estados") {
      dados <- dados %>% filter(Sigla_UF == input$estado_mapa)
    }
    
    valores <- dados[[input$variavel_mapa]]
    
    pal <- colorNumeric(palette = "YlOrRd", domain = valores, na.color = "transparent")
    
    leaflet(data = dados) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor = ~pal(valores),
        fillOpacity = 0.7,
        color = "white",
        weight = 1,
        popup = ~paste(
          "<strong>Município:</strong>", dados$`Nome Município`, "<br>",
          "<strong>Código Município:</strong>", dados$`Código Município`, "<br>",
          "<strong>Estado:</strong>", dados$Sigla_UF, "<br>",
          "<strong>", input$variavel_mapa, ":</strong>", valores
        )
      ) %>%
      {if (input$show_legend) addLegend(., pal = pal, values = valores, 
                                        title = input$variavel_mapa, position = "bottomright") else .}
  })
  
  #------------#------------#------------#------------------------------
  #-------------- PAIEL 2 - DATA EXPLORER LADO SERVIDOR ---------------- 
  #------------#------------#------------#------------------------------
  
  # Reactive para gerar a base de dados que será utilizada tanto para a Interface
  df_DE <- reactive({
    # Retorne NULL se a escolha for o placeholder
    if (input$data_type_DE == "Escolha uma opção...") return(NULL)
    
    # Filtrando a tabela a partir dos dados selecionados na interface
    if (input$data_type_DE == "Dados_Brutos") {
      st_drop_geometry(dados_brutos)
    } else {
      st_drop_geometry(dados_per_capita)
    }
  })
  
  
  # Verificação ao selecionar o estado sem escolher a base
  
  observeEvent(input$estado_mapa_DE, {
    if (input$data_type_DE == "Escolha uma opção...") {
      showNotification("Por favor, selecione uma base de dados antes de escolher o estado.", type = "warning")
      
      # Desmarcando a seleção do estado
      updateSelectInput(session, "estado_mapa_DE", selected = character(0))
    }
  })
  
  
  
  # Observador para atualizar as variáveis de acordo com a base selecionada no segundo painel
  observe({
    req(input$data_type_DE != "Escolha uma opção...")  # Verifica se a base foi selecionada
    
    variaveis_DE <- names(df_DE())[sapply(df_DE(), is.numeric)]
    variaveis_DE <- variaveis_DE[!variaveis_DE %in% c("Código UF", "Código Região")]
    variaveis_DE <- sort(variaveis_DE)
    updateSelectInput(session, "variavel_mapa_DE", choices = variaveis_DE, selected = variaveis_DE[1])
  })
  
  # Interface UI para o seletor de variáveis
  output$variavel_mapa_ui_DE <- renderUI({
    selectInput("variavel_mapa_DE", "Selecione a Variável para ser apresentada na tabela ao lado:", 
                choices = NULL,
                multiple=TRUE)
  })
  
  # Observer para apresentar apenas as cidades do estado previamente selecionado dentro do PAINEL 2
  
  observe({
    req(input$data_type_DE != "Escolha uma opção...")  # Verifica se a base foi selecionada
    
    
    Cidades_DE <- if (is.null(input$estado_mapa_DE)) character(0) else {
      filter(df_DE(), df_DE()$Sigla_UF %in% input$estado_mapa_DE) %>%
        `$`('Nome Município') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$cidades_DE[input$cidades_DE %in% Cidades_DE])
    updateSelectizeInput(session, 
                         "cidades_DE", 
                         choices = Cidades_DE,
                         selected = stillSelected, 
                         server = TRUE)
  })
  
  
  # Reactive para gerar a base de dados que será utilizada tanto para a tabela quanto para o gráfico
  df_Final <- reactive({
    # Filtrando a tabela a partir dos dados selecionados na iterface
    
    df_DE() %>%
      filter(
        is.null(input$estado_mapa_DE) | df_DE()$Sigla_UF %in% input$estado_mapa_DE,
        is.null(input$cidades_DE) | df_DE()$`Nome Município` %in% input$cidades_DE) %>%
      select("Código Município",
             "Nome Município",
             "Nome UF", 
             input$variavel_mapa_DE) %>% # Adicionamos dinamicamente todas as variaveis selecionadas pelo usuário
      rename_with(~ substr(.,0,20))     # Diminuindo o tamanho do nome da variável para melhorar o layout da tabela
    
  })
  
  
  # Renderizando a tabela final
  output$DataTable_DE <- DT::renderDataTable({
    req(input$data_type_DE != "Escolha uma opção...")  # Impede que o código prossiga se a opção padrão estiver selecionada
    
    # Valida a existência de dados, caso contrário mostra uma mensagem amigável
    validate(
      need(!is.null(df_Final()), "Nenhuma tabela a ser exibida.")
    )
    
    
    DT::datatable(df_Final(), escape = FALSE)
  }, options = list(scrollX = TRUE))
  
  # Alterando para renderUI para exibir uma mensagem amigável
  output$DataTable_DE_UI <- renderUI({
    # Verifica se a opção padrão está selecionada
    if (input$data_type_DE == "Escolha uma opção...") {
      # Exibe uma mensagem amigável
      HTML("<p>Selecione uma base de dados para visualizar a tabela.</p>")
    } else {
      # Renderiza a tabela quando uma opção é selecionada
      DT::dataTableOutput("DataTable_DE")
    }
  })
  
  # Limitar a seleção a 15 variáveis e exibir uma mensagem de alerta se exceder
  observeEvent(input$variavel_mapa_DE, {
    if (length(input$variavel_mapa_DE) > 15) {
      # Manter apenas as 15 primeiras variáveis selecionadas
      updateSelectInput(session, "variavel_mapa_DE", selected = input$variavel_mapa_DE[1:15])
      
      # Exibir uma notificação de alerta
      showNotification("Você pode selecionar no máximo 15 variáveis para exibir na tabela.", type = "error")
    }
  })
  
  
  # Renderizando o gráfico que será apresentado abaixo do Texto
  output$plot_DE <- plotly::renderPlotly({
    
    # validando se o usuário selecionou o mínimo necessário de variáveis para gerar o gráfico
    validate(
      #need(input$data_point_1_DE[1], 'Check at least one letter!'),
      need(input$variavel_mapa_DE[2] != '', 'Por favor selecione 2 variáveis para adicionar à tabela e gerar o gráfico')
    )
    
    p <- df_Final() %>%
      #tibble::rownames_to_column()  %>%
      ggplot(aes(
        x = .data[[substr(input$variavel_mapa_DE[1],0,20)]], # estou fazendo o substr() para que as colunas tenham extamente o mesmo nome da variável de Input
        y = .data[[substr(input$variavel_mapa_DE[2],0,20)]],
        text = .data[["Nome Município"]]
      )) +
      geom_point() +
      theme_minimal()
    
    plotly::ggplotly(p)
  })
  
  #------------#------------#------------#------------------------------ 
  #------------ PAIEL 3 - RAIO X MUNICIPAL  LADO SERVIDOR -------------- 
  #------------#------------#------------#------------------------------ 
  
  # Reactive para gerar a base de dados que será utilizada por todo o Painel 3 
  df_RM <- reactive({
    # Retorne NULL se a escolha for o placeholder
    if (input$data_type_RM == "Escolha uma opção...") return(NULL)
    
    # Filtrando a tabela a partir dos dados selecionados na interface
    if (input$data_type_RM == "Dados Brutos") {
      st_drop_geometry(dados_brutos)
    } else {
      st_drop_geometry(dados_per_capita)
    }
  })
  
  # Verificação ao selecionar o estado sem escolher a base
  observeEvent(input$estado_mapa_RM, {
    if (input$data_type_RM == "Escolha uma opção...") {
      showNotification("Por favor, selecione uma base de dados antes de escolher o estado.", type = "warning")
      
      # Desmarcando a seleção do estado
      updateSelectInput(session, "estado_mapa_RM", selected = character(0))
    }
  })
  
  # Observador para atualizar as variáveis de acordo com a base selecionada no terceiro painel
  observe({
    req(input$data_type_RM != "Escolha uma opção...")  # Verifica se a base foi selecionada
    
    variaveis_RM <- names(df_RM())[sapply(df_RM(), is.numeric)]
    variaveis_RM <- variaveis_RM[!variaveis_RM %in% c("Código UF", "Código Região")]
    variaveis_RM <- sort(variaveis_RM)
    updateSelectInput(session, "variavel_mapa_RM", choices = variaveis_RM, selected = variaveis_RM[1])
  })
  
  # Interface UI para o seletor de variáveis
  output$variavel_mapa_ui_RM <- renderUI({
    selectInput("variavel_mapa_RM", "Selecione a Variável para ser apresentada na tabela ao lado:", 
                choices = NULL,
                multiple=TRUE)
  })
  
  # Observador para apresentar apenas as cidades do estado previamente selecionado dentro do PAINEL 3
  observe({
    req(input$data_type_RM != "Escolha uma opção...")  # Verifica se a base foi selecionada
    
    Cidades_RM <- if (is.null(input$estado_mapa_RM)) character(0) else {
      filter(df_RM(), df_RM()$Sigla_UF %in% input$estado_mapa_RM) %>%
        `$`('Nome Município') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$cidades_RM[input$cidades_RM %in% Cidades_RM])
    updateSelectizeInput(session, 
                         "cidades_RM", 
                         choices = Cidades_RM,
                         selected = stillSelected, 
                         server = TRUE)
  })
  
  # Calculando os valores mínimos e máximos globais da base de dados completa
  valores_min_max <- reactive({
    req(input$variavel_mapa_RM)  # Verifica se há variáveis selecionadas
    
    df_RM() %>%
      select(input$variavel_mapa_RM) %>%
      summarise(across(everything(), list(Mínimo = min, Máximo = max), na.rm = TRUE)) %>%
      tidyr::pivot_longer(cols = everything(), names_to = c("Variável", ".value"), names_sep = "_")
  })
  
  # Reactive para gerar a base de dados que será utilizada para gerar o gráfico do Raio-X Municipal
  df_RM_Final <- reactive({
    req(input$variavel_mapa_RM)  # Certifica-se de que variáveis foram selecionadas
    
    # Filtrando a tabela a partir dos dados selecionados na interface
    df_RM() %>%
      mutate(Nome_Completo = paste(df_RM()$`Nome Município`, ",", Sigla_UF)) %>%
      filter(
        is.null(input$estado_mapa_RM) | df_RM()$Sigla_UF %in% input$estado_mapa_RM,
        is.null(input$cidades_RM) | df_RM()$`Nome Município` %in% input$cidades_RM) %>%
      select("Nome_Completo", input$variavel_mapa_RM) %>%
      # Normalização min-max baseada nos valores globais da base de dados completa
      mutate(across(-Nome_Completo, 
                    ~ (. - valores_min_max()$Mínimo[which(valores_min_max()$Variável == cur_column())]) /
                      (valores_min_max()$Máximo[which(valores_min_max()$Variável == cur_column())] - 
                         valores_min_max()$Mínimo[which(valores_min_max()$Variável == cur_column())]),
                    .names = "Valor_Escalonado_{col}")) %>%
      tidyr::pivot_longer(cols = starts_with("Valor_Escalonado"), 
                          names_to = "Variável", 
                          names_prefix = "Valor_Escalonado_", 
                          values_to = "Valor_Escalonado")
  })
  
  # Renderizando o gráfico scatterpolar
  output$scatterpolar_rm <- plotly::renderPlotly({
    validate(
      need(input$cidades_RM[2] != '', 'Por favor selecione ao menos 2 cidades'),
      need(input$variavel_mapa_RM[3] != '', 'Por favor selecione no mínimo 3 variáveis para adicionar à tabela e gerar o gráfico')
    )
    
    plot_ly(
      df_RM_Final(),
      type = 'scatterpolar',
      mode = "closest",
      fill = 'toself'
    ) %>%
      add_trace(
        r = ~Valor_Escalonado,
        theta = ~Variável,
        showlegend = TRUE,
        mode = "markers",
        name = ~Nome_Completo
      )
  })
  
  
  # Reactive para gerar a base de dados e também a tabela informativa com os valores originais, mínimos, máximos e valores escalonados
  df_RM_Info <- reactive({
    req(input$variavel_mapa_RM, input$cidades_RM)  # Certifica-se de que variáveis e cidades foram selecionadas
    
    # Filtrando para os municípios e variáveis selecionados
    df_selecionado <- df_RM() %>%
      filter(`Nome Município` %in% input$cidades_RM) %>%
      select(Nome_Completo = `Nome Município`, input$variavel_mapa_RM)
    
    # Calculando os valores mínimos e máximos de cada variável com base no universo completo da base de dados
    valores_min_max <- df_RM() %>%
      select(input$variavel_mapa_RM) %>%
      summarise(across(everything(), list(Mínimo = min, Máximo = max), na.rm = TRUE)) %>%
      tidyr::pivot_longer(cols = everything(), names_to = c("Variável", ".value"), names_sep = "_")
    
    # Adicionando a coluna "Valor_Escalonado" ao dataframe df_selecionado usando os valores globais
    valores_escalonados <- df_selecionado %>%
      mutate(across(-Nome_Completo, 
                    ~ (. - valores_min_max$Mínimo[which(valores_min_max$Variável == cur_column())]) /
                      (valores_min_max$Máximo[which(valores_min_max$Variável == cur_column())] - 
                         valores_min_max$Mínimo[which(valores_min_max$Variável == cur_column())]),
                    .names = "Valor_Escalonado_{col}")) %>%
      tidyr::pivot_longer(cols = starts_with("Valor_Escalonado"), 
                          names_to = "Variável", 
                          names_prefix = "Valor_Escalonado_", 
                          values_to = "Valor_Escalonado")
    
    # Transformando para o formato long e juntando com os valores mínimo e máximo
    valores_long <- df_selecionado %>%
      tidyr::pivot_longer(cols = -Nome_Completo, names_to = "Variável", values_to = "Valor Original") %>%
      left_join(valores_min_max, by = "Variável") %>%
      left_join(valores_escalonados, by = c("Nome_Completo", "Variável")) %>%
      select(Nome_Completo, Variável, `Valor Original`, Mínimo, Máximo, Valor_Escalonado)  # Selecionando apenas as colunas necessárias
    
    return(valores_long)
  })
  
  
  
  # Código para gerar a tabela resumo abaixo do gráfico de scatterplot
  output$informacao_tabela_RM <- DT::renderDataTable({
    req(df_RM_Info())  # Certifica-se de que a tabela existe
    
    DT::datatable(df_RM_Info(), options = list(pageLength = 5, scrollX = TRUE))
  })
  
  
  
  #------------#------------#------------#------------------------------ 
  #------------ PAINEL 4 - Modelos Regressão LADO SERVIDOR ------------- 
  #------------#------------#------------#------------------------------ 
  
  # Reactive para definir a coluna de resíduos com base na variável dependente e no modelo selecionado
  modelo_residuos_coluna <- reactive({
    req(input$var_dependente, input$modelo)
    
    # Seleciona o nome da coluna de resíduos com base nos inputs
    if (input$var_dependente == "Total de Idosos per capita (TIpc)") {
      switch(input$modelo,
             "Regressão Linear" = "model_Linear_Res_TIpc",
             "Regressão SAR" = "model_SAR_Res_TIpc",
             "GWR Multiscale" = "model_GWR.MULT_Res_TIpc")
    } else if (input$var_dependente == "Mediana de Idade dos Idosos (MII)") {
      switch(input$modelo,
             "Regressão Linear" = "model_Linear_Res_MII",
             "Regressão SAR" = "model_SAR_Res_MII",
             "GWR Multiscale" = "model_GWR.MULT_Res_MII")
    }
  })
  
  # Título dinâmico para o mapa
  output$titulo_mapa_residuos <- renderUI({
    req(input$modelo, input$var_dependente)
    titulo_mapa <- paste("Resíduos do modelo de regressão:", input$modelo, "-", input$var_dependente)
    HTML(paste0("<h4>", titulo_mapa, "</h4>"))
  })
  
  # Título dinâmico para o sumário do modelo
  output$titulo_summary_modelos <- renderUI({
    req(input$modelo, input$var_dependente)
    titulo_sumario <- paste("Resumo do modelo de regressão criado:", input$modelo, "-", input$var_dependente)
    HTML(paste0("<h4>", titulo_sumario, "</h4>"))
  })
  
  # Renderização do mapa de resíduos
  output$mapa_residuos <- renderLeaflet({
    req(modelo_residuos_coluna())
    
    dados_mapa <- if (input$estado_mapa_modelos != "Todos os estados") {
      sf_objeto_GWR %>% filter(Sigla_UF == input$estado_mapa_modelos)
    } else {
      sf_objeto_GWR
    }
    
    residuos <- dados_mapa[[modelo_residuos_coluna()]]
    pal <- colorNumeric("RdYlBu", domain = residuos, na.color = "transparent", reverse = TRUE)
    
    leaflet(data = dados_mapa) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor = ~pal(residuos),
        fillOpacity = 0.7,
        color = "white",
        weight = 1,
        popup = ~paste(
          "<strong>Município:</strong>", Nome.Município, "<br>",
          "<strong>UF:</strong>", Sigla_UF, "<br>",
          "<strong>Resíduo:</strong>", residuos
        )
      ) %>%
      {if (input$exibir_legenda_modelos) addLegend(., pal = pal, values = residuos, 
                                                   title = paste("Resíduos -", input$modelo), 
                                                   position = "bottomright") else .}
  })
  
  
  # Função para sumarizar o modelo GWR Multiscale de forma consistente
  summary_gwr <- function(modelo) {
    # Extrai as estatísticas globais principais do modelo
    global_r2 <- modelo$GW.diagnostics$R2  # Exemplo para o R² global (ajuste conforme necessário)
    global_aic <- modelo$GW.diagnostics$AICc  # Exemplo para AIC (ajuste conforme necessário)
    
    # Mostra os coeficientes globais estimados
    cat("Coeficientes globais estimados:\n")
    print(modelo$SDF$summaryStats)  # Ajuste conforme a estrutura real do seu modelo
    
    # Exibe o R² global e o AIC
    cat("\nR² Global:", global_r2, "\n")
    cat("AICc Global:", global_aic, "\n")
    
    # Outras informações de diagnóstico
    cat("\nEstatísticas de diagnóstico do modelo:\n")
    print(modelo$GW.diagnostics)
  }
  
  # Função para sumarizar o modelo GWR Multiscale de forma detalhada
  summary_gwr <- function(modelo) {
    cat("Resumo do Modelo GWR Multiscale\n")
    cat("----------------------------------\n")
    
    print(modelo$this.call)
    cat("----------------------------------\n")
    
    # R² Global e AICc Global, conforme disponíveis
    if (!is.null(modelo$GW.diagnostic$R2.val)) {
      cat("R² Global:", modelo$GW.diagnostic$R2.val, "\n")
      cat("R² Global Ajustado:", modelo$GW.diagnostic$R2adj, "\n")
    }
    if (!is.null(modelo$GW.diagnostic$AICc)) {
      cat("AICc Global:", modelo$GW.diagnostic$AICc, "\n")
    }
    
    # Estatísticas locais (por exemplo, estatísticas resumidas)
    cat("\nResumo das larguras de banda calculadas:\n")
    if (!is.null(modelo$GW.arguments$bws)) {
      print(modelo$GW.arguments$bws)
    } else {
      cat("Estatísticas locais não disponíveis\n")
    }
    
    # Resumo dos coeficientes globais e das estatísticas adicionais
    cat("\nCoeficientes Estimados por Variável com Erros Padrão e Valores-T:\n")
    if (!is.null(modelo$SDF)) {
      # Extrai os nomes dos coeficientes, erros padrão e valores-t
      coeficientes <- names(modelo$SDF)[!grepl("_SE|_TV", names(modelo$SDF))]
      
      for (coef in coeficientes) {
        cat("\nResumo para:", coef, "\n")
        
        # Mostra o summary do coeficiente principal
        cat("Coeficiente:\n")
        print(summary(modelo$SDF[[coef]]))
        
        # Checa se o erro padrão e valor-t estão presentes e imprime
        coef_se <- paste0(coef, "_SE")
        if (coef_se %in% names(modelo$SDF)) {
          cat("Erro Padrão:\n")
          print(summary(modelo$SDF[[coef_se]]))
        } else {
          cat("Erro Padrão não disponível\n")
        }
        
        coef_tv <- paste0(coef, "_TV")
        if (coef_tv %in% names(modelo$SDF)) {
          cat("Valor-T:\n")
          print(summary(modelo$SDF[[coef_tv]]))
        } else {
          cat("Valor-T não disponível\n")
        }
      }
    } else {
      cat("Coeficientes não disponíveis\n")
    }
    
    cat("----------------------------------\n")
    cat("FIM DO RESUMO\n")
  }
  
  
  
  # Renderização do sumário do modelo com ajuste para o GWR Multiscale
  output$summary_output_modelos <- renderPrint({
    req(input$var_dependente, input$modelo)
    
    var_dep <- ifelse(input$var_dependente == "Total de Idosos per capita (TIpc)", "TIpc", "MII")
    modelo_selecionado <- models[[var_dep]][[input$modelo]]
    
    if (input$modelo == "GWR Multiscale") {
      summary_gwr(modelo_selecionado)
    } else {
      summary(modelo_selecionado)
    }
  })
  
  
}

# Executa a aplicação Shiny
shinyApp(ui = ui, server = server)