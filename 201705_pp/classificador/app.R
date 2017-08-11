library(shiny)
library(captchaReceitaAudio)
library(magrittr)
library(tidyverse)
library(shinyBS)
addResourcePath("dados", normalizePath("../data"))

scale2 <- function(x) as.numeric(scale(x))
visualizar_corte <- function(df_com_letras_identificadas) {
  ggplot(df_com_letras_identificadas %>%
           mutate(eh_letra = final_diff %% 2 != 0), aes(x = tempo)) +
    geom_line(aes(y = scale2(som), alpha = eh_letra), show.legend = FALSE) +
    geom_line(aes(y = scale2(w0), colour = "w0"), show.legend = FALSE) +
    geom_line(aes(y = scale2(final), colour = "final"), show.legend = FALSE) +
    geom_line(aes(y = scale2(final_diff), colour = "final_diff"), show.legend = FALSE) +
    theme_minimal() +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank()) +
    labs(x = "", y = "")
}

#########################################################################################
ui <- shinyUI(fluidPage(

  titlePanel("Classificador de CAPTCHAS"),
  bsButton("novo_captcha", "Novo Captcha", icon("refresh"), block = FALSE),
  uiOutput("captchas_sumario_contador", style = "display:inline-block; width: 50%;margin:5px 0 0 20px", class = "lead"),
  hr(),
  fluidRow(
    column(width = 6,
           plotOutput("captcha_img", height = 200),
           uiOutput("captcha_aud"),
           plotOutput("grafico_vis_corte")
    ),
    column(width = 6,
           fluidRow(
             column(width = 6,
                    textInput("captcha_resposta", "Captcha", placeholder = "Preencha com as 6 letras"),
                    checkboxInput("captcha_corte_ok", "Letras cortadas corretamente"),
                    actionButton("enviar_captcha", label = "Enviar captcha", icon("check"))
             ),
             column(width = 6,
                    radioButtons("captcha_defeitos", "Captcha com defeito",
                                 choices = c("Sem som", "Difícil/impossível de decifrar",
                                             "Letras cortadas incorretamente"),
                                 selected = character(0)),
                    bsButton("enviar_defeito", label = "Reportar defeito", icon("exclamation-triangle"), style = "danger")
             )
           ),
           fluidRow(
             column(width = 6, style = "padding:5px 0",
                    bsAlert("status")
             )
           ),
           h4("Prévia:"),
           tableOutput("previa")
    )
  ),
  fluidRow(
    column(width = 6,
           verbatimTextOutput("debug")
    ),
    column(width = 2,
           plotOutput("captchas_sumario_grafico")
    )
  )


)
)

#########################################################################################
sortear_captcha <- function(data) {
  data %>%
    mutate(arq = sprintf("../data/captchas/%s.png", captcha_id),
           tem_img = file.exists(arq)) %>%
    dplyr::filter(resposta %>% is.na) %>%
    dplyr::filter(tem_img) %>%
    sample_n(1) %$%
    captcha_id
}

limpar_inputs <- function(session, captcha_sugerido = character(0)) {
  updateTextInput(session, inputId = "captcha_resposta", value = captcha_sugerido)
  updateRadioButtons(session, inputId = "captcha_defeitos", selected = character(0))
  updateCheckboxInput(session, "captcha_corte_ok", value = FALSE)
}

atualiza_captchas_sumario <- function(data, input, output) {
  output$captchas_sumario_grafico <- renderPlot({
    ggplot(data) +
      geom_bar(aes(x = resposta, fill = resposta), show.legend = FALSE) +
      theme_minimal() +
      theme(axis.ticks = element_blank(),
            panel.grid = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_text(angle = 45, hjust = 1))
  }, height = 200)

  output$captchas_sumario_contador <- renderUI({
    n_oks <- sum(data$resposta %in% "OK")
    n_captchas <- nrow(data)
    p_oks <- scales::percent(n_oks/n_captchas)
    p(sprintf("%s de %s captchas respondidos (%s).",
              n_oks, n_captchas, p_oks))
  })
}

#########################################################################################
server <- shinyServer(function(input, output, session) {

  load(file = "../data/captchas.RData")
  atualiza_captchas_sumario(captchas, input, output)
  captcha_da_vez <- reactiveValues()
  captcha_da_vez$arq <- sortear_captcha(captchas)
  # observe({print(paste("a0.4", file.exists(sprintf("../data/captchas/%s.png", captcha_da_vez$arq))))})

  observeEvent(input$novo_captcha, {
    load("../data/captchas.RData")
    captcha_da_vez$arq <- sortear_captcha(captchas)
    # print(paste("a1", file.exists(sprintf("../data/captchas/%s.png", captcha_da_vez$arq))))
  })

  df_com_letras_identificadas <- reactive({
    # print(paste("a2", file.exists(sprintf("../data/captchas/%s.png", captcha_da_vez$arq))))
    a <- identificar_letras(sprintf("../data/captchas/%s.wav",
                                    captcha_da_vez$arq))
    return(a)
  })

  output$debug <- renderPrint({
    # print(paste("a3", file.exists(sprintf("../data/captchas/%s.png", captcha_da_vez$arq))))
    list(arquivo = captcha_da_vez$arq,
         getwd = getwd(),
         captcha_resposta = input$captcha_resposta,
         RDSs = data.frame(rds = list.files("../data/captchas_resposta")),
         captcha_defeitos = input$captcha_defeitos,
         captcha_corte_ok = input$captcha_corte_ok)
  })

  output$captcha_img <- renderImage({
    # print(paste("a4", file.exists(sprintf("../data/captchas/%s.png", captcha_da_vez$arq))))
    arq_img <- sprintf("../data/captchas/%s.png", captcha_da_vez$arq)
    # print(paste("a4.1", file.exists(sprintf("../data/captchas/%s.png", captcha_da_vez$arq))))

    lista <- list(src = arq_img,
         contentType = 'image/png',
         width = 500,
         alt = "Imagem do captcha")
    # print(paste("a4.2", file.exists(sprintf("../data/captchas/%s.png", captcha_da_vez$arq))))
  }, deleteFile = FALSE)

  output$captcha_aud <- renderUI({
    # print(paste("a5", file.exists(sprintf("../data/captchas/%s.png", captcha_da_vez$arq))))
    arq_aud <- sprintf("dados/captchas/%s.wav", captcha_da_vez$arq)
    tags$audio(src = arq_aud, type = "audio/wav", controls = NA)
  })

  output$grafico_vis_corte <- renderPlot({
    # print(paste("a6", file.exists(sprintf("../data/captchas/%s.png", captcha_da_vez$arq))))
    visualizar_corte(df_com_letras_identificadas())
  })

  letras <- eventReactive(input$captcha_resposta, {
    # print(paste("a7", file.exists(sprintf("../data/captchas/%s.png", captcha_da_vez$arq))))
    letras <- strsplit(input$captcha_resposta, "")[[1]] %>% magrittr::extract(. != "")
    letras <- c(letras, rep("", 6 - length(letras)))[1:6]
  })

  df_com_letras_cortadas <- reactive({
    # print(paste("a8", file.exists(sprintf("../data/captchas/%s.png", captcha_da_vez$arq))))
    cortar_som_em_letras(df_com_letras_identificadas())
  })

  df_com_resposta <- reactive({
    # print(paste("a9", file.exists(sprintf("../data/captchas/%s.png", captcha_da_vez$arq))))
    df_com_letras_cortadas() %>%
      mutate(resposta = letras(),
             final_diff = sprintf("Letra %s", order(final_diff)))
  })

  output$previa <- renderTable({
    # print(paste("a10", file.exists(sprintf("../data/captchas/%s.png", captcha_da_vez$arq))))
    df_com_resposta() %>% dplyr::select(-som_letra)
  })

  captcha_valido <- eventReactive(input$enviar_captcha, {
    # print(paste("a11", file.exists(sprintf("../data/captchas/%s.png", captcha_da_vez$arq))))
    (stringi::stri_length(input$captcha_resposta) == 6) && (letras() %>% stringi::stri_detect_regex("[0-9a-zA-Z]") %>% all)
  })

  observeEvent(input$enviar_defeito, {
    # print(paste("a12", file.exists(sprintf("../data/captchas/%s.png", captcha_da_vez$arq))))
    if(input$captcha_defeitos %>% length %>% equals(0) %>% not){
      load("../data/captchas.RData")

      captchas[captchas$captcha_id %in% captcha_da_vez$arq, "resposta"] <- input$captcha_defeitos
      save(captchas, file = "../data/captchas.RData")
      # print(paste("a15", file.exists(sprintf("../data/captchas/%s.png", captcha_da_vez$arq))))

      closeAlert(session, alertId = "status_msg")
      createAlert(session, "status", alertId = "status_msg", title = NULL, style = "info",
                  content = sprintf("Defeito reportado (arquivo %s).", captcha_da_vez$arq), append = FALSE)


      atualiza_captchas_sumario(captchas, input, output)
      captcha_da_vez$arq <- sortear_captcha(captchas)
      # print(paste("a14", file.exists(sprintf("../data/captchas/%s.png", captcha_da_vez$arq))))
    }
  })

  observeEvent(input$enviar_captcha, {
    # print(paste("a16", file.exists(sprintf("../data/captchas/%s.png", captcha_da_vez$arq))))
    updateCheckboxInput(session, "captcha_corte_ok", value = FALSE)
    if(captcha_valido() & input$captcha_corte_ok) {
      load("../data/captchas.RData")

      captchas[captchas$captcha_id %in% captcha_da_vez$arq, "resposta"] <- "OK"
      save(captchas, file = "../data/captchas.RData")
      # print(paste("a17", file.exists(sprintf("../data/captchas/%s.png", captcha_da_vez$arq))))

      createAlert(session, "status", alertId = "status_msg", title = "Sucesso!", style = "success",
                  content = "Captcha enviado!", append = FALSE)

      atualiza_captchas_sumario(captchas, input, output)
      # print(paste("a18", file.exists(sprintf("../data/captchas/%s.png", captcha_da_vez$arq))))

      # print(sprintf("/home/athos/Documentos/captchaReceitaAudio/data/captchas/%s.png", captcha_da_vez$arq))
      # print(file.exists(sprintf("../data/captchas_resposta/%s_%s.png", captcha_da_vez$arq, input$captcha_resposta)))
      # print(captcha_da_vez$arq)
      teste <- file.copy(from = sprintf("../data/captchas/%s.png", captcha_da_vez$arq),
                         to = sprintf("../data/captchas_resposta/%s_%s.png", captcha_da_vez$arq, input$captcha_resposta), overwrite = TRUE)
      # print(teste)
      saveRDS(df_com_resposta(), file = sprintf("../data/captchas_resposta/%s.rds", captcha_da_vez$arq))
      captcha_da_vez$arq <- sortear_captcha(captchas)
      # print(paste("a19", file.exists(sprintf("../data/captchas/%s.png", captcha_da_vez$arq))))
    } else {
      if(!input$captcha_corte_ok) {
        createAlert(session, "status", alertId = "status_msg", title = "Erro", style = "warning",
                    content = "Por favor confirme se as letras foram cortadas corretamente.", append = FALSE)
      } else {
        createAlert(session, "status", alertId = "status_msg", title = "Erro", style = "danger",
                    content = "Captcha inválido.", append = FALSE)
      }
    }
  })

  observeEvent(captcha_da_vez$arq, {
    # print(paste("a20", file.exists(sprintf("../data/captchas/%s.png", captcha_da_vez$arq))))
    captcha_sugerido <- df_com_letras_cortadas() %$% som_letra %>% map(decifra_letra) %>% reduce(paste0)
    limpar_inputs(session, captcha_sugerido)

  })

  observeEvent(input$captcha_resposta, {closeAlert(session, alertId = "status_msg")})


})

shinyApp(ui = ui, server = server)

