library(shiny)
library(bslib)
library(ggplot2)
library(ggwordcloud)
library(stopwords)
library(tidyverse)
library(tidytext)
library(reshape2)
library(wordcloud)
library(textdata)

# загружаем стоп-слова
sw <- stopwords(language = "en", source = "snowball")

# функция для очистки текста, токенизации и создания чанков
clean_text <- function(text_file){
  text <- readLines(text_file$datapath)
  
  text_cleared <- text |>
    str_replace_all("[^[:alnum:]-'’` ]", " ") |>
    str_replace_all("[`'’]", "'") |>
    str_to_lower()
  
  tokens <- tibble(text = text_cleared) |>
    unnest_tokens(word, text)
  
  wo_stopwords <- tokens |>
    filter(!word %in% sw)
  
  list <- pull(wo_stopwords, word)
  
  N <- length(list)
  
  num_chunks <- 50
  chunk_size <- ceiling(N / num_chunks)
  chunk_ids <- rep(1:num_chunks, each = chunk_size, length.out = N)
  
  tbl_chunks <- tibble(
    wo_stopwords,
    chunk_id = chunk_ids
  )
  
  return(tbl_chunks)
}

ui <- fluidPage(
  theme = bs_theme(bootswatch = "zephyr"),
  
  titlePanel("Анализ эмоциональной тональности текста"),
  tags$h3("Авторы: Дмитриева Ксения, Моисеева Екатерина, Сатдартов Константин"),
  tags$h4("Ссылка на репозиторий: ",
  tags$a(href = "https://github.com/ekaterinam164-arch/R_final_project",
  "GitHub",
  target = "_blank")),
  
  sidebarLayout(
    sidebarPanel(
      width = 8,
      p("Это приложение помогает определить эмоциональную тональность текста: 
      вставьте свой txt-файл с текстом на английском языке или загрузите файл с текстом из нашего репозитория."),
      p("Затем выберите лексикон: AFINN или NRC. Первый показывает, насколько отрицательная или положительная лексика
      в тексте, а второй разделяет слова по отдельным эмоциям."),
      p("После того, как вы выбрали лексикон AFINN, можете составить облако основных 
        позитивных и негативных слов в отдельном чанке. 
        Для этого просто выберите чанк и нажмите на кнопку «Выбрать чанк»."),
      p("После того, как вы выбрали лексикон NRC, вы также можете выбрать
        отдельный чанк и посмотреть, как внутри него соотносятся друг с другом
        эмоции. Порядок действий будет таким же."),
      br(),
      fileInput("user_file", 
                "Выберите txt-файл здесь:",
                buttonLabel = "Поиск файла...",
                placeholder = "Файл не выбран"),
      actionButton("afinn_btn", 
                   "afinn", 
                   class = "btn-primary"), # кнопка для анализа при помощи AFINN
      actionButton("nrc_btn", 
                   "nrc",
                   class = "btn-primary") # кнопка для анализа при помощи NCR
    ),
    mainPanel(
      width = 8,
      tags$h3("Результаты анализа"),
      plotOutput("analysis_plot", height = 250),
      uiOutput("chunk_selector"),
      plotOutput("chunk_plot", height = 250),
      plotOutput("nrc_plot", height = 250),
      uiOutput("chunk_nrc_selector"),
      plotOutput("chunk_nrc_plot", height = 250)
    )
  )
)

server <- function(input, output) {
  
  # AFINN
  afinn_result <- reactive({
    req(input$user_file)
    basic_tbl <- clean_text(input$user_file)
    
    afinn <- readRDS("afinn_lexicon.rds")
    
    # слова с тональностью (для облака слов)
    tbl_words_sent <- basic_tbl |> 
      inner_join(afinn) |>
      mutate(tone = case_when(value >= 0 ~ "pos",
                              value < 0 ~ "neg"))
    
    # тональность по чанкам (для графика)
    tbl_chunk_sent <- basic_tbl |> 
      inner_join(afinn) |>
      group_by(chunk_id) |> 
      summarise(sum = sum(value)) |> 
      arrange(chunk_id) |>
      mutate(tone = case_when(sum >= 0 ~ "pos",
                              sum < 0 ~ "neg"))
    
    # возвращаем список с обоими результатами
    list(
      chunk_sent = tbl_chunk_sent,
      words_sent = tbl_words_sent,
      num_chunks = max(tbl_chunk_sent$chunk_id)  # динамическое количество чанков
    )
    
  }) |> bindEvent(input$afinn_btn)
  
  # график тональности по чанкам (AFINN)
  output$analysis_plot <- renderPlot({
    result <- afinn_result()
    req(result)
    
    num_chunks <- result$num_chunks
    
    ggplot(result$chunk_sent, aes(chunk_id, sum, fill = tone)) +
      geom_col(show.legend = FALSE) + 
      scale_x_continuous(breaks = seq(0, num_chunks + 1, 5)) + 
      scale_fill_manual(values = c("neg" = "#3B528B", 
                                   "pos" = "#E63946")) +
      labs(title = "Изменение эмоциональной тональности (AFINN)",
           x = "Номер отрезка",
           y = "Суммарная тональность") +
      theme_light()
  })
  
  # селектор чанка
  output$chunk_selector <- renderUI({
    result <- afinn_result()
    req(result)
    
    tagList(
      tags$p("Хотите посмотреть на анализ слов в отдельном чанке? Введите номер чанка:"),
      numericInput("chunk_num", 
                   label = NULL, 
                   value = 1, 
                   min = 1, 
                   max = result$num_chunks),
      actionButton("chunk_btn", 
                   "Выбрать чанк", 
                   class = "btn-primary")
    )
  })
  
  # реактивное выражение для выбранного чанка (AFINN)
  chunk_result <- reactive({
    result <- afinn_result()
    req(result, input$chunk_num)
    
    # Фильтруем слова для выбранного чанка
    chunk_words <- result$words_sent |> 
      filter(chunk_id == input$chunk_num) |> 
      count(word, tone, sort = TRUE)
    
    list(data = chunk_words, 
         chunk_num = input$chunk_num)
    
  }) |> bindEvent(input$chunk_btn)
  
  # график для выбранного чанка (облако слов AFINN)
  output$chunk_plot <- renderPlot({
    chunk_data <- chunk_result()
    req(chunk_data)
    
    # создаем облако слов с разными цветами для позитивных и негативных слов
    ggplot(chunk_data$data, aes(label = word, size = n, color = tone)) +
      geom_text_wordcloud(
        area_corr = TRUE,
        rm_outside = TRUE,
        grid_margin = 1
      ) +
      scale_size_area(max_size = 40) +
      scale_color_manual(values = c('neg' = 'grey', 'pos' = 'pink')) +
      labs(
        title = "Облака слов по эмоциональной тональности (AFINN)",
        subtitle = paste("Анализ отрезка:", chunk_data$chunk_num)
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10),
        strip.text = element_text(size = 12, face = "bold"),
        legend.position = "none"
      ) +
      facet_wrap(~tone)
  })
  
  # NCR
  nrc_result <- reactive({
    req(input$user_file)
    basic_tbl <- clean_text(input$user_file)
    
    nrc_lexicon <- readRDS("nrc_lexicon.rds")
    
    nrc_wider <- nrc_lexicon |>
      mutate(type = ifelse(sentiment %in% c("positive", "negative"), "sentiment", "emotion")) |>
      mutate(value = 1) |>
      pivot_wider(id_cols = word, 
                  names_from = sentiment, 
                  values_from = value, 
                  values_fill = 0)
    
    tbl_words_nrc <- basic_tbl |> 
      inner_join(nrc_wider) |>
      mutate(tone = case_when(positive == 1 ~ 1,
                              negative == 1 ~ -1,
                              .default = 0), 
             .keep = 'unused',
             .after = 'chunk_id')
    
    tbl_chunk_nrc <- tbl_words_nrc |>
      group_by(chunk_id) |>
      summarise(
        sum = sum(tone),
        across(c(trust, fear, sadness, anger, surprise, disgust, joy, anticipation), 
               \(x) sum(x)))
    
    tbl_chunk_nrc <- tbl_chunk_nrc |> 
      mutate(tone = case_when(sum >= 0 ~ "pos",
                              sum < 0 ~ "neg",
                              sum == 0 ~ 'neut'),
             .after = 'chunk_id')
    
    tbl_chunk_emotion <- tbl_chunk_nrc |>
      select(chunk_id, trust, fear, sadness, anger, surprise, disgust, joy, anticipation) |>
      pivot_longer(-chunk_id, names_to = "emotion", values_to = "count") |>
      group_by(chunk_id) |>
      slice_max(count, n = 1, with_ties = FALSE) |>
      ungroup()
    
    list(
      chunk_emotion = tbl_chunk_emotion,
      chunk_nrc = tbl_chunk_nrc,
      words_nrc = tbl_words_nrc,
      num_nrc_chunks = max(tbl_chunk_nrc$chunk_id)
    )
    
  }) |> bindEvent(input$nrc_btn)
  
  # график для NRC (доминирующие эмоции)
  output$nrc_plot <- renderPlot({
    result <- nrc_result()
    req(result)
    
    emotion_colors <- c(
      "trust" = "#A5C000",
      "surprise" = "pink", 
      "joy" = "#F5C000",
      "anticipation" = "#F2DCA8",
      "fear" = "#9B8E9E",         
      "sadness" = "#9BB0C4", 
      "anger" = "#C97171",  
      "disgust" = "#7EA07E" 
    )
    
    ggplot(result$chunk_emotion, aes(x = chunk_id, y = count, fill = emotion)) +
      geom_col(show.legend = TRUE) +
      scale_fill_manual(values = emotion_colors) +
      scale_x_continuous(breaks = seq(0, result$num_nrc_chunks + 1, 5)) +  # ИСПРАВЛЕНО
      labs(title = "Доминирующая эмоция в каждом отрезке текста (NRC)",
           x = "Номер отрезка",
           y = "Количество слов",
           fill = "Эмоция") +
      theme_light() +
      theme(legend.position = "bottom")
  })
  
  # селектор nrc-чанка
  output$chunk_nrc_selector <- renderUI({
    result <- nrc_result()
    req(result)
    
    tagList(
      tags$p("Хотите посмотреть, как распределяются эмоции в отдельном чанке? Введите номер чанка:"),
      numericInput("chunk_nrc_num", 
                   label = NULL, 
                   value = 1, 
                   min = 1, 
                   max = result$num_nrc_chunks),
      actionButton("chunk_nrc_btn", 
                   "Выбрать чанк", 
                   class = "btn-primary")
    )
  })
  
  # реактивное выражение для выбранного чанка (nrc)
  chunk_nrc_result <- reactive({
    result <- nrc_result()
    req(result, input$chunk_nrc_num)
    
    # данные для круговой диаграммы
    pie_data <- result$chunk_nrc |> 
      filter(chunk_id == input$chunk_nrc_num) |> 
      select(-chunk_id, -tone, -sum) |>
      pivot_longer(everything(), names_to = "emotion", values_to = "count") |>
      filter(count > 0) |>
      mutate(percentage = count / sum(count) * 100)
    
    list(data = pie_data, 
         chunk_num = input$chunk_nrc_num)
    
  }) |> bindEvent(input$chunk_nrc_btn)
  
  # график для выбранного чанка
  output$chunk_nrc_plot <- renderPlot({
    pie_data <- chunk_nrc_result()
    req(pie_data)
    
    emotion_colors <- c(
      "trust" = "#A5C000",
      "surprise" = "pink", 
      "joy" = "#F5C000",
      "anticipation" = "#F2DCA8",
      "fear" = "#9B8E9E",         
      "sadness" = "#9BB0C4", 
      "anger" = "#C97171",  
      "disgust" = "#7EA07E" 
    )
    
    # создаем круговую диаграмму
    ggplot(pie_data$data, aes(x = "", y = count, fill = emotion)) +
      geom_col(width = 1, color = "white") +
      coord_polar(theta = "y") +
      scale_fill_manual(values = emotion_colors) +
      labs(title = "Распределение эмоций в чанке (NRC)",
           subtitle = paste("Анализ отрезка:", pie_data$chunk_nrc_num),
           fill = "Эмоция") +
      theme_void() +
      theme(
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        legend.position = "right",
        legend.title = element_text(face = "bold")
      ) +
      geom_text(
        aes(label = paste0(round(percentage, 1), "%")),
        position = position_stack(vjust = 0.5),
        size = 4
      )
  })
}

# запуск приложения
shinyApp(ui = ui, server = server)