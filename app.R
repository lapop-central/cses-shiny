# # -----------------------------------------------------------------------
# CSES DATA PLAYGROUND
# Date: April 25h, 2025
# Author: Robert Vidigal, PhD
# Purpose: CSES Shiny Data Playground
# Data In: cses_shiny_data.rds / cses_variable_labels.csv / cses_labs.rds
# Data Out: N/A
# Prev file: see shiny_preprocessing.R
# Status: On-going
# Machine: Windows OS
# # -----------------------------------------------------------------------

# Packages loading
# # -----------------------------------------------------------------------
library(lapop)
library(haven)
library(dplyr)
library(stringr)
library(shiny)
library(shinyWidgets)
library(Hmisc)
library(tidyr)

lapop_fonts() # LAPOP GRAPH STYLE

# IMD CSES Data (only preselected variables)
dstrata <- readRDS("./cses_shiny_data.rds")

# No weights
dstrata$no_weight = 1

# Labels data (for DP display)
vars_labels <- read.csv("./cses_variable_labels.csv", encoding = "latin1")
vars_labels$responses_en_rec <- tolower(vars_labels$responses_en_rec)
vars_labels$responses_en_rec <- gsub("\\d+\\.", "", vars_labels$responses_en_rec)

# Labs vector (for DP display)
labs <- readRDS("./cses_labs.rds")

# Error handling function (so app does not break)
Error<-function(x){
  tryCatch(x,error=function(e) return(FALSE))
}

# CSES election-year studies for TS
waves_total = c("1996", "1997", "1998", "1999", "2000", "2001", "2002",
                "2003", "2004", "2005", "2006", "2007", "2008", "2009",
                "2010", "2011", "2012", "2013", "2014", "2015", "2016",
                "2017", "2018", "2019", "2020",  "2021")

# Helper function for Time-series (handle missing values at end or middle of series)
# # -----------------------------------------------------------------------
omit_na_edges <- function(df) {
  # Find which rows have NA values
  na_rows <- apply(df, 1, function(row) any(is.na(row)))

  # Find the first and last non-NA row
  first_non_na <- which(!na_rows)[1]
  last_non_na <- which(!na_rows)[length(which(!na_rows))]

  # Subset df to only include rows between the first and last non-NA rows
  df_clean <- df[first_non_na:last_non_na, ]

  return(df_clean)
}

# Custom weighted averages & CIs, to speed up computational speed vs. survey_mean()
# # -----------------------------------------------------------------------
weighted.ttest.ci <- function(x, weights) {
  nx <- length(x)
  vx <- Hmisc::wtd.var(x, weights, normwt = TRUE, na.rm = TRUE) # Weighted variance
  mx <- weighted.mean(x, weights, na.rm = TRUE) # Weighted mean
  stderr <- sqrt(vx/nx)
  tstat <- mx/stderr ## not mx - mu
  cint <- qt(1 - 0.05/2, nx - 1)
  cint <- tstat + c(-cint, cint)
  confint = cint * stderr
  result = data.frame(prop = mx, lb = confint[1], ub = confint[2])
  return(result)
}

# Helper function for mover plot (weighting and handling NAs)
# # -----------------------------------------------------------------------
process_data <- function(data, outcome_var, recode_range,
                         group_var, var_label, weight_var) {

  if (is.null(group_var)) {
    return(NULL)
  }

  processed_data <- data %>%
    drop_na(!!sym(outcome_var)) %>%
    mutate(outcome_rec = case_when(
      is.na(!!sym(outcome_var)) ~ NA_real_,
      !!sym(outcome_var) >= recode_range[1] & !!sym(outcome_var) <= recode_range[2] ~ 100,
      TRUE ~ 0
    )) %>%
    group_by(vallabel = haven::as_factor(haven::zap_missing(!!sym(group_var)))) %>%
    summarise_at(vars("outcome_rec"), list(~weighted.ttest.ci(., !!sym(weight_var)))) %>%
    unnest_wider(col = "outcome_rec") %>%
    mutate(
      varlabel = var_label,
      proplabel = paste0(round(prop), "%")
    ) %>%
    drop_na(.)

  return(processed_data)
}

# # -----------------------------------------------------------------------
# Creating User Interface UI!
# # -----------------------------------------------------------------------
ui <- fluidPage(

  titlePanel("CSES Data Playground"),

  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      selectInput("variable", "Variable",
                  labs[order(names(labs))],
                  selected = "IMD3010"),

      # Default picks most recent module
      pickerInput(inputId = "module",
                  label = "CSES Module",
                  choices = sort(levels(as_factor(dstrata$IMD1008_MOD)[!is.na(dstrata$IMD1008_MOD)])),
                  selected = c("MODULE 5"),
                  options = list(`actions-box` = TRUE),
                  multiple = TRUE),

      # COUNTRY
      pickerInput(inputId = "pais",
                  label = "Countries",
                  choices = sort(levels(as_factor(dstrata$pais)[!is.na(dstrata$pais)])),
                  options = list(`actions-box` = TRUE),
                  multiple = TRUE),

      # This fixes a formatting issue with checkboxGroupInput() below
      tags$head(
        tags$style(
          HTML(
            ".checkbox-inline {
                    margin-left: 0px;
                    margin-right: 10px;
          }
         .checkbox-inline+.checkbox-inline {
                    margin-left: 0px;
                    margin-right: 10px;
          }
        "
          )
        )
      ),

      # This triggers the "Generate" button
      tags$script(HTML("
      Shiny.addCustomMessageHandler('clickGenerateButton', function(message) {
    $('#go').click();
  });
")),
      # This makes the slider input to allow only integers for CSES years
      tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}"),

      pickerInput(inputId = "wave",
                  label = "Survey Years",
                  choices = c("1996" = "1996", "1997" = "1997", "1998" = "1998",
                              "1999" = "1999", "2000" = "2000", "2001" = "2001",
                              "2002" = "2002", "2003" = "2003", "2004" = "2004",
                              "2005" = "2005", "2006" = "2006", "2007" = "2007",
                              "2008" = "2008", "2009" = "2009", "2010" = "2010",
                              "2011" = "2011", "2012" = "2012", "2013" = "2013",
                              "2014" = "2014", "2015" = "2015", "2016" = "2016",
                              "2017" = "2017", "2018" = "2018", "2019" = "2019",
                              "2020" = "2020", "2021" = "2021"),
                  options = list(`actions-box` = TRUE),
                  multiple = TRUE),

      # WEIGHT selection radio buttons ----
      radioButtons("weight_type", "Weighting Variable",
                   choices = list("No weights" = "no_weight",
                                  "Demographic Weight" = "weight_demographic",
                                  "Sample Weight" = "weight_sample"),
                   selected = "no_weight"),

      # Show recode slider only for TS, CC, and mover plots (not for histogram)
      conditionalPanel(
        'input.tabs == "Time Series" |
        input.tabs == "Cross Country" |
        input.tabs == "Breakdown"',
        uiOutput("sliderUI"),
      ),

      # Add additional breakdown variable in mover plot
      conditionalPanel(
        'input.tabs == "Breakdown"',
        selectInput("variable_sec", "Secondary Variable",
                    c("None" = "None",
                      labs[order(names(labs))])),
        checkboxGroupInput("demog", "Demographic Variables",
                           c("Gender" = "gendermc",
                             "Age" = "age",
                             "Wealth" = "wealth",
                             "Education" = "edre",
                             "Urban/Rural" = "ur"),
                           selected = c("gendermc", "age", "edre"), # GENDERMC?
                           inline = TRUE)
      ),

      actionButton("go", "Generate")

    ),

    # Main panel for displaying outputs ----
    # # -----------------------------------------------------------------------
    mainPanel(

      # Output: Formatted text for caption ----
      h3(textOutput("caption")),
      h5(textOutput("wording")),
      h5(textOutput("response")),

      tabsetPanel(id = "tabs",
                  tabPanel("Histogram", plotOutput("hist")),

                  tabPanel("Time Series", plotOutput("ts")),

                  tabPanel("Cross Country", plotOutput("cc")),

                  tabPanel("Breakdown", plotOutput("mover"))
      ),
      br(),
      fluidRow(column(12, "",
                      downloadButton(outputId = "downloadPlot", label = "Download Figure"),
                      downloadButton(outputId = "downloadTable", label = "Download Table")))
    )
  )
)

# # -----------------------------------------------------------------------
# Define server logic to plot various variables ----
# # -----------------------------------------------------------------------

# The server function will be called when each client (browser) loads the app.
server <- function(input, output, session) {

  # Triggers "go" between server and ui to generate default plots
  observe({
    if (!is.null(input$module) && !is.null(input$pais) && !is.null(input$wave)) {
      isolate({
        session$sendCustomMessage("clickGenerateButton", list())
      })
    }
  })

  formulaText <- reactive({
    paste(input$variable)
  })

  outcome <- reactive({
    input$variable
  })

  variable_sec <- reactive({
    input$variable_sec
  })

  variable_sec_lab <- reactive({
    vars_labels$question_short_en[which(vars_labels$column_name == paste(variable_sec()))]
  })

  sliderParams <- reactiveValues(valuex = c(1, 1))

  # Reactive: Filter dataset based on selected module(s)
  # # -----------------------------------------------------------------------
  filtered_data <- reactive({
    req(input$module)
    dstrata %>%
      filter(IMD1008_MOD %in% input$module)
  })

  # Observe changes in module input to update wave and pais
  observeEvent(filtered_data(), {
    data <- filtered_data()

    wave_choices <- sort(unique(data$wave))
    pais_choices <- sort(unique(data$pais))

    updatePickerInput(
      session = session,
      inputId = "wave",
      choices = wave_choices,
      selected = wave_choices  # you can leave this empty if no preselection
    )

    updatePickerInput(
      session = session,
      inputId = "pais",
      choices = pais_choices,
      selected = pais_choices
    )
  })

  # Set default slider values:
  # # -----------------------------------------------------------------------
  # 2-point: 1-1
  # 3-point: 3-3
  # 4-point: 1-2
  # 5-point: 4-5
  # 6-point:
  # 7-point: 5-7
  # 10-point: 8-10
  # ALL OTHER: MEAN

  observeEvent(input$variable, {
    if (max(as.numeric(dstrata[[formulaText()]]), na.rm=TRUE) == 1) {
      sliderParams$valuex <- c(1, 1)
    } else if (max(as.numeric(dstrata[[formulaText()]]), na.rm=TRUE) == 2) {
      sliderParams$valuex <- c(1, 1)
    } else if (max(as.numeric(dstrata[[formulaText()]]), na.rm=TRUE) == 3) {
      sliderParams$valuex <- c(3, 3)
    } else if (max(as.numeric(dstrata[[formulaText()]]), na.rm=TRUE) == 4) {
      sliderParams$valuex <- c(1, 2)
    } else if (max(as.numeric(dstrata[[formulaText()]]), na.rm=TRUE) == 5) {
      sliderParams$valuex <- c(4, 5)
    } else if (max(as.numeric(dstrata[[formulaText()]]), na.rm=TRUE) == 6) {
      sliderParams$valuex <- c(3, 3)
    } else if (max(as.numeric(dstrata[[formulaText()]]), na.rm=TRUE) == 7) {
      sliderParams$valuex <- c(5, 7)
    } else if (max(as.numeric(dstrata[[formulaText()]]), na.rm=TRUE) == 10) {
      sliderParams$valuex <- c(8, 10)
    } else if (max(as.numeric(dstrata[[formulaText()]]), na.rm=TRUE) > 10) {
      mean_val <- mean(as.numeric(dstrata[[formulaText()]]), na.rm = TRUE)
      sliderParams$valuex <- c(mean_val, mean_val)
    }
  })

  output$sliderUI <- renderUI({
    sliderInput(inputId = "recode",
                label = "Response values included in percentage",
                min = min(as.numeric(dstrata[[formulaText()]]), na.rm=TRUE),
                max = max(as.numeric(dstrata[[formulaText()]]), na.rm=TRUE),
                value = sliderParams$valuex,
                step = 1)
  })

  # Filtering data based on user's selection (dff)
  dff <- eventReactive(input$go, ignoreNULL = FALSE, {
    dstrata %>%
      filter(as_factor(wave) %in% input$wave) %>% # year
      filter(pais_nam %in% input$pais) # country
  })

  # Rendering var caption based on user's var selection
  cap <- renderText({
    vars_labels$question_short_en[which(vars_labels$column_name == formulaText())]
  })

  output$caption <- eventReactive(input$go, ignoreNULL = FALSE, {
    cap()
  })
  # Rendering wording based on user's var selection
  word <- renderText({
    vars_labels$question_en[which(vars_labels$column_name == formulaText())]
  })

  output$wording <- eventReactive(input$go, ignoreNULL = FALSE, {
    word()
  })

  # Rendering ROs based on user's var selection
  resp <- renderText({
    vars_labels$responses_en_rec[which(vars_labels$column_name == formulaText())]
  })

  output$response <- eventReactive(input$go, ignoreNULL = FALSE, {
    resp()
  })

  # Rendering User selected recode value(s)
  slider_values <- renderText({
    if(input$recode[1] == input$recode[2]) {
      paste0("(value: ", unique(input$recode), ")")
    } else {
      paste0("(range: ", paste(input$recode, collapse = " to "), ")")
    }
  })

  output$selected_values <- eventReactive(input$go, ignoreNULL = FALSE, {
    slider_values()
  })

# SOURCE INFO WITH PAIS and WAVE
# # -----------------------------------------------------------------------
  source_info_both <- reactive({
    # Get country abbreviations that match selected country names
    pais_abbr <- dstrata %>%
      filter(pais_nam %in% input$pais) %>%
      distinct(pais_nam, pais_lab) %>%
      arrange(match(pais_nam, input$pais)) %>%  # preserve input order
      pull(pais_lab)

    pais_display <- paste(pais_abbr, collapse = ", ")
    wave_display <- paste(input$wave, collapse = ", ")

    paste0(", CSES Data Playground\n",
           str_wrap(paste0("Years: ", wave_display,
           ". Countries: ", pais_display), 130),
           "\n\n",
           str_wrap(paste0(word(), resp()), 130)
    )
  })

  source_info_pais <- reactive({
    # Get country abbreviations that match selected country names
    pais_abbr <- dstrata %>%
      filter(pais_nam %in% input$pais) %>%
      distinct(pais_nam, pais_lab) %>%
      arrange(match(pais_nam, input$pais)) %>%  # preserve input order
      pull(pais_lab)

    pais_display <- paste(pais_abbr, collapse = ", ")
    wave_display <- paste(input$wave, collapse = ", ")

    paste0(", CSES Data Playground\n", "Countries: ",  str_wrap(pais_display, 130),
           "\n\n",
           str_wrap(paste0(word(), resp()), 130)
    )
  })

  source_info_wave <- reactive({
    wave_display <- paste(input$wave, collapse = ", ")

    paste0(", CSES Data Playground\n", "Years: ", str_wrap(wave_display, 130),
           "\n\n",
           str_wrap(paste0(word(), resp()), 130)
    )
  })

#################################################### NEEED TO REMOVE LAPOP LAB

  # Histogram
  # # -----------------------------------------------------------------------
  # must break into data event, graph event, and renderPlot to get download buttons to work
  histd <- eventReactive(input$go, ignoreNULL = FALSE, {
    hist_df <- Error(
      dff() %>%
        drop_na(!!sym(outcome()), !!sym(input$weight_type)) %>%
        group_by(cat = haven::as_factor(!!sym(outcome()))) %>%
        summarise(w = sum(!!sym(input$weight_type), na.rm = TRUE)) %>%
        mutate(
          prop = w / sum(w) * 100,
          proplabel = paste0(round(prop), "%"),
          cat = str_wrap(as.character(cat), width = 25)
        )
    )

    validate(
      need(hist_df, "Error: no data available. Please verify that this question was asked in this country/year combination.")
    )
    return(hist_df)
  })

  histg <- eventReactive(input$go, ignoreNULL = FALSE, {
    histg <- lapop_hist(histd(),
                        ymax = ifelse(any(histd()$prop > 90), 110, 100),
                        source_info = ", CSES Data Playground")
    return(histg)
  })

  output$hist <- renderPlot({
    return(histg())
  })


  # Time-series
  # # -----------------------------------------------------------------------
  tsd <- eventReactive(input$go, ignoreNULL = FALSE, {
    dta_ts <- Error(
      dff() %>%
        drop_na(!!sym(outcome()), !!sym(input$weight_type)) %>%
        mutate(outcome_rec = case_when(
          !!sym(outcome()) >= input$recode[1] &
            !!sym(outcome()) <= input$recode[2] ~ 100,
          TRUE ~ 0
        )) %>%
        group_by(wave = as.character(as_factor(wave))) %>%
        summarise_at(
          vars("outcome_rec"),
          list(~weighted.ttest.ci(., !!sym(input$weight_type)))
        ) %>%
        unnest_wider(col = "outcome_rec") %>%
        mutate(proplabel = paste0(round(prop), "%")) %>%
        filter(prop != 0)
    )

    validate(
      need(dta_ts, "Error: no data available. Please verify that this question was asked in this country/year combination.")
    )

    dta_ts <- merge(dta_ts,
                    data.frame(wave = as.character(waves_total), empty = 1),
                    by = "wave", all.y = TRUE)

    return(omit_na_edges(dta_ts))
  })


  tsg <- eventReactive(input$go, ignoreNULL = FALSE, {
    tsg = lapop_ts(tsd(),
                   ymax = ifelse(any(tsd()$prop > 88, na.rm = TRUE), 110, 100),
                   #label_vjust = -1.5,
                   label_vjust = ifelse(any(tsd()$prop > 80, na.rm = TRUE), -1.1, -1.5),
                   source_info = ", CSES Data Playground",
                   subtitle = "% in selected category")
    return(tsg)
  })


  output$ts <- renderPlot({
    return(tsg())
  })

  # Cross Country
  # # -----------------------------------------------------------------------
  ccd <- eventReactive(input$go, ignoreNULL = FALSE, {
    dta_cc <- Error(
      dff() %>%
        drop_na(!!sym(outcome()), !!sym(input$weight_type)) %>%
        mutate(outcome_rec = case_when(
          !!sym(outcome()) >= input$recode[1] &
            !!sym(outcome()) <= input$recode[2] ~ 100,
          TRUE ~ 0
        )) %>%
        group_by(vallabel = pais_lab) %>%
        summarise_at(
          vars("outcome_rec"),
          list(~weighted.ttest.ci(., !!sym(input$weight_type)))
        ) %>%
        unnest_wider(col = "outcome_rec") %>%
        filter(prop != 0) %>%
        mutate(proplabel = paste0(round(prop), "%"))
    )

    validate(
      need(dta_cc, "Error: no data available. Please verify that this question was asked in this country/year combination")
    )

    return(dta_cc)
  })

  ccg <- eventReactive(input$go, ignoreNULL = FALSE, {
    ccg = lapop_cc(ccd(), sort = "hi-lo",
                   subtitle = "% in selected category",
                   ymax = ifelse(any(ccd()$prop > 90, na.rm = TRUE), 110, 100),
                   source_info = ", CSES Data Playground")
    return(ccg)
  })

  output$cc <- renderPlot({
    return(ccg())
  })

  # Use function for each demographic breakdown variable
  # # -----------------------------------------------------------------------
  secdf <- eventReactive(input$go, ignoreNULL = FALSE, {
    if (input$variable_sec == "None") {
      NULL
    } else if (variable_sec() == outcome()) {
      showNotification(HTML("You cannot break the outcome variable by itself."), type = "error")
      NULL
    } else {
      process_data(
        data = dff(),
        outcome_var = outcome(),
        recode_range = input$recode,
        group_var = input$variable_sec,
        weight_var = input$weight_type,
        var_label = stringr::str_wrap(variable_sec_lab(), width = 25)
      )
    }
  })

  genderdf <- eventReactive(input$go, ignoreNULL = FALSE, {
    if ("gendermc" %in% input$demog) {
      process_data(
        data = dff(),
        outcome_var = outcome(),
        recode_range = input$recode,
        weight_var = input$weight_type,
        group_var = "gendermc",
        var_label = "Gender"
      )
    } else {
      NULL
    }
  })

  wealthdf <- eventReactive(input$go, ignoreNULL = FALSE, {
    if ("wealth" %in% input$demog) {
      process_data(
        data = dff(),
        outcome_var = outcome(),
        recode_range = input$recode,
        weight_var = input$weight_type,
        group_var = "wealthf",
        var_label = "Wealth"
      )
    } else {
      NULL
    }
  })

  eddf <- eventReactive(input$go, ignoreNULL = FALSE, {
    if ("edre" %in% input$demog) {
      process_data(
        data = dff(),
        outcome_var = outcome(),
        recode_range = input$recode,
        weight_var = input$weight_type,
        group_var = "edrerf",
        var_label = "Education"
      )
    } else {
      NULL
    }
  })

  agedf <- eventReactive(input$go, ignoreNULL = FALSE, {
    if ("age" %in% input$demog) {
      process_data(
        data = dff(),
        outcome_var = outcome(),
        recode_range = input$recode,
        weight_var = input$weight_type,
        group_var = "age",
        var_label = "Age"
      )
    } else {
      NULL
    }
  })

  urdf <- eventReactive(input$go, ignoreNULL = FALSE, {
    if ("ur" %in% input$demog) {
      process_data(
        data = dff(),
        outcome_var = outcome(),
        recode_range = input$recode,
        weight_var = input$weight_type,
        group_var = "ur",
        var_label = "Place of\nResidence"
      )
    } else {
      NULL
    }
  })

  # Combine demographic data frames into one df
  moverd <- eventReactive(input$go, ignoreNULL = FALSE, {
    dta_mover <- Error(rbind(secdf(), genderdf(), agedf(), wealthdf(), eddf(), urdf()))
    validate(
      need(dta_mover, "Error: no data available. Please verify that this question was asked in this country/year combination")
    )
    dta_mover$vallabel <- as.character(dta_mover$vallabel)
    return(dta_mover)
  })

  moverg <- eventReactive(input$go, ignoreNULL = FALSE, {
    moverg <- lapop_mover(moverd(),
                          subtitle = "% in selected category",
                          ymax = ifelse(any(moverd()$prop > 90, na.rm = TRUE), 119,
                                        ifelse(any(moverd()$prop > 80, na.rm = TRUE), 109, 100)),
                          source_info = ", CSES Data Playground")
    return(moverg)
  })

  output$mover <- renderPlot({
    return(moverg())
  })

# # -----------------------------------------------------------------------
# DOWNLOAD SECTION
# # -----------------------------------------------------------------------

# Download Plot
# # -----------------------------------------------------------------------
  output$downloadPlot <- downloadHandler(
    filename = function(file) {
      ifelse(input$tabs == "Histogram", paste0("hist_", outcome(),".svg"),
             ifelse(input$tabs == "Time Series",  paste0("ts_", outcome(),".svg"),
                    ifelse(input$tabs == "Cross Country",  paste0("cc_", outcome(),".svg"),
                           paste0("mover_", outcome(),".svg"))))
    },

    content = function(file) {
      if(input$tabs == "Histogram") {
        title_text <- isolate(cap())
        word_text <- isolate(word())

        hist_to_save <- lapop_hist(histd(),
                                   main_title = title_text,
                                   subtitle = "% in selected category ",
                                   ymax = ifelse(any(histd()$prop > 90), 110, 100),
                                   source_info = source_info_both()
        )

        lapop_save(hist_to_save, file)
        showNotification(HTML("Histogram plot download complete ✓ "), type = "message")

      } else if (input$tabs == "Time Series") {
        title_text <- isolate(cap())
        subtitle_text <- slider_values()

        # Check for single time period
        #if(any(table(tsd()$wave) == 1)) {
        #  showNotification(
        #    "Caution: your selection includes only one time period",
        #    type = "warning",
        #    duration = 5
        # )
          #return()  # Stops further execution
        #}

        ts_to_save <-  lapop_ts(tsd(),
                                main_title = title_text,
                                subtitle = paste0("% in selected category ", subtitle_text),
                                ymax = ifelse(any(tsd()$prop > 88, na.rm = TRUE), 110, 100),
                                label_vjust = ifelse(any(tsd()$prop > 80, na.rm = TRUE), -1.1, -1.5),
                                source_info = source_info_pais()
        )

        lapop_save(ts_to_save, file)
        showNotification(HTML("Time series plot download complete ✓ "), type = "message")

      } else if (input$tabs == "Cross Country") {
        title_text <- isolate(cap())
        subtitle_text <- slider_values()

        cc_to_save <- lapop_cc(ccd(), sort = "hi-lo",
                               main_title = title_text,
                               subtitle = paste0("% in selected category ", subtitle_text),
                               ymax = ifelse(any(ccd()$prop > 90, na.rm = TRUE), 110, 100),
                               source_info = source_info_wave()
        )

        lapop_save(cc_to_save, file)
        showNotification(HTML("Cross country plot download complete ✓ "), type = "message")

      } else {
        title_text <- isolate(cap())
        subtitle_text <- slider_values()
        word_text <- isolate(word())

        mover_to_save <- lapop_mover(
          moverd(),
          main_title = title_text,
          subtitle = paste0("% in selected category ", subtitle_text),
          ymax = ifelse(any(moverd()$prop > 90, na.rm = TRUE), 119,
                        ifelse(any(moverd()$prop > 80, na.rm = TRUE), 109, 100)),
          source_info = source_info_both()
        )

        lapop_save(mover_to_save, file)
        showNotification(HTML("Break down plot download complete ✓ "), type = "message")

      }
    }
  )

# DOWNLOAD TABLE
 # -----------------------------------------------------------------------
  output$downloadTable <- downloadHandler(
    filename = function(file) {
      ifelse(input$tabs == "Histogram", paste0("hist_", outcome(),".csv"),
             ifelse(input$tabs == "Time Series",  paste0("ts_", outcome(),".csv"),
                    ifelse(input$tabs == "Cross Country",  paste0("cc_", outcome(),".csv"),
                           paste0("mover_", outcome(),".csv"))))
    },
    content = function(file) {
      if(input$tabs == "Histogram") {
        write.csv(histd(), file)
        showNotification(HTML("Histogram file download complete ✓ "), type = "message")

      } else if (input$tabs == "Time Series") {
        write.csv(tsd(), file)
        showNotification(HTML("Time series file download complete ✓ "), type = "message")

      } else if (input$tabs == "Cross Country") {
        write.csv(ccd(), file)
        showNotification(HTML("Cross country file download complete ✓ "), type = "message")

      } else {
        write.csv(moverd(), file)
        showNotification(HTML("Break down file download complete ✓ "), type = "message")

      }
    }
  )
}

# RUN APP
# # -----------------------------------------------------------------------
shinyApp(ui, server)

# # -----------------------------------------------------------------------
# END
# # -----------------------------------------------------------------------
