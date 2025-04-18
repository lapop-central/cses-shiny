# # -----------------------------------------------------------------------
# CSES DATA PLAYGROUND
# Date: April 10th, 2025
# Author: Robert Vidigal, PhD
# Purpose: CSES Shiny Data Playground
# Data In: cses_shiny_data.rds / cses_variable_labels.csv / cses_labs.rds
# Data Out: N/A
# Prev file: None
# Status: On-going
# Machine: Windows OS
# # -----------------------------------------------------------------------

# Packages loading
# # -----------------------------------------------------------------------
library(lapop)
library(haven)
library(dplyr)
library(stringr)
library(shinyWidgets)
library(Hmisc)
library(tidyr)

lapop_fonts()

# IMD CSES Data (only selected variables)
dstrata <- readRDS("./cses_shiny_data.rds")

# Labels data (for DP display)
vars_labels <- read.csv("./Data preprocessing/cses_variable_labels.csv", encoding = "latin1")

# Labs vector (for DP display)
labs <- readRDS("./cses_labs.rds")

# Error handling function (so app does not break)
Error<-function(x){
  tryCatch(x,error=function(e) return(FALSE))
}

# CSES election-year studies
#####years_total = c("2004", "2006", "2008", "2010", "2012", "2014", "2016/17", "2018/19", "2021", "2023")


# Helper function for cleaning Time-series
# handle missing values at end or middle of series
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

# Helper function for mover plot
process_data <- function(data, outcome_var, recode_range, group_var, var_label,
                         weight_var = "weight_demographic") {

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
# Creating User Interface
# # -----------------------------------------------------------------------
ui <- fluidPage(

  titlePanel("CSES Data Playground"),

  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(


      selectInput("variable", "Variable",
                  labs[order(names(labs))],
                  selected = "IMD3010"),

      # Default picks all countries
      pickerInput(inputId = "pais",
                  label = "Countries",
                  choices = sort(levels(as_factor(dstrata$pais)[!is.na(dstrata$pais)])),
                  selected = c("Czech Republic/Czechia", "Germany",
                               "Netherlands"),
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

      # This makes the slider input to allow only integers for CSES years
      tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}"),

      pickerInput(inputId = "year",
                  label = "CSES Survey Rounds",
                  choices = c("1996" = "1996",
                              "1997" = "1997",
                              "1998" = "1998",
                              "1999" = "1999",
                              "2000" = "2000",
                              "2001" = "2001",
                              "2002" = "2002",
                              "2003" = "2003",
                              "2004" = "2004",
                              "2005" = "2005",
                              "2006" = "2006",
                              "2007" = "2007",
                              "2008" = "2008",
                              "2009" = "2009",
                              "2010" = "2010",
                              "2011" = "2011",
                              "2012" = "2012",
                              "2013" = "2013",
                              "2014" = "2014",
                              "2015" = "2015",
                              "2016" = "2016",
                              "2017" = "2017",
                              "2018" = "2018",
                              "2019" = "2019",
                              "2020" = "2020",
                              "2021" = "2021"),
                  selected = c("2021"),
                  options = list(`actions-box` = TRUE),
                  # options = list
                  multiple = TRUE),

      # NEW WEIGHT SELECTION RADIO BUTTONS
      radioButtons("weight_type", "Weighting Variable",
                   choices = list("Demographic Weight" = "weight_demographic",
                                  "Sample Weight" = "weight_sample"),
                   selected = "weight_demographic"),

      # Show recode slider only for time series, CC, and breakdown/mover (not hist)
      conditionalPanel(
        'input.tabs == "Time Series" | input.tabs == "Cross-Country" | input.tabs == "Breakdown"',
        uiOutput("sliderUI"),
      ),

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
    mainPanel(

      # Output: Formatted text for caption ----
      h3(textOutput("caption")),
      h5(textOutput("wording")),
      h5(textOutput("response")),

      tabsetPanel(id = "tabs",
                  tabPanel("Histogram", plotOutput("hist")),

                  tabPanel("Time Series", plotOutput("ts")),

                  tabPanel("Cross-Country", plotOutput("cc")),

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

# The server function will be called when each client (browser) loads the Shiny app.
server <- function(input, output, session) {

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

  # Set default slider values:
  # 2-point:
  # 3-point:
  # 4-point:
  # 5-point:
  # 6-point:
  # 7-point:
  # 10-point:
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

  # Filtering data based on user's selection
  dff <- eventReactive(input$go, ignoreNULL = FALSE, {
    dstrata %>%
      filter(as_factor(year) %in% input$year) %>%
      filter(pais_nam %in% input$pais)
  })

  # Rendering var caption based on user's var selection
  cap <- renderText({
    vars_labels$question_short_en[which(vars_labels$column_name == formulaText())]
  })

  output$caption <- eventReactive(input$go, ignoreNULL = FALSE, {
    cap()
  })
  # Rendering qwording based on user's var selection
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


  # Histogram
  # must break into data event, graph event, and renderPlot to get download buttons to work
  histd <- eventReactive(input$go, ignoreNULL = FALSE, {
    hist_df = Error(
      dff() %>%
        group_by(across(outcome())) %>%
        summarise(n = n())  %>%
        drop_na() %>%
        rename(cat = 1) %>%
        mutate(prop = prop.table(n) * 100,
               proplabel = paste(round(prop), "%", sep = ""),
               cat = str_wrap(as.character(haven::as_factor(cat)), width = 25)))

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
  tsd <- eventReactive(input$go, ignoreNULL = FALSE, {
    dta_ts = Error(
      dff() %>%
        drop_na(outcome()) %>%
        mutate(outcome_rec = case_when(
          is.na(!!sym(outcome())) ~ NA_real_,
          !!sym(outcome()) >= input$recode[1] &
            !!sym(outcome()) <= input$recode[2] ~ 100,
          TRUE ~ 0)) %>%
        group_by(as.character(as_factor(year))) %>%
        summarise_at(vars("outcome_rec"),
                     list(~weighted.ttest.ci(., weight_demographic))) %>%
        unnest_wider(col = "outcome_rec") %>%
        mutate(proplabel = paste0(round(prop), "%")) %>%
        rename(.,  year = 1) %>%
        filter(prop != 0)
    )
    validate(
      need(dta_ts, "Error: no data available. Please verify that this question was asked in this country/year combination.")
    )
    dta_ts = merge(dta_ts, data.frame(year = as.character(years_total), empty = 1), by = "year", all.y = TRUE)
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

  # Cross-Country
  ccd <- eventReactive(input$go, ignoreNULL = FALSE, {
    dta_cc = Error(
      dff() %>%
        drop_na(outcome()) %>%
        mutate(outcome_rec = case_when(
          is.na(!!sym(outcome())) ~ NA_real_,
          !!sym(outcome()) >= input$recode[1] &
            !!sym(outcome()) <= input$recode[2] ~ 100,
          TRUE ~ 0)) %>%
        group_by(vallabel = pais_lab) %>%
        summarise_at(vars("outcome_rec"),
                     list(~weighted.ttest.ci(., weight_demographic))) %>%
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
  secdf <- eventReactive(input$go, ignoreNULL = FALSE, {
    if (input$variable_sec == "None") {
      NULL
    } else {
      process_data(
        data = dff(),
        outcome_var = outcome(),
        recode_range = input$recode,
        group_var = input$variable_sec,
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

  # Download figures (hist, ts, cc, mover)

  output$downloadPlot <- downloadHandler(
    filename = function(file) {
      ifelse(input$tabs == "Histogram", "hist.svg",
             ifelse(input$tabs == "Time Series", "ts.svg",
                    ifelse(input$tabs == "Cross-Country", "cc.svg", "mover.svg")))
    },
    content = function(file) {
      if(input$tabs == "Histogram") {
        lapop_save(histg(), file)
      } else if (input$tabs == "Time Series") {
        lapop_save(tsg(), file)
      } else if (input$tabs == "Cross Country") {
        lapop_save(ccg(), file)
      } else {
        lapop_save(moverg(), file)
      }
    }
  )

  output$downloadTable <- downloadHandler(
    filename = function(file) {
      ifelse(input$tabs == "Histogram", "hist.csv",
             ifelse(input$tabs == "Time Series", "ts.csv",
                    ifelse(input$tabs == "Cross-Country", "cc.csv", "mover.csv")))
    },
    content = function(file) {
      if(input$tabs == "Histogram") {
        write.csv(histd(), file)
      } else if (input$tabs == "Time Series") {
        write.csv(tsd(), file)
      } else if (input$tabs == "Cross Country") {
        write.csv(ccd(), file)
      } else {
        write.csv(moverd(), file)
      }
    }
  )
}

shinyApp(ui, server)

# END
