# # -----------------------------------------------------------------------
### CSES DATA PLAYGROUND
# Date: October 14th, 2025
# Author: Robert Vidigal, PhD
# Purpose: CSES Shiny Data Playground based on LAPOP Lab Data Playground
# Prev file: ./shiny_preprocessing.R
# Machine: Windows OS
# Status: On-going
# # -----------------------------------------------------------------------
### Data In:
# 1. cses_shiny_data.rda
# 2. cses_variable_labels.csv
# 3. cses_labs.rds
# 4. and fonts from /wwww/
### Data Out: N/A
# # -----------------------------------------------------------------------
options(shiny.useragg = TRUE) # speed it up

# # -----------------------------------------------------------------------
# Packages loading
# # -----------------------------------------------------------------------
library(lapop); library(bslib); library(htmltools); require(bsplus)
suppressPackageStartupMessages(library(dplyr))
library(tidyr); library(stringr); library(haven)
require(shiny); library(shinyWidgets)
suppressPackageStartupMessages(library(Hmisc, exclude = c("src", "summarize", "units", "format.pval")))

lapop_fonts() # LAPOP GRAPH STYLE

# IMD CSES Data (only preselected variables)
# # -----------------------------------------------------------------------
#dstrata <- readRDS("./cses_shiny_data.rds") # 102 MB, rda below performs better
load(file="./cses_shiny_data.rda"); dstrata<-cses_shiny_data; rm(cses_shiny_data)

# Labels data (for DP display)
vars_labels <- read.csv("./cses_variable_labels.csv", encoding = "latin1")

# Labs vector (for DP display)
labs <- readRDS("./cses_labs.rds")
drop_labs <- c("IMD2001_2", "IMD2002", "IMD2003", "IMD2006", "IMD2007")
labs_sec <- labs[ !(unname(labs) %in% drop_labs) ]

# Error handling function (so app does not break)
Error<-function(x){
  tryCatch(x,error=function(e) return(FALSE))
}

# CSES election-year studies for TS
waves_total = c("1996", "1997", "1998", "1999", "2000", "2001", "2002",
                "2003", "2004", "2005", "2006", "2007", "2008", "2009",
                "2010", "2011", "2012", "2013", "2014", "2015", "2016",
                "2017", "2018", "2019", "2020",  "2021")


# # -----------------------------------------------------------------------
# Helper function for TS (handle missing values at end or middle of series)
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

# # -----------------------------------------------------------------------
# Custom weighted averages & CIs, much faster than survey_mean()
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

# # -----------------------------------------------------------------------
# Helper for missing country-year by outcome_var
# # -----------------------------------------------------------------------
get_missing_combinations <- function(data, outcome_var, wave_var,
                                     selected_waves, selected_countries) {
  # Convert wave values to string using haven labels
  data <- data %>%
    mutate(wave_str = as.character(haven::as_factor(.data[[wave_var]])))

  # Build the full country-wave grid
  all_combos <- expand.grid(
    pais_nam = selected_countries,
    wave = selected_waves,
    stringsAsFactors = FALSE
  )

  # Subset only relevant countries
  data <- data %>%
    filter(pais_nam %in% selected_countries)

  # Summarize: how many valid (non-NA and not 0) values exist per combo
  summary <- data %>%
    group_by(pais_nam, wave = wave_str) %>%
    summarise(
      n_valid = sum(!is.na(.data[[outcome_var]]) & .data[[outcome_var]] != 0),
      .groups = "drop"
    )

  # Merge and detect missing
  missing <- all_combos %>%
    left_join(summary, by = c("pais_nam", "wave")) %>%
    filter(is.na(n_valid) | n_valid == 0) %>%
    select(pais_nam, wave)

  return(missing)
}

# -----------------------------------------------------------------------
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
# BOOTSTRAP THEME
# # -----------------------------------------------------------------------
cses_theme <- bs_theme(
  version = 5,
  bootswatch = "cosmo",
  bg = "#ffffff",
  fg = "#212529",
  primary = "#C4722A",
  secondary = "#C4722A",
  success = "#28a745",
  info = "#0066cc",
  warning = "#dc3545",
  danger = "#dc3545",
  #base_font = font_google("Open Sans"),
  #heading_font = font_google("Roboto Slab"),
  #code_font = font_google("Fira Mono"),
  #font_scale = 1
)

# # -----------------------------------------------------------------------
# HOVER POP-UP FOR LEFTSIDE MENU
# # -----------------------------------------------------------------------
info_badge <- function(text, title, content) {
  bsplus::bs_embed_popover(
    tags$span(text, tags$span(icon("info-circle"), class = "me-1",  style = "color:#C4722A;")),
    title = title,
    content = content,
    placement = "right",
    trigger = "click",
    container = "body"
  )
}


# # -----------------------------------------------------------------------
# N-SIZE FUNCTION TO PULL COUNTRY-YEAR COMBOS
# # -----------------------------------------------------------------------
get_sample_counts <- function(
    data, outcome_var,
    wave_var = "wave", country_var = "pais_nam",
    selected_waves = NULL, selected_countries = NULL,
    complete_grid = FALSE
) {
  df <- data
  if (!is.null(selected_waves))     df <- dplyr::filter(df, .data[[wave_var]] %in% selected_waves)
  if (!is.null(selected_countries)) df <- dplyr::filter(df, .data[[country_var]] %in% selected_countries)
  df <- dplyr::filter(df, !is.na(.data[[outcome_var]]))

  per_wave <- df |>
    dplyr::count(wave = .data[[wave_var]], name = "n") |>
    dplyr::arrange(wave)

  per_country <- df |>
    dplyr::count(pais = .data[[country_var]], name = "n") |>
    dplyr::arrange(pais)

  per_country_wave <- df |>
    dplyr::count(pais = .data[[country_var]], wave = .data[[wave_var]], name = "n") |>
    dplyr::arrange(pais, wave)

  if (complete_grid) {
    all_waves <- if (!is.null(selected_waves)) selected_waves else sort(unique(data[[wave_var]]))
    all_countries <- if (!is.null(selected_countries)) selected_countries else sort(unique(data[[country_var]]))

    per_country_wave <- per_country_wave |>
      tidyr::complete(pais = all_countries, wave = all_waves, fill = list(n = 0)) |>
      dplyr::arrange(pais, wave)

    per_country <- per_country_wave |>
      dplyr::group_by(pais) |>
      dplyr::summarise(n = sum(n), .groups = "drop") |>
      dplyr::arrange(pais)

    per_wave <- per_country_wave |>
      dplyr::group_by(wave) |>
      dplyr::summarise(n = sum(n), .groups = "drop") |>
      dplyr::arrange(wave)
  }

  list(
    overall = nrow(df),
    per_wave = per_wave,
    per_country = per_country,
    per_country_wave = per_country_wave
  )
}

# # -----------------------------------------------------------------------
# # -----------------------------------------------------------------------
# # -----------------------------------------------------------------------

# # -----------------------------------------------------------------------
# Creating User Interface UI!
# # -----------------------------------------------------------------------
ui <- fluidPage(
  theme = cses_theme,
  tags$h2("CSES Data Playground (beta version)",
          style = "color: #C4722A; font-weight: bold; font-size: 36px;"),

  sidebarLayout(
    # ----- Sidebar panel for inputs
    sidebarPanel(width = 3,
      selectInput("variable", "Outcome",
                  labs[order(names(labs))],
                  selected = "IMD3010"),
      # Default picks most recent module
      pickerInput(inputId = "module",
                  label = tagList(info_badge("Module",
                          HTML("Please select which CSES Modules to be available in the analysis. Then, select which countries and years below."),
                          "Module")),
                  choices = sort(levels(as_factor(dstrata$IMD1008_MOD)[!is.na(dstrata$IMD1008_MOD)])),
                  selected = c("MODULE 5"),
                  options = list(`actions-box` = TRUE),
                  multiple = TRUE),

      # ----- COUNTRY
      pickerInput(inputId = "pais",
                  label = "Countries",
                    #tagList(info_badge("Countries",
                    #HTML("Please select which countries to be included in the analysis."),
                    #"Countries")),
                  choices = sort(levels(as_factor(dstrata$pais)[!is.na(dstrata$pais)])),
                  options = list(`actions-box` = TRUE),
                  multiple = TRUE),

      # ----- WAVE
      pickerInput(inputId = "wave",
                  label = "Years",
                  #tagList(info_badge("Years",
                  #                           HTML("Please select which years to be included in the analysis."),
                  #                           "Years")),
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

      # ----- WEIGHT selection radio buttons
      bsplus::use_bs_popover(),
      radioButtons(
        inputId = "weight_type",
        label = tagList(info_badge("Weights",
                        HTML("Further information on weights is available in <b>Part 6</b> of CSES Module 4."),
                        "Weights")),
        # For a link, add:
        # HTML('Further information on weights is available in <b>Part 6</b> of CSES Module 4. <br><a href=\"#\" target=\"_blank\">Open doc</a>')
        choiceValues = c("no_weight", "weight_demographic", "weight_sample"),
        choiceNames  = list(
          info_badge("Unweighted", "No weights applied. Raw proportions/percentages.",
                     "Unweighted"),
          info_badge("Demographic weight", "Post-stratification targets.",
                     "Demographic weight"),
          info_badge("Sample weight", "Design/selection probability weights.",
                     "Sample weight")
        ),
        selected = "no_weight"),

      # This fixes a formatting issue with checkboxGroupInput() below
      tags$head(
        tags$style(
          HTML("
          .checkbox-inline {
                    margin-left: 0px;
                    margin-right: 10px;
          }
         .checkbox-inline+.checkbox-inline {
                    margin-left: 0px;
                    margin-right: 10px;
         }
    .shiny-notification {
      width: 615px !important; /* max width */
      max-height: 140px; /* max height */
      word-wrap: break-word;
      white-space: normal;
      overflow-y: auto; /* scrollbar */
      right: 330px !important; /* shift away from right edge */
      box-sizing: border-box;
      font-size: 14px;
    }"))),

      # This triggers the "Generate" button
      tags$script(HTML("
      Shiny.addCustomMessageHandler('clickGenerateButton', function(message) {
    $('#go').click();
  });
")),
      # This makes the slider input to allow only integers for CSES years
      tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}"),

# Make popovers white + wire TRUE hover with JavaScript
      tags$style(HTML("
  .popover {
    --bs-popover-bg: #ffffff;
    --bs-popover-border-color: #dddddd;
    --bs-popover-header-bg: #ffffff;
    --bs-popover-header-color: #212529;
    --bs-popover-body-color: #212529;
    border-color: #dddddd;
  }
  .popover .popover-header,
  .popover .popover-body {
    background-color: #ffffff;
    color: #212529;
  }
")),
      tags$script(HTML("
(function() {
  function upgradeDataAttr(el){
  // Force manual disable
    if (el.getAttribute('data-toggle') === 'popover') {
      el.setAttribute('data-bs-toggle','popover');
      el.removeAttribute('data-toggle');
    }
  }

  function getPopover(el){
    // Force manual trigger so we fully control hover behavior
    return bootstrap.Popover.getOrCreateInstance(el, {
      container: 'body',
      trigger: 'manual',
      html: true, sanitize: false
    });
  }

  function addHoverBehavior(el){
    var timer = null;
    var inst  = getPopover(el);

    function startHide(delay){
      if (timer) clearTimeout(timer);
      timer = setTimeout(function(){
        inst.hide();
      }, delay);
    }
    function cancelHide(){
      if (timer) { clearTimeout(timer); timer = null; }
    }

    // Show on hover
    el.addEventListener('mouseenter', function(){
      cancelHide();
      inst.show();

      // Auto-dismiss after 10s (unless user is hovering the popover)
      startHide(10000);
    });

    // Hide shortly after leaving the icon (unless pointer is on the popover)
    el.addEventListener('mouseleave', function(){
      // small delay to allow moving into the popover
      setTimeout(function(){
        var pop = document.getElementById(el.getAttribute('aria-describedby'));
        if (!pop || !pop.matches(':hover')) inst.hide();
      }, 150);
    });

    // Keep open while hovering the popover; hide when leaving it
    el.addEventListener('shown.bs.popover', function(){
      var pop = document.getElementById(el.getAttribute('aria-describedby'));
      if (!pop) return;
      pop.addEventListener('mouseenter', cancelHide);
      pop.addEventListener('mouseleave', function(){
        startHide(150);  // quick close after leaving the box
      });
    });

    // Prevent click toggling from fighting our hover logic
    el.addEventListener('click', function(e){ e.preventDefault(); });
  }

  document.addEventListener('DOMContentLoaded', function(){
    document.querySelectorAll('[data-bs-toggle=\"popover\"], [data-toggle=\"popover\"]').forEach(function(el){
      upgradeDataAttr(el);
      getPopover(el);       // ensure BS5 instance exists
      addHoverBehavior(el); // wire hover behavior + auto-dismiss
    });
  });
})();
")),
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
        selectInput("variable_sec",
                    label = tagList(
          info_badge("Subgroup for analysis",
                    HTML("Optionally split the Breakdown plot by another subgroup from the dataset.
                 Select <b>None</b> to disable."), "Secondary Variable")),
                    c("None" = "None",
                      labs_sec[order(names(labs_sec))])),
        checkboxGroupInput("demog", "Demographic Variables",
                           c("Gender" = "gendermc",
                             "Age" = "age",
                             "Income" = "wealth",
                             "Education" = "edre",
                             "Urban/Rural" = "ur"),
                           selected = c("gendermc", "age", "edre"),
                           inline = TRUE)),
      # Include button in UI (disabled)
      #actionButton("go", "Generate")
      tags$div(
        style = "display: none;",
        actionButton("go", "Generate"))),

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
                  tabPanel("Breakdown", plotOutput("mover"))),
      br(),
      fluidRow(column(12,
                      tags$div(style = "margin-top:-15px"),
                      downloadButton(outputId = "downloadPlot", label = "Download Figure"),
                      downloadButton(outputId = "downloadTable", label = "Download Table"),
                      tags$div(style = "height:10px"),
                      uiOutput("ns_card"),
                      #uiOutput("missing_warning_card"),
        )
      )
    )
  )
)

# # -----------------------------------------------------------------------
# # -----------------------------------------------------------------------
# # -----------------------------------------------------------------------

# # -----------------------------------------------------------------------
# Define SERVER logic
# # -----------------------------------------------------------------------
# The server function will be called when each client (browser) loads the app.
server <- function(input, output, session) {
  observe({
    req(input$variable)
    if (!input$variable %in% names(dstrata)) {
      showNotification("Selected variable not found in data!", type = "error")
    }
    if (!input$weight_type %in% names(dstrata)) {
      showNotification("Selected weight column not found!", type = "error")
    }
  })

  # Triggers "go" between server and ui to generate default plots
  observe({
    if (!is.null(input$module) && !is.null(input$pais) && !is.null(input$wave)) {
      isolate({
        session$sendCustomMessage("clickGenerateButton", list())
      })
    }
  })

  # Check the number of selected variables for breakdown
   observeEvent(input$demog, {
    if (length(input$demog) > 3) {
      # Show a warning message
      showNotification(HTML("You should only select a maximum of 3 demographic variables to plot."), type = "warning")
    }
  })

  # MAKE IT REACTIVE
  # # -----------------------------------------------------------------------
  formulaText <- reactive({
    paste(input$variable)
  })

  outcome <- reactive({
    input$variable
  })

  wave <- reactive({
    input$wave
  })

  outcome_code <- reactive({
    vars_labels$column_name[which(vars_labels$column_name == paste(outcome()))]
  })

  outcome_lab <- reactive({
    vars_labels$question_short_en[which(vars_labels$column_name == paste(outcome()))]
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
      dplyr::filter(IMD1008_MOD %in% input$module)
  })

# OLD CODE THAT WOULD FORCE PRESELECTION, BUT IT BREAKS THE APP WITH FULL DATASET
# Observe changes in module input to update wave and pais
#  observeEvent(filtered_data(), {
#    data <- filtered_data()
#
#    wave_choices <- sort(unique(data$wave))
#    pais_choices <- sort(unique(data$pais))
#
#    updatePickerInput(
#      session = session,
#      inputId = "wave",
#      choices = wave_choices,
#      selected = wave_choices  # you can leave this empty if no preselection
#    )
#
#    updatePickerInput(
#      session = session,
#      inputId = "pais",
#      choices = pais_choices,
#      selected = pais_choices
#    )
#  })

all_waves  <- sort(unique(dstrata$wave))
all_paises <- sort(unique(dstrata$pais))

observeEvent(input$module, {
  req(input$module)
  valid <- dplyr::filter(dstrata, IMD1008_MOD %in% input$module)
  valid_waves  <- sort(unique(valid$wave))
  valid_paises <- sort(unique(valid$pais))
  wave_disabled <- !(all_waves  %in% valid_waves)
  pais_disabled <- !(all_paises %in% valid_paises)
  new_wave_sel <- intersect(isolate(input$wave), valid_waves)
  if (length(new_wave_sel) == 0) new_wave_sel <- valid_waves
  new_pais_sel <- intersect(isolate(input$pais), valid_paises)
  if (length(new_pais_sel) == 0) new_pais_sel <- valid_paises
  shinyWidgets::updatePickerInput(
    session, "wave",
    choices = all_waves,
    selected = new_wave_sel,
    choicesOpt = list(
      disabled = wave_disabled,
      style    = ifelse(wave_disabled, "color:#999;", "")
    )
  )
  shinyWidgets::updatePickerInput(
    session, "pais",
    choices = all_paises,
    selected = new_pais_sel,
    choicesOpt = list(
      disabled = pais_disabled,
      style    = ifelse(pais_disabled, "color:#999;", "")
    )
  )
})

# Set default slider values:
# # -----------------------------------------------------------------------
# 2-point: 1-1
# 3-point: 3-3
# 4-point: 1-2
# 5-point: 4-5
# 6-point: 3-3
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

# RECODE SLIDER
# # -----------------------------------------------------------------------
output$sliderUI <- renderUI({
  sliderInput(inputId = "recode",
              label = tagList(info_badge("Which values do you want to graph?",
                      HTML("Please select which outcome values to be displayed."),
                      "Which values do you want to graph?")),
              min = min(as.numeric(dstrata[[formulaText()]]), na.rm=TRUE),
              max = max(as.numeric(dstrata[[formulaText()]]), na.rm=TRUE),
              value = sliderParams$valuex,
              step = 1)
})

# Filtering data based on user's selection (dff)
dff <- eventReactive(input$go, ignoreNULL = FALSE, {
  dstrata %>%
    dplyr::filter(as_factor(wave) %in% input$wave) %>% # year
    dplyr::filter(pais_nam %in% input$pais) # country
})

# Rendering var caption based on user's var selection
cap <- renderText({
  vars_labels$question_short_en[which(vars_labels$column_name == formulaText())]
})
output$caption <- renderText({
  cap()
})

# Rendering variable code + wording based on user's var selection
word <- renderText({
  paste0(toupper(vars_labels$column_name[which(vars_labels$column_name == formulaText())]), ". ",
         vars_labels$question_en[which(vars_labels$column_name == formulaText())])
})
output$wording <- renderText({
  word()
})

# Rendering ROs based on user's var selection
resp <- renderText({
  vars_labels$responses_en_rec[which(vars_labels$column_name == formulaText())]
})
output$response <- renderText({
  resp()
})

# Rendering variable_sec ROs
resp_sec <- renderText({
  vars_labels$responses_en_rec[which(vars_labels$column_name == input$variable_sec)]
})
output$response_sec <- renderText({
  resp_sec()
})

# Rendering User selected recode value(s)
slider_values <- renderText({
  if(input$recode[1] == input$recode[2]) {
    paste0("(value: ", unique(input$recode), ")")
  } else {
    paste0("(range: ", paste(input$recode, collapse = " to "), ")")
  }
})
output$selected_values <- renderText({
  slider_values()
})

# # -----------------------------------------------------------------------
# WARNING CARD FOR MISSING COMBOS (SWAPPED FOR N-SIZE CARD)
# # -----------------------------------------------------------------------
# output$missing_warning_card <- renderUI({
#   req(input$go > 0, input$wave, input$pais)

#   # Normalize wave and country inputs
#   selected_waves <- as.character(input$wave)
#   selected_countries <- as.character(input$pais)

#   # Step 1: Compute missing combinations
#   missing <- get_missing_combinations(
#     data = dff(),
#     outcome_var = outcome(),
#     wave_var = "wave",
#     selected_waves = selected_waves,
#     selected_countries = selected_countries
#   )

#   # Step 2: Skip if none missing
#   if (nrow(missing) == 0) return(NULL)

#   # Add country abbreviations
#   missing <- missing %>%
#     left_join(dstrata %>% distinct(pais_nam, pais_lab), by = "pais_nam")

#   # Format message: YEAR: COUNTRIES
#   warning_text <- missing %>%
#     group_by(wave) %>%
#     summarise(
#       country_list = paste(sort(unique(pais_lab)), collapse = ", "),
#       .groups = "drop"
#     ) %>%
#     mutate(combo_label = paste0("<b>", wave, "</b>: ", country_list)) %>%
#     pull(combo_label) %>%
#     paste(collapse = "<br>")

#   # Display warning card
#   tags$div(
#     style = "
#     border: 2px solid #ffc107;
#     border-radius: 8px;
#     padding: 15px;
#     background-color: #fff8e1;
#     margin-bottom: 20px;
#     max-height: 120px;
#     overflow-y: auto;
#     ",
#     HTML(paste0(
#       "<span style='font-size:16px; color: #856404;'>⚠️ <b>Warning:</b> The following country-years have no data for <b>",
#       outcome(), "</b>:<br>", warning_text
#     ))
#   )
# })

# # -----------------------------------------------------------------------
# N-SIZE CARD
# # -----------------------------------------------------------------------
output$ns_card <- renderUI({
    req(dff(), outcome(), input$wave, input$pais)

selected_waves <- as.character(input$wave)
selected_countries <- as.character(input$pais)

ns <- get_sample_counts(
      data = dff(),
      outcome_var = outcome(),
      wave_var = "wave",
      country_var = "pais_nam",   # adjust if your helper uses a different input col
      selected_waves = selected_waves,
      selected_countries = selected_countries
    )

    # If absolutely no non-missing data, show a gentle note
    if (is.null(ns$overall) || ns$overall == 0) {
      return(tags$div(
        style = "border:2px solid #17a2b8; border-radius:8px; padding:14px; background:#e9f7ff; margin-bottom:20px;",
        HTML(paste0("ℹ️ <b>Ns</b> for <b>", outcome(), "</b>: No non-missing observations in the current selection."))
      ))
    }

    # Expect columns: ns$per_wave (wave, n) and ns$per_country_wave (pais, wave, n)
    pCW <- ns$per_country_wave
    # If your helper names the country column differently, change "pais" below

    # Control whether to show zeros
    show_zeros <- FALSE

    # Order waves nicely
    waves <- unique(pCW$wave)

    # If waves are numeric-like but char, coerce to numeric for sorting (silently)
    suppressWarnings({
      wave_num <- suppressWarnings(as.numeric(as.character(waves)))
      if (all(!is.na(wave_num))) waves <- waves[order(wave_num)] else waves <- sort(waves)
    })

    # Create a quick lookup for total N per wave
    per_wave_tbl <- ns$per_wave |>
      dplyr::mutate(wave_chr = as.character(wave)) |>
      dplyr::select(wave_chr, n)

    # Build one <details> block per wave
    blocks <- lapply(seq_along(waves), function(i) {
      w <- waves[i]
      w_chr <- as.character(w)

      wt <- per_wave_tbl$n[match(w_chr, per_wave_tbl$wave_chr)]
      wt <- ifelse(is.na(wt), 0, wt)

      rows <- pCW |>
        dplyr::filter(as.character(wave) == w_chr)

      if (!show_zeros) rows <- dplyr::filter(rows, n > 0)

      rows <- dplyr::arrange(rows, dplyr::desc(n), .by_group = FALSE)

      items <- lapply(seq_len(nrow(rows)), function(j) {
        n_j <- format(rows$n[j], big.mark = ",")
        is_zero <- isTRUE(rows$n[j] == 0)
        li_style <- if (is_zero) "color:#6c757d;" else NULL
        # country column is "pais" as returned by the helper
        tags$li(
          tags$span(HTML(paste0("<b>", rows$pais[j], "</b>: N=", n_j))),
          style = li_style
        )
      })

      tags$details(
        open = (i == 1),  # first year open by default
        class = "ns-year",
        tags$summary(
          HTML(paste0("<b>", w_chr, "</b> — Total N=", format(wt, big.mark=",")))
        ),
        tags$ul(items)
      )
    })

    tags$div(
      style = "border:2px solid #17a2b8;
      border-radius:8px;
      padding:14px;
      background:#e9f7ff;
      margin-bottom:20px;
      max-height:180px;
      overflow-y:auto;",
      # Title + grand total
      tags$div(
        HTML(paste0(
          "📊 <b>Sample sizes</b> (non-missing <b>", outcome(), "</b>)<br>",
          "<b>Total across selection:</b> ", format(ns$overall, big.mark = ",")
        )),
        style = "margin-bottom:6px;"
      ),
      tags$hr(style="margin:8px 0;"),
      # Small CSS polish for the dropdowns
      tags$style(HTML("
      details.ns-year { margin-bottom: 8px; }
      details > summary { cursor: pointer; list-style: none; }
      details > summary::-webkit-details-marker { display: none; }
    ")),
      blocks
    )
  })

# # -----------------------------------------------------------------------
# SOURCE INFO WITH PAIS and WAVE FOR EXPORT
# # -----------------------------------------------------------------------
  source_info_both <- reactive({
    # Get country abbreviations that match selected country names
    pais_abbr <- dstrata %>%
      dplyr::filter(pais_nam %in% input$pais) %>%
      distinct(pais_nam, pais_lab) %>%
      arrange(match(pais_nam, input$pais)) %>%  # preserve input order
      pull(pais_lab)

    pais_display <- paste(pais_abbr, collapse = ", ")
    wave_display <- paste(input$wave, collapse = ", ")

    paste0(
      "Source: CSES Data Playground\n\n",  # Extra newline after source
      str_wrap(paste0(
        "Years selected: ", paste(input$wave, collapse = ", "),
        ". Countries selected: ", paste(pais_abbr, collapse = ", ")), 130),
      "\n\n",  # Double newline before question
      str_wrap(paste0(word(), " ", resp()), 130)
    )
  })

  source_info_pais <- reactive({
    # Get country abbreviations that match selected country names
    pais_abbr <- dstrata %>%
      dplyr::filter(pais_nam %in% input$pais) %>%
      distinct(pais_nam, pais_lab) %>%
      arrange(match(pais_nam, input$pais)) %>%  # preserve input order
      pull(pais_lab)

    pais_display <- paste(pais_abbr, collapse = ", ")
    wave_display <- paste(input$wave, collapse = ", ")

    paste0("Source: CSES Data Playground\n", "Countries selected: ",  str_wrap(pais_display, 130),
           "\n\n",
           str_wrap(paste0(word(), " ", resp()), 130)
    )
  })

  source_info_wave <- reactive({
    wave_display <- paste(input$wave, collapse = ", ")

    paste0("Source: CSES Data Playground\n", "Years selected: ", str_wrap(wave_display, 130),
           "\n\n",
           str_wrap(paste0(word(), " ", resp()), 130)
    )
  })

# # -----------------------------------------------------------------------
# PLOTS
# # -----------------------------------------------------------------------

# Histogram
# # -----------------------------------------------------------------------
# must break into data event, graph event, and renderPlot to get download to work
  histd <- reactive({
    req(dff(), input$variable, input$weight_type)

    if (!input$variable %in% names(dff()) ||
        !input$weight_type %in% names(dff())) {
      return(NULL)
    }

    tryCatch({
      dff() %>%
        drop_na(!!sym(input$variable), !!sym(input$weight_type)) %>%
        group_by(cat = haven::as_factor(!!sym(input$variable))) %>%
        summarise(w = sum(!!sym(input$weight_type), na.rm = TRUE)) %>%
        mutate(
          prop = w / sum(w) * 100,
          proplabel = paste0(round(prop), "%"),
          cat = str_wrap(as.character(cat), width = 25)
        )
    }, error = function(e) {
      NULL
    })
  })

  histg <- reactive({lapop_hist(histd(),
                        ymax = ifelse(any(histd()$prop > 90), 110, 100),
                        source_info = "Source: CSES Data Playground")})

  output$hist <- renderPlot({
    req(dff(), nrow(dff()) > 0, input$variable, input$variable %in% names(dff()))
    return(histg())
  })

# Time-series
# # -----------------------------------------------------------------------
  tsd <- reactive({
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
        dplyr::filter(prop != 0)
    )

    validate(
      need(dta_ts, "Error: no data available. Please verify that this question was asked in this country/year combination.")
    )

    dta_ts <- merge(dta_ts,
                    data.frame(wave = as.character(waves_total), empty = 1),
                    by = "wave", all.y = TRUE)

    return(omit_na_edges(dta_ts))
  })

  tsg <- reactive({lapop_ts(tsd(),
                   ymax = ifelse(any(tsd()$prop > 88, na.rm = TRUE), 110, 100),
                   #label_vjust = -1.5,
                   label_vjust = ifelse(any(tsd()$prop > 80, na.rm = TRUE), -1.1, -1.5),
                   source_info = "Source: CSES Data Playground",
                   subtitle = "% in selected category")
  })


  output$ts <- renderPlot({
    return(tsg())
  })

# Cross Country
# # -----------------------------------------------------------------------
  ccd <- reactive({
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
        dplyr::filter(prop != 0) %>%
        mutate(proplabel = paste0(round(prop), "%"))
    )

    validate(
      need(dta_cc, "Error: no data available. Please verify that this question was asked in this country/year combination")
    )

    return(dta_cc)
  })

  ccg <- reactive({lapop_cc(ccd(), sort = "hi-lo",
                   subtitle = "% in selected category",
                   ymax = ifelse(any(ccd()$prop > 90, na.rm = TRUE), 110, 100),
                   source_info = "Source: CSES Data Playground")
  })

  output$cc <- renderPlot({
    return(ccg())
  })

# Breakdown
# # -----------------------------------------------------------------------
# Use function for each demographic breakdown variable

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
  moverd <- reactive({
    req(dff(), input$recode, input$weight_type)

    dta_mover <- Error(rbind(
      if (input$variable_sec != "None" && input$variable_sec != outcome()) {
        process_data(
          data = dff(),
          outcome_var = outcome(),
          recode_range = input$recode,
          group_var = input$variable_sec,
          weight_var = input$weight_type,
          var_label = str_wrap(variable_sec_lab(), width = 25)
        )
      },
      if ("gendermc" %in% input$demog) {
        process_data(dff(), outcome(), input$recode, "gendermc", "Gender",
                     input$weight_type)
      },
      if ("age" %in% input$demog) {
        process_data(dff(), outcome(), input$recode, "age", "Age",
                     input$weight_type)
      },
      if ("wealth" %in% input$demog) {
        process_data(dff(), outcome(), input$recode, "wealthf", "Wealth",
                     input$weight_type)
      },
      if ("edre" %in% input$demog) {
        process_data(dff(), outcome(), input$recode, "edrerf", "Education",
                     input$weight_type)
      },
      if ("ur" %in% input$demog) {
        process_data(dff(), outcome(), input$recode, "ur", "Place of\nResidence",
                     input$weight_type)
      }
    ))

    validate(
      need(dta_mover, "Error: no data available. Please verify that this question was asked in this country/year combination")
    )

    dta_mover$vallabel <- as.character(dta_mover$vallabel)
    return(dta_mover)
  })


  moverg <- reactive({
    moverg <- lapop_mover(moverd(),
                          subtitle = "% in selected category",
                          ymax = ifelse(any(moverd()$prop > 90, na.rm = TRUE), 119,
                                        ifelse(any(moverd()$prop > 80, na.rm = TRUE), 109, 100)),
                          source_info = "Source: CSES Data Playground")
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

      weight_suffix <- switch(input$weight_type, # Add weight type to file export
                              "no_weight" = "unweighted",
                              "weight_demographic" = "demogweighted",
                              "weight_sample" = "sampleweighted")

      ifelse(input$tabs == "Histogram", paste0("hist_", outcome(), "_", weight_suffix, ".svg"),
             ifelse(input$tabs == "Time Series",  paste0("ts_", outcome(), "_", weight_suffix, ".svg"),
                    ifelse(input$tabs == "Cross Country",  paste0("cc_", outcome(), "_", weight_suffix, ".svg"),
                           paste0("mover_", outcome(), "_", weight_suffix, ".svg")))) # Add plot type to file export
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
                               label_angle = 90,
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
        write.csv(histd(), file, row.names=F)
        showNotification(HTML("Histogram file download complete ✓ "),
                         type = "message")

      } else if (input$tabs == "Time Series") {
        write.csv(tsd(), file, row.names=F)
        showNotification(HTML("Time series file download complete ✓ "),
                         type = "message")

      } else if (input$tabs == "Cross Country") {
        write.csv(ccd(), file, row.names=F)
        showNotification(HTML("Cross country file download complete ✓ "),
                         type = "message")

      } else {
        write.csv(moverd(), file, row.names=F)
        showNotification(HTML("Break down file download complete ✓ "),
                         type = "message")

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
