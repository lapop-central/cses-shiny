# # -----------------------------------------------------------------------
### CSES DATA PLAYGROUND
# Date: November 10th, 2025
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
require(shiny); library(shinyWidgets); require(shinyjs); require(ggtext)
suppressPackageStartupMessages(library(Hmisc, exclude = c("src", "summarize", "units", "format.pval")))

lapop_fonts() # LAPOP GRAPH STYLE

# IMD CSES Data (only preselected variables)
# # -----------------------------------------------------------------------
# RDA FILE BEST COMPRESSION FOR RSHINY
load(file="./cses_shiny_data.rda");

# Labels data (for DP display)
vars_labels <- read.csv("./cses_variable_labels.csv", encoding = "latin1")

# Labs vector (for DP display outcomes versus secondary vars that include macro vars)
labs <- readRDS("./cses_labs.rds")
labs_sec <- readRDS("./cses_labs_sec.rds")
load(file="./world.rda")

# Dropping Demographics (OLD, ALLOW USERS TO USE BOTH RAW AND RECODE DEMOG VARS)
#drop_demoglabs <- c("IMD2001_2", "IMD2002", "IMD2003", "IMD2006", "IMD2007") # Demographics
#labs_sec <- labs[ !(unname(labs) %in% drop_demoglabs) ]

# # -----------------------------------------------------------------------
# Error handling function (so app does not break easily)
# # -----------------------------------------------------------------------

Error<-function(x){
  tryCatch(x,error=function(e) return(FALSE))
}

# CSES election-year vector for TS
waves_total = c("1996", "1997", "1998", "1999", "2000", "2001", "2002",
                "2003", "2004", "2005", "2006", "2007", "2008", "2009",
                "2010", "2011", "2012", "2013", "2014", "2015", "2016",
                "2017", "2018", "2019", "2020",  "2021")

# Escape stuff that ggtext/markdown treats specially
sanitize_for_ggtext <- function(x) {
  x <- as.character(x)
  # HTML specials
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;",  x, fixed = TRUE)
  x <- gsub(">", "&gt;",  x, fixed = TRUE)
  # Markdown link/emphasis/backtick
  x <- gsub("\\[", "&#91;", x)
  x <- gsub("\\]", "&#93;", x)
  x <- gsub("\\(", "&#40;", x)
  x <- gsub("\\)", "&#41;", x)
  x <- gsub("\\*", "&#42;", x)
  x <- gsub("_",  "&#95;", x)
  x <- gsub("`",  "&#96;", x)
  x
}

# # -----------------------------------------------------------------------
# Helper function for TS
# # -----------------------------------------------------------------------
# (handle missing values at end or middle of series)

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
# Custom weighted averages & CIs, much faster than survey_mean() etc
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

# # -----------------------------------------------------------------------
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
# N-SIZE FUNCTION TO PULL COUNTRY-YEAR COMBOS (CHATGPT)
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

  useShinyjs(),

  theme = cses_theme,
  tags$h2("CSES Data Playground",
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
                  choices = sort(levels(as_factor(cses_shiny_data$IMD1008_MOD)[!is.na(cses_shiny_data$IMD1008_MOD)])),
                  selected = c("MODULE 5"),
                  options = list(`actions-box` = TRUE),
                  multiple = TRUE),

      # ----- COUNTRY
      pickerInput(inputId = "pais",
                  label = "Countries",
                    #tagList(info_badge("Countries",
                    #HTML("Please select which countries to be included in the analysis."),
                    #"Countries")),
                  choices = sort(levels(as_factor(cses_shiny_data$pais)[!is.na(cses_shiny_data$pais)])),
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
        input.tabs == "World Map" |
        input.tabs == "Breakdown"',

        uiOutput("sliderUI"),
        # Mean Value toggle
       # checkboxInput("use_mean", "Mean value", FALSE),
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
                  tabPanel("Breakdown", plotOutput("mover")),
                  tabPanel("World Map", plotOutput("map"))),

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
    if (!input$variable %in% names(cses_shiny_data)) {
      showNotification("Selected variable not found in data!", type = "error")
    }
    if (!input$weight_type %in% names(cses_shiny_data)) {
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
    cses_shiny_data %>%
      dplyr::filter(IMD1008_MOD %in% input$module)
  })

# OLD CODE THAT WOULD FORCE PRESELECTION, BUT IT BREAKS THE APP WITH FULL DATASET
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
#
#    updatePickerInput(
#      session = session,
#      inputId = "pais",
#      choices = pais_choices,
#      selected = pais_choices
#    )
})

  all_waves  <- sort(unique(cses_shiny_data$wave))
  all_paises <- sort(unique(cses_shiny_data$pais))

  observeEvent(input$module, {
    req(input$module)

    # Filter for the selected module
    valid <- dplyr::filter(cses_shiny_data, IMD1008_MOD %in% input$module)
    valid_waves  <- sort(unique(valid$wave))
    valid_paises <- sort(unique(valid$pais))

    wave_disabled <- !(all_waves %in% valid_waves)
    pais_disabled <- !(all_paises %in% valid_paises)

    # 🔹 Automatically select *all valid waves* when a module is chosen
    shinyWidgets::updatePickerInput(
      session, "wave",
      choices = all_waves,
      selected = valid_waves,  # <-- changed line
      choicesOpt = list(
        disabled = wave_disabled,
        style    = ifelse(wave_disabled, "color:#999;", "")
      )
    )

    # 🔹 Automatically select *all valid countries* too (optional)
    shinyWidgets::updatePickerInput(
      session, "pais",
      choices = all_paises,
      selected = valid_paises,  # <-- changed line
      choicesOpt = list(
        disabled = pais_disabled,
        style    = ifelse(pais_disabled, "color:#999;", "")
      )
    )
  })


# Set default recode slider values:
# # -----------------------------------------------------------------------
# 2-point: 1-1
# 3-point: 3-3
# 4-point: 1-2
# 5-point: 4-5
# 6-point: 3-3
# 7-point: 5-7
# 10-point: 8-10
# ALL OTHER: MEAN

# UPDATE SLIDER DEFAULTS AND MEAN BEHAVIOR
# -----------------------------------------------------------------------
observeEvent({
  list(input$variable, input$use_mean)
}, {
  # compute numeric vector safely
  xvals <- suppressWarnings(as.numeric(cses_shiny_data[[formulaText()]]))
  maxval <- max(xvals, na.rm = TRUE)

  # --- DEFAULT RECODE RANGES ---
  if (maxval == 1) {
    sliderParams$valuex <- c(1, 1)
  } else if (maxval == 2) {
    sliderParams$valuex <- c(1, 1)
  } else if (maxval == 3) {
    sliderParams$valuex <- c(3, 3)
  } else if (maxval == 4) {
    sliderParams$valuex <- c(1, 2)
  } else if (maxval == 5) {
    sliderParams$valuex <- c(4, 5)
  } else if (maxval == 6) {
    sliderParams$valuex <- c(3, 3)
  } else if (maxval == 7) {
    sliderParams$valuex <- c(5, 7)
  } else if (maxval == 10) {
    sliderParams$valuex <- c(8, 10)
  } else {
    mean_val <- mean(xvals, na.rm = TRUE)
    sliderParams$valuex <- c(mean_val, mean_val)
  }

  # --- IF USER SELECTED "USE MEAN VALUE" ---
  if (isTRUE(input$use_mean)) {
    mean_val <- mean(xvals, na.rm = TRUE)
    sliderParams$valuex <- c(mean_val, mean_val)
  }

  # force slider update
  updateSliderInput(
    session,
    inputId = "recode",
    value = sliderParams$valuex
  )
})

# RECODE SLIDER
# # -----------------------------------------------------------------------
output$sliderUI <- renderUI({
  sliderInput(
    inputId = "recode",
    label = tagList(
      info_badge(
        "Which values do you want to graph?",
        HTML("Please select which outcome values to be displayed."),
        "Which values do you want to graph?"
      )
    ),
    min = min(as.numeric(cses_shiny_data[[formulaText()]]), na.rm = TRUE),
    max = max(as.numeric(cses_shiny_data[[formulaText()]]), na.rm = TRUE),
    value = sliderParams$valuex,
    step = 1
  )
})


# Filtering data based on user's selection (dff)
dff <- eventReactive(input$go, ignoreNULL = FALSE, {
  cses_shiny_data %>%
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


# Toggle recode slider
# # -----------------------------------------------------------------------
observe({
  if (isTRUE(input$use_mean)) {
    shinyjs::disable("recode")
  } else {
    shinyjs::enable("recode")
  }
})

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

# -----------------------------------------------------------------------
# SOURCE INFO WITH ACTUAL DATA AVAILABILITY (not just user selections)
# -----------------------------------------------------------------------
source_info_both <- reactive({
  req(dff(), outcome(), input$wave, input$pais)

  # Reuse your helper to get Ns
  ns <- get_sample_counts(
    data = dff(),
    outcome_var = outcome(),
    wave_var = "wave",
    country_var = "pais_nam",
    selected_waves = input$wave,
    selected_countries = input$pais
  )

  # Extract actual combinations with nonzero N
  valid_combos <- ns$per_country_wave %>%
    dplyr::filter(n > 0)

  # Actual waves and countries that have data
  valid_waves <- sort(unique(valid_combos$wave))
  valid_countries <- sort(unique(valid_combos$pais))

  # Get abbreviations for these countries (match order)
  pais_abbr <- cses_shiny_data %>%
    dplyr::filter(pais_nam %in% valid_countries) %>%
    distinct(pais_nam, pais_lab) %>%
    arrange(match(pais_nam, valid_countries)) %>%
    pull(pais_lab)

  paste0(
    "Source: CSES Data Playground\n\n",
    str_wrap(paste0(
      "Years: ", paste(valid_waves, collapse = ", "),
      ". Countries: ", paste(pais_abbr, collapse = ", ")
    ), 130),
    "\n\n",
    str_wrap(paste0(word(), " ", resp()), 130)
  )
})

# -----------------------------------------------------------------------
source_info_pais <- reactive({
  req(dff(), outcome(), input$wave, input$pais)

  ns <- get_sample_counts(
    data = dff(),
    outcome_var = outcome(),
    wave_var = "wave",
    country_var = "pais_nam",
    selected_waves = input$wave,
    selected_countries = input$pais
  )

  valid_combos <- ns$per_country_wave %>%
    dplyr::filter(n > 0)

  valid_countries <- sort(unique(valid_combos$pais))

  pais_abbr <- cses_shiny_data %>%
    dplyr::filter(pais_nam %in% valid_countries) %>%
    distinct(pais_nam, pais_lab) %>%
    arrange(match(pais_nam, valid_countries)) %>%
    pull(pais_lab)

  paste0(
    "Source: CSES Data Playground\n",
    "Countries: ", str_wrap(paste(pais_abbr, collapse = ", "), 130),
    "\n\n",
    str_wrap(paste0(word(), " ", resp()), 130)
  )
})

# -----------------------------------------------------------------------
source_info_wave <- reactive({
  req(dff(), outcome(), input$wave, input$pais)

  ns <- get_sample_counts(
    data = dff(),
    outcome_var = outcome(),
    wave_var = "wave",
    country_var = "pais_nam",
    selected_waves = input$wave,
    selected_countries = input$pais
  )

  valid_combos <- ns$per_country_wave %>%
    dplyr::filter(n > 0)

  valid_waves <- sort(unique(valid_combos$wave))

  paste0(
    "Source: CSES Data Playground\n",
    "Years: ", str_wrap(paste(valid_waves, collapse = ", "), 130),
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
                    by = "wave", all.y = TRUE) %>%
      dplyr::filter(!is.na(prop)) # TO EXCLUDE YEARS NOT IN THE SELECTION
                                  # THEN YEARS ARE NOT SEQUENTIAL

    return(omit_na_edges(dta_ts))
  })

  tsg <- reactive({lapop_ts(tsd(),
                   ymax = ifelse(any(tsd()$prop > 85, na.rm = TRUE), 110, 100),
                   #label_vjust = -1.5,
                   label_vjust = ifelse(any(tsd()$prop > 80, na.rm = TRUE), -1.1, -1.5),
                   source_info = "Source: CSES Data Playground",
                   subtitle = "% in selected category",
                   ci_type = "errorbar")
  })

  output$ts <- renderPlot({
    return(tsg())
  })

# Cross Country
# # -----------------------------------------------------------------------
# define macro (aggregate-level) variables
continuous_vars <- c("IMD3001_TS", "IMD5054_2", "IMD5057_1", "IMD5035",
                     "IMD5056_2", "IMD5055_1", "IMD5053_1", "IMD5052_2")

  ccd <- reactive({
    var_sel <- outcome()
    rec_min <- input$recode[1]
    rec_max <- input$recode[2]

    # CASE 1: Continuous macro-level variable (mean = TRUE)
    if (var_sel %in% continuous_vars) {
      curr_outcome <- sym(var_sel)

      dta_cc <- dff() %>%
        # Apply recode range first
        mutate(
          tmp_val = as.numeric(!!curr_outcome),
          tmp_val = ifelse(tmp_val >= rec_min & tmp_val <= rec_max, tmp_val, NA_real_)
        ) %>%
        group_by(vallabel = pais_lab) %>%
        summarise(
          prop = mean(tmp_val, na.rm = TRUE)
        ) %>%
        ungroup() %>%
        mutate(
          lb = NA_real_,
          ub = NA_real_,
          proplabel = ifelse(is.na(prop), "", sprintf("%.2f", prop))
        ) %>%
        filter(!is.na(prop))

      # CASE 2: Regular individual-level variable (weighted proportion)
    } else {

      dta_cc <- dff() %>%
        drop_na(!!sym(var_sel), !!sym(input$weight_type)) %>%
        mutate(outcome_rec = case_when(
          !!sym(var_sel) >= rec_min & !!sym(var_sel) <= rec_max ~ 100,
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
    }

    validate(
      need(nrow(dta_cc) > 0,
           "Error: no data available. Please verify that this question was asked in this country/year combination.")
    )

    return(dta_cc)
  })

  ccg <- reactive({
    lapop_cc(
      ccd(),
      sort = "hi-lo",
      subtitle = ifelse(outcome() %in% continuous_vars,
                        "Countries (within selected range)",
                        "% in selected category"),
      ymax = ifelse(outcome() %in% continuous_vars, 6,
                    ifelse(any(ccd()$prop > 90, na.rm = TRUE), 110, 100)),
      source_info = "Source: CSES Data Playground"
    )
  })

  output$cc <- renderPlot({
    ccg()
  })

# World Map
# # -----------------------------------------------------------------------
  mapd <- reactive({

    var_sel <- outcome()
    rec_min <- input$recode[1]
    rec_max <- input$recode[2]

    continuous_vars <- c("IMD3001_TS", "IMD5054_2", "IMD5057_1", "IMD5035",
                         "IMD5056_2", "IMD5055_1", "IMD5053_1", "IMD5052_2")

    req(input$module)

    # --- NEW: allow only one module at a time ---
    validate(
      need(
        length(input$module) == 1,
        "Please select only ONE module to display a map."
      )
    )

    # CASE 1: Continuous macro variable (mean values)
    if (var_sel %in% continuous_vars) {

      dta_map <- dff() %>%
        mutate(
          tmp_val = as.numeric(.data[[var_sel]]),
          tmp_val = ifelse(tmp_val >= rec_min & tmp_val <= rec_max, tmp_val, NA_real_)
        ) %>%
        group_by(pais_lab = pais_lab) %>%       # IMPORTANT: must exist in your dataset
        summarise(
          value = mean(tmp_val, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        filter(!is.na(value))

    } else {

      # CASE 2: Categorical / proportion variables
      dta_map <- dff() %>%
        drop_na(.data[[var_sel]], .data[[input$weight_type]]) %>%
        mutate(outcome_rec = case_when(
          .data[[var_sel]] >= rec_min & .data[[var_sel]] <= rec_max ~ 100,
          TRUE ~ 0
        )) %>%
        group_by(pais_lab = pais_lab) %>%
        summarise_at(
          vars("outcome_rec"),
          list(~weighted.ttest.ci(., .data[[input$weight_type]]))
        ) %>%
        unnest_wider(col = "outcome_rec") %>%
        filter(prop > 0) %>%
        rename(value = prop)
    }

    validate(
      need(nrow(dta_map) > 0,
           "Error: no map data available for this country/year/variable selection.")
    )

    return(dta_map)
  })

  mapg <- reactive({
    lapop_map(
      mapd(), survey = "CSES",
      source_info = "\nSource: CSES Data Playground"
    )
  })

  output$map <- renderPlot({
    mapg()
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

      weight_suffix <- switch(input$weight_type, # Add weight type to plot export
                              "no_weight" = "unweighted",
                              "weight_demographic" = "demogweighted",
                              "weight_sample" = "sampleweighted")

      ifelse(input$tabs == "Histogram", paste0("hist_", outcome(), "_", weight_suffix, ".svg"),
             ifelse(input$tabs == "Time Series",  paste0("ts_", outcome(), "_", weight_suffix, ".svg"),
                    ifelse(input$tabs == "Cross Country",  paste0("cc_", outcome(), "_", weight_suffix, ".svg"),
                           ifelse(input$tabs == "World Map",  paste0("map_", outcome(), "_", weight_suffix, ".svg"),
                                  paste0("mover_", outcome(), "_", weight_suffix, ".svg"))))) # Add plot type to file export
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

      } else if (input$tabs == "World Map") {
        title_text <- isolate(cap())
        subtitle_text <- slider_values()

        map_to_save <- lapop_map(mapd(),
                               main_title = title_text,
                               subtitle = paste0("% in selected category ", subtitle_text),
                               source_info = paste0("\n", source_info_both()),
                               survey = "CSES"
        )

        lapop_save(map_to_save, file)
        showNotification(HTML("Map plot download complete ✓ "), type = "message")

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

      weight_suffix <- switch(input$weight_type, # Add weight type to file export
                              "no_weight" = "unweighted",
                              "weight_demographic" = "demogweighted",
                              "weight_sample" = "sampleweighted")

      ifelse(input$tabs == "Histogram", paste0("hist_", outcome(), "_", weight_suffix, ".csv"),
             ifelse(input$tabs == "Time Series",  paste0("ts_", outcome(), "_", weight_suffix,".csv"),
                    ifelse(input$tabs == "Cross Country",  paste0("cc_", outcome(), "_", weight_suffix, ".csv"),
                           ifelse(input$tabs == "World Map",  paste0("map_", outcome(), "_", weight_suffix, ".csv"),
                                  paste0("mover_", outcome(), "_", weight_suffix, ".csv")))))
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

      } else if (input$tabs == "World Map") {
        write.csv(mapd(), file, row.names=F)
        showNotification(HTML("Map file download complete ✓ "),
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
