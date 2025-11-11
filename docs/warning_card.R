
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
