

#' Find isotopes
#'
#' @param isotopic_mass_diff
#' @param peaks
#' @param mz_threshold
#' @param rt_threshold
#' @importFrom purrr map
#' @returns A list of indices of the isotopes
find_isotopes <- function(isotopic_mass_diff,peaks, mz_threshold, rt_threshold){
  seq_len(nrow(peaks)) |>
    purrr::map(~{
      valid_indices <- base::which(
        abs(peaks$mz - peaks$mz[.x]+isotopic_mass_diff) <= mz_threshold &
          abs(peaks$rt - peaks$rt[.x]) <= rt_threshold)
      valid_indices[valid_indices != .x]
    },
    .progress = T
    )
}



#' Isotope data frame
#'
#' @param peaks
#' @param mz_col
#' @param rt_col
#' @param isotope_differences
#' @param mz_threshold
#' @param rt_threshold
#' @importFrom purrr map imap_dfr
#' @importFrom dplyr arrange mutate select everything
#' @returns
#' @examples
return_isotope_df <- function(peaks, mz_col, rt_col, isotope_differences, mz_threshold, rt_threshold){


  peaks$mz <- as.numeric(peaks[[mz_col]])
  peaks$rt <- as.numeric(peaks[[rt_col]])

  matches <-
    isotope_differences |>
    purrr::map(find_isotopes, peaks, mz_threshold, rt_threshold)


  # Merge the lists in matches
  merged <- list()
  for (i in seq_along(matches)) {
    for (j in seq_along(matches[[i]])) {
      merged[[j]] <- if (length(merged) < j) c(matches[[i]][[j]]) else c(merged[[j]], matches[[i]][[j]])
    }
  }

  merged |>
    imap_dfr(~{
      if (length(.x)>0){
        peaks[c(.y, .x),] |>
          dplyr::arrange(mzmed) |>
          dplyr::mutate(matchID = .y) |>
          dplyr::select(matchID, everything())
      }
    })
}




#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom vroom locale vroom
#' @importFrom readr write_csv
#' @noRd
app_server <- function(input, output, session) {

  # Load the file
  data <- reactive({
    req(input$file)
    options(shiny.maxRequestSize = 500*1024^2)

    denmark_locale <- vroom::locale(decimal_mark = ",", grouping_mark = ".", date_format = "%d-%m-%Y", time_format = "%H:%M:%S")
    us_locale      <- vroom::locale(decimal_mark = ".", grouping_mark = ",", date_format = "%m/%d/%Y", time_format = "%I:%M:%S %p")

    locale = if (input$locale) denmark_locale else us_locale
    vroom::vroom(input$file$datapath, locale = locale)
  })

  # Update the selectInput choices
  observe({
    req(data())
    updateSelectInput(session, "mz", choices = colnames(data()))
    updateSelectInput(session, "rt", choices = colnames(data()))
  })

  isotope_data <- reactive({
    req(input$compute)
    req(data())
    return_isotope_df(data(), mz_col = input$mz, rt_col = input$rt, parsed_isotopes(), input$mass_threshold, input$rt_threshold)
  })

  # Compute the isotope peaks
  output$peaks <- renderTable({
    req(input$compute)
    isotope_data()[1:10,1:6]
  })

  output$data <- renderTable({
    req(input$file)
    data()[1:10,1:6]
  })

  output$data_dim <- renderText({
    paste("Rows:", nrow(data()), "Columns:", ncol(data()))
  })
  output$isotope_data_dim <- renderText({
    paste("Rows:", nrow(isotope_data()), "Columns:", ncol(isotope_data()))
  })

  parsed_isotopes <- reactive({
    as.numeric(strsplit(input$isotope, ",")[[1]])
  })

  output$parsed_isotopes <- renderPrint({
    parsed_isotopes()
  })

  # Download the isotopes result
  output$download <- downloadHandler(
    filename = function() {
      paste("isotope_result", Sys.Date(), ".csv", sep = "_")
    },
    content = function(file) {
      readr::write_csv(isotope_data(), file)
    }
  )

}
