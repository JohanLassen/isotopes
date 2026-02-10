

#' Find isotopes
#'
#' @param isotopic_mass_diff single isotope difference (mz diff)
#' @param peaks dataframe with mz and rt info
#' @param mz_threshold Acceptable level of deviation from expected value
#' @param rt_threshold Acceptable level of deviation from expected value
#' @importFrom purrr map
#' @returns A list of indices of the isotopes
find_isotopes <- function(isotopic_mass_diff,peaks, mz_threshold, rt_threshold){
  seq_len(nrow(peaks)) |>
    purrr::map(~{
      valid_indices <- base::which(
        (
          (abs(peaks$mz - peaks$mz[.x] + isotopic_mass_diff) <= mz_threshold) &
          (abs(peaks$rt - peaks$rt[.x]) <= rt_threshold)
         )
        # |
        # (
        #   (abs(peaks$mz - peaks$mz[.x] - isotopic_mass_diff) <= mz_threshold) &
        #   (abs(peaks$rt - peaks$rt[.x]) <= rt_threshold)
        # )

          )
      valid_indices[valid_indices != .x]
    },
    .progress = T
    )
}



#' Isotope data frame
#'
#' @param peaks dataframe with mz and rt info
#' @param mz_col mz column anme
#' @param rt_col rt column name
#' @param isotope_differences vector of isotope differences (mz diff)
#' @param mz_threshold Acceptable level of deviation from expected value
#' @param rt_threshold Acceptable level of deviation from expected value
#' @importFrom purrr map imap_dfr
#' @importFrom dplyr arrange mutate select everything
#' @returns A dataframe with the isotopes
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
          dplyr::arrange(mz) |>
          dplyr::mutate(matchID = .y) |>
          dplyr::select(matchID, everything())
      }
    })
}

#' return_isotope_df <- function(peaks, mz_col, rt_col, isotope_differences, mz_threshold, rt_threshold) {
#'
#'   peaks$mz <- as.numeric(peaks[[mz_col]])
#'   peaks$rt <- as.numeric(peaks[[rt_col]])
#'   n <- nrow(peaks)
#'
#'   groups <- list()
#'
#'   for (i in seq_len(n)) {
#'     # Find all peaks that are isotopes of peak i (higher mz only)
#'     isotope_indices <- which(
#'       sapply(seq_len(n), function(j) {
#'         if (i == j) return(FALSE)
#'         diff <- peaks$mz[j] - peaks$mz[i]
#'         if (diff <= 0) return(FALSE)  # Only look forward (higher mz)
#'         rt_ok <- abs(peaks$rt[j] - peaks$rt[i]) <= rt_threshold
#'         mz_ok <- any(abs(diff - isotope_differences) <= mz_threshold)
#'         rt_ok && mz_ok
#'       })
#'     )
#'
#'     if (length(isotope_indices) > 0) {
#'       indices <- sort(c(i, isotope_indices))
#'       key <- paste(indices, collapse = "-")
#'       groups[[key]] <- indices  # Automatically deduplicates by key
#'     }
#'   }
#'
#'   if (length(groups) == 0) return(data.frame())
#'
#'   dplyr::bind_rows(lapply(seq_along(groups), function(idx) {
#'     peaks[groups[[idx]], ] |>
#'       dplyr::arrange(mz) |>
#'       dplyr::mutate(matchID = idx) |>
#'       dplyr::select(matchID, dplyr::everything())
#'   }))
#' }
#'
#'

#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom vroom locale vroom
#' @importFrom readr write_csv
#' @importFrom writexl write_xlsx
#' @importFrom readxl read_excel
#' @noRd
app_server <- function(input, output, session) {

  # Load the file
  data <- reactive({
    req(input$file)
    options(shiny.maxRequestSize = 500*1024^2)

    denmark_locale <- vroom::locale(decimal_mark = ",", grouping_mark = ".", date_format = "%d-%m-%Y", time_format = "%H:%M:%S")
    us_locale      <- vroom::locale(decimal_mark = ".", grouping_mark = ",", date_format = "%m/%d/%Y", time_format = "%I:%M:%S %p")

    locale = if (input$locale) denmark_locale else us_locale

    if (grepl("[.]xls", input$file$name)) {
      return(readxl::read_excel(input$file$datapath))
    } else{
      vroom::vroom(input$file$datapath, locale = locale)
    }
  })

  # Update the selectInput choices
  observe({
    req(data())
    updateSelectInput(session, "mz", choices = colnames(data()))
    updateSelectInput(session, "rt", choices = colnames(data()))
  })

  isotope_data <- eventReactive(input$compute, {
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
      readr::write_excel_csv(isotope_data(), file)
    }
  )

  output$excel <- downloadHandler(
    filename = function() {
      paste("isotope_result", Sys.Date(), ".xlsx", sep = "_")
    },
    content = function(file) {
      writexl::write_xlsx(isotope_data(), file)
    }
  )

}
