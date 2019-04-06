#' @title Select picker Input Control
#'
#' @description
#' Create a select picker (\url{https://developer.snapappointments.com/bootstrap-select/})
#'
#' @param inputId The \code{input} slot that will be used to access the value.
#' @param label Display a text in the center of the switch.
#' @param choices List of values to select from. If elements of the
#'  list are named then that name rather than the value is displayed to the user.
#' @param selected The initially selected value (or multiple values if multiple = TRUE).
#' If not specified then defaults to the first value for single-select lists
#'  and no values for multiple select lists.
#' @param multiple Is selection of multiple items allowed?
#' @param options List of options, see \link{pickerOptions} for all available options.
#' For limit the number of selections, see example below.
#' @param choicesOpt Options for choices in the dropdown menu.
#' @param width The width of the input : 'auto', 'fit', '100px', '75\%'.
#' @param inline Put the label and the picker on the same line.
#'
#' @return A select control that can be added to a UI definition.
#'
#' @importFrom shiny restoreInput
#' @importFrom htmltools tags
#'
#' @export
pickerInput <- function(inputId, label = NULL, choices, selected = NULL, multiple = FALSE,
                        options = list(), choicesOpt = NULL, width = NULL, inline = FALSE) {
  choices <- choicesWithNames(choices)
  selected <- shiny::restoreInput(id = inputId, default = selected)
  if (!is.null(options) && length(options) > 0)
    names(options) <- paste("data", names(options), sep = "-")
  if (!is.null(width))
    options <- c(options, list("data-width" = width))
  if (!is.null(width) && width %in% c("fit"))
    width <- NULL
  options <- lapply(options, function(x) {
    if (identical(x, TRUE))
      "true"
    else if (identical(x, FALSE))
      "false"
    else x
  })
  selectProps <- dropNulls(c(list(id = inputId, class = "selectpicker form-control"), options))
  selectTag <- do.call(tags$select, c(selectProps, pickerSelectOptions(choices, selected, choicesOpt)))

  if (multiple)
    selectTag$attribs$multiple <- "multiple"
  divClass <- "form-group shiny-input-container"
  labelClass <- "control-label"
  if (inline) {
    divClass <- paste(divClass, "form-horizontal")
    selectTag <- htmltools::tags$div(class="col-sm-10", selectTag)
    labelClass <- paste(labelClass, "col-sm-2")
  }
  pickerTag <- htmltools::tags$div(
    class = divClass,
    style = if (!is.null(width)) paste0("width: ", htmltools::validateCssUnit(width), ";"),
    if (!is.null(label)) htmltools::tags$label(class = labelClass, `for` = inputId, label),
    if (!is.null(label) & !inline) htmltools::tags$br(),
    selectTag
  )
  # Dep
  attachShinyWidgetsDep(pickerTag, "picker")
}


#' @title Select picker Input Control
#'
#' @description
#' Create a select picker (\url{https://developer.snapappointments.com/bootstrap-select/})
#'
#' @param inputId The \code{input} slot that will be used to access the value.
#' @param choices List of values to select from. If elements of the
#'  list are named then that name rather than the value is displayed to the user.
#' @param selected The initially selected value (or multiple values if multiple = TRUE).
#' If not specified then defaults to the first value for single-select lists
#'  and no values for multiple select lists.
#' @param choicesOpt Options for choices in the dropdown menu.
#'
#' @return A select control that can be added to a UI definition.
#'
#' @importFrom htmltools tags
#'
#' @export
pickerInputNEW <- function(inputId, choices, selected = NULL,
                        choicesOpt = NULL) {
  chl <- as.list(choices)
  names(chl) <- choices

  options <- list("data-width" = "120px")
  selectProps <- c(list(id = inputId, class = "selectpicker form-control"), options)
  selectTag <- do.call(tags$select, c(selectProps, pickerSelectOptions(chl, selected, choicesOpt)))

  selectTag <- htmltools::tags$div(class = "col-sm-10", selectTag)

  pickerTag <- htmltools::tags$div(
    class = "form-group shiny-input-container form-horizontal",
    style = "width: 120px;",
    htmltools::tags$label(class = "control-label col-sm-2",
                          `for` = inputId, ""),
    selectTag
  )
  attachShinyWidgetsDep(pickerTag, "picker")
}



#' Generate pickerInput options
#'
#' @param choices a named list
#' @param selected selected value if any
#' @param choicesOpt additional option ofr choices
#'
#' @importFrom htmltools HTML htmlEscape tagList
#'
#' @noRd
pickerSelectOptions <- function(choices, selected = NULL, choicesOpt = NULL) {
  if (is.null(choicesOpt))
    choicesOpt <- list()
  l <- sapply(choices, length)
  csum <- cumsum(l)
  m <- matrix(data = c(c(1, csum[-length(l)] + 1), csum), ncol = 2)
  html <- lapply(seq_along(choices), FUN = function(i) {
    label <- names(choices)[i]
    choice <- choices[[i]]
    if (is.list(choice)) {
      optionTag <- list(
        label = htmltools::htmlEscape(label, TRUE),
        pickerSelectOptions(
          choice, selected,
          choicesOpt = lapply(
            X = choicesOpt,
            FUN = function(j) {
              j[m[i, 1]:m[i, 2]]
            }
          )
        )
      )

      optionTag <- dropNulls(optionTag)
      do.call(htmltools::tags$optgroup, optionTag)
    } else {
      optionTag <- list(
        value = choice, htmltools::HTML(htmltools::htmlEscape(label)),
        style = choicesOpt$style[i],
        `data-icon` = choicesOpt$icon[i],
        `data-subtext` = choicesOpt$subtext[i],
        `data-content` = choicesOpt$content[i],
        disabled = if (!is.null(choicesOpt$disabled[i]) && choicesOpt$disabled[i]) "disabled",
        selected = if (choice %in% selected) "selected" else NULL
      )
      # optionTag$attribs <- c(optionTag$attribs, list(if (choice %in% selected) " selected" else ""))
      optionTag <- dropNulls(optionTag)
      do.call(htmltools::tags$option, optionTag)
    }
  })
  return(htmltools::tagList(html))
}

