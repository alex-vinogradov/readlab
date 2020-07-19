#' Imports Data Frame from Excel File with Variable and Value Labels
#' @param file Excel workbook with two sheets: for data and dictionary
#' @param data.sheet Name or number of sheet that contains data
#' @param vars.sheet Name or number of sheet that contains data dictionary
#' @param ... Additional parameters to readxl::read_excel() function
#' @examples
#' # read_labelled("example.xlsx")
#' # read_labelled("example.xlsx", "Data View", "Variable View")
#' @importFrom readxl read_excel
#' @importFrom jsonlite fromJSON
#' @importFrom haven labelled
#' @export
read_labelled <- function(file, data.sheet = 1, vars.sheet = 2, ...) {
  data <- read_excel(file, sheet = data.sheet, ...)
  if ("tbl" %in% class(vars.sheet)) {
    vars <- vars.sheet
  } else {
    vars <- read_excel(file, sheet = vars.sheet)
  }

  if (!"variable" %in% names(vars)) stop("There is no 'variable' column in dictionary", call. = FALSE)
  if (!"label" %in% names(vars)) stop("There is no 'label' column in dictionary", call. = FALSE)
  if (!"values" %in% names(vars)) stop("There is no 'values' column in dictionary", call. = FALSE)

  varnames <- names(data)

  for (var in vars$variable) {
    if (var %in% varnames) {
      varlab <- vars[vars$variable == var, "label", TRUE]
      if (is.na(varlab)) varlab <- NULL
      values <- vars[vars$variable == var, "values", TRUE]
      if (!is.na(values)) values <- unlist(fromJSON(values)) else values <- NULL
      data[[var]] <- labelled(data[[var]], values, varlab)
      attr(data[[var]], "format.spss") <- "F8.0"
    } else {
      cat("Variable does not exist:", var, "\n")
    }
  }
  return(data)
}
