#' quiz
#'
#' @description Construct quiz
#'
#' @import htmlwidgets
#'
#' @param caption for compatibility with learnR
#' @param ... see details
#'
#' @details
#' Each \code{quiz} is made up of a number of \code{question} calls
#'
#' Each \code{question} is made up of the question text and an number of
#' \code{answer} calls
#'
#' @export
#' @examples
#' \dontrun{
#' # Make a simple quiz with two questions
#' quiz(
#'   caption = "",
#'   question(
#'     "What is the collective term for Windows, MacOS, Unix, *etc.*?",
#'     answer("Operating system", correct = TRUE),
#'     answer("Graphical user interface"),
#'     answer("Integrated development environment"),
#'     answer("Secondary computing software")
#'   ),
#'   question(
#'     "What is R?",
#'     answer("A high-level programming language", correct = TRUE),
#'     answer("An IDE for Windows and MacOS"),
#'     answer("Machine code for RStudio"),
#'     answer("Baby don't hurt me.")
#'   )
#' )
#' }
quiz <- function(caption = "", ...) {
  quizInner(message = list(...), width = NULL, height = NULL, elementId = NULL)
}

quizInner <- function(message, width = NULL, height = NULL, elementId = NULL) {


  # create widget
  htmlwidgets::createWidget(
    name = "quiz",
    message,
    width = width,
    height = "100%",
    package = "niceQuiz",
    elementId = elementId
  )
}

quiz_output <- function(outputId, width = "100%", height = "400px") {
  htmlwidgets::shinyWidgetOutput(outputId, "quiz", width, height, package = "niceQuiz")
}

renderQuiz <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) {
    expr <- substitute(expr)
  } # force quoted
  htmlwidgets::shinyRenderWidget(expr, quiz_output, env, quoted = TRUE)
}


renderMarkdown <- function(text) {
  z <- stringr::str_remove_all(markdown::renderMarkdown(text = text), "\n")
  z <- stringr::str_remove_all(z, "<p>")
  z <- stringr::str_remove_all(z, "</p>")
  return(z)
}

#' answer
#'
#' Construct an answer
#' @param text the text do be displayed for the answer
#' @param ... see details
#' @details
#' Each \code{question} is made up of a number of \code{answer} calls
#' Each \code{answer} call require the text for the answer and a flag
#' to indicate whether or not it is the correct answer
#' @examples
#' answer("response", correct = TRUE)
#' answer("response")
#'
#' @export
answer <- function(text, correct = FALSE) {
  c(renderMarkdown(text = text), ifelse(correct, "true", "false"))
}

#' question
#'
#' @details Construct a question
#' @param text the text do be displayed for the question
#' @param ... see details
#' @details
#' Each \code{question} is made up of a number of \code{answer} calls
#' @examples
#' question(
#'   text = "A question",
#'   answer("response", correct = TRUE),
#'   answer("response")
#' )
#' @export
question <- function(text, ...) {
  list(question = renderMarkdown(text = text), answers = list(...))
}
