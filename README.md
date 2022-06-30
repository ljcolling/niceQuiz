
<!-- README.md is generated from README.Rmd. Please edit that file -->

# niceQuiz: learnR style quizzes in plain javascript

<!-- badges: start -->
<!-- badges: end -->

`niceQuiz` allows you to generate `learnR` style multi-choice quizzes in
plain old js/css/html. This means that they can be hosted on a static
site without the need to run a shiny server to host `learnR`. The syntax
for `niceQuiz` and `learnr` quizzes are *almost* identical, which means
that there’s almost no need to edit your existing quizzes.

## Installation

You can install the development version of niceQuiz like so:

``` r
 devtools::install_github("ljcolling/niceQuiz")
```

## Example

This is a basic example which shows you how to set up a quiz.

``` r
library(niceQuiz)

quiz(caption = "",
  question("What is the collective term for Windows, MacOS, Unix, *etc.*?",
    answer("Operating system", correct = TRUE),
    answer("Graphical user interface"),
    answer("Integrated development environment"),
    answer("Secondary computing software")
  ),
  question("What is R?",
    answer("A high-level programming language", correct = TRUE),
    answer("An IDE for Windows and MacOS"),
    answer("Machine code for RStudio"),
    answer("Baby don't hurt me.")
  ),
  question("How many panes does RStudio contain?",
    answer("1"),
    answer("2"),
    answer("3"),
    answer("4", correct = TRUE)
  ),
  question("Which one is NOT a feature of a good algorithm?",
    answer("There are no infinite loops."),
    answer("Instructions are clearly explained."),
    answer("Individual steps are simple."),
    answer("It is linear.", correct = TRUE)
  )
)
```

Check out [Basic usage](./articles/Basic-usage.html) to see a rendered
example.
