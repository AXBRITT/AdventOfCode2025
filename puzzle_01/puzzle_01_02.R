#' puzzle_01_02.R
#' @name puzzle_01_02
#' @title puzzle_01_02
#' @description
#' Solution to part 02
#' of puzzle 01
#' of Advent of Code 2025
#' @author AXBRITT
#' @import here magrittr
#'

# Load required libraries ----
library(here)
library(magrittr)

# Setup relative paths ----
here::i_am(
  base::paste(
    "puzzle_01",
    "puzzle_01_02.R",
    sep = .Platform$file.sep
  )
)

# load input data ----
## Source: https://adventofcode.com/2025/day/1/input
input_data <- utils::read.csv(
  here::here(
    "puzzle_01",
    "input",
    "INPUT.csv"
  )
) |>
  base::as.data.frame()

# Define initial conditions ----
start_digit <- 50
digit_count <- 100 # 0 to 99
initial_row <- base::data.frame(
  INPUT = "",
  STEP = start_digit,
  OUTPUT = 0
)

# Define functions to use to make this easier ----
## Ensure value is always a number from 0 to 99 by using modulus
this_value <- function(modified_value, digit_count) {
  return_value <- modified_value %% digit_count
  return(return_value)
}

## Recursive function to count passing 0 ----
rotate_step <- function(
  current_value,
  step_change,
  digit_count,
  loop_count,
  zero_count
) {
  current_value <- this_value(current_value + step_change, digit_count)
  loop_count <- loop_count - 1
  if(current_value == 0) {
    zero_count = zero_count + 1
  }
  if(loop_count == 0) {
    output_data <- base::data.frame(
      STEP = current_value,
      OUTPUT = zero_count
    )
    return(output_data)
  } else {
    rotate_step(
      current_value,
      step_change,
      digit_count,
      loop_count,
      zero_count
    )
  }
}

## Calculate the output of each rotation
this_rotation <- function(this_input, current_value) {
  direction <- substr(this_input, 1, 1)
  current_value <- current_value |> base::as.integer()
  this_value <- base::substr(
    this_input,
    2,
    base::nchar(this_input)
  ) |> as.integer()
  switch(
    direction,
    L = return_value <- rotate_step(
      current_value = current_value,
      step_change = -1,
      digit_count = digit_count,
      loop_count = this_value,
      zero_count = 0
    ),
    R = return_value <- rotate_step(
      current_value = current_value,
      step_change = 1,
      digit_count = digit_count,
      loop_count = this_value,
      zero_count = 0
    ),
  )
  return(return_value)
}

# Start looping through input_data ----
output_data <- initial_row
this_return <- base::data.frame(
  STEP = start_digit,
  OUTPUT = 0
)

for (i in base::seq_len(base::length(input_data$INPUT))) {
  this_input <- input_data[i, "INPUT"]
  this_return <- this_rotation(this_input, this_return$STEP)
  this_row <- base::data.frame(
    INPUT = this_input,
    STEP = this_return$STEP,
    OUTPUT = this_return$OUTPUT
  )
  output_data <- base::rbind(
    output_data,
    this_row
  )
}

# Handle output_data ----
## Show output_data and password to user ----
View(output_data)
password <- base::sum(output_data$OUTPUT)
print(password)
## Create output directory ----
if(!base::dir.exists(
  here::here(
    "puzzle_01",
    "output"
  )
)) {
  base::dir.create(
    here::here(
      "puzzle_01",
      "output"
    )
  )
}

## Write output_data to csv ----
utils::write.csv(
  output_data,
  file = here::here(
    "puzzle_01",
    "output",
    "OUTPUT_02.csv"
  )
)

## Write password to password.txt ----
base::cat(
  password,
  file = here::here(
    "puzzle_01",
    "output",
    "password_02.txt"
  )
)