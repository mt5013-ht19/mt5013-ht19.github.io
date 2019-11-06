Grading criteria of the MT5013 course - ht2019
================
Michael Höhle
2019-11-06

The present document operationalises the description found in the
[course plan](https://sisu.it.su.se/pdf_creator/33466/44998), which
consists of 3 parts:

  - Homework lab exercises (3 ECTS)
  - Written exam (1.5 ECTS)
  - Project work (3 ECTS)

## Criterion for each submodule

### Homework lab exercises

The pass/fail grade is formed using the AND operation on the 6 binary
homework grades.

### Exam

Linear point scale with thresholds determining the grades from A-F. See
the old exam for an example.

### Project work

Combination of points in several categories (compare with the course
goals, but also readability of the report and pitching the project in
the oral presentation). The specific criterion will be announced when
the project work is released. The point scale is then discretized. This
leads to one grade on the A-F scale.

## Combining the three grades

One has to get a pass grade (i.e. ‘G’ for the binary grade and ‘E’ or
better for the character grade) in each of the three momenter to pass
the entire course. Subject to the homework labs being passed, a weighted
mean of the grade from the exam and project is performed using the ECTS
points of each ‘moment’ as weight.

In R code:

``` r
######################################################################
#'
#' Illustration of the operationalized grading criteria of the MT5013
#' course.
#'
#' Author: Michael Höhle
#' Date: 2019-11-06
######################################################################

suppressPackageStartupMessages(library(tidyverse))


#' Define the measuring scales (character grade and pass/fail grade)
betyg_char <- rev(letters[1:6])
betyg_ug <- c("u","g")

#' Function to convert alphabetical grade into numeric scale 0..5
convert_char_to_numeric <- function(char_grade) {
  as.numeric(factor(char_grade, levels=betyg_char)) - 1
}

#' Function to convert pass/fail into numeric scale 0..1
convert_ug_to_numeric <- function(grade) {
  case_when( grade == "u" ~ 0, grade == "g" ~ 1)
}

#' Combine by weighting according to the ECTS points of each moment
weighted_grade <- function(lab, exam, projekt) {
  # Make a vector containing the three components
  grade_vec <- c(lab=convert_ug_to_numeric(lab),
                 exam=convert_char_to_numeric(exam),
                 projekt=convert_char_to_numeric(projekt))
  # Check if parts are passed
  pass_all <- all( grade_vec > 0)
  # Weights for the combination
  slutbetyg_weights <- c(lab=0, exam=1.5, projekt=3)
  # Calculate weighted_grade if pass_all, otherwise the grade is 'f'
  weighted_grade <- pass_all * sum(slutbetyg_weights/sum(slutbetyg_weights) * grade_vec)
  # Return result
  return( weighted_grade ) 
}

#' Function to convert weighted grade into a character grade
convert_numeric_to_char <- function(weighted_grade) {
  return(betyg_char[round(weighted_grade) + 1])
}

#' Helper function putting the weighted_grade and
#' convert_numeric_to_char into sequence.

combined_grade <- function(lab, exam, projekt) {
  return(convert_numeric_to_char(weighted_grade(lab, exam, projekt)))
}
```

``` r
#' Make a data.frame with all possible grades
grade_df <- expand.grid(lab=betyg_ug, exam=betyg_char, projekt=betyg_char)

#' Calculate the weighted grade and the resulting character grade
#' for each possible combination of the three grades.
grade_df <- grade_df %>% arrange(desc(lab), desc(exam), desc(projekt)) %>% rowwise %>%
    mutate(weighted_grade = weighted_grade(lab, exam, projekt),
           final_grade = convert_numeric_to_char(weighted_grade))
```

### Result

``` r
grade_df %>% print(n=nrow(grade_df))
```

    ## Source: local data frame [72 x 5]
    ## Groups: <by row>
    ## 
    ## # A tibble: 72 x 5
    ##    lab   exam  projekt weighted_grade final_grade
    ##    <fct> <fct> <fct>            <dbl> <chr>      
    ##  1 g     a     a                 5    a          
    ##  2 g     a     b                 4.33 b          
    ##  3 g     a     c                 3.67 b          
    ##  4 g     a     d                 3    c          
    ##  5 g     a     e                 2.33 d          
    ##  6 g     a     f                 0    f          
    ##  7 g     b     a                 4.67 a          
    ##  8 g     b     b                 4    b          
    ##  9 g     b     c                 3.33 c          
    ## 10 g     b     d                 2.67 c          
    ## 11 g     b     e                 2    d          
    ## 12 g     b     f                 0    f          
    ## 13 g     c     a                 4.33 b          
    ## 14 g     c     b                 3.67 b          
    ## 15 g     c     c                 3    c          
    ## 16 g     c     d                 2.33 d          
    ## 17 g     c     e                 1.67 d          
    ## 18 g     c     f                 0    f          
    ## 19 g     d     a                 4.00 b          
    ## 20 g     d     b                 3.33 c          
    ## 21 g     d     c                 2.67 c          
    ## 22 g     d     d                 2    d          
    ## 23 g     d     e                 1.33 e          
    ## 24 g     d     f                 0    f          
    ## 25 g     e     a                 3.67 b          
    ## 26 g     e     b                 3    c          
    ## 27 g     e     c                 2.33 d          
    ## 28 g     e     d                 1.67 d          
    ## 29 g     e     e                 1    e          
    ## 30 g     e     f                 0    f          
    ## 31 g     f     a                 0    f          
    ## 32 g     f     b                 0    f          
    ## 33 g     f     c                 0    f          
    ## 34 g     f     d                 0    f          
    ## 35 g     f     e                 0    f          
    ## 36 g     f     f                 0    f          
    ## 37 u     a     a                 0    f          
    ## 38 u     a     b                 0    f          
    ## 39 u     a     c                 0    f          
    ## 40 u     a     d                 0    f          
    ## 41 u     a     e                 0    f          
    ## 42 u     a     f                 0    f          
    ## 43 u     b     a                 0    f          
    ## 44 u     b     b                 0    f          
    ## 45 u     b     c                 0    f          
    ## 46 u     b     d                 0    f          
    ## 47 u     b     e                 0    f          
    ## 48 u     b     f                 0    f          
    ## 49 u     c     a                 0    f          
    ## 50 u     c     b                 0    f          
    ## 51 u     c     c                 0    f          
    ## 52 u     c     d                 0    f          
    ## 53 u     c     e                 0    f          
    ## 54 u     c     f                 0    f          
    ## 55 u     d     a                 0    f          
    ## 56 u     d     b                 0    f          
    ## 57 u     d     c                 0    f          
    ## 58 u     d     d                 0    f          
    ## 59 u     d     e                 0    f          
    ## 60 u     d     f                 0    f          
    ## 61 u     e     a                 0    f          
    ## 62 u     e     b                 0    f          
    ## 63 u     e     c                 0    f          
    ## 64 u     e     d                 0    f          
    ## 65 u     e     e                 0    f          
    ## 66 u     e     f                 0    f          
    ## 67 u     f     a                 0    f          
    ## 68 u     f     b                 0    f          
    ## 69 u     f     c                 0    f          
    ## 70 u     f     d                 0    f          
    ## 71 u     f     e                 0    f          
    ## 72 u     f     f                 0    f
