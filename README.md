
<!-- README.md is generated from README.Rmd. Please edit that file -->

# zoomchattr <img src="man/figures/hex-zoomchattr.png" align="right" width="240">

<!-- badges: start -->
<!-- badges: end -->

zoomchattr takes the directory of Zoom chat files that’s been
accumulating on your computer for the past two years, parses them either
individually or *en masse*, and makes them into a tibble with one chat
message per row.

## Installation

You can install the development version of zoomchattr like so:

``` r
remotes::install_github("kjhealy/zoomchattr")
```

## Example

On my Mac, Zoom stores chat files and recordings in `~/Documents/Zoom`.
Each meeting is a separate folder which can possibly contain a text file
named either `chat.txt` or `meeting_saved_chat.txt`. Zoom chat files
come in two varieties, depending on the Zoom version. The package has
functions to read both kinds, but the main function should read both
types. Either zip up your `~/Documents/Zoom` folder (or equivalent) and
unzip it in your project directory, or get the files directly from their
source. (The package does not edit or overwrite any chat transcripts, it
just reads them in). For example, to get the filenames them from the
native directory on a Mac, do e.g.

``` r
library(tidyverse)
library(zoomchattr)

fnames <- get_zoom_names(path = "~/Documents/Zoom")

fnames
#> /Users/kjhealy/Documents/Zoom/2022-04-21 10.11.23  Data Wrangling with R (April 2022)/meeting_saved_chat.txt
#> /Users/kjhealy/Documents/Zoom/2022-04-21 16.12.25  Data Wrangling with R (April 2022)/meeting_saved_chat.txt
#> /Users/kjhealy/Documents/Zoom/2022-04-22 10.10.36  Data Wrangling with R (April 2022)/meeting_saved_chat.txt
#> /Users/kjhealy/Documents/Zoom/2022-04-22 16.06.06  Data Wrangling with R (April 2022)/meeting_saved_chat.txt
#> /Users/kjhealy/Documents/Zoom/2022-04-23 10.34.58  Data Wrangling with R (April 2022)/meeting_saved_chat.txt
```

Then, read in the files with `parse_all_zoom_chat()`:

``` r

df <- parse_all_zoom_chat(fnames)

dim(df)
#> [1] 111   5

colnames(df)
#> [1] "file"    "time"    "from"    "to"      "message"

head(df)
#> A tibble: 6 × 5
#> file                                               time                from          to          message                                       
#> <chr>                                              <dttm>              <chr>         <chr>       <chr>                                         
#> /Users/kjhealy/Documents/Zoom/2022-04-21 10.11.23… 2022-04-21 10:11:23 Kaity       Kieran Hea… "Hi Kieran, I’ll be logging out now. Please t…
#> /Users/kjhealy/Documents/Zoom/2022-04-21 10.11.23… 2022-04-21 11:04:58 lausmither… Kieran Hea… "How do you get to R studio preferences with …
#> /Users/kjhealy/Documents/Zoom/2022-04-21 10.11.23… 2022-04-21 11:05:28 Kieran Healy laursmither… "It should be under \"Tools > Global Options”"
#> /Users/kjhealy/Documents/Zoom/2022-04-21 10.11.23… 2022-04-21 11:07:56 lauraabcder… Kieran Hea… "Ok thx"                                      
#> /Users/kjhealy/Documents/Zoom/2022-04-21 10.11.23… 2022-04-21 11:21:12 Laura Grpea… Everyone    "Is there a keyboard shortcut for the pipe op…
#> /Users/kjhealy/Documents/Zoom/2022-04-21 10.11.23… 2022-04-21 11:23:06 Taylor Fnam… Everyone    "Ctrl + Shift + M (Windows)"                  
```

Individual chat files can be parsed with `parse_zoom_chat()`, which
takes a single file path only.
