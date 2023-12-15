# well-plate-assistant
Shiny app for automatic parsing and formatting of 96 well plate data

## Running the app

You can run the app remotely using the following [code](https://github.com/rstudio/shiny_example) in a simple R script.

```
library(shiny)

# Easiest way is to use runGitHub
runGitHub("well-plate-assistant", "frank-britto")

# Run a tar or zip file directly
#runUrl("https://github.com/frank-britto/well-plate-assistant/archive/master.tar.gz")
runUrl("https://github.com/frank-britto/well-plate-assistant/archive/master.zip")

```
Alternatively, clone the repository and execute `runApp()`. 

## Features

### About the input data format

The app was tested for files exported from a TECAN M200 Pro, using the matrix or `FluorStyle` export tool, named *block-shaped*. That means that the data was exported in the following format:

| time | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 |
|---|:---:|---:|---|---|---|---|---|---|---|---|---|---|
| A |  |  |  |  |  |  |  |  |  |  |  |  |
| B |  |  |  |  |  |  |  |  |  |  |  |  |
| C |  |  |  |  |  |  |  |  |  |  |  |  |
| D |  |  |  |  |  |  |  |  |  |  |  |  |
| E |  |  |  |  |  |  |  |  |  |  |  |  |
| F |  |  |  |  |  |  |  |  |  |  |  |  |
| G |  |  |  |  |  |  |  |  |  |  |  |  |
| H |  |  |  |  |  |  |  |  |  |  |  |  |

repeated for each measurement. Another supported file format is the following, called *wide-shaped*.

| time | A1 | A2 | A3 | A4 | A5 | ... | H7 | H8 | H9 | H10 | H11 | H12 |
|---|:---:|---:|---|---|---|---|---|---|---|---|---|---|
|  |  |  |  |  |  |  |  |  |  |  |  |  |
|  |  |  |  |  |  |  |  |  |  |  |  |  |
|  |  |  |  |  |  |  |  |  |  |  |  |  |
|  |  |  |  |  |  |  |  |  |  |  |  |  |
|  |  |  |  |  |  |  |  |  |  |  |  |  |
|  |  |  |  |  |  |  |  |  |  |  |  |  |
|  |  |  |  |  |  |  |  |  |  |  |  |  |
|  |  |  |  |  |  |  |  |  |  |  |  |  |

Actually, any input file is converted into this last *wide-shaped* format before further processing.

### About data extraction
