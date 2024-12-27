# well-plate-assistant
Shiny app for automatic parsing and formatting of 96 well plate data from Infinite 200 Series microplate reader from TECAN.

## Running the app

Deployed in shinyapps.io through the following link:  https://well-plate-assistant.shinyapps.io/well-plate-assistant/

## Features

### About the input data format

The app was tested for files exported from a TECAN M200 Pro, using the matrix or `FluorStyle` export format, named *block-shaped*. That means that the data was exported in the following format:

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

**IMPORTANT**: in this version, optical density and fluorescent data should be uploaded as different files. See `raw_data/dummy/od_dummy` and `raw_data/dummy/flu_dummy` for an example.

### About data extraction

The app generates 4 matrixes that capture the information of the 96 well plate. 

* `od_matrix` contains the optical density information. It´s in the *wide-shape* format
* `flu_matrix` contains the fluorescence information; also in *wide-shape* format
* `design_matrix` contains information about the experimental design. Basically, to which condition (e.g concentration of inducer) each well is associated
* `blank_matrix` contains the well coordinates of the blank(s), and to which data wells are they associated
