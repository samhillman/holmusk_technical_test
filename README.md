# Holmusk R Project

This repository contains the R code and analysis for part of a technical test for Holmusk.

## Getting Started

### Prerequisites

To work with this project, you will need R installed on your system (version 4.1.1 or higher). 

### Installation

The project uses `renv` for dependency management. This ensures that you have the exact versions of R packages that were used in the development of the project, making the analysis reproducible.

To initiate the project with `renv`, follow these steps:

1. Clone the repository to your local machine:

    ```bash
    git clone https://github.com/samhillman/holmusk_technical_test
    ```

2. Navigate to the project directory:

    ```bash
    cd holmusk_r_project_technical_test
    ```

3. Open the project in RStudio by clicking on the `holmusk_technical_test.Rproj` file.

4. Once the project is open in RStudio, the `renv` package should automatically recognize the `renv.lock` file in the project directory. If `renv` is not yet installed, install it using the following command in the R console:

    ```r
    install.packages("renv")
    ```

5. To restore the project library, run:

    ```r
    renv::restore()
    ```

   This will install all the necessary packages as specified in the `renv.lock` file.

### Structure

The project is structured as follows:

- `/src`: This directory contains the R scripts for analysis.
- `/data`: This directory contains the datasets used in the analysis.
- `/output`: This directory will hold any output from the scripts, such as figures and processed data files.
- `/documents`: Any additional documentation related to the project.
- `renv.lock`: A file containing a snapshot of the project's R dependencies.


