<a href="http://biadwiki.org/"><img src="https://biadwiki.org/biad.logo.png" alt="BIAD" height="150"/></a>

# BIAD : Big Interdisciplinary Archaeological Database

## Installation

To install this package from GitHub, you can use the `devtools` package. If you do not have `devtools` installed, you can install it first using `install.packages("devtools")`. Then, install the package using one of the following methods:

- To install the latest version directly from GitHub, use:
  ```R
  devtools::install_github("BIADwiki/BIADconnect")
  ```

- To load the package without installing, for testing purposes, use:
  ```R
  devtools::load_all("/path/to/BIADconnect")
  ```

Replace `"/path/to/BIADwiki"` with the actual path to the cloned BIADwiki repository on your local machine.

## Storing Credentials

To  connect to BIAD from R using this package you will need the necessary credentials. For this package to interact with BIAD it assumes your credential are stored as environmnt variable.

The easiest way to do that, if you plan on using these credential only through  R is to put them in a `.Renviron` file.

Create or edit the `.Renviron` file in your home directory to store your database. This file should contain your credentials and be located in a secure location on your machine. **Do not commit the `.Renviron` file to version control systems like Git.**

the variable should be named as follow:

```bash
BIAD_DB_USER='your_database_username'
BIAD_DB_PASS='your_database_password'
BIAD_DB_HOST='hostname'
BIAD_DB_PORT=1234
```

