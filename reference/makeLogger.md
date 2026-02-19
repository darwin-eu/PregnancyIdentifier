# Create a new logger

Create a new logger

## Usage

``` r
makeLogger(outputDir, outputLogToConsole = TRUE)
```

## Arguments

- outputDir:

  The directory where the log should be created

- outputLogToConsole:

  (\`logical(1)\`) If \`TRUE\` (default), log messages are written to
  the console as well as the log file. If \`FALSE\`, only the file
  appender is used (useful in tests to keep output clean).

## Value

A log4r logger object
