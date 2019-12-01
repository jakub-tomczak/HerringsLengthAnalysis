library(knitr)
library(markdown)

knit('report.Rmd', 'README.md')
markdownToHTML("README.md", "report.html")
