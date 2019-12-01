library(knitr)
library(markdown)

knit('report.Rmd', 'README.md',  encoding = 'UTF-8')
markdownToHTML("README.md", "report.html",  encoding = 'UTF-8')
