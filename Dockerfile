FROM r-base

WORKDIR /src

RUN R -e "install.packages(c('dplyr', 'e1071', 'caret', 'rpart', 'taRifx', 'stringr', 'rpart.plot'),
                           dependencies = TRUE, 
                           repos = 'http://cran.rstudio.com/')"

CMD ["Rscript", "DataModeler.R"]

