FROM xnonr/rbase:0.0.1

WORKDIR /src

# RUN R -e "install.packages(c('dplyr', 'e1071', 'caret', 'rpart', 'taRifx', 'stringr', 'rpart.plot'), \
#                            dependencies = TRUE, \
#                            repos = 'http://cran.rstudio.com/')"

COPY src/ .

ENV auto='FALSE'
ENV csv='Telco_Customer_Churn.csv'

CMD ["Rscript", "DataModeler.R", ${auto}, ${csv}]

