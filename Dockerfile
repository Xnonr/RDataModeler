FROM xnonr/rbase:0.0.2

WORKDIR /src

COPY src/ .

ENV auto='FALSE'
ENV csv='Telco_Customer_Churn.csv'

CMD ["sh", "-c", "Rscript DataModeler.R ${auto} ${csv}"]
