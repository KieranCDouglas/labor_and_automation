library(arrow)

consulados <- read_parquet("~/Library/CloudStorage/Dropbox/parquet/consulados_edomex.parquet")
edos_usa   <- read_parquet("~/Library/CloudStorage/Dropbox/parquet/edos_usa_edomex.parquet")
entidad_mx <- read_parquet("~/Library/CloudStorage/Dropbox/parquet/entidad_mx_edousa.parquet")

print(consulados)
print(edos_usa)
print(entidad_mx)
