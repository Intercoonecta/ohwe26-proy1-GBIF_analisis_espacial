#cargar paquete
library(satin)

# cargar datos ersst
ersst <- readRDS("./data/ersst/ersst_EC.rds")

# que hay
ersst

# grafico del primer año/mes
plot(ersst)



# grafico de enero de 1970
ersst@period$tmStart[1441]

plot(ersst, period = 1441)
