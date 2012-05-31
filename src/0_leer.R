library('ProjectTemplate')
load.project()

datos <- X31mayo
datos$Error <- as.numeric(gsub("[^0-9\\.]", "", datos$Error))
datos$del <- as.Date(datos$del, "%d/%m/%y")
datos$al <- as.Date(datos$al, "%d/%m/%y")
datos$ID <- 1:nrow(datos)
datos.aux <- ddply(datos, "ID", function(sub){
    data.frame(fecha = seq(from = sub$del, to = sub$al, by = 1),
    sub[,!names(sub) %in% c("del", "al")], row.names = NULL)
})

datos.1 <- melt(datos.aux[,
    c("fecha", "Encuesta", "Muestra", "EPN", "JVM", "AMLO", "GQ", "NR")],
    id.vars = c("fecha", "Encuesta", "Muestra"))
datos.1[datos.1$value == "ND", 'value'] <- NA
datos.1$value <- as.numeric(datos.1$value)
datos.1$Muestra <- as.numeric(datos.1$Muestra)    

datos.1 <- ddply(datos.1, c("fecha", "variable"), transform, muestra.tot = sum(Muestra, na.rm = T))
datos.1$w <- datos.1$Muestra / datos.1$muestra.tot

datos.2 <- ddply(datos.1, c("fecha", "variable"), summarise,
    prom = sum(value * w, na.rm = T),
    prom.mod = sum(value * w, na.rm = T))

datos.3 <- ddply(datos.2, "fecha", function(sub){
    sub[sub$variable == "AMLO", 'prom.mod'] <- sub[sub$variable == "AMLO", 'prom.mod'] + 
        sub[sub$variable == "NR", 'prom.mod'] * 1/2
    sub[sub$variable == "JVM", 'prom.mod'] <- sub[sub$variable == "JVM", 'prom.mod'] + 
        sub[sub$variable == "NR", 'prom.mod'] * 1/2
    sub[sub$variable == "NR", 'prom.mod'] <- sub[sub$variable == "NR", 'prom.mod'] * 0
    sub
})
names(datos.3)[names(datos.3)== "variable"] <- "candidato"
datos.4 <- melt(datos.3[datos.3$fecha > "2012-03-01",], id.vars = c("fecha", "candidato"))
ult.ant <- as.Date("2012-05-16")
debate <- as.Date("2012-05-06")
ibero <- as.Date("2012-05-11")
ggplot(datos.4[datos.4$variable == "prom", ], aes(x = fecha, y = value, colour = candidato, group = candidato)) + 
    geom_line() + facet_wrap(~variable) + geom_vline(xintercept = as.numeric(ibero))

