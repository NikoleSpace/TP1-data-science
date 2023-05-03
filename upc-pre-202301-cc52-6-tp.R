rm(list=ls(all=TRUE))
#CARGANDO LIBRERIAS
library(ggplot2)
library(mlr)  #summary
library(dplyr)


#CARGA DE DATOS
setwd("C:/Users/HP/Downloads")
booking <- read.csv("hotel_bookings.csv", sep = ',', head = TRUE)
#INSPECCIÓN DE DATOS
nrow(booking)
ncol(booking)
names(booking)
str(booking)
head(booking)
view(booking)
summary(booking)

#                   LIMPIEZA DE DATOS
booking$hotel<-as.factor(booking$hotel)
booking$arrival_date_month<-as.factor(booking$arrival_date_month)
booking$meal<-as.factor(booking$meal)
booking$country<-as.factor(booking$country)
booking$market_segment<-as.factor(booking$market_segment)
booking$distribution_channel<-as.factor(booking$distribution_channel)
booking$reserved_room_type<-as.factor(booking$reserved_room_type)
booking$assigned_room_type<-as.factor(booking$assigned_room_type)
booking$deposit_type<-as.factor(booking$deposit_type)
booking$agent<-as.factor(booking$agent)
booking$company<-as.factor(booking$company)
booking$customer_type<-as.factor(booking$customer_type)
booking$reservation_status<-as.factor(booking$reservation_status)
booking$reservation_status_date<-as.factor(booking$reservation_status_date)

booking<-booking%>%
  mutate(country = case_when(
    country=='PRT'~'PORTUGAL',
    country=='GBR'~'REINO UNIDO',
    country=='FRA'~'FRANCIA',
    country=='ESP'~'ESPAÑA',
    country=='DEU'~'ALEMANIA',
    country=='IRL'~'IRLANDA',
    country=='ITA'~'ITALIA',
    country=='BEL'~'BELGICA',
    country=='NLD'~'PAÍSES BAJOS',
    country=='USA'~'EEUU'
  ))

##VISUALIZACIÓN DE CANCELACIONES-HOTELES-TIPODECLIENTES
tipo.clientes<-booking %>%
  select(is_canceled,customer_type,hotel) %>%
  group_by(is_canceled,customer_type,hotel) %>%
  summarise(customer.count=n())

tipo.clientes<-tipo.clientes%>%
  mutate(customer_type = case_when(
    customer_type=='Contract'~ 'Contrato',
    customer_type=='Group'~ 'Grupo',
    customer_type=='Transient'~ 'Transitorio',
    customer_type=='Transient-Party'~ 'Grupo-Transitorio',
  ))

tipo.clientes<-tipo.clientes%>%
  rename(Estado_Reserva=is_canceled)



tipo.clientes$Estado_Reserva<-as.factor(tipo.clientes$Estado_Reserva)

ggplot(data=tipo.clientes, aes(x=customer_type,
                               y=tipo.clientes$customer.count, 
                               fill=Estado_Reserva))+
  geom_histogram(stat = "identity", position = position_dodge(width = 1))+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text=element_text(face = "bold"),
        axis.text.x = element_text(angle = 80, vjust = 0.5, hjust=0.5),
        panel.grid.minor = element_line(colour = "black",linetype = "dotted"),
        panel.background=element_rect(fill = "gray")
  )+
  geom_text(position = position_dodge(1),
            angle=60,vjust=0.8,hjust=0.3,
            aes(label=tipo.clientes$customer.count))+
  facet_wrap(~hotel)+
  labs(title="TIPO DE CLIENTES POR HOTEL")+
  xlab("TIPO DE CLIENTES")+
  ylab("TOTAL DE CLIENTES")+
  scale_fill_discrete(
    name= "Estado de la Reserva",
    breaks=c("0", "1"),
    labels=c("No Cancelado", "Cancelado")
  )

#---------------------------VISUALIZACIÓN POR MES---------------------------------

fechas.consumos<-booking%>%
  select(arrival_date_month,hotel,customer_type)%>%
  group_by(arrival_date_month,hotel,customer_type)%>%
  summarise(Total_Clientes=n())

fechas.consumos<-fechas.consumos%>%
  mutate(arrival_date_month = case_when(
    arrival_date_month=='January'~ 'Enero',
    arrival_date_month=='February'~ 'Febrero',
    arrival_date_month=='March'~ 'Marzo',
    arrival_date_month=='April'~ 'Abril',
    arrival_date_month=='May'~ 'Mayo',
    arrival_date_month=='June'~ 'Junio',
    arrival_date_month=='July'~ 'Julio',
    arrival_date_month=='August'~ 'Agosto',
    arrival_date_month=='September'~ 'Setiembre',
    arrival_date_month=='October'~ 'Octubre',
    arrival_date_month=='November'~ 'Noviembre',
    arrival_date_month=='December'~ 'Diciembre'
  ))

fechas.consumos<-fechas.consumos%>%
  mutate(customer_type = case_when(
    customer_type=='Contract'~ 'Contrato',
    customer_type=='Group'~ 'Grupo',
    customer_type=='Transient'~ 'Transitorio',
    customer_type=='Transient-Party'~ 'Grupo-Transitorio',
  ))


fechas.consumos$arrival_date_month <- factor(fechas.consumos$arrival_date_month, 
                                             levels = c("Enero", "Febrero", "Marzo", "Abril", 
                                                        "Mayo", "Junio", "Julio", "Agosto", 
                                                        "Setiembre", "Octubre", "Noviembre", "Diciembre"))

fechas.consumos$customer_type <- factor(fechas.consumos$customer_type, 
                                        levels = c("Contrato", "Grupo", 
                                                   "Transitorio", "Grupo-Transitorio"))

ggplot(data=fechas.consumos, 
       aes(x=fechas.consumos$arrival_date_month,y=Total_Clientes,fill=customer_type))+
  geom_histogram(stat = "identity", position = position_dodge(width = 1))+
  theme(plot.title = element_text(hjust = 1.0, face = "bold"),
        axis.text=element_text(face = "bold"),
        axis.text.x = element_text(angle = 60, vjust = 1.0, hjust=1),
        panel.grid.minor = element_line(colour = "gray",linetype = "dotted"),
        panel.background=element_rect(fill = "white")
  )+
  facet_wrap(~hotel)+
  xlab("MESES")+
  ylab("TOTAL DE CLIENTES")+
  scale_fill_discrete(
    name="TIPO DE CLIENTE"
  )

#---------------Visualización de Tipo de Habitación por Año------------------

clientes.habitación<-booking%>%
  select(arrival_date_year,assigned_room_type,reserved_room_type)%>%
  group_by(arrival_date_year,assigned_room_type,reserved_room_type)%>%
  summarise(TotalClientes=n())

summary(clientes.habitación)

ggplot(data=clientes.habitación,aes(x=reserved_room_type,y=TotalClientes,
                                    fill=assigned_room_type))+
  geom_histogram(stat = "identity", position = position_dodge(1))+
  facet_wrap(~arrival_date_year)+
  labs(title="TIPO DE RESERVA POR AÑO (2015-2017)")+
  xlab("TIPO DE HABITACIÓN RESERVADA")+
  ylab("CLIENTES TOTALES")+
  scale_fill_discrete(
    name="Tipo de habitación"
  )


#-----------------Visualizacion de paises con más reservas------------------------
reservas_por_pais <- table(subset(booking, is_canceled == 0)$country)

top_10_paises <- head(sort(reservas_por_pais, decreasing = TRUE), 10)
barplot(top_10_paises, 
        col = "lightblue", 
        main = "Top 10 países con más reservas", 
        xlab = "País", 
        ylab = "Cantidad de reservas")

#Visualizacion de cantidad de clientes con hijos/bebes son o no clientes repetidos
viz3 <- booking%>%
  mutate(
    adults_with_children = ifelse((adults > 0 & children > 0) | babies > 0, "Adultos con hijos/bebes", "Adultos sin hijos/bebes")
  ) %>%
  mutate(
    is_repeated_guest = factor(is_repeated_guest, levels = c(0, 1), labels = c("No", "Sí"))
  )

ggplot(viz3, aes(x = adults_with_children, fill = is_repeated_guest)) +
  geom_bar(position = "dodge") +
  labs(title = "Clientes adultos con/sin hijos o bebes y si son repetidos o no",
       x = "Adultos con/sin hijos o bebes",
       y = "Cantidad de clientes") +
  scale_fill_discrete(name = "¿Es cliente repetido?",
                      labels = c("No", "Sí")) +
  theme_minimal()


#Visualizacion de precios de habitaciones por tipo de hotel y por segmentos

adr_por_mes <- aggregate(booking$adr, 
                         by = list(booking$arrival_date_month, 
                                   booking$customer_type, 
                                   booking$hotel), 
                         FUN = mean)
colnames(adr_por_mes) <- c("Mes", "Tipo de Cliente", "Tipo de Hotel", "ADR Promedio")

adr_por_mes<-adr_por_mes%>%
  mutate(Mes = case_when(
    Mes=='January'~ 'Enero',
    Mes=='February'~ 'Febrero',
    Mes=='March'~ 'Marzo',
    Mes=='April'~ 'Abril',
    Mes=='May'~ 'Mayo',
    Mes=='June'~ 'Junio',
    Mes=='July'~ 'Julio',
    Mes=='August'~ 'Agosto',
    Mes=='September'~ 'Setiembre',
    Mes=='October'~ 'Octubre',
    Mes=='November'~ 'Noviembre',
    Mes=='December'~ 'Diciembre'
  ))

adr_por_mes$Mes <- factor(adr_por_mes$Mes, 
                      levels = c("Enero", "Febrero", "Marzo", "Abril", 
                            "Mayo", "Junio", "Julio", "Agosto", 
                            "Setiembre", "Octubre", "Noviembre", "Diciembre"))

ggplot(data = adr_por_mes, 
       aes(x = Mes, y = `ADR Promedio`, fill = `Tipo de Cliente`)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  facet_grid(cols = vars(`Tipo de Hotel`)) +
  labs(title = "ADR promedio por mes, tipo de cliente y tipo de hotel", 
       x = "Mes", 
       y = "ADR Promedio") +
  theme(
    axis.text.x = element_text(angle = 60, vjust = 1.0, hjust=1)
  ) +
  scale_fill_discrete(
    name = "Tipo de Cliente",
    labels=c("Contrato","Grupo","Transitorio","Grupo Transitorio")
    )
