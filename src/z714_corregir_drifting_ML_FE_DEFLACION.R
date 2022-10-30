#require vm con
#   8 vCPU
#  64 GB  memoria RAM
# 256 GB  espacio en disco


#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")



#Parametros del script
kexperimento  <- "DR7141"

kexp_input  <- "CA7060"

#valores posibles  "ninguno" "rank_simple" , "rank_cero_fijo" , "deflacion"
kmetodo  <- "deflacion"
# FIN Parametros del script


#------------------------------------------------------------------------------
#Esta es la parte que los alumnos deben desplegar todo su ingenio

AgregarVariables  <- function( dataset )
{
  gc()
  #INICIO de la seccion donde se deben hacer cambios con variables nuevas

  #creo un ctr_quarter que tenga en cuenta cuando los clientes hace 3 menos meses que estan
  dataset[  , ctrx_quarter_normalizado := ctrx_quarter ]
  dataset[ cliente_antiguedad==1 , ctrx_quarter_normalizado := ctrx_quarter * 5 ]
  dataset[ cliente_antiguedad==2 , ctrx_quarter_normalizado := ctrx_quarter * 2 ]
  dataset[ cliente_antiguedad==3 , ctrx_quarter_normalizado := ctrx_quarter * 1.2 ]

  #variable extraida de una tesis de maestria de Irlanda
  dataset[  , mpayroll_sobre_edad  := mpayroll / cliente_edad ]

  #se crean los nuevos campos para MasterCard  y Visa, teniendo en cuenta los NA's
  #varias formas de combinar Visa_status y Master_status
  dataset[ , vm_status01       := pmax( Master_status,  Visa_status, na.rm = TRUE) ]
  dataset[ , vm_status02       := Master_status +  Visa_status ]
  dataset[ , vm_status03       := pmax( ifelse( is.na(Master_status), 10, Master_status) , ifelse( is.na(Visa_status), 10, Visa_status) ) ]
  dataset[ , vm_status04       := ifelse( is.na(Master_status), 10, Master_status)  +  ifelse( is.na(Visa_status), 10, Visa_status)  ]
  dataset[ , vm_status05       := ifelse( is.na(Master_status), 10, Master_status)  +  100*ifelse( is.na(Visa_status), 10, Visa_status)  ]

  dataset[ , vm_status06       := ifelse( is.na(Visa_status), 
                                          ifelse( is.na(Master_status), 10, Master_status), 
                                          Visa_status)  ]

  dataset[ , mv_status07       := ifelse( is.na(Master_status), 
                                          ifelse( is.na(Visa_status), 10, Visa_status), 
                                          Master_status)  ]


  #combino MasterCard y Visa
  dataset[ , vm_mfinanciacion_limite := rowSums( cbind( Master_mfinanciacion_limite,  Visa_mfinanciacion_limite) , na.rm=TRUE ) ]

  dataset[ , vm_Fvencimiento         := pmin( Master_Fvencimiento, Visa_Fvencimiento, na.rm = TRUE) ]
  dataset[ , vm_Finiciomora          := pmin( Master_Finiciomora, Visa_Finiciomora, na.rm = TRUE) ]
  dataset[ , vm_msaldototal          := rowSums( cbind( Master_msaldototal,  Visa_msaldototal) , na.rm=TRUE ) ]
  dataset[ , vm_msaldopesos          := rowSums( cbind( Master_msaldopesos,  Visa_msaldopesos) , na.rm=TRUE ) ]
  dataset[ , vm_msaldodolares        := rowSums( cbind( Master_msaldodolares,  Visa_msaldodolares) , na.rm=TRUE ) ]
  dataset[ , vm_mconsumospesos       := rowSums( cbind( Master_mconsumospesos,  Visa_mconsumospesos) , na.rm=TRUE ) ]
  dataset[ , vm_mconsumosdolares     := rowSums( cbind( Master_mconsumosdolares,  Visa_mconsumosdolares) , na.rm=TRUE ) ]
  dataset[ , vm_mlimitecompra        := rowSums( cbind( Master_mlimitecompra,  Visa_mlimitecompra) , na.rm=TRUE ) ]
  dataset[ , vm_madelantopesos       := rowSums( cbind( Master_madelantopesos,  Visa_madelantopesos) , na.rm=TRUE ) ]
  dataset[ , vm_madelantodolares     := rowSums( cbind( Master_madelantodolares,  Visa_madelantodolares) , na.rm=TRUE ) ]
  dataset[ , vm_fultimo_cierre       := pmax( Master_fultimo_cierre, Visa_fultimo_cierre, na.rm = TRUE) ]
  dataset[ , vm_mpagado              := rowSums( cbind( Master_mpagado,  Visa_mpagado) , na.rm=TRUE ) ]
  dataset[ , vm_mpagospesos          := rowSums( cbind( Master_mpagospesos,  Visa_mpagospesos) , na.rm=TRUE ) ]
  dataset[ , vm_mpagosdolares        := rowSums( cbind( Master_mpagosdolares,  Visa_mpagosdolares) , na.rm=TRUE ) ]
  dataset[ , vm_fechaalta            := pmax( Master_fechaalta, Visa_fechaalta, na.rm = TRUE) ]
  dataset[ , vm_mconsumototal        := rowSums( cbind( Master_mconsumototal,  Visa_mconsumototal) , na.rm=TRUE ) ]
  dataset[ , vm_cconsumos            := rowSums( cbind( Master_cconsumos,  Visa_cconsumos) , na.rm=TRUE ) ]
  dataset[ , vm_cadelantosefectivo   := rowSums( cbind( Master_cadelantosefectivo,  Visa_cadelantosefectivo) , na.rm=TRUE ) ]
  dataset[ , vm_mpagominimo          := rowSums( cbind( Master_mpagominimo,  Visa_mpagominimo) , na.rm=TRUE ) ]

  #a partir de aqui juego con la suma de Mastercard y Visa
  dataset[ , vmr_Master_mlimitecompra:= Master_mlimitecompra / vm_mlimitecompra ]
  dataset[ , vmr_Visa_mlimitecompra  := Visa_mlimitecompra / vm_mlimitecompra ]
  dataset[ , vmr_msaldototal         := vm_msaldototal / vm_mlimitecompra ]
  dataset[ , vmr_msaldopesos         := vm_msaldopesos / vm_mlimitecompra ]
  dataset[ , vmr_msaldopesos2        := vm_msaldopesos / vm_msaldototal ]
  dataset[ , vmr_msaldodolares       := vm_msaldodolares / vm_mlimitecompra ]
  dataset[ , vmr_msaldodolares2      := vm_msaldodolares / vm_msaldototal ]
  dataset[ , vmr_mconsumospesos      := vm_mconsumospesos / vm_mlimitecompra ]
  dataset[ , vmr_mconsumosdolares    := vm_mconsumosdolares / vm_mlimitecompra ]
  dataset[ , vmr_madelantopesos      := vm_madelantopesos / vm_mlimitecompra ]
  dataset[ , vmr_madelantodolares    := vm_madelantodolares / vm_mlimitecompra ]
  dataset[ , vmr_mpagado             := vm_mpagado / vm_mlimitecompra ]
  dataset[ , vmr_mpagospesos         := vm_mpagospesos / vm_mlimitecompra ]
  dataset[ , vmr_mpagosdolares       := vm_mpagosdolares / vm_mlimitecompra ]
  dataset[ , vmr_mconsumototal       := vm_mconsumototal  / vm_mlimitecompra ]
  dataset[ , vmr_mpagominimo         := vm_mpagominimo  / vm_mlimitecompra ]

  #Aqui debe usted agregar sus propias nuevas variables
  
  #BENEFICIO
  dataset[,FE_mbeneficios_total:= rowSums(cbind(mcajeros_propios_descuentos,mtarjeta_visa_descuentos,mtarjeta_master_descuentos), na.rm=TRUE) ]
  dataset[,FE_cbeneficios_total:= rowSums(cbind(ccajeros_propios_descuentos,ctarjeta_visa_descuentos,ctarjeta_master_descuentos), na.rm=TRUE) ]
  #COMISIONES
  dataset[,FE_mcomisiones_total:= rowSums(cbind(mcomisiones_mantenimiento,mcomisiones_otras), na.rm=TRUE) ]
  dataset[,FE_ccomisiones_total:= rowSums(cbind(ccomisiones_mantenimiento,ccomisiones_otras), na.rm=TRUE) ]
  #BENEFICIOS/COMISIONES ($)
  dataset[,FE_mbeneficios_comisiones:= ifelse(FE_mcomisiones_total==0,0,(FE_mbeneficios_total/FE_mcomisiones_total) )]
  #INGRESOS/EDAD
  dataset[,FE_ingreso_edad:= ifelse(rowSums(cbind(cpayroll_trx,cpayroll2_trx),mcheques_depositados)==0,1,(rowSums(cbind(cpayroll_trx,cpayroll2_trx,mcheques_depositados)/cliente_edad )))]
  #DESDE CUANDO TIENE TARJETA
  dataset[,FE_tarjera_antifuedad_master:= ((cliente_antiguedad*30)-Master_fechaalta)]
  dataset[,FE_tarjera_antifuedad_visa:= ((cliente_antiguedad*30)-Visa_fechaalta)]
  #ANTIGUEDAD
  dataset[,FE_edad_antiguedad:= ((cliente_edad*12)/cliente_antiguedad) ]
  #PAGO SERVICIOS
  dataset[ , FE_pago_servicios:= rowSums( cbind( mpagodeservicios,  mpagomiscuentas) , na.rm=TRUE ) ]
  #EGRESOS DEBITO AUTOMATICO
  dataset[ , FE_egresos_debito_automatico:= rowSums( cbind( mttarjeta_master_debitos_automaticos,  mttarjeta_master_debitos_automaticos,mcuenta_debitos_automaticos) , na.rm=TRUE ) ]
  #EGRESOS TOTALES
  dataset[ , FE_egresos_totales:= rowSums( cbind(mpagodeservicios,mcomisiones_mantenimiento,mcomisiones_otras,mtransferencias_emitidas,mextraccion_autoservicio,mcheques_emitidos, mttarjeta_master_debitos_automaticos,  mttarjeta_master_debitos_automaticos,mcuenta_debitos_automaticos) , na.rm=TRUE ) ]
  #INGRESOS TOTALES
  dataset[ , FE_ingreso_total_real:= rowSums( cbind(mpayroll,mpayroll2,mcheques_depositados) , na.rm=TRUE ) ]
  dataset[ , FE_ingreso_total_noreal:= rowSums( cbind(mpayroll,mpayroll2,mcheques_depositados,mcheques_depositados_rechazados) , na.rm=TRUE ) ]
  #INGRESOS/COMISIONES
  dataset[,FE_ingreso_comisiones:= ifelse(FE_mcomisiones_total==0,1,(FE_ingreso_total_real/FE_mcomisiones_total))]
  #MONTO CUENTA PREMIUN
  dataset[ , FE_cuenta_premium:= rowSums( cbind(mcuenta_corriente,  mcaja_ahorro) , na.rm=TRUE ) ]
  #LIMITE DE COMPRA
  dataset[ , FE_limite_compra_visa:= ifelse(rowSums(cbind(Master_mconsumospesos,Master_mconsumosdolares,Visa_mconsumospesos,Visa_mconsumosdolares))==0,1,(Visa_mlimitecompra)/(rowSums(cbind(Master_mconsumospesos,Master_mconsumosdolares,Visa_mconsumospesos,Visa_mconsumosdolares))))]
  dataset[ , FE_limite_compra_master:= ifelse(rowSums(cbind(Master_mconsumospesos,Master_mconsumosdolares,Visa_mconsumospesos,Visa_mconsumosdolares))==0,1,(Master_mlimitecompra)/(rowSums(cbind(Master_mconsumospesos,Master_mconsumosdolares,Visa_mconsumospesos,Visa_mconsumosdolares))))]
  #Limite compra tarjeta
  dataset[ , FE_lim_compra_tarjeta:= rowSums( cbind( Master_mlimitecompra,  Visa_mlimitecompra) , na.rm=TRUE ) ]
  
  #IRSE DEL PAIS?
  dataset[ , FE_irse_del_pais:= rowSums( cbind(cforex_buy,  mtransferencias_emitidas) , na.rm=TRUE ) ]
  #PRESTAMOS
  dataset[ , FE_prestamos_cantidad:= rowSums( cbind(cprestamos_prendarios,  cprestamos_hipotecarios,cprestamos_personales) , na.rm=TRUE ) ]
  dataset[ , FE_prestamos_monto:= rowSums( cbind(mprestamos_prendarios,  mprestamos_hipotecarios,mprestamos_personales) , na.rm=TRUE ) ]
  #ADHESION
  dataset[ , FE_adhesion_servicios:= rowSums( cbind(tcallcenter,ccaja_seguridad) , na.rm=TRUE ) ]
  #CONSIDERACIONES DE CERRAR CUENTA (cero cerraria la cuenta)
  dataset[ , FE_cierre_total:= ifelse(Master_status>5 & Visa_status>5 & mpayroll==0 & mtransferencias_emitidas>mtransferencias_recibidas&cplazo_fijo==0 & cinversion1==0 & cinversion2==0,0, 1 ) ]
  dataset[ , FE_cierre_parcial:= ifelse(Master_status>5 & Visa_status>5 & mpayroll==0 & mtransferencias_emitidas>mtransferencias_recibidas,0, 1 ) ]
  #PRODUCTOS TOTALES A BINARIA (cero cerraria la cuenta)
  dataset[ , FE_cierre_productos_total:= ifelse(cprestamos_personales==0 & cprestamos_prendarios==0 & mpayroll==0 & cprestamos_hipotecarios==0 & cplazo_fijo==0 & cinversion1==0 & cinversion2 ==0 & cseguro_vida == 0 & cseguro_auto==0 & cseguro_vivienda==0 & cseguro_accidentes_personales==0 ,0, 1 ) ]
  #BALANCE CLIENTE
  dataset[ , FF_balance_cliente:=(FE_ingreso_total_real-FE_egresos_totales)]
  #PAGO MINIMO
  dataset[ , FE_pago_min_VISA:=ifelse(FE_ingreso_total_real==0, 0,(Visa_mpagominimo/FE_ingreso_total_real))]
  dataset[ , FE_pago_min_MASTER:=ifelse(FE_ingreso_total_real==0,0,(Master_mpagominimo/FE_ingreso_total_real))]
  dataset[ , FE_pago_min_total:=ifelse(FE_ingreso_total_real==0,0, rowSums(cbind(Visa_mpagominimo+Master_mpagominimo))/FE_ingreso_total_real)]
  #Arraigo cliente
  dataset[ , FE_arraigo_cliente:= rowSums( cbind( tcuentas,  ccuenta_corriente, ccaja_ahorro, ctarjeta_debito, ctarjeta_visa, ctarjeta_master) , na.rm=TRUE ) ]
  #SEGUROS
  dataset[ , FE_seguros:= rowSums( cbind( cseguro_auto,  cseguro_vivienda, cseguro_accidentes_personales) , na.rm=TRUE ) ]
  #Edad vs. prestamos
  dataset[,FE_edad_deuda:= ifelse(FE_prestamos_monto==0,0,(cliente_edad/FE_prestamos_monto) )]
  #saldo vs consumo con tarjeta
  dataset[ , FE_saldo_consumo_tarjeta:= ifelse(rowSums( cbind( mtarjeta_visa_consumo,  mtarjeta_master_consumo))==0,10,( mcuentas_saldo)/rowSums( cbind( mtarjeta_visa_consumo,  mtarjeta_master_consumo) , na.rm=TRUE ) )]
  #adelantos en efvo. con tarjeta
  dataset[ , FE_adelantos_efvo_tarjeta:= rowSums( cbind( Master_cadelantosefectivo,  Visa_cadelantosefectivo) , na.rm=TRUE ) ]
  #Adelantos efvo vs cheques rechazados
  dataset[,FE_edad_deuda:= ifelse(FE_adelantos_efvo_tarjeta==0,0,(ccheques_depositados_rechazados/FE_adelantos_efvo_tarjeta) )]
  #actividad voluntaria/cantidad
  dataset[,FE_quarter:=ifelse(ctrx_quarter==0,0,(active_quarter/ctrx_quarter))]
  
  

  #valvula de seguridad para evitar valores infinitos
  #paso los infinitos a NULOS
  infinitos      <- lapply(names(dataset),function(.name) dataset[ , sum(is.infinite(get(.name)))])
  infinitos_qty  <- sum( unlist( infinitos) )
  if( infinitos_qty > 0 )
  {
    cat( "ATENCION, hay", infinitos_qty, "valores infinitos en tu dataset. Seran pasados a NA\n" )
    dataset[mapply(is.infinite, dataset)] <<- NA
  }


  #valvula de seguridad para evitar valores NaN  que es 0/0
  #paso los NaN a 0 , decision polemica si las hay
  #se invita a asignar un valor razonable segun la semantica del campo creado
  nans      <- lapply(names(dataset),function(.name) dataset[ , sum(is.nan(get(.name)))])
  nans_qty  <- sum( unlist( nans) )
  if( nans_qty > 0 )
  {
    cat( "ATENCION, hay", nans_qty, "valores NaN 0/0 en tu dataset. Seran pasados arbitrariamente a 0\n" )
    cat( "Si no te gusta la decision, modifica a gusto el programa!\n\n")
    dataset[mapply(is.nan, dataset)] <<- 0
  }

}
#------------------------------------------------------------------------------
#deflaciona por IPC
#momento 1.0  31-dic-2020 a las 23:59

drift_deflacion  <- function( campos_monetarios )
{
  vfoto_mes <- c( 201901, 201902, 201903, 201904, 201905, 201906,
                  201907, 201908, 201909, 201910, 201911, 201912,
                  202001, 202002, 202003, 202004, 202005, 202006,
                  202007, 202008, 202009, 202010, 202011, 202012,
                  202101, 202102, 202103, 202104, 202105 )

  vIPC  <- c( 1.9903030878, 1.9174403544, 1.8296186587,
              1.7728862972, 1.7212488323, 1.6776304408,
              1.6431248196, 1.5814483345, 1.4947526791,
              1.4484037589, 1.3913580777, 1.3404220402,
              1.3154288912, 1.2921698342, 1.2472681797,
              1.2300475145, 1.2118694724, 1.1881073259,
              1.1693969743, 1.1375456949, 1.1065619600,
              1.0681100000, 1.0370000000, 1.0000000000,
              0.9680542110, 0.9344152616, 0.8882274350,
              0.8532444140, 0.8251880213 )

  tb_IPC  <- data.table( "foto_mes"= vfoto_mes,
                         "IPC" = vIPC )

  dataset[ tb_IPC,
           on= c("foto_mes"),
           (campos_monetarios) :=  .SD * i.IPC ,
           .SDcols = campos_monetarios ]

}

#------------------------------------------------------------------------------

drift_rank_simple  <- function( campos_drift )
{
  for( campo in campos_drift )
  {
    cat( campo, " " )
    dataset[ , paste0(campo,"_rank") :=  (frank( get(campo), ties.method="random") - 1) / ( .N -1 ), by= foto_mes]
    dataset[ , (campo) := NULL ]
  }
}
#------------------------------------------------------------------------------
#El cero se transforma en cero
#los positivos se rankean por su lado
#los negativos se rankean por su lado

drift_rank_cero_fijo  <- function( campos_drift )
{
  for( campo in campos_drift )
  {
    cat( campo, " " )
    dataset[ get(campo) ==0, paste0(campo,"_rank") := 0 ]
    dataset[ get(campo) > 0, paste0(campo,"_rank") :=   frank(  get(campo), ties.method="random")  / .N, by= foto_mes ]
    dataset[ get(campo) < 0, paste0(campo,"_rank") :=  -frank( -get(campo), ties.method="random")  / .N, by= foto_mes ]
    dataset[ , (campo) := NULL ]
  }
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Aqui comienza el programa

setwd("~/buckets/b1")

#cargo el dataset donde voy a entrenar
#esta en la carpeta del exp_input y siempre se llama  dataset.csv.gz
dataset_input  <- paste0( "./exp/", kexp_input, "/dataset_ML_FE.csv.gzX" )
dataset  <- fread( dataset_input )

#creo la carpeta donde va el experimento
dir.create( paste0( "./exp/", kexperimento, "/"), showWarnings = FALSE )
setwd(paste0( "./exp/", kexperimento, "/"))   #Establezco el Working Directory DEL EXPERIMENTO



#primero agrego las variables manuales
AgregarVariables( dataset )

setorder( dataset, foto_mes, numero_de_cliente )

#por como armé los nombres de campos, estos son los campos que expresan variables monetarias
campos_monetarios  <- colnames(dataset)
campos_monetarios  <- campos_monetarios[campos_monetarios %like% "^(m|Visa_m|Master_m|vm_m)"]

#aqui aplico un metodo para atacar el data drifting
#hay que probar experimentalmente cual funciona mejor
switch( 
kmetodo,
  "ninguno"        = cat( "No hay correccion del data drifting" ),
  "rank_simple"    = drift_rank_simple( campos_monetarios ),
  "rank_cero_fijo" = drift_rank_cero_fijo( campos_monetarios ),
  "deflacion"      = drift_deflacion( campos_monetarios ) 
)



fwrite( dataset,
        file="dataset_ML_FE_DF.csv.gz",
        sep= "," )
