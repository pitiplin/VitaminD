getTextData <- function(path="../TEXT") {

	files<-list.files(path)
	
	Mapa<-vector("list",length(files))
	
	for (indexFiles in 1:length(files)){
	
		filename<-paste0(path, "/",files[indexFiles])
		
		out <- tryCatch(
			{		
				f <- readLines(filename)
			
				NombreDelPaciente<-as.character(gsub("^\\s+|\\s+$", "", read.table(textConnection(grep("Nombre del paciente:",f,value=TRUE)),sep=":")[[2]]))
				DireccionDelPaciente<-as.character(gsub("^\\s+|\\s+$", "", read.table(textConnection(grep("Dirección:",f,value=TRUE)[[1]]),sep=":")[[2]]))
				Ciudad<-as.character(gsub("^\\s+|\\s+$", "", read.table(textConnection(grep("Ciudad:",f,value=TRUE)),sep=":")[[2]]))
				EstadoProvincia<-as.factor(gsub("^\\s+|\\s+$", "", read.table(textConnection(grep("Estado/Provincia:",f,value=TRUE)),sep=":")[[2]]))
				CodigoPostal<-as.numeric(read.table(textConnection(grep("Código postal:",f,value=TRUE)),sep=":")[[2]])
			
				cline <- grep("I.D. del paciente:",f,value=TRUE)
				
				if (as.character(read.table(textConnection(as.character(read.table(textConnection(cline),sep=":")[[2]])))[[1]])=="Edad"){
					IDDelPaciente<-as.numeric(NA)
				}
				else{
					IDDelPaciente<-as.numeric(read.table(textConnection(as.character(read.table(textConnection(cline),sep=":")[[2]])))[[1]])
				}
				
				Edad<-as.numeric(read.table(textConnection(cline),sep=":")[[3]])
				
				cline <- grep("Número de identidad:",f,value=TRUE)
				
				if (as.character(read.table(textConnection(as.character(read.table(textConnection(cline),sep=":")[[2]])))[[1]])=="Sexo"){
					NumeroDeIdentidad<-as.numeric(NA)
				}
				else{
					NumeroDeIdentidad<-as.numeric(read.table(textConnection(as.character(read.table(textConnection(cline),sep=":")[[2]])))[[1]])
				}
				
				Sexo<-as.factor(read.table(textConnection(cline),sep=":")[[3]])
				
				cline <- grep("Compañía aseguradora:",f,value=TRUE)
				
				if (as.character(read.table(textConnection(as.character(read.table(textConnection(cline),sep=":")[[2]])))[[1]])=="Estatura"){
					CompañiaAseguradora<-as.character(NA)
				}
				else{
					CompañiaAseguradora<-as.character((gsub("^\\s+|\\s+$", "", read.table(textConnection(as.character(read.table(textConnection(cline),sep=":")[[2]])))[[1]])))
				}
				
				Estatura<-as.numeric(read.table(textConnection(cline),sep=":")[[3]])
				
				Peso<-as.numeric(read.table(textConnection(grep("Peso:",f,value=TRUE)),sep=":")[[2]])
				
				NumeroDeLaMedicion<-as.numeric(read.table(textConnection(grep("Núm. de la medición:",f,value=TRUE)),sep=":")[[2]])
				MotivoDeLaPrueba<-as.character(gsub("^\\s+|\\s+$", "", read.table(textConnection(grep("Motivo de la prueba:",f,value=TRUE)),sep=":")[[2]]))
				
				cline <- grep("Medicamentos:",f,value=TRUE)
				
				if (as.character(read.table(textConnection(as.character(read.table(textConnection(cline),sep=":")[[2]])))[[1]])=="Dosis"){
					Medicamentos<-as.character(NA)
				}
				else{
					Medicamentos<-as.character((gsub("^\\s+|\\s+$", "", read.table(textConnection(as.character(read.table(textConnection(cline),sep=":")[[2]])))[[1]])))
				}
				
				if (as.character(read.table(textConnection(as.character(read.table(textConnection(cline),sep=":")[[3]])))[[1]])=="Hora"){
					Dosis<-as.character(NA)
				}
				else{
					Dosis<-as.character((gsub("^\\s+|\\s+$", "", read.table(textConnection(as.character(read.table(textConnection(cline),sep=":")[[3]])))[[1]])))
				}
				
				Hora<-as.character(read.table(textConnection(cline),sep=":")[[4]])  #chequear formato de la hora
				
				InformacionDelPaciente<-list(NombreDelPaciente,DireccionDelPaciente,Ciudad,EstadoProvincia,CodigoPostal,IDDelPaciente,Edad,NumeroDeIdentidad,Sexo,CompañiaAseguradora,Estatura,Peso,NumeroDeLaMedicion,MotivoDeLaPrueba,Medicamentos,Dosis,Hora)
				names(InformacionDelPaciente) <- c("NombreDelPaciente","Direccion","Ciudad","EstadoProvincia","CodigoPostal","IDDelPaciente","Edad","NumeroDeIdentidad","Sexo","CompañiaAseguradora","Estatura","Peso","NumeroDeLaMedicion","MotivoDeLaPrueba","Medicamentos","Dosis","Hora")
				
				Medico<-as.character(gsub("^\\s+|\\s+$", "", read.table(textConnection(grep("Médico:",f,value=TRUE)),sep=":")[[2]]))
				DireccionDelMedico<-as.character(gsub("^\\s+|\\s+$", "", read.table(textConnection(grep("Dirección:",f,value=TRUE)[[2]]),sep=":")[[2]]))
				Telefono<-as.character(gsub("^\\s+|\\s+$", "", read.table(textConnection(grep("Teléfono:",f,value=TRUE)),sep=":")[[2]]))
				
				InformacionDelMedico<-list(Medico,DireccionDelMedico,Telefono)
				names(InformacionDelMedico) <- c("Medico","Direccion","Telefono")
				
				library(chron)
				
				cline <- grep("Fecha de inicio:",f,value=TRUE)
				
				if (as.character(read.table(textConnection(as.character(read.table(textConnection(cline),sep=":")[[2]])))[[1]])=="Duración total"){
					FechaDeInicio<-as.character(NA)
				}
				else{
					FechaDeInicio<-as.character((gsub("^\\s+|\\s+$", "", read.table(textConnection(as.character(read.table(textConnection(cline),sep=":")[[2]])))[[1]])))
				}
				
				cline <- grep("Hora de inicio:",f,value=TRUE)
				
				if (as.character(read.table(textConnection(as.character(read.table(textConnection(cline),sep=":")[[3]])))[[1]])=="Lecturas satisfactorias"){
					HoraDeInicio<-as.character(NA)
				}
				else{
					HoraDeInicio<-as.character(paste0(read.table(textConnection(cline),sep=":")[[2]], ":",read.table(textConnection(as.character(read.table(textConnection(cline),sep=":")[[3]])))[[1]],":00"))
				}
				
				FechaDeInicio <- chron(FechaDeInicio,format=c(dates="d/m/Y", times="H:M:S"), times=HoraDeInicio)
				
				if (as.character(read.table(textConnection(as.character(read.table(textConnection(cline),sep=":")[[3]])))[[1]])=="Lecturas satisfactorias"){
					LecturasSatisfactorias<-as.numeric(read.table(textConnection(cline),sep=":")[[3]])
				}
				else{
					LecturasSatisfactorias<-as.numeric(read.table(textConnection(cline),sep=":")[[4]])
				}
				
				cline <- grep("Fecha de cese:",f,value=TRUE)
				
				if (as.character(read.table(textConnection(as.character(read.table(textConnection(cline),sep=":")[[2]])))[[1]])=="Porcentaje satisfactorio"){
					FechaDeCese<-as.character(NA)
				}
				else{
					FechaDeCese<-as.character((gsub("^\\s+|\\s+$", "", read.table(textConnection(as.character(read.table(textConnection(cline),sep=":")[[2]])))[[1]])))
				}
				
				PorcentajeSatisfactorio<-as.numeric((gsub("^\\s+|\\s+$|%", "", read.table(textConnection(cline),sep=":")[[3]])))
				
				cline <- grep("Hora de cese:",f,value=TRUE)
				HoraDeCese<-as.character(paste0(read.table(textConnection(cline),sep=":")[[2]],":",read.table(textConnection(cline),sep=":")[[3]],":00"))
				FechaDeCese <- chron(FechaDeCese,format=c(dates="d/m/Y", times="H:M:S"), times=HoraDeCese)
				
				DatosTecnicos<-list(FechaDeInicio,LecturasSatisfactorias,FechaDeCese,PorcentajeSatisfactorio)
				names(DatosTecnicos) <- c("FechaDeInicio","LecturasSatisfactorias","FechaDeCese","PorcentajeSatisfactorio")
				
				Comentarios<-as.character(gsub("^\\s+|\\s+$", "", read.table(textConnection(grep("Comentarios:",f,value=TRUE)),sep=":")[[2]]))
				
				headerLength<-4
				DatosSinProcesardePAAText<-character()
				DatosSinProcesardePAATextAux<-f
				
				while(length(grep("Datos sin procesar de PAA",DatosSinProcesardePAATextAux,value=F,fixed=T))>0){
					DatosSinProcesardePAATextAux<-DatosSinProcesardePAATextAux[(grep("Datos sin procesar de PAA",DatosSinProcesardePAATextAux,value=F,fixed=T)[[1]]+headerLength):length(DatosSinProcesardePAATextAux)]
					DatosSinProcesardePAAText<-c(DatosSinProcesardePAAText, DatosSinProcesardePAATextAux[1:(match("",DatosSinProcesardePAATextAux)-1)])
				}
				
				if (length(DatosSinProcesardePAAText)>0){
				
					mydata = read.table(textConnection(DatosSinProcesardePAAText), blank.lines.skip=TRUE, col.names = c("Lectura","DiaYHora","Sistolica","Diastolica","PAM","FrecuenciaCardiaca","CodigoEvento","EstadoCorreccion","ActividadEnDiario" ,"AUX"), skip=3, fill=TRUE, stringsAsFactors=FALSE, na.strings="")
					
					lectura <- as.character(mydata[["Lectura"]])
					
					for (i in 1:nrow(mydata)){    
						if (mydata[i,2] == "R" || mydata[i,2] == "M"){            
							lectura[i]<-paste0(lectura[i], " ", mydata[i,2])
							
							for (j in 2:(ncol(mydata)-1)){                    
								mydata[i,j] <- mydata[i,j+1]
							}            
						}	
					}
				
				  diayhora <- strsplit(mydata[["DiaYHora"]], "-")
				  
				  dia <- numeric()
				  hora <- character()
				  
					for (i in 1:nrow(mydata)){    
						dia <- as.numeric(c(dia, diayhora[[i]][1]))
						hora <- c(hora, paste0(diayhora[[i]][2], ":00"))    
					}
				
				  hora <- chron(times=hora)
				  
				  mydata[["EstadoCorreccion"]] <- as.factor(mydata[["EstadoCorreccion"]])
				  mydata[["ActividadEnDiario"]] <- as.factor(mydata[["ActividadEnDiario"]])
				  
				  DatosSinProcesardePAA = data.frame(lectura,dia,hora,mydata[["Sistolica"]],mydata[["Diastolica"]],mydata[["PAM"]],mydata[["FrecuenciaCardiaca"]],mydata[["CodigoEvento"]],mydata[["EstadoCorreccion"]],mydata[["ActividadEnDiario"]]) 
				  colnames(DatosSinProcesardePAA) <- c("Lectura","Dia","Hora","Sistolica","Diastolica","PAM","FrecuenciaCardiaca","CodigoEvento","EstadoCorreccion","ActividadEnDiario")

				  cond1<-hora>"06:00:00"
					cond2<-hora<"23:00:00"
					diurna<-cond1&cond2

					sistolica<-as.numeric(as.character(mydata[["Sistolica"]]))
					sistolicaDiurna<-sistolica[diurna]
					sistolicaDiurna<-sistolicaDiurna[sistolicaDiurna>0]
					sistolicaNocturna<-sistolica[!diurna]
					sistolicaNocturna<-sistolicaNocturna[sistolicaNocturna>0]
					sistolica<-sistolica[sistolica>0]

					diastolica<-as.numeric(as.character(mydata[["Diastolica"]]))
					diastolicaDiurna<-diastolica[diurna]
					diastolicaDiurna<-diastolicaDiurna[diastolicaDiurna>0]
					diastolicaNocturna<-diastolica[!diurna]
					diastolicaNocturna<-diastolicaNocturna[diastolicaNocturna>0]
					diastolica<-diastolica[diastolica>0]

					PAM<-as.numeric(as.character(mydata[["PAM"]]))
					PAMDiurna<-PAM[diurna]
					PAMDiurna<-PAMDiurna[PAMDiurna>0]
					PAMNocturna<-PAM[!diurna]
					PAMNocturna<-PAMNocturna[PAMNocturna>0]
					PAM<-PAM[PAM>0]

					frecuenciaCardiaca<-as.numeric(as.character(mydata[["FrecuenciaCardiaca"]]))
					frecuenciaCardiacaDiurna<-frecuenciaCardiaca[diurna]
					frecuenciaCardiacaDiurna<-frecuenciaCardiacaDiurna[frecuenciaCardiacaDiurna>0]
					frecuenciaCardiacaNocturna<-frecuenciaCardiaca[!diurna]
					frecuenciaCardiacaNocturna<-frecuenciaCardiacaNocturna[frecuenciaCardiacaNocturna>0]
					frecuenciaCardiaca<-frecuenciaCardiaca[frecuenciaCardiaca>0]
					
					ARVSistolica<-arv(sistolica)
					ARVSistolicaDiurna<-arv(sistolicaDiurna)
					ARVSistolicaNocturna<-arv(sistolicaNocturna)
					
					ARVDiastolica<-arv(diastolica)
					ARVDiastolicaDiurna<-arv(diastolicaDiurna)
					ARVDiastolicaNocturna<-arv(diastolicaNocturna)
					
					ARVPAM<-arv(PAM)
					ARVPAMDiurna<-arv(PAMDiurna)
					ARVPAMNocturna<-arv(PAMNocturna)
					
					ARVFrecuenciaCardiaca<-arv(frecuenciaCardiaca)
					ARVFrecuenciaCardiacaDiurna<-arv(frecuenciaCardiacaDiurna)
					ARVFrecuenciaCardiacaNocturna<-arv(frecuenciaCardiacaNocturna)
					
					ARV<-c(ARVSistolica,ARVSistolicaDiurna,ARVSistolicaNocturna,ARVDiastolica,ARVDiastolicaDiurna,ARVDiastolicaNocturna,ARVPAM,ARVPAMDiurna,ARVPAMNocturna,ARVFrecuenciaCardiaca,ARVFrecuenciaCardiacaDiurna,ARVFrecuenciaCardiacaNocturna)
					names(ARV)<-c("Sistolica","SistolicaDiurna","SistolicaNocturna","Diastolica","DiastolicaDiurna","DiastolicaNocturna","PAM","PAMDiurna","PAMNocturna","FrecuenciaCardiaca","FrecuenciaCardiacaDiurna","FrecuenciaCardiacaNocturna")
				  
				  Tabla<-list(InformacionDelPaciente,InformacionDelMedico,DatosTecnicos,Comentarios,DatosSinProcesardePAA,ARV)
				  names(Tabla) <- c("InformacionDelPaciente","InformacionDelMedico","DatosTecnicos","Comentarios","DatosSinProcesardePAA","ARV")
				  
				  Mapa[[indexFiles]]<-Tabla
				  
				  print(paste0(indexFiles, " - ",  filename, " - PROCESS OK"))
				}
			}	,
			error=function(cond){
				print(paste0(indexFiles, " - ",  filename, " - PROCESS FAILED"))
			}
		)		
	}
	
	Mapa[sapply(Mapa, is.null)] <- NULL
	
	Mapa
}

arv <- function(x = vector()) {

	ARV<-0
	
	for (i in 1:(length(x)-1)){
		ARV<-ARV+abs(x[i+1]-x[i])
	}
	
	ARV<-ARV/(length(x)-1)
	
	ARV
}

convertDataToDataFrame <- function(data = list()) {

	n <- length(data)

	IDDelPaciente <- numeric(n)
	ARVSistolica <- numeric(n)
	ARVSistolicaDiurna <- numeric(n)
	ARVSistolicaNocturna <- numeric(n)
	ARVDiastolica <- numeric(n)
	ARVDiastolicaDiurna <- numeric(n)
	ARVDiastolicaNocturna <- numeric(n)
	ARVPAM <- numeric(n)
	ARVPAMDiurna <- numeric(n)
	ARVPAMNocturna <- numeric(n)
	ARVFrecuenciaCardiaca <- numeric(n)
	ARVFrecuenciaCardiacaDiurna <- numeric(n)
	ARVFrecuenciaCardiacaNocturna <- numeric(n)


	for (i in 1:length(data)){
	
		IDDelPaciente[i] <- data[[i]]$InformacionDelPaciente$IDDelPaciente
		ARVSistolica[i] <- data[[i]]$ARV["Sistolica"]
		ARVSistolicaDiurna[i] <- data[[i]]$ARV["SistolicaDiurna"]
		ARVSistolicaNocturna[i] <- data[[i]]$ARV["SistolicaNocturna"]
		ARVDiastolica[i] <- data[[i]]$ARV["Diastolica"]
		ARVDiastolicaDiurna[i] <- data[[i]]$ARV["DiastolicaDiurna"]
		ARVDiastolicaNocturna[i] <- data[[i]]$ARV["DiastolicaNocturna"]
		ARVPAM[i] <- data[[i]]$ARV["PAM"]
		ARVPAMDiurna[i] <- data[[i]]$ARV["PAMDiurna"]
		ARVPAMNocturna[i] <- data[[i]]$ARV["PAMNocturna"]
		ARVFrecuenciaCardiaca[i] <- data[[i]]$ARV["FrecuenciaCardiaca"]
		ARVFrecuenciaCardiacaDiurna[i] <- data[[i]]$ARV["FrecuenciaCardiacaDiurna"]
		ARVFrecuenciaCardiacaNocturna[i] <- data[[i]]$ARV["FrecuenciaCardiacaNocturna"]
	}
	
	df = data.frame(IDDelPaciente,ARVSistolica,ARVSistolicaDiurna,ARVSistolicaNocturna,ARVDiastolica,ARVDiastolicaDiurna,ARVDiastolicaNocturna,ARVPAM,ARVPAMDiurna,ARVPAMNocturna,ARVFrecuenciaCardiaca,ARVFrecuenciaCardiacaDiurna,ARVFrecuenciaCardiacaNocturna)
	
	#remove duplicated rows
	df<-df[!duplicated(df["IDDelPaciente"]), ]
}

getExcelData <- function(file="../EXCEL/tabla_VITD-Completa (Reparado).xls") {

	library(gdata)
	
	df = read.xls(file)
	
	#remove duplicated rows
	df<-df[!duplicated(df["NHC..número"]), ]
}

mergeExcelTextData <- function(	dataExcel=data.frame(), dataText=data.frame(), outfile="../EXCEL/data.xls") {

	dataMerged<-merge(dataExcel, dataText, by.x="NHC..número", by.y="IDDelPaciente", all.x=T)
	WriteXLS("dataMerged",outfile)
}

##	Ejemplo de uso
#	source("leerTablas.R")
#	mergeExcelTextData(getExcelData(), convertDataToDataFrame(getTextData()))