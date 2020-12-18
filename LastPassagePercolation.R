#Aqui tenemos una funcion que nos dara las coordenadas de los vecinos, 
#recibe un punto en Z^2 y nos devuelve los vecinos de arriba y derecha.
vec<-function(u){
  r1<-c(u[1]+1,u[2])
  r2<-c(u[1],u[2]+1)
  vecinos<-rbind(r1,r2)
  return(vecinos)
}

#Esta funcion da un vector cuyo primer parametro es aquel de la 
#distribucion exponencial y tambien nos da el numero de vecinos.
proba<-function(x){
  n_vecinos<-length(x[,1])
  n_vecinos
  return(c(1,n_vecinos))
}

#La siguiente funcion elegira al vecino que salio en el menor tiempo. 
#Recibe como parametro la matriz que tiene en sus renglones a los vecinos 
#a los que podemos ir.
sig_punto<-function(x){
  prob<-proba(x)
  pi<-rexp(prob[2],prob[1])   
  y<-c(1:prob[2])	
  m <- matrix(data=cbind(y, pi), ncol=2)
  j <- min(m[,2])
  for(n in 1:prob[2]){
    if(m[n,2]==j){	
      i<-n
      break  # rompo cuando encuentro el renglon del valor mas chico
    }	 
  }
  return(c(i,x[i,])) 
}

#Esta funcion nos permite eliminar el punto que salio de la 
#matriz de vecinos. Ya que no queremos que vuelva a ser elegido.
#Recibe como parametro a x que es la matriz de vecinos e i 
#que es el renglon de la matriz que va a salir.
vec_disp<-function(x,i){
  x=x[-c(i),]
  return(x)
}
  
#Crearemos una funcion que recibe al nuevo punto y verifica que
#los vecinos de este punto no se encuentren en la matriz de vecinos
#y que sean elegibles. 
act_vecinos<-function(nuevo_punto,vecinos,puntos){
  nuevos_vecinos<-vec(nuevo_punto)
  indices<-numeric()
      #Aqui guardaremos los indices de la matriz vecinos que 
      #realmente no son nuevos vecinos o bien estan en la matriz puntos
      #o bien no son elegibles.
  for(i in 1:2){#Para recorrer los 2 nuevos vecinos
    if(nuevos_vecinos[i,1]!=0 && nuevos_vecinos[i,2]!=0){
      a<-0  
      for(j in 1:nrow(puntos)){#Para recorrer la matriz puntos
        if(((nuevos_vecinos[i,1]-1)==puntos[j,1]) 
             && (nuevos_vecinos[i,2]==puntos[j,2])){
          a<-a+1
          break  #Rompo para que no siga comparando
        }
      }
      for(j in 1:nrow(puntos)){#Para recorrer la matriz puntos
        if((nuevos_vecinos[i,1]==puntos[j,1]) 
            && ((nuevos_vecinos[i,2]-1)==puntos[j,2])){
          a<-a+1
          break  #Rompo para que no siga comparando
        }
      }
      if(a<2){
        indices=c(indices,i)  
        #Actualizamos indices guardando el numero de reglon donde pasa.
      }
    }
  }  
  if(length(indices)!=0){
    nuevos_vecinos<-nuevos_vecinos[-c(indices),]  
    #Aqui quitamos los renglones que no son nuevos vecinos.
  }
  vecinos<-rbind(vecinos,nuevos_vecinos)       
  #Agregamos los verdaderos nuevos vecinos a la matriz vecinos.
  return(vecinos)
} 

#Ahora haremos una funcion que recibe como parametro a
# k que es el numero de pasos que queremos.
fpp<-function(k){    
  puntos<-c(0,0)   
  #Tendremos una matriz que guardara en sus renglones los puntos que
  #fueron elegidos, el primer punto es el origen.
  vecinos<-vec(puntos) 
  #Una matriz que guarda en sus renglones a los posibles vecinos, 
  #La iniciaremos con los vecinos del origen.
  tiempo<-1
  while(tiempo<=k){
    aux<-sig_punto(vecinos)
    #Aqui tenemos a que punto de los posibles vecinos salta.
    nuevo_punto<-c(aux[2],aux[3]) 
    #coordenadas del nuevo punto.
    puntos<-rbind(puntos,nuevo_punto) 
    #Actualizamos la matriz de puntos agregando el nuevo punto.    
    vecinos<-act_vecinos(nuevo_punto,vecinos,puntos)  
    #Actualizamos la matriz de vecinos agregando los vecinos del nuevo punto.    
    vecinos<-vec_disp(vecinos,aux[1])  
    #Actualizamos la matriz de vecinos eliminando el punto que salio.
    tiempo<-tiempo+1  
    #Actualizamos el tiempo.
  }

  #Extraemos las coordenadas de los puntos para graficarlos.
  X<-puntos[,1]
  Y<-puntos[,2]
  all<-c(X,Y)
  range=c(min(all),max(all)+1)
  fig<-plot(0,0,main = "Modelo de crecimiento de k=100 pasos",
            xlim=range,ylim = range)
  for(i in 1:length(puntos[,1])){#Graficamos los cuadros.
    rect(puntos[i,1],puntos[i,2],puntos[i,1]+1,puntos[i,2]+1 ) 
  }
  return(fig)
}
