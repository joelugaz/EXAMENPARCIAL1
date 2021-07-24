##1
x<- seq(-10,10,0.5)
y<-((exp(x)^2)-1)/((exp(x)^2)+1)

plot(x,y)

#Hallamos la derivada
f<-expression(((exp(x)^2)-1)/((exp(x)^2)+1))
derivada<-D(f,"x")
derivada
## Grafica de la derivada
g_deriv<-2 * (exp(x) * exp(x))/((exp(x)^2) + 1) - ((exp(x)^2) - 1) * (2 *(exp(x) * exp(x)))/((exp(x)^2) + 1)^2
plot(x,g_deriv)

##3
area.triangulo <- function(b,h) {
  area <- (b*h)/2
    return(area)
}
area.triangulo(4,3)

perimetro.triangulo <- function(l) {
  peri <- l*3
  return(peri)
}
perimetro.triangulo(4)


## 4
Tc<-function(L,CN,S){
  res<- (((((0.0136*L^0.8)*(1000/CN))-9)^0.7)/S^0.5)
  return(res)
}
Tc(20,15,28)

##5

sistema<-function(v,r){
  m<-matrix(v,ncol = 3)
  ds<-det(m)
  for (n in 1:length(r)) {
    if(n==1){
      m<-matrix(v,ncol = 3)
      m[,n]<-r
      dx<-det(m)
    }else if(n==2){
      m<-matrix(v,ncol = 3)
      m[,n]<-r
      dy<-det(m)
    }else{
      m<-matrix(v,ncol = 3)
      m[,n]<-r
      dz<-det(m)
    }
  }
  x<-dx/ds
  y<-dy/ds
  z<-dz/ds
  r<-c(x,y,z)
  return(r)
}
vari<-c(2,5,1,1,-4,-1,3,1,-4)
res<-c(7,-19,4)
sistema(vari,res)



##2da parte
##1

library(sf)
library(tidyverse)

cuencas <- st_read("C:/examen progra/uh_datos.shp")
plot(cuencas)

# respuesta a ----
cuencas %>%
  st_drop_geometry() %>%
  group_by(AAA) %>%
  summarize(promedio = mean(pcp)) %>%
  arrange(desc(promedio))

# respuesta b ----
cuencas %>%
  st_drop_geometry() %>%
  group_by(NOMBRE) %>%
  summarize(precipitacion = sum(pcp),
            evapo = sum(pet)) %>%
  mutate(aridez = precipitacion/evapo)

# respuesta c ----
cuencas %>%
  st_drop_geometry() %>%
  group_by(NOMBRE) %>%
  summarize(precipitacion = sum(pcp),
            evapo = sum(pet)) %>% 
  mutate(aridez = precipitacion/evapo,
         ia = case_when(aridez >= 1 ~ "Humedo",
                        aridez >= .7 & aridez < 1 ~ "Subhumedo humedo",
                        aridez >= .5 & aridez < .7 ~ "Subhumedo seco",
                        aridez >= .2 & aridez < .5 ~ "Semiarido",
                        aridez >= .05 & aridez < .2 ~ "Arido",
                        aridez >= 0 & aridez < .05 ~ "Hiperarido"))

# respuesta d ----
# para el AAA 'IX'
cuencas %>%
  st_drop_geometry() %>%
  filter(AAA == "IX") %>%
  select(NOMBRE, pcp, pet) %>%
  pivot_longer(!NOMBRE, names_to = "variable", values_to = "valor") %>%
  ggplot() +
  geom_boxplot(aes(y = valor)) +
  facet_wrap(~ variable)


##2
library(sf)
library(tidyverse)
long <- c(272841.7, 272893.6, 272892.5, 272913.8, 272911.2, 272837.5)
lat <- c(8666459.9, 8666456.9, 8666446.1, 8666441.5, 8666399.9, 8666407.9)
df <- cbind.data.frame(long, lat)

poligono <-function(df){
  polig <- df %>%
    st_as_sf(coords = c("long", "lat"), crs = 32718) %>%
    summarise(geometry = st_combine(geometry)) %>%
    st_cast("POLYGON")
  plot(polig)
}
poligono(df)
