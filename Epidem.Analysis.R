######################
###
### Epidemic Simulator - Bachelor Thesis
### Student: Anamaria Bica
### West University, Timisoara
### Year 3, Faculty of Computer Science
###
### Coordinator: Daniela Zaharie
### Supervisor: Leonard Mada
### Syonic SRL

### based on:
### Team Project 2021
### Students:
###   Dora Calea, Ioana Obreja,
###   Liviu Sopon and Dragos Ursan
###   West University, Timisoara
library(deSolve)
library(ggplot2)
### Global options
opt.stat.max.cutoff = 0.8; # 80% 


summarySIR = function(x){
  
}

##f1 = function(){
#  IAll_cum = T[1] - T;
#  Iy_cum = Sy[1] - Sy;
#  Io_cum = So[1] - So;
#  dIT = diff(IAll_cum);
#  dIy = diff(Iy_cum);
#  dIo = diff(Io_cum);
#  maxCutoff = max(dIT) * opt.stat.max.cutoff;
#  isHigher = (dIT >= maxCutoff);
#  daysHigh = rle(isHigher);
#  daysHigh = daysHigh$lengths[daysHigh$values > 0];
#  print(daysHigh)
##}
#f1()


