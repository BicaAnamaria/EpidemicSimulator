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

### Global options
opt.stat.max.cutoff = 0.8; # 80% 


summarySIR = function(x){
  #table(x$T);
  IAll_cum = x$T[1] - x$T;
  Iy_cum = x$Sy[1] - x$Sy;
  Io_cum = x$So[1] - x$So;
  Ivaccy = x$Vy[1] - x$Vy;
  Ivacco = x$Vo[1] - x$Vo;
  
  dIT = diff(IAll_cum);
  strIT = paste("Infections (Total/day) =", dIT, sep=" ");
  
  dIy = diff(Iy_cum);
  strIy = paste("Infected Young per day =", dIy, sep=" ");
  
  dIo = diff(Io_cum);
  strIo = paste("Infected Old per day =", dIo, sep=" ");
  print(strIo)
  
  dIvaccy = diff(Ivaccy);
  dIvacco = diff(Ivacco);

  maxCutoff = max(dIT) * opt.stat.max.cutoff;
  isHigher = (dIT >= maxCutoff);
  daysHigh = rle(isHigher);
  daysHigh = daysHigh$lengths[daysHigh$values > 0];
  print(daysHigh)
}



