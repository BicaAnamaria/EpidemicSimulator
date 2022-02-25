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
  # Daily vaccinated
  if(is.null(x$Vaccy)){
    Vaccy = 0;
  } else {
    Vaccy = x$Vaccy[1] - x$Vaccy;
  }
  if(is.null(x$Vaccy)){
    Vacco = 0;
  } else {
    Vacco = x$Vacco[1] - x$Vacco;
  }
  # Infected cumulative
  IAll_cum = x$T[1] - x$T - Vaccy - Vacco;
  Iy_cum = x$Sy[1] - x$Sy - Vaccy;
  Io_cum = x$So[1] - x$So - Vacco;
 
  # Daily infected
  dIT = diff(IAll_cum);
  dIy = diff(Iy_cum);
  dIo = diff(Io_cum);

  # maximum number of infected persons/day
  maxCutoff = max(dIT) * opt.stat.max.cutoff;
  # days with number of infections >= cutoff
  isHigher = (dIT >= maxCutoff);
  daysHigh = rle(isHigher);
  daysHigh = daysHigh$lengths[daysHigh$values > 0];
  
  # maximum number of infected young persons/day
  maxCutoffY = max(dIy) * opt.stat.max.cutoff;
  # days with number of infections >= cutoff
  isHigherY = (dIy >= maxCutoffY);
  daysHighY = rle(isHigherY);
  daysHighY = daysHighY$lengths[daysHighY$values > 0];
  
  stat = paste(c("Total: ","Young: ", "Old:"), c(daysHigh, daysHighY))
  # matrice ?, solutionare new line, de pus titlu Nr of days of ...
  
  return(stat)
}



