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
opt.population.size = 1E+6;


summarySIR = function(x){
  #table(x$T);
  # Daily vaccinated
  type = attr(x, "Model");
  # if (type == "Hospitalization") -> ceva
  print(type)
  if(is.null(x$Vaccy)){
    Vy = 0;
  } else {
    Vy = x$Vy[1] - x$Vy;
  }
  if(is.null(x$Vy)){
    Vo = 0;
  } else {
    Vo = x$Vo[1] - x$Vo;
  }
  # Infected cumulative
  IAll_cum = x$T[1] - x$T - Vy - Vo;
  Iy_cum = x$Sy[1] - x$Sy - Vy;
  Io_cum = x$So[1] - x$So - Vo;
 
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
  
  # maximum number of infected young persons/day
  maxCutoffO = max(dIo) * opt.stat.max.cutoff;
  # days with number of infections >= cutoff
  isHigherO = (dIo >= maxCutoffO);
  daysHighO = rle(isHigherO);
  daysHighO = daysHighO$lengths[daysHighO$values > 0];
  
  #stat = paste(c("Total: ","Young: ", "Old:"), 
   #            c(daysHigh, daysHighY, daysHighO))

  dITMax = round(max(dIT) * opt.population.size);
  dITCutoff = round(maxCutoff * opt.population.size);
  
  results = data.frame(
    Age = c("Total:", "Young:", "Old:"), 
    "Duration (days)" = c(daysHigh, daysHighY, daysHighO),
    "Max infections" = c(dITMax),
    "Cutoff" = c(dITCutoff)
  )
  
  
  print(results)
  return(results)
}



