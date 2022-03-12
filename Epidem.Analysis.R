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

calculate_parameters <- function(dX){
  # maximum number of infected persons/day
  maxCutoff = max(dX) * opt.stat.max.cutoff;
  # days with number of infections >= cutoff
  isHigher = (dX >= maxCutoff);
  daysHigh = rle(isHigher);
  daysHigh = daysHigh$lengths[daysHigh$values > 0];
  dXMax = round(max(dX) * opt.population.size);
  dXCutoff = round(maxCutoff * opt.population.size);
  
  
  return(c(daysHigh, dXMax, dXCutoff));
}

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
  
  param_T = calculate_parameters(dIT);
  param_Iy = calculate_parameters(dIy);
  param_Io = calculate_parameters(dIo);
  
  results = data.frame(
    Age = c("Total:", "Young:", "Old:"), 
    "Duration (days)" = c(param_T[1], param_Iy[1], param_Io[1]),
    "Max infections" = c(param_T[2], param_Iy[2], param_Io[2]),
    "Cutoff" = c(param_T[3], param_Iy[3], param_Io[3]),
    "Unit" = "Persons/million",
    check.names = FALSE
  )
  
  # repetate statistici pt mortalitate si spitalizari
  # fct cu parametri 
  # R bind => rbind(df1, df2, df3)
  # fct pt old/y/t 
  
  print(results)
  return(results)
}



