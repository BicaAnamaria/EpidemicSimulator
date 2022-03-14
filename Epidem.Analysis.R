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

calculate_parameters <- function(dX, category = NULL){
  # maximum number of infected persons/day
  maxCutoff = max(dX) * opt.stat.max.cutoff;
  # days with number of infections >= cutoff
  isHigher = (dX >= maxCutoff);
  daysHigh = rle(isHigher);
  daysHigh = daysHigh$lengths[daysHigh$values > 0];
  dXMax = round(max(dX) * opt.population.size);
  dXCutoff = round(maxCutoff * opt.population.size);
  if(is.null(category)){
          return(data.frame(daysHigh, dXMax, dXCutoff));
  } else {
          return(data.frame(category, daysHigh, dXMax, dXCutoff));
  }
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
  
  ctgs = c("Total:", "Young:", "Old:");
  param_IT = calculate_parameters(dIT, ctgs[1]);
  param_Iy = calculate_parameters(dIy, ctgs[2]);
  param_Io = calculate_parameters(dIo, ctgs[3]);
  
  results = rbind(param_IT, param_Iy, param_Io);
  names(results) = c("Age", "Duration (days)", "Max infections", "Cutoff");
  results$Unit = "Persons/million";
  
 
  
  # repetate statistici pt mortalitate si spitalizari
  # fct cu parametri 
  # R bind => rbind(df1, df2, df3)
  # fct pt old/y/t 
  print(results)
  return(results)
}

summarySIRDeath = function(x){
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
  Dc_cum = x$Dc[1] - x$Dc - Vy;
  Dh_cum = x$Dh[1] - x$Dh - Vo;
  
  # Daily infected
  dDc = diff(Dc_cum);
  dDh = diff(Dh_cum);
  
  ctgs = c("Young:", "Old:");
  param_Dc = calculate_parameters(dDc, ctgs[2]);
  param_Dh = calculate_parameters(dDh, ctgs[3]);
  
  results = rbind( param_Dc, param_Dh);
  names(results) = c("Age", "Duration (days)", "Max deaths", "Cutoff");
  results$Unit = "Persons/million";
  
  return(results)
}

summarySIRHosp = function(x){
  
}


