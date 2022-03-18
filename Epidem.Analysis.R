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

# generic function
summarySIR = function(x){
  results = rbind(summarySIR_Infected(x), summarySIR_Death(x));
  print(results)
  return(results)
}

summarySIR_Infected = function(x)
{
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
  # names(results) = c("Age", "Duration (days)", "Max infections", "Cutoff");
  # TODO: Very ugly hack
  names(results) = c("Age", "Duration (days)", "Max", "Cutoff");
  results$Unit = "Persons/million";
  
  # TODO: coloana in fata cu ce inseamna + statistical hosp
 
  
  # repetate statistici pt mortalitate si spitalizari
  # fct cu parametri 
  # R bind => rbind(df1, df2, df3)
  # fct pt old/y/t 
  return(results)
}

summarySIR_Death = function(x){
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
  # Death cumulative
  Dc_cum = x$Dc - x$Dc[1];
  Dh_cum = x$Dh - x$Dh[1];
  DT_cum = Dc_cum + Dh_cum;
  
  # Daily death
  dDT = diff(DT_cum);
  dDc = diff(Dc_cum);
  dDh = diff(Dh_cum);
  
  ctgs = c("Total:", "Community:", "Hospital:");
  param_DT = calculate_parameters(dDT, ctgs[1]);
  param_Dc = calculate_parameters(dDc, ctgs[2]);
  param_Dh = calculate_parameters(dDh, ctgs[3]);
  
  results = rbind(param_DT, param_Dc, param_Dh);
  
  #names(results) = c("Age", "Duration (days)", "Max deaths", "Cutoff");
  # TODO: Very ugly hack
  names(results) = c("Age", "Duration (days)", "Max", "Cutoff");
  results$Unit = "Persons/million";
  
  return(results)
}

summarySIRHosp = function(x){
  
}


