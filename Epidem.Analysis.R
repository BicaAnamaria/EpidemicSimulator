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


calculate_parameters <- function(dX, category = NULL, type = "Infected"){
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
          return(data.frame(type, category, daysHigh, dXMax, dXCutoff));
  }
}


# generic function
summarySIR = function(x){
  results = rbind(summarySIR_Infected(x), summarySIR_Death(x), summarySIR_Hosp(x));
  print(results)
  return(results)
}

computeSummary = function(x, y, ctgs) {
  
  Dx_cum = - x + x[1] - y;
  dDx = diff(Dx_cum);
  param_Dx = calculate_parameters(dDx, ctgs, "Infected");
  
  return(param_Dx)
  
}

summarySIR_Infected = function(x)
{
  type = attr(x, "Model");
  
  if(type == "Hospitalization" ){
    param_Iy = computeSummary(x$Sy, 0, "Infected young: ")
    param_Io = computeSummary(x$So, 0,  "Infected old: ")
    param_IAll = computeSummary(x$T, 0,  "Total: ")
    
    results = rbind(param_IAll, param_Iy, param_Io);
  }
  else if( type == "Vaccination" | type == "Vaccination Stratified"){
    param_Iy = computeSummary(x$Sy, x$Vy, "Infected young: ")
    param_Io = computeSummary(x$So, x$Vo,  "Infected old: ")
    param_IAll = computeSummary(x$T, x$Vy + x$Vo,  "Total: ")
    
    results = rbind(param_IAll, param_Iy, param_Io);
  }
  else if( type == "2 Viruses" ){
    param_I = computeSummary(x$S, 0,  "Total infected: ")
    
    results = param_I;
  }
  else if( type == "AG3" ){
    param_Ic = computeSummary(x$Sc, 0, "Infected young: ")
    param_Ia = computeSummary(x$Sa, 0, "Infected adults: ")
    param_Io = computeSummary(x$So, 0,  "Infected old: ")
    param_IAll = computeSummary(x$T, 0,  "Total: ")
    
    results = rbind(param_IAll, param_Ic, param_Ia, param_Io);
  }
  
  # names(results) = c("Age", "Duration (days)", "Max infections", "Cutoff");
  # TODO: Very ugly hack
  names(results) = c("Type", "Age", "Duration (days)", "Max", "Cutoff");
  results$Unit = "Persons/million";

  
  # TODO: coloana in fata cu ce inseamna + statistical hosp

  return(results)
}


computeSummary0 = function(x, ctgs) {
  
  Dx_cum = x - x[1];
  dDx = diff(Dx_cum);
  param_Dx = calculate_parameters(dDx, ctgs, "Death");
  
  return(param_Dx)
  
}

summarySIR_Death = function(x){
  type = attr(x, "Model");
 
  if(type == "Hospitalization"){
    param_Dc = computeSummary0(x$Dc,  "Community: ")
    param_Dh = computeSummary0(x$Dh,  "Hospital: ")
    param_DT = computeSummary0(x$Dc + x$Dh , "Total: ");
    
    results = rbind(param_DT, param_Dc, param_Dh);
  }
  else if(type == "Vaccination Stratified"){
    param_Dy = computeSummary0(x$Dy,  "Young: ")
    param_Do = computeSummary0(x$Do,  "Old: ")
    param_DT = computeSummary0(x$Dy + x$Do , "Total: ");
    
    results = rbind(param_DT, param_Dy, param_Do);
  }
  else if(type == "2 Viruses"){
    param_DV1 = computeSummary0(x$DV1,  "Virus1: ")
    param_DV2 = computeSummary0(x$DV2,  "Virus2: ")
    param_DT = computeSummary0(x$DV1 + x$DV2 , "Total: ");
    
    results = rbind(param_DT, param_DV1, param_DV2);
  }
  else if(type == "Vaccination"){
    param_D = computeSummary0(x$D,  "Death: ")
    
    results = param_D;
  }
  else if(type == "AG3"){
    param_Dc = computeSummary0(x$Dc,  "Children: ")
    param_Da = computeSummary0(x$Da,  "Adults: ")
    param_Do = computeSummary0(x$Do,  "Old: ")
    param_DT = computeSummary0(x$Dc + x$Da + x$Do , "Total: ");
    
    results = rbind(param_DT, param_Dc, param_Da, param_Do);
  }
  
  #names(results) = c("Age", "Duration (days)", "Max deaths", "Cutoff");
  # TODO: Very ugly hack
  names(results) = c("Type", "Age", "Duration (days)", "Max", "Cutoff");
  results$Unit = "Persons/million";
  
  return(results)
}

computeSummary1 = function(x, ctgs) {
  
  Dx_cum = x - x[1];
  dDx = diff(Dx_cum);
  param_Dx = calculate_parameters(dDx, ctgs, "Hosp");
  
  return(param_Dx)
  
}

summarySIR_Hosp = function(x){
  type = attr(x, "Model");
  
  if(type == "Hospitalization" | type == "Vaccination Stratified"){
    #results = calculate_death(x, type, list(c('Dc', 'Dh')))
    param_H = computeSummary1(x$H,  "Hosp: ")
    param_Hy = computeSummary1(x$Hy,  "Hosp young: ")
    param_Ho = computeSummary1(x$Ho, "Hosp old: ");
    param_Hcum = computeSummary1(x$Hcum, "Hosp cumulative: ");
    
    results = rbind(param_H, param_Hy, param_Ho, param_Hcum);
  }
  else if(type == "2 Viruses"){
    param_HV1 = computeSummary1(x$HV1,  "Hops V1: ")
    param_HV2 = computeSummary1(x$HV2,  "Hops V2: ")
    param_Hcum = computeSummary1(x$Hcum, "Hosp cumulative: ");
    
    results = rbind(param_HV1, param_HV2, param_Hcum);
  }
  else if(type == "Vaccination"){
    param_H = computeSummary1(x$H,  "Hosp: ")
    param_Hcum = computeSummary1(x$Hcum, "Hosp cumulative: ");
    
    results = rbind(param_H, param_Hcum);
  }
  else if(type == "AG3"){
    param_Hc = computeSummary1(x$Hc,  "Children: ")
    param_Ha = computeSummary1(x$Ha,  "Adults: ")
    param_Ho = computeSummary1(x$Ho,  "Old: ")
    param_Hcum = computeSummary1(x$Hcum, "Hosp cumulative: ");
    
    results = rbind(param_Hc, param_Ha, param_Ho, param_Hcum);
  }
  
  #names(results) = c("Age", "Duration (days)", "Max deaths", "Cutoff");
  # TODO: Very ugly hack
  names(results) = c("Type", "Age", "Duration (days)", "Max", "Cutoff");
  results$Unit = "Persons/million";
  
  return(results)
}


