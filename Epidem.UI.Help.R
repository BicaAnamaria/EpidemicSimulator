helpEpidem = function(){
  txtTitles = c("SIR", "Hospitalization Model", "Vaccine Model", 
              "Vaccine Stratified Model", "2 Viruses Model",
              "Age Groups Model", "Analysis")
  txtHelp = c("Test1", "Test2", "", "", "", "", "")
  txt = lapply(seq(length(txtTitles)), function(id){
    fluidRow(HTML("<b>", txtTitles[id], "</b> <br>", txtHelp[id]) )
  })
  return(txt)
}