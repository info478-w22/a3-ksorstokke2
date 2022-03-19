server <- function(input, output) {
  # infection probability: https://www.cdc.gov/mmwr/volumes/70/wr/mm7036a3.htm
  # 0.324, 0.077 by mask
  
  param <- param.dcm(inf.prob = 0.324, act.rate = 0.5, rec.rate = 0.1)
  init <- init.dcm(s.num = 76000000, i.num = 1000)
  control <- control.dcm(type = "SIS", nsteps = 365)
  mod <- dcm(param, init, control)
  output$dcmPlot <- plot(mod)
}