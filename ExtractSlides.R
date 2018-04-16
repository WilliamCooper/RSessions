for (i in 3:9) {
  setwd (sprintf ('~/RStudio/RSessions/RSessions/Session%d', i))
  cmd <- sprintf ('cpdf -split -o S%%%%.pdf -i ../www/Session%d.pdf', i)
  system (cmd)
  Fl <- sort (list.files (pattern="S...pdf"))
  for (Fp in Fl) {
    Fn <- sub ('.pdf', '.html', Fp)
    system(sprintf ('convert -density 600 -resize 25%% %s %s', Fp, Fn))
  }
}
