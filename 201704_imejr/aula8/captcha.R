# captcha.R

#* @get /baixar
baixar <- function() {
  arq <- tempfile(tmpdir = '.', 
                  fileext = '.jpg', 
                  pattern = 'captcha')
  captchaTJRS::download(arq)
}

#* @get /quebrar
quebrar <- function(img = 'captcha.jpg') {
  captchaTJRS::predizer(img)
}
