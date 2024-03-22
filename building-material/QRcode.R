library(qrcode)
f_qrcode <- qr_code("https://jogaudard.github.io/fluxible/")
plot(f_qrcode)
generate_svg(f_qrcode, filename = "man/figures/f_qrcode.svg")