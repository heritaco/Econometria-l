# pip install pdfkit

import pdfkit

# Archivo HTML de entrada
input_html = 'Calificaciones.html'
# Archivo PDF de salida
output_pdf = 'Calificaciones.pdf'

# Convierte el HTML a PDF
pdfkit.from_file(input_html, output_pdf)

print(f'Archivo PDF guardado en {output_pdf}')