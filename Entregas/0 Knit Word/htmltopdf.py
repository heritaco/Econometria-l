# pip install pdfkit

import pdfkit

# Archivo HTML de entrada
input_html = r'C:\Users\herie\OneDrive\Documentos\GitHub\Econometria l\Entregas\1 Regresion lineal simple\0 Calificaciones\Calificaciones.html'
# Archivo PDF de salida
output_pdf = r'C:\Users\herie\OneDrive\Documentos\GitHub\Econometria l\Entregas\1 Regresion lineal simple\0 Calificaciones\Calificaciones.pdf'

# Convierte el HTML a PDF
pdfkit.from_file(input_html, output_pdf)

print(f'Archivo PDF guardado en {output_pdf}')