# Tabla() captura de algunos valores claves a retornar

    Code
      output_Tabla[["x"]][["container"]]
    Output
      [1] "<table class=\"display\">\n  <thead>\n    <tr>\n      <th colspan=\"3\" class=\"dt-center\">ENCABEZADO</th>\n    </tr>\n    <tr>\n      <th>year</th>\n      <th>season</th>\n      <th>abbrv</th>\n    </tr>\n  </thead>\n</table>"

---

    Code
      output_Tabla[["x"]][["caption"]]
    Output
      [1] "<caption style=\"caption-side: bottom; text-align: center;\">\n  <em>LEYENDA</em>\n</caption>"

---

    Code
      output_Tabla[["x"]][["options"]][["initComplete"]][1]
    Output
      [1] "function(settings, json) {\n$(this.api().table().header()).css({'background-color':\n'#FF1234'\n, 'color': '#000000'});\n}"

