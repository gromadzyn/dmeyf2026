# Genera instrucciones_target_engineering.pdf
# Uso (desde la raiz del repo):
#   uv run --with reportlab python target_engineering/gen_pdf.py target_engineering/instrucciones_target_engineering.pdf

import sys
from reportlab.lib.pagesizes import A4
from reportlab.lib.units import mm
from reportlab.lib import colors
from reportlab.lib.styles import ParagraphStyle
from reportlab.platypus import (SimpleDocTemplate, Paragraph, Spacer, Table,
                                TableStyle, Preformatted, KeepTogether)

AZUL = colors.HexColor("#2E6DB4")
GRIS = colors.HexColor("#F2F3F5")
BORDE = colors.HexColor("#D9DCE1")
AMARILLO = colors.HexColor("#FFF6DB")
NARANJA = colors.HexColor("#F2A900")
TXT = colors.HexColor("#222222")

base = ParagraphStyle("base", fontName="Helvetica", fontSize=9.3, leading=12.8,
                      textColor=TXT, spaceAfter=4)
titulo = ParagraphStyle("t", parent=base, fontName="Helvetica-Bold", fontSize=19,
                        leading=23, textColor=AZUL, spaceAfter=1)
subt = ParagraphStyle("st", parent=base, fontSize=10, textColor=colors.grey, spaceAfter=8)
h2 = ParagraphStyle("h2", parent=base, fontName="Helvetica-Bold", fontSize=13.5,
                    leading=16, textColor=AZUL, spaceBefore=7, spaceAfter=3)
h3 = ParagraphStyle("h3", parent=base, fontName="Helvetica-Bold", spaceBefore=3, spaceAfter=2)
code = ParagraphStyle("code", fontName="Courier", fontSize=7.8, leading=10.5, textColor=TXT)
bullet = ParagraphStyle("b", parent=base, leftIndent=12, bulletIndent=3, spaceAfter=2)
celda = ParagraphStyle("c", parent=base, fontSize=8.8, leading=11.5, spaceAfter=0)
celda_b = ParagraphStyle("cb", parent=celda, fontName="Helvetica-Bold")

W = A4[0] - 40 * mm


def C(s):
    return f'<font face="Courier">{s}</font>'


def P(s, st=base):
    return Paragraph(s, st)


def caja(texto, fondo=GRIS, borde=BORDE, estilo=None, barra=False):
    contenido = Preformatted(texto, code) if estilo is None else Paragraph(texto, estilo)
    t = Table([[contenido]], colWidths=[W])
    st = [("BACKGROUND", (0, 0), (-1, -1), fondo),
          ("LEFTPADDING", (0, 0), (-1, -1), 8), ("RIGHTPADDING", (0, 0), (-1, -1), 8),
          ("TOPPADDING", (0, 0), (-1, -1), 6), ("BOTTOMPADDING", (0, 0), (-1, -1), 6)]
    if barra:
        st.append(("LINEBEFORE", (0, 0), (0, -1), 3, borde))
    else:
        st.append(("BOX", (0, 0), (-1, -1), 0.5, borde))
    t.setStyle(TableStyle(st))
    return [t, Spacer(1, 5)]


def cod(t):
    return caja(t)


def aviso(t):
    return caja(t, fondo=AMARILLO, borde=NARANJA, estilo=base, barra=True)


def tabla(filas, anchos):
    data = [[Paragraph(c, celda_b if i == 0 else celda) for c in f] for i, f in enumerate(filas)]
    t = Table(data, colWidths=anchos)
    t.setStyle(TableStyle([
        ("BACKGROUND", (0, 0), (-1, 0), GRIS),
        ("GRID", (0, 0), (-1, -1), 0.5, BORDE),
        ("VALIGN", (0, 0), (-1, -1), "TOP"),
        ("LEFTPADDING", (0, 0), (-1, -1), 5), ("RIGHTPADDING", (0, 0), (-1, -1), 5),
        ("TOPPADDING", (0, 0), (-1, -1), 3), ("BOTTOMPADDING", (0, 0), (-1, -1), 3)]))
    return [t, Spacer(1, 6)]


def B(items):
    return [Paragraph(i, bullet, bulletText="•") for i in items]


def pie(c, doc):
    c.saveState()
    c.setFont("Helvetica", 7.5)
    c.setFillColor(colors.grey)
    c.drawString(20 * mm, 12 * mm, "DMEyF 2026 — Grupo B — Target Engineering")
    c.drawRightString(A4[0] - 20 * mm, 12 * mm, f"Página {doc.page}")
    c.restoreState()


ramas = [("0.75", ""), ("0.50", ""), ("0.25", ""), ("0.00", ", solo BAJA+2"),
         ("1.50", ", control")]

s = []
s += [P("Target Engineering: correr y comparar las ramas", titulo),
      P("Versión 2 (baseline de BASELINE_FINAL.ipynb, max_bin 31). Una rama por VM, con 8 hilos", subt)]
s += [P("Son 5 ramas: el mismo modelo que Exp_0, con los BAJA+1 entrenando con otro peso. Cada rama "
        "tarda lo mismo que Exp_0 y usa <b>8 hilos</b>, igual que Exp_0. Cada VM corre <b>una rama "
        "por vez</b>; para correr varias en paralelo se usan varias VMs. Las ramas se reparten entre el "
        "grupo y al final se juntan en una sola VM para comparar con Wilcoxon.")]
s += aviso("Siempre sin sudo, y <b>nunca dos ramas a la vez en la misma VM</b>. Toda rama se lanza con "
           "<b>OMP_NUM_THREADS=8</b> adelante, aunque la VM tenga 12 CPUs: si falta, LightGBM usa todas "
           "las CPUs, los árboles cambian y la rama ya no es comparable con Exp_0.")

s += [KeepTogether([P("Qué cambió en la versión 2: hay que empezar de nuevo", h2),
      P(f"{C('lightgbm_bo.R')} (Exp_0) y {C('te_nucleo.R')} (las ramas) ahora siguen "
        f"{C('BASELINE_FINAL.ipynb')} del repo colaborativo {C('germandasr/DMEyF_Exp_Colaborativos')}, "
        "con nuestra semilla primigenia <b>170383</b>:")] +
      B([f"Parámetros fijos del {C('config.yaml')} del notebook: <b>max_bin = 31</b> (antes 255, el "
         f"valor por defecto) y {C('deterministic = TRUE')}. El resto ya eran los valores por defecto.",
         "BO: primero el 70/30 sobre todo el train y el undersampling 0.1 solo en el 70%. Se valida "
         "en el 30% completo y la AUC es la máxima entre las iteraciones (best_score), como en el "
         f"notebook. {C('min_sum_hessian_in_leaf')} se busca en escala lineal.",
         "Producción: con <b>cada semilla</b> se vuelve a sortear el undersampling 0.1. Las 10 "
         "semillas se sortean igual que en el notebook.",
         "<b>10 corridas</b> por rama (y por Exp_0): todo el pipeline (BO + producción de 10 "
         "semillas) se repite con 10 semillas primigenias, 170383 y 9 primos sorteados a partir de "
         f"ella, las mismas en todas las ramas. Cada corrida queda en {C('sp_&lt;semilla&gt;/')} y "
         "es una observación para Wilcoxon. Cuesta ~10 veces una corrida de la versión anterior.",
         f"Exp_0 también se respalda en el bucket, igual que las ramas, en {C('gs://BUCKET/exp/Exp_0')}."]))]
s += [P("Todo lo corrido con la versión anterior (Exp_0 y las ramas) ya no es comparable: <b>se "
        "vuelve a correr todo, empezando por Exp_0</b>. En cada VM o compu que tenga corridas "
        "viejas, y en tu bucket si las respaldaste ahí, movelas a un costado (no se borra nada):")]
s += cod("mkdir -p ~/dmeyf/exp_v1\n"
         "mv ~/dmeyf/exp/Exp_0* ~/dmeyf/exp/TE_w* ~/dmeyf/exp_v1/ 2>/dev/null\n"
         "gcloud storage mv gs://BUCKET/exp gs://BUCKET/exp_v1   # solo si tenias algo en el bucket")
s += [P(f"Si te olvidás, el script frena solo: cada carpeta guarda su {C('configuracion.yml')} y, si "
        "encuentra checkpoints de otra versión (en la carpeta o en el bucket), termina con un "
        "<b>Error</b> que dice qué carpeta mover. Mové esa carpeta y volvé a lanzar.")]

s += [P("0. Repartir las ramas", h2),
      P("Anoten en la planilla quién corre <b>Exp_0</b> y quién cada rama, para que ninguna se corra "
        "dos veces ni quede sin correr. Exp_0 es una corrida más (una VM): puede ir en paralelo con "
        "las ramas. La que termine todo junta los resultados (paso 6) y corre el análisis: conviene "
        "que sea quien tiene Exp_0.")]

s += [P("1. Preparar la VM (una sola vez)", h2),
      P("Usá el template de la cátedra de <b>8 o 12 vCPUs</b>. Cualquiera de los dos corre una rama; "
        "con 12 quedan 4 CPUs libres para el sistema. Si es la VM de Exp_0, esto ya está hecho: "
        "saltá al paso 2.")]
s += cod("git clone https://github.com/gromadzyn/dmeyf2026.git ~/dmeyf2026\n"
         "mkdir -p ~/dmeyf/datasets ~/dmeyf/exp")
s += [P(f"Falta {C('competencia_02_crudo.csv.gz')} (711 MB) en {C('~/dmeyf/datasets/')}, sin descomprimir "
        "ni renombrar. Subirlo por la ventana SSH suele colgarse: cada uno lo pasa por <b>su propio "
        "bucket</b> de Cloud Storage, en su proyecto.")]
s += [KeepTogether([P("Tu bucket (una sola vez)", h3)] + B([
        "Consola de Google Cloud → <i>Cloud Storage → Buckets → Crear</i>. Nombre único (ej. "
        f"{C('dmeyf-&lt;tu_usuario&gt;')}), ubicación <i>Region</i>, la misma de tu VM. Lo demás, por defecto.",
        "Entrá al bucket → <i>Subir archivos</i> → competencia_02_crudo.csv.gz desde tu compu. "
        "Esperá el 100% (abajo a la derecha)."]))]
s += [P(f"En la VM ({C('BUCKET')} = el nombre de tu bucket):", h3)]
s += cod("gcloud storage cp gs://BUCKET/competencia_02_crudo.csv.gz ~/dmeyf/datasets/\n"
         "ls -l ~/dmeyf/datasets/      # 711222839 bytes\n"
         "echo BUCKET > ~/dmeyf/bucket.txt")
s += [P(f"Con {C('bucket.txt')}, cada rama sube su avance a {C('gs://BUCKET/exp/')} después de cada "
        "evaluación de la BO y de cada semilla. Si la VM desaparece, no se pierde lo ya calculado (paso 5).")]
s += [P(f"Si dice {C('AccessDeniedException')} o {C('insufficient scopes')}: detené la VM, "
        "<i>Editar → Permisos de acceso a la API</i> → <b>Storage: Lectura y escritura</b> "
        "(escritura sirve para el paso 6), guardá y volvé a iniciarla.")]
s += [P("Los paquetes de R se instalan solos en tu home la primera vez que lanzás una rama: esa "
        "primera vez tarda unos 20 minutos más en arrancar.")]

s += [P("2. Antes de cada rama", h2),
      P("Que no haya otra corrida y que nada ocupe la CPU:")]
s += cod("ps aux | grep -E '[t]e_nucleo|[l]ightgbm_bo'   # no tiene que aparecer nada\n"
         "top -bn1 | head -15            # nada cerca de 100 en %CPU\n"
         "cd ~/dmeyf2026 && git pull")

s += [KeepTogether([P("3. Lanzar Exp_0 o la rama", h2),
                    P("Copiá y pegá el bloque de la corrida que te toca. Después podés cerrar la "
                      "ventana de SSH: la corrida sigue.")])]
s += [KeepTogether([P("Exp_0 (baseline, el control de todas las ramas)", h3)] +
                   cod("cd ~/dmeyf2026\n"
                       "OMP_NUM_THREADS=8 nohup Rscript lightgbm_bo.R > ~/dmeyf/exp/Exp_0.log 2>&1 &"))]
for w, extra in ramas:
    s += [KeepTogether([P(f"Rama peso {w}{extra}", h3)] +
                       cod(f"cd ~/dmeyf2026\n"
                           f"OMP_NUM_THREADS=8 nohup Rscript target_engineering/TE_w{w}.R > /dev/null 2>&1 &"))]
s += [KeepTogether([P("A los 20-30 minutos, chequeá que arrancó y la versión de LightGBM (cambiá TE_w0.75 por tu rama, o por Exp_0):")] +
      cod("ps aux | grep -E '[t]e_nucleo|[l]ightgbm_bo'  # tiene que aparecer\n"
          "tail -5 ~/dmeyf/exp/TE_w0.75.log           # sin 'Error' ni 'Execution halted'\n"
          "grep -m2 Respaldo ~/dmeyf/exp/TE_w0.75.log # tiene que decir 'Respaldo: OK'\n"
          "Rscript -e 'packageVersion(\"lightgbm\")'    # la misma que en la VM de Exp_0") +
      B(["Si el log termina en <b>Error</b> o <b>Execution halted</b>, mandá las últimas 30 líneas "
         f"({C('tail -30')}) al grupo.",
         f"Si en vez de <b>Respaldo: OK</b> dice <b>ATENCION</b>, la rama corre igual pero sin respaldo: "
         f"revisá {C('~/dmeyf/bucket.txt')} y los permisos de Storage (paso 1), y volvé a lanzar el bloque.",
         "Si la versión de LightGBM no es la misma que en la VM de Exp_0, frená la rama (paso 5) y "
         "avisá antes de seguir: otra versión también cambia los árboles."]))]

s += [P("4. Saber si terminó", h2),
      P("Reemplazá 0.75 por la rama que está corriendo:")]
s += cod("grep '########' ~/dmeyf/exp/TE_w0.75.log | tail -1   # en que corrida va (k/10)\n"
         "ls ~/dmeyf/exp/TE_w0.75/sp_*/PARAM.yml | wc -l      # corridas terminadas: 10 = terminó\n"
         "ls ~/dmeyf/exp/TE_w0.75/resumen_corridas.txt        # se escribe al final de todo")
s += B([f"Cada corrida tiene su carpeta {C('sp_&lt;semilla&gt;/')}. Primero hace la BO: 12 evaluaciones, "
        f"cada una es una línea de {C('bo_evaluaciones.txt')} (la columna {C('minutos')} dice cuánto tardó). "
        "Después hace las 10 semillas, cada una es un archivo en semillas/. Al final escribe su "
        f"{C('PARAM.yml')} y pasa a la corrida siguiente.",
        "Los tiempos de la versión anterior ya no sirven de referencia: anoten en la planilla los de "
        "la primera corrida de Exp_0 nuevo (por 10 da el total).",
        f"Con 10 {C('PARAM.yml')} y {C('resumen_corridas.txt')}, terminó. Para Exp_0, lo mismo en "
        f"{C('~/dmeyf/exp/Exp_0/')}. Si te toca otra rama, volvé al paso 2."])

s += [P("5. Si se corta o se traba", h2),
      P("Si la VM se reinició o R murió, volvé a lanzar el mismo bloque del paso 3 (con "
        f"{C('OMP_NUM_THREADS=8')}): retoma sola desde donde estaba.")]
s += [P("Si la VM <b>desapareció</b> de la lista (las VMs Spot se pueden interrumpir y borrar en "
        f"cualquier momento; en <i>Logs Explorer</i> aparece {C('compute.instances.preempted')}): creá "
        f"otra con el template, hacé el paso 1 con el <b>mismo bucket</b> (incluido {C('bucket.txt')}) y "
        "volvé a lanzar el mismo bloque del paso 3. Baja sola del bucket lo que ya estaba hecho y sigue "
        "desde ahí: se pierde solo la evaluación o la semilla que estaba a medias. Las corridas "
        "terminadas no se rehacen.")]
s += [KeepTogether([P("Si pasan más de 3 h sin semilla nueva aunque R use mucha CPU, buscá con "
        f"{C('top')} otro proceso que ocupe CPU y frenalo. Si no se recupera:")] +
      cod("pkill -f TE_w; pkill -f te_nucleo.R; pkill -f lightgbm_bo.R\n"
          "ps aux | grep -E '[t]e_nucleo|[l]ightgbm_bo'   # no tiene que aparecer nada") +
      [P("y volvé a lanzar el mismo bloque.")])]
s += aviso("<b>No borres</b> las carpetas ~/dmeyf/exp/Exp_0 ni ~/dmeyf/exp/TE_w... de la versión 2: "
           "ahí está lo que ya se calculó.")

s += [KeepTogether([P("Alternativa: correr una rama en tu compu", h2),
      P("Si tenés una compu con <b>Linux</b>, o con <b>Windows</b> usando WSL2 (más abajo), una rama "
        "también se puede correr ahí en vez de en una VM. No se interrumpe como una VM Spot, pero tiene que quedar prendida todo lo que dure la rama. "
        "Anotala en la planilla igual que cualquier rama.")] +
      tabla([["Requisito", "Por qué"],
             ["8 núcleos o más (mejor 12 o más)", "La rama usa 8 hilos; si son justo 8, la compu queda "
              "muy lenta mientras corre."],
             ["64 GB de RAM", "Con la versión anterior, R llegó a usar ~25 GB en la VM, más lo que ocupa "
              "LightGBM. La versión 2 guarda el train completo para volver a sortear el undersampling "
              "con cada semilla: puede usar algo más. Mientras corre, no abras nada pesado."],
             ["La <b>misma versión de LightGBM</b> que en la VM de Exp_0",
              "Con otra versión cambian los árboles y la rama ya no es comparable."],
             ["R, y para compilar los paquetes: gcc, g++ y make",
              "Los paquetes que falten se instalan solos la primera vez, como en la VM."]],
            [W * 0.38, W * 0.62]))]
s += [P(f"Preparación (una sola vez). El dataset se baja de tu bucket desde la consola, sin "
        f"{C('gcloud')}, y se guarda en {C('~/dmeyf/datasets/')} sin descomprimir ni renombrar:")]
s += cod("git clone https://github.com/gromadzyn/dmeyf2026.git ~/dmeyf2026\n"
         "mkdir -p ~/dmeyf/datasets ~/dmeyf/exp\n"
         "ls -l ~/dmeyf/datasets/                    # 711222839 bytes\n"
         "Rscript -e 'packageVersion(\"lightgbm\")'    # si ya está, la misma que en la VM de Exp_0")
s += [KeepTogether([P("Antes de lanzar, hacé el paso 2. Después, el bloque de tu rama (cambiá 0.75):")] +
      cod("cd ~/dmeyf2026 && git pull\n"
          "OMP_NUM_THREADS=8 nohup systemd-inhibit --what=sleep:idle --why=\"rama TE\" \\\n"
          "  Rscript target_engineering/TE_w0.75.R > /dev/null 2>&1 &"))]
s += B([f"{C('systemd-inhibit')} evita que la compu se suspenda sola mientras corre la rama. No la "
        "suspendas ni la apagues a mano.",
        f"No hace falta {C('bucket.txt')}: el disco de tu compu no desaparece. En el log va a decir "
        f"{C('ATENCION: sin .../bucket.txt')}, y es normal.",
        "El chequeo, saber si terminó y qué hacer si se corta son iguales que en la VM (pasos 3, 4 y 5): "
        "si se corta la luz o reiniciás, relanzá el mismo bloque y retoma.",
        "En el paso 6, empaquetá la carpeta con el mismo <b>tar</b> y pasá el archivo directo por "
        "Drive o mail, sin bucket."])

s += [KeepTogether([P("En Windows (con WSL2)", h3),
      P("WSL2 es un Ubuntu que corre adentro de Windows. Con un procesador Intel o AMD, los árboles "
        "salen comparables con Exp_0. En una Mac con chip M, no: es otra arquitectura y puede cambiar "
        "los árboles.")])]
s += [KeepTogether([P("1. En <b>PowerShell como administrador</b>; después reiniciá la compu y abrí "
        "<i>Ubuntu</i> desde el menú Inicio, que pide un usuario y una contraseña de Linux:")] +
      cod("wsl --install -d Ubuntu"))]
s += [KeepTogether([P("2. Por defecto WSL2 usa solo la mitad de la RAM. Creá con el Bloc de notas "
        + C('C:\\Users\\&lt;tu_usuario&gt;\\.wslconfig') + " (sin .txt al final) con esto, poniendo "
        "tu RAM total menos 8 GB:")] +
      cod("[wsl2]\nmemory=56GB") +
      [P(f"Guardalo, corré {C('wsl --shutdown')} en PowerShell y volvé a abrir Ubuntu. Ahí chequeá:")] +
      cod("free -g      # total cerca de lo que pusiste en memory\n"
          "nproc        # 8 o más"))]
s += [KeepTogether([P("3. En Ubuntu, instalá R y lo necesario para compilar los paquetes. Acá sí va "
        f"{C('sudo')}: es tu propio Linux, no la VM.")] +
      cod("sudo apt update\n"
          "sudo apt install -y r-base r-base-dev build-essential git \\\n"
          "  libcurl4-openssl-dev libssl-dev zlib1g-dev"))]
s += [KeepTogether([P("4. Bajá el dataset de tu bucket con el navegador de Windows y copialo a Ubuntu. "
        f"Dejalo en {C('~/dmeyf')}, no en {C('/mnt/c')}: leer desde el disco de Windows es mucho más lento.")] +
      cod("git clone https://github.com/gromadzyn/dmeyf2026.git ~/dmeyf2026\n"
          "mkdir -p ~/dmeyf/datasets ~/dmeyf/exp\n"
          "cp /mnt/c/Users/TU_USUARIO/Downloads/competencia_02_crudo.csv.gz ~/dmeyf/datasets/\n"
          "ls -l ~/dmeyf/datasets/      # 711222839 bytes"))]
s += [KeepTogether([P("5. En Windows, <i>Configuración → Sistema → Energía</i>: suspender "
        "<b>Nunca</b> (enchufada) mientras corre. Hacé el paso 2 general (<i>Antes de cada rama</i>) y lanzá tu rama (cambiá 0.75):")] +
      cod("cd ~/dmeyf2026 && git pull\n"
          "OMP_NUM_THREADS=8 nohup Rscript target_engineering/TE_w0.75.R > /dev/null 2>&1 &"))]
s += B(["<b>No cierres la ventana de Ubuntu</b> mientras corre: Windows puede apagar WSL y frenar "
        "la rama. Si pasa, abrí Ubuntu y relanzá el mismo bloque: retoma.",
        "La primera vez los paquetes de R tardan más en instalarse que en la VM, porque se compilan. "
        "Antes de dejarla corriendo, chequeá como en el paso 3 general (<i>Lanzar la rama</i>) que la versión de LightGBM sea la misma "
        "que en la VM de Exp_0.",
        "Para el paso 6 general, el tar se copia a Windows con "
        f"{C('cp ~/dmeyf/exp/ramas_*.tar.gz /mnt/c/Users/TU_USUARIO/Downloads/')} y se pasa por Drive o mail."])

s += [KeepTogether([P("6. Juntar todo en una VM", h2),
      P("El análisis necesita <b>Exp_0 y las 5 ramas</b> (10 corridas cada una) en la misma VM. "
        "Cada uno, cuando terminaron sus ramas, las empaqueta y las sube a su bucket:")] +
      cod("cd ~/dmeyf/exp && tar czf ramas_$(whoami).tar.gz TE_w*/\n"
          "gcloud storage cp ramas_$(whoami).tar.gz gs://BUCKET/"))]
s += [P("Después lo baja desde la consola (<i>Cloud Storage</i> → su bucket → el archivo → "
        "<i>Descargar</i>) y se lo pasa a quien corre el análisis (Drive, mail, etc.; pesa poco). "
        "Quien corre el análisis sube todos esos archivos y el de Exp_0 a <b>su</b> bucket desde la "
        "consola, y en su VM:")]
s += cod("gcloud storage cp 'gs://BUCKET/*.tar.gz' ~/\n"
         "cd ~/dmeyf/exp && for f in ~/ramas_*.tar.gz ~/Exp_0.tar.gz; do tar xzf $f; done\n"
         "for d in ~/dmeyf/exp/Exp_0 ~/dmeyf/exp/TE_w*; do echo \"$d $(ls $d/sp_*/PARAM.yml | wc -l)\"; done\n"
         "                                    # todas tienen que dar 10")
s += [P(f"Si Exp_0 corrió en esa misma VM, ya está en {C('~/dmeyf/exp/Exp_0')}: no hace falta subirlo. "
        f"Si no, quien lo corrió lo empaqueta con {C('cd ~/dmeyf/exp && tar czf Exp_0.tar.gz Exp_0/')} "
        "y lo pasa igual.")]

s += [P("7. Comparar con Wilcoxon", h2)]
s += cod("cd ~/dmeyf2026 && git pull\n"
         "Rscript target_engineering/te_analisis.R | tee ~/dmeyf/exp/TE_analisis.log")
s += [P(f"No entrena nada: tarda unos minutos. Las tablas quedan en {C('~/dmeyf/exp/TE_analisis/')}. Qué mirar:")]
s += B(["<b>Familia 1</b>: la unidad es la <b>corrida</b>: su ganancia es la ganancia máxima del "
        "ensamble de sus 10 semillas. Cada peso contra Exp_0, Wilcoxon pareado por semilla primigenia "
        "(las 10 corridas de la rama contra las 10 de Exp_0), con corrección de Holm sobre los 5 pesos. "
        "Al final dice <b>H1 se ACEPTA</b> o <b>H1 NO se acepta</b>. Con 10 corridas, un peso tiene que "
        "ganarle a Exp_0 en unas 9 de 10 para ser significativo.",
        "<b>Ensambles (familia 2)</b>: no hay que correr nada más. Se arman con las predicciones "
        "guardadas, corrida por corrida, para <b>cada peso</b> y cada α de 0.1 a 0.9: α·rank(Exp_0) + (1−α)·rank(w). Cada "
        "ensamble se compara contra Exp_0 y contra el peso solo, con Holm sobre los 9 α. H2 se decide "
        "con el mejor peso y al final dice <b>H2 se ACEPTA</b> o <b>NO</b>. Ojo: el mejor peso se elige "
        "en el mismo mes que se evalúa, así que H2 está sesgada a favor.",
        "<b>Bordes</b>: para cada rama, en cuántas de sus 10 corridas el hiperparámetro óptimo quedó "
        "contra el borde del rango."])

s += [P("8. Fase 2 (solo si H1 se acepta)", h2),
      P("Si algún w &lt; 1 ganó, el análisis termina la familia 1 con una línea así:")]
s += cod("Fase 2: w = 0.5 gana con p corregido < 0.05 -> correr TE_w0.40 y TE_w0.60")
s += [P("Esos dos pesos (el mejor ± 0.1) se corren como cualquier rama, con los pasos 2, 4 y 5: uno "
        "por VM (en dos VMs a la vez, o uno después del otro). Cambiá el peso por el que dijo el análisis:")]
s += cod("cd ~/dmeyf2026/target_engineering\n"
         "OMP_NUM_THREADS=8 nohup Rscript te_nucleo.R 0.40 > ~/dmeyf/exp/TE_w0.40.log 2>&1 &")
s += [P(f"En el nombre del log usá el peso con dos decimales ({C('TE_w0.40.log')}), igual que la "
        f"carpeta que crea el script ({C('~/dmeyf/exp/TE_w0.40/')}). Si el mejor fue 0, la fase 2 es "
        "solo 0.10. Juntalas como en el paso 6.")]
s += [P("Cuando terminen, volvé a correr el análisis del paso 7. Los pesos nuevos se detectan solos: "
        "aparece la tabla <b>Fase 2</b> (contra Exp_0, con Holm sobre esos 2 pesos) y entran también "
        "en los ensambles. Si H1 no se acepta, no hay fase 2: pasá al paso 9.")]

s += [KeepTogether([P("9. Compartir los resultados", h2)] +
      cod("cd ~/dmeyf/exp && tar czf resultados_TE.tar.gz TE_w* TE_analisis*\n"
          "gcloud storage cp resultados_TE.tar.gz gs://BUCKET/") +
      [P("Bajalo desde la consola (<i>Cloud Storage</i> → tu bucket → el archivo → <i>Descargar</i>) y "
         "compartilo con el grupo. Cuando ya no corra nada, <b>apagá las VMs</b>.")])]

doc = SimpleDocTemplate(sys.argv[1], pagesize=A4, leftMargin=20 * mm, rightMargin=20 * mm,
                        topMargin=14 * mm, bottomMargin=18 * mm,
                        title="Target Engineering: correr las ramas", author="Grupo B")
doc.build(s, onFirstPage=pie, onLaterPages=pie)
