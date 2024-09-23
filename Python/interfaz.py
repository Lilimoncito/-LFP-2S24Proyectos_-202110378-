import tkinter as tk
import graphviz
from tkinter import scrolledtext, filedialog, Menu
import webbrowser
import subprocess

class Analizador_App:

    def __init__(self,root):
        self.root = root
        self.root.title("Analizador_Lexico")

        self.frame_info = tk.Frame(self.root)
        self.frame_info.pack(side=tk.LEFT, padx=10)
        #cuadro de texto para pegar el texto
        self.cuadro_texto = scrolledtext.ScrolledText(self.root, wrap = tk.WORD, width = 50, height = 10 )
        self.cuadro_texto.pack(pady=10)
        #boton analizar
        self.boton_analizar = tk.Button(self.root, text = "analizar", command= self.analizar_texto)
        self.boton_analizar.pack(pady = 10)
         # Crear la barra de menú
        self.menu_bar = Menu(self.root, tearoff=0)
        self.root.config(menu=self.menu_bar)

        # Menú "Archivo"
        archivo_menu = Menu(self.menu_bar, tearoff=0)
        self.menu_bar.add_cascade(label="Menu", menu=archivo_menu)
        self.menu_bar.add_command(label = 'Acerca de', command=self.abrir_ventana_texto)
        self.menu_bar.add_command(label = 'Salir', command= self.salir_aplicacion)
        archivo_menu.add_command(label="Abrir archivo", command=self.abrir_archivo)
        archivo_menu.add_command(label="Guardar", command=self.guardar_archivo)
        archivo_menu.add_command(label="Guardar como", command=self.guardar_como_archivo)
        archivo_menu.add_command(label="Abrir archivo", command=self.abrir_archivo)

        self.filename = None
        self.label_imagen_sat = None
        self.label_info_sat = None
        self.label_grafica = None
    def salir_aplicacion(self):
        self.root.quit()
    def abrir_ventana_texto(self):
        ventana_texto = tk.Toplevel(self.root)
        ventana_texto.title("Datos del estudiante")

        texto = "Datos del estudiante: \n Nombre:Cesar Armando Garcia Franco \n Carnet:202110378 \n Curso: Lenguajes Formales de Programacion Seccion B+"

        # Agregar un Label con el texto
        label_texto = tk.Label(ventana_texto, text=texto, padx=10, pady=10)
        label_texto.pack()

        # Agregar un botón para cerrar la ventana
        boton_cerrar = tk.Button(ventana_texto, text="Cerrar", command=ventana_texto.destroy)
        boton_cerrar.pack(pady=10)
    def analizar_texto(self):
        texto = self.cuadro_texto.get("1.0", tk.END).strip()
        print (f"texto a analizar:" , texto)

        with open("entrada.txt", "w") as f:
            f.write(texto)

        try:
            result = subprocess.run([r"C:\Users\cesar\OneDrive\Documentos\Fortran\Proyecto1_202110378\Fortran\Main.exe", "entrada.txt"], capture_output=True, text = True)
            print (f"salida del analizador: {result.stdout}")
            print (f"errores: {result.stderr}")
            self.generar_grafica()
            self.mostrar_imagen(r"C:\Users\cesar\OneDrive\Documentos\Fortran\Proyecto1_202110378\grafica.png")
            webbrowser.open(r"C:\Users\cesar\OneDrive\Documentos\Fortran\Proyecto1_202110378\tokens.html")
            self.leer_datos_pais()
        except FileNotFoundError:
            print("no se encontro el archivo ejecutable")
            webbrowser.open(r"C:\Users\cesar\OneDrive\Documentos\Fortran\Proyecto1_202110378\errores_tokens.html")

    def abrir_archivo(self):
        # Abrir un cuadro de diálogo para seleccionar un archivo .org
        archivo = filedialog.askopenfilename(title="Abrir archivo", filetypes=[("Archivos .org", "*.org")])
        if archivo:
            with open(archivo, "r") as f:
                contenido = f.read()
                self.cuadro_texto.delete("1.0", tk.END)  # Limpiar el cuadro de texto
                self.cuadro_texto.insert(tk.END, contenido)  # Insertar el contenido del archivo

    def guardar_archivo(self):
        if self.filename:  # Si ya hay un archivo abierto
            with open(self.filename, 'w') as f:
                f.write(self.cuadro_texto.get("1.0", tk.END))
        else:
            self.guardar_como_archivo()  # Llama a guardar como si no hay archivo

    def guardar_como_archivo(self):
        archivo = filedialog.asksaveasfilename(defaultextension=".org", filetypes=[("Archivos .org", "*.org"), ("Todos los archivos", "*.*")])
        if archivo:
            with open(archivo, 'w') as f:
                f.write(self.cuadro_texto.get("1.0", tk.END))
            self.filename = archivo  # Actualizar el nombre del archivo actual
    
    def generar_grafica(self):
        doth_path = r"C:\Users\cesar\OneDrive\Documentos\Fortran\Proyecto1_202110378\grafica.dot"
        grafica = graphviz.Source.from_file(doth_path)
        grafica.render('grafica', format = 'png', cleanup=True)

    def mostrar_imagen(self, ruta_imagen):
        # Cargar la imagen 
        self.imagen_grafica = tk.PhotoImage(file = ruta_imagen)
        

    # Obtener dimensiones originales
        original_width = self.imagen_grafica.width()
        original_height = self.imagen_grafica.height()

        # Definir el tamaño deseado
        desired_width = 500  # Ajusta este valor según tus necesidades
        desired_height = 500  # Ajusta este valor según tus necesidades

        # Calcular el factor de escalado
        scale_x = desired_width / original_width
        scale_y = desired_height / original_height
        scale = min(scale_x, scale_y)

        # Redimensionar la imagen
        new_width = int(original_width * scale)
        new_height = int(original_height * scale)
        self.imagen_grafica = self.imagen_grafica.subsample(int(original_width / new_width), int(original_height / new_height))
        if self.label_grafica is None:
            self.label_grafica = tk.Label(self.root, image=self.imagen_grafica)
            self.label_grafica.pack(side=tk.RIGHT, padx=10)  # Muestra la gráfica a la derecha
        else:
            self.label_grafica.config(image=self.imagen_grafica)
            self.label_grafica.image = self.imagen_grafica

    def leer_datos_pais(self):
        # Leer el archivo de salida generado por Fortran
        archivo_salida = r"C:\Users\cesar\OneDrive\Documentos\Fortran\Proyecto1_202110378\datos_saturacion.txt" 
        nombre_pais = ""
        saturacion = ""
        bandera = ""

        # Leer los datos del archivo
        with open(archivo_salida, "r") as f:
            for line in f:
                if "Nombre del pais:" in line:
                    nombre_pais = line.split(":")[1].strip()
                elif "Saturacion:" in line:
                    saturacion = line.split(":")[1].strip()
                elif "Bandera:" in line:
                    bandera = line.split(":",1)[1].strip()
                    bandera = bandera.replace("C:", r"C:\Users\cesar\OneDrive\Documentos\Fortran\Proyecto1_202110378")
                    print (f'{bandera}')

        # Mostrar los datos en la ventana
        self.mostrar_info_pais(nombre_pais, saturacion)
        self.mostrar_pais_menos_saturado_imagen(bandera)
    def mostrar_info_pais(self, nombre, saturacion):
         # Crear o actualizar el label con la información del país
        if self.label_info_sat is None:
            self.label_info_sat = tk.Label(self.root, text=f"Nombre del país: {nombre}\nSaturación: {saturacion}")
            self.label_info_sat.pack(pady=10)
        else:
            self.label_info_sat.config(text=f"Nombre del país: {nombre}\nSaturación: {saturacion}")
    def mostrar_pais_menos_saturado_imagen(self, ruta_imagen):
         self.imagen_tk = tk.PhotoImage(file=ruta_imagen)

         original_width = self.imagen_tk.width()
         original_height = self.imagen_tk.height()
         # Definir el tamaño deseado
         desired_width = 200  # Ajusta este valor según tus necesidades
         desired_height = 200  # Ajusta este valor según tus necesidades

        # Calcular el factor de escalado
         scale_x = desired_width / original_width
         scale_y = desired_height / original_height
         scale = min(scale_x, scale_y)

        # Redimensionar la imagen
         new_width = int(original_width * scale)
         new_height = int(original_height * scale)
         self.imagen_tk = self.imagen_tk.subsample(int(original_width / new_width), int(original_height / new_height))
         if  self.label_imagen_sat is None:
                self.label_imagen_sat = tk.Label(self.root, image=self.imagen_tk, width=150, height=150)
                self.label_imagen_sat.pack(pady=10)
         else:
                self.label_imagen_sat.config(image=self.imagen_tk)
                self.label_imagen_sat.image = self.imagen_tk  # Evitar que la imagen sea recolectada por el garbage collector


if __name__ == "__main__":
    root = tk.Tk()
    app = Analizador_App(root)
    root.mainloop()


