import tkinter as tk
import graphviz
from tkinter import scrolledtext, filedialog, Menu
import webbrowser
import subprocess

class Analizador_App:

    def __init__(self,root):
        self.root = root
        self.root.title = ("Analizador_Lexico")
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
        archivo_menu.add_command(label="Abrir archivo", command=self.abrir_archivo)
        archivo_menu.add_command(label="Guardar", command=self.guardar_archivo)
        archivo_menu.add_command(label="Guardar como", command=self.guardar_como_archivo)
        archivo_menu.add_command(label="Abrir archivo", command=self.abrir_archivo)

        self.filename = None

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
        # Cargar la imagen usando Pillow
        self.imagen_tk = tk.PhotoImage(file = ruta_imagen)

        self.label_imagen = tk.Label(self.root, image=self.imagen_tk)
        self.label_imagen.pack(pady=10)


if __name__ == "__main__":
    root = tk.Tk()
    app = Analizador_App(root)
    root.mainloop()


