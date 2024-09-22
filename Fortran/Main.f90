module grafica_module
    use Continente_module
    implicit none
    type :: grafica
        character (len = 50) :: nombre
        integer :: num_continentes
        type(continente), dimension(:), allocatable :: continentes !lista de continentes asociados a la grafica
        contains

        procedure generar_dot
    end type grafica

    contains

    subroutine generar_dot(self, archivo_dot)
        class(grafica), intent(in) :: self
        character(len=*), intent(in) :: archivo_dot
        integer :: i,j
        character(len = 100) :: linea
        character(len = 10) :: color_nodo
        integer :: unidad
        
        !abrir el archivo para la escritura
        unidad = 10
        open(unit = unidad, file = archivo_dot, status = 'replace')

        !escribir el encabezado del archivo dot
        ! Escribir el encabezado del archivo dot
    write(unidad, '(A)') 'digraph "', trim(self%nombre), '" {'
    

    ! Conectar el nombre de la gráfica con los continentes
    do i = 1, self%num_continentes
        ! Escribir la conexión del nombre de la gráfica con el continente
        write(unidad, '(A)') '   "', trim(self%nombre), '" -> "', &
            trim(self%continentes(i)%nombre), '" [shape="box"];'

        ! Recorrer los países del continente
        do j = 1, self%continentes(i)%num_paises
            ! Determinar el color basado en la saturación
            select case(self%continentes(i)%paises(j)%saturacion)
                case(0:15)
                    color_nodo = 'white'
                case(16:30)
                    color_nodo = 'blue'
                case(31:45)
                    color_nodo = 'green'
                case(46:60)
                    color_nodo = 'yellow'
                case(61:75)
                    color_nodo = 'orange'
                case(76:100)
                    color_nodo = 'red'
                case default
                    color_nodo = 'gray'  ! En caso de error o valores fuera de rango
            end select

            ! Escribir el nodo del país con su color correspondiente
            write(unidad, '(A)') '   "', trim(self%continentes(i)%paises(j)%nombre), &
                '" [style=filled, fillcolor=', color_nodo, '];'

            ! Escribir la conexión entre el continente y el país
            write(unidad, '(A)') '   "', trim(self%continentes(i)%nombre), '" -> "', &
                trim(self%continentes(i)%paises(j)%nombre), '";'
        end do
    end do

    ! Cerrar el archivo dot
    write(unidad, '(A)') '}'
    close(unidad)
    end subroutine
end module

module Pais_module
    implicit none

    type :: pais
        character(len=50) :: nombre
        integer :: poblacion
        integer :: saturacion
        character(len=100) :: bandera
        logical :: tiene_nombre
        logical :: tiene_poblacion
        logical :: tiene_saturacion
        logical :: tiene_bandera
    end type pais

contains

end module 

module Continente_module
    use Pais_module
    implicit none
    
    type :: continente
        character(len = 50) :: nombre
        type(pais), dimension(:), allocatable :: paises !lista de paises que contiene el modulo continentes
        integer :: num_paises
        logical :: tiene_nombre
    
    end type continente
     
    
end module 

module Token_module
    implicit none

    type :: token
        character(len=50) :: tipo
        character(len=50) :: valor
    end type token

end module Token_module

module Analizador_Lexico_module
    use Token_module
    use grafica_module
    use Continente_module
    implicit none

    type :: analizador_lexico
        type(token), allocatable :: tokens (:) !lista de tokens
        type(token), allocatable :: errores_list(:) !lista de errores
        type(grafica):: grafica
        integer :: num_tokens
        integer :: num_errores
        character(len=:), allocatable :: texto
        integer :: posicion_actual
        integer :: longitud
        logical :: en_grafica
        logical :: grafica_tiene_nombre
        logical :: en_continente
        logical :: en_pais
        logical :: existe_error_lexico
    

    contains

    procedure :: analizar
    procedure :: tokenizar_texto
    procedure :: leer_identificadores
    procedure :: leer_numeros
    procedure :: leer_cadena
    procedure :: leer_simbolos
    procedure :: generar_html
    procedure :: generar_html_errores
    end type analizador_lexico
    
contains

subroutine analizar(self, entrada)
    class(analizador_lexico), intent(inout) :: self
    character(len=*), intent(in) :: entrada
    self%texto = entrada
    self%longitud = len(trim(entrada))
    self%num_tokens = 0
    self%num_errores = 0
    self%posicion_actual = 1
    self%en_grafica = .false.
    self%en_continente = .false.
    self%grafica_tiene_nombre = .false.
    self%en_pais = .false.
    self%existe_error_lexico = .false.
    allocate(self%tokens(500)) 
    allocate(self%errores_list(500))
    allocate(self%grafica%continentes(20))
    call tokenizar_texto(self)


end subroutine

subroutine tokenizar_texto(self)
    class(analizador_lexico), intent(inout) :: self
    character(len=1) :: caracter
    integer :: i

    do while (self%posicion_actual <= self%longitud)
        i = self%posicion_actual
        caracter = self%texto(i:i)

        !ignorar espacios en blanco
        if (caracter == ' ' .or. caracter == new_line('A')) then
            self%posicion_actual = i+1
            cycle
        end if

        !detectar identificadores (palabras reservadas etc)
        if (caracter >= 'a' .and. caracter <= 'z') then
            call leer_identificadores(self)
            cycle
        end if

        !detectar numeros enteros
        if (caracter >= '0' .and. caracter <= '9') then
            call leer_numeros (self)
            cycle
        end if

        !detectar cadenas entre comillas
        if (caracter == '"') then
            call leer_cadena (self)
            cycle
        end if

        !detectar simbolos especiales
        if (caracter == '{' .or. caracter == '}' .or. caracter == ':' .or. caracter == ';')then
            call leer_simbolos(self,caracter)
            cycle
        end if

        !si se llega aqui, el caracter no fue reconocido
        print*, "el caracter no fue reconocido", caracter
        self%posicion_actual = i+1
    end do



end subroutine
!subrutina para obtener el nombre de la grafica

!subrutina para leer identificadores
subroutine leer_identificadores (self)
    class(analizador_lexico), intent(inout) :: self
    character (len=1) :: caracter
    character (len=50) :: lexema
    integer :: i

    lexema = ''
    i = self%posicion_actual
    caracter = self%texto(i:i)

    !leer letras y guines bajos
    do while((caracter >= 'a' .and. caracter <= 'z') .or.(caracter >= 'A' .and. caracter <= 'Z').or. caracter == '_')
        print*, caracter
        lexema = trim(lexema) // caracter
        i = i+1
        if(i > self%longitud) exit
        caracter = self%texto(i:i)

    end do

    if(lexema == "grafica") then
        self%en_grafica = .true.
        call agregar_token (self, 'IDENTIFICADOR', lexema)
        self%posicion_actual = i
    elseif (lexema == 'nombre')then
        call agregar_token (self, 'IDENTIFICADOR', lexema)
        self%posicion_actual = i
    elseif (lexema == 'continente') then
        self%en_continente = .true.
        self%grafica%num_continentes = self%grafica%num_continentes+1
        self%grafica%continentes(self%grafica%num_continentes)%nombre = ''
        self%grafica%continentes(self%grafica%num_continentes)%tiene_nombre = .false.
        call agregar_token (self, 'IDENTIFICADOR', lexema)
        self%posicion_actual = i
    elseif (lexema == 'pais') then
        self%en_pais = .true.
        self%en_continente = .false.
        
        if(self%grafica%continentes(self%grafica%num_continentes)%num_paises == 0) then
            allocate(self%grafica%continentes(self%grafica%num_continentes)&
                %paises(20))
        end if

        self%grafica%continentes(self%grafica%num_continentes)%num_paises = & 
            self%grafica%continentes(self%grafica%num_continentes)%num_paises+1 
        
        self%grafica%continentes(self%grafica%num_continentes)%paises &
            (self%grafica%continentes(self%grafica%num_continentes)%num_paises)%nombre = ''
        self%grafica%continentes(self%grafica%num_continentes)%paises & 
            (self%grafica%continentes(self%grafica%num_continentes)%num_paises)%tiene_nombre = .false.
        self%grafica%continentes(self%grafica%num_continentes)%paises &
            (self%grafica%continentes(self%grafica%num_continentes)%num_paises)%poblacion = 0
        self%grafica%continentes(self%grafica%num_continentes)%paises &
            (self%grafica%continentes(self%grafica%num_continentes)%num_paises)%tiene_poblacion = .false.
        self%grafica%continentes(self%grafica%num_continentes)%paises &
            (self%grafica%continentes(self%grafica%num_continentes)%num_paises)%saturacion = 0
        self%grafica%continentes(self%grafica%num_continentes)%paises &
            (self%grafica%continentes(self%grafica%num_continentes)%num_paises)%tiene_saturacion = .false.
        self%grafica%continentes(self%grafica%num_continentes)%paises &
            (self%grafica%continentes(self%grafica%num_continentes)%num_paises)%bandera = ''
        self%grafica%continentes(self%grafica%num_continentes)%paises &
            (self%grafica%continentes(self%grafica%num_continentes)%num_paises)%tiene_bandera = .false.

        call agregar_token (self, 'IDENTIFICADOR' ,lexema)
        self%posicion_actual = i

    elseif (lexema == 'poblacion')then
        call agregar_token (self, 'IDENTIFICADOR', lexema)
        self%posicion_actual = i
    
    elseif (lexema == 'saturacion')then
        call agregar_token (self, 'IDENTIFICADOR', lexema)
        self%posicion_actual = i

    elseif (lexema == 'bandera')then
        call agregar_token (self, 'IDENTIFICADOR', lexema)
        self%posicion_actual = i
    else
        self%existe_error_lexico = .true.
        call agregar_error (self, 'LEXICO', lexema)
        self%posicion_actual = i

    end if


end subroutine

! subrutina para leer numeros enteros
subroutine leer_numeros(self)
    class(analizador_lexico), intent(inout) :: self
    character (len=1) :: caracter
    character (len=50) :: numero
    integer :: i, numero_entero

    numero = ''
    i = self%posicion_actual
    caracter = self%texto(i:i)

    !leer numeros
    do while(caracter >= '0' .and. caracter <= '9')
        print*, caracter
        numero = trim(numero) // caracter
        i = i+1

        if (i > self%longitud) exit
        caracter = self%texto(i:i)

    end do
    read (numero, *) numero_entero

    if (self%en_pais .and. .not. self%grafica%continentes(self%grafica%num_continentes) & 
            %paises(self%grafica%continentes(self%grafica%num_continentes)%num_paises)%tiene_poblacion) then
        
        self%grafica%continentes(self%grafica%num_continentes) & 
            %paises(self%grafica%continentes(self%grafica%num_continentes)%num_paises)%poblacion = numero_entero
    
        self%grafica%continentes(self%grafica%num_continentes) & 
            %paises(self%grafica%continentes(self%grafica%num_continentes)%num_paises)%tiene_poblacion = .true.

    elseif (self%en_pais .and. .not. self%grafica%continentes(self%grafica%num_continentes) & 
            %paises(self%grafica%continentes(self%grafica%num_continentes)%num_paises)%tiene_saturacion) then
        
        self%grafica%continentes(self%grafica%num_continentes) & 
            %paises(self%grafica%continentes(self%grafica%num_continentes)%num_paises)%saturacion = numero_entero
    
        self%grafica%continentes(self%grafica%num_continentes) & 
            %paises(self%grafica%continentes(self%grafica%num_continentes)%num_paises)%tiene_saturacion = .true.
        print*, self%texto(i:i)
        if (self%texto(i:i) == '%')then
            caracter = self%texto(i:i)
            call agregar_token(self, 'porcentaje',caracter)
            i = i+1
            print*, 'se agrego un porcentaje'
        end if
    end if
    !agregar el token tipo numero
    call agregar_token(self, "NUMERO", numero)
    self%posicion_actual = i
end subroutine

!subrutina para leer cadenas
subroutine leer_cadena(self)
    class(analizador_lexico), intent(inout) :: self
    character (len = 1) :: caracter
    character (len = 50) :: cadena
    integer :: i

    cadena = ''
    i = self%posicion_actual+1
    caracter = self%texto(i:i)

    !leer cadena
    do while (caracter /= '"')
        print*, caracter
        cadena = trim(cadena) // caracter
        i = i+1

        if(i > self%longitud) exit
        caracter = self%texto(i:i)
    end do
     ! Determinar si la cadena es para la gráfica o para el continente
    if (.not. self%grafica_tiene_nombre .and. self%en_grafica) then
        self%grafica%nombre = cadena
        self%grafica%num_continentes = 0
        self%grafica_tiene_nombre = .true.
        print*, 'lexema:', cadena, 'pertenece a grafica'
    else if (self%en_continente .and. .not. self%grafica%continentes(self%grafica%num_continentes)%tiene_nombre) then
        self%grafica%continentes(self%grafica%num_continentes)%nombre = cadena
        self%grafica%continentes(self%grafica%num_continentes)%tiene_nombre = .true.
        print*, 'lexema:', cadena, 'pertenece a continente', self%grafica%num_continentes

    !si no se cumple una de las anteriores, entonces estamos en pais, o encontramos un error lexico verificamos si ya tiene nombre o si ya tiene bandera
    
    else if (self%en_pais .and. .not. self%grafica%continentes(self%grafica%num_continentes) & 
                %paises(self%grafica%continentes(self%grafica%num_continentes)%num_paises)%tiene_nombre ) then

        self%grafica%continentes(self%grafica%num_continentes) & 
            %paises(self%grafica%continentes(self%grafica%num_continentes)%num_paises)%nombre = cadena

        self%grafica%continentes(self%grafica%num_continentes) & 
            %paises(self%grafica%continentes(self%grafica%num_continentes)%num_paises)%tiene_nombre = .true.
        print*, 'lexema:', cadena, 'pertenece a pais_nombre'
    else if (self%en_pais .and. .not. self%grafica%continentes(self%grafica%num_continentes) & 
                %paises(self%grafica%continentes(self%grafica%num_continentes)%num_paises)%tiene_bandera) then
        
        self%grafica%continentes(self%grafica%num_continentes) & 
            %paises(self%grafica%continentes(self%grafica%num_continentes)%num_paises)%bandera = cadena

        self%grafica%continentes(self%grafica%num_continentes) & 
            %paises(self%grafica%continentes(self%grafica%num_continentes)%num_paises)%tiene_bandera = .true.
        print*, 'lexema:', cadena, 'pertenece a pais_bandera'
        
    end if
    !agregar el token
    call agregar_token (self, "CADENA", cadena)
    self%posicion_actual = i+1 !nos saltamos la ultima comilla
end subroutine

!subrutina para leer simbolos especiales
subroutine leer_simbolos(self, simbolo)
    class(analizador_lexico), intent(inout) :: self
    character (len = 1), intent(in) :: simbolo
    print*, simbolo
    !agregar el token
    call agregar_token(self, "SIMBOLO", simbolo)
    self%posicion_actual = self%posicion_actual+1

end subroutine
!subrutina para agregar tokens
subroutine agregar_token(self, tipo, valor)

    class(analizador_lexico), intent(inout) :: self
    character(len=*), intent(in) :: tipo , valor
    
    self%num_tokens = self%num_tokens +1
    if (self%num_tokens > size(self%tokens)) then
        print*, 'has alcanzado la capacidad macima de tokens permitidos'
        return
    end if

    self%tokens(self%num_tokens)%tipo = trim(tipo)
    self%tokens(self%num_tokens)%valor = trim(valor)
    print*, 'Token agregado', trim(tipo) ,':' ,trim(valor)
    
end subroutine
!subrutina para agregar errores
subroutine agregar_error(self, tipo, valor)
    class(analizador_lexico), intent(inout) :: self
    character (len=*), intent(in) :: tipo, valor

    self%num_errores = self%num_errores + 1

    if (self%num_errores > size(self%errores_list)) then
        print*, 'Has alcanzado la capacidad maxima de errores permitidos'
        return
    end if

    self%errores_list(self%num_errores)%tipo = trim(tipo)
    self%errores_list(self%num_errores)%valor = trim(valor)
    print*, 'Error encontrado: ', trim(tipo), ': ', trim(valor)

end subroutine

subroutine generar_html(self, archivo_html)
    class(analizador_lexico), intent(in) :: self
    character(len=*), intent(in) :: archivo_html
    integer :: unidad, i

    ! Abrir el archivo HTML para escritura
    open(unit = unidad, file = archivo_html, status = 'replace')

    ! Escribir el encabezado del HTML
    write(unidad, *) '<html>'
    write(unidad, *) '<head><title>Tokens</title></head>'
    write(unidad, *) '<body>'
    write(unidad, *) '<h1>Lista de Tokens</h1>'
    write(unidad, *) '<table border="1">'
    write(unidad, *) '<tr><th>Tipo</th><th>Nombre</th><th>Valor</th></tr>'  ! Encabezado actualizado

    ! Escribir cada token en una fila de la tabla
    do i = 1, self%num_tokens
        write(unidad, *) '<tr><td>', trim('Token'), '</td><td>', trim(self%tokens(i)%tipo), &
            '</td><td>', trim(self%tokens(i)%valor), '</td></tr>'
    end do

    ! Cerrar la tabla y el documento HTML
    write(unidad, *) '</table>'
    write(unidad, *) '</body>'
    write(unidad, *) '</html>'

    ! Cerrar el archivo
    close(unidad)
end subroutine

subroutine generar_html_errores(self, archivo_html)
    class(analizador_lexico), intent(in) :: self
    character(len=*), intent(in) :: archivo_html
    integer :: unidad, i

    ! Abrir el archivo HTML para escritura
    open(unit = unidad, file = archivo_html, status = 'replace')

    ! Escribir el encabezado del HTML
    write(unidad, *) '<html>'
    write(unidad, *) '<head><title>Tokens</title></head>'
    write(unidad, *) '<body>'
    write(unidad, *) '<h1>Lista de Errores</h1>'
    write(unidad, *) '<table border="1">'
    write(unidad, *) '<tr><th>Tipo</th><th>Error</th><th>Valor</th></tr>'  ! Encabezado actualizado

    ! Escribir cada token en una fila de la tabla
    do i = 1, self%num_errores
        write(unidad, *) '<tr><td>', trim('Token'), '</td><td>', trim(self%errores_list(i)%tipo), &
            '</td><td>', trim(self%errores_list(i)%valor), '</td></tr>'
    end do

    ! Cerrar la tabla y el documento HTML
    write(unidad, *) '</table>'
    write(unidad, *) '</body>'
    write(unidad, *) '</html>'

    ! Cerrar el archivo
    close(unidad)
end subroutine


end module Analizador_Lexico_module

program proyecto_1
    use Analizador_Lexico_module
    implicit none

    type (analizador_lexico) :: lexer
    character (len=200) :: texto
    integer :: i, ios
    character (len =1000) :: entrada, linea
    integer :: unit

     ! Inicializar el archivo de entrada
     ! Inicializar el archivo de entrada
    unit = 10
    open(unit=unit, file="entrada.txt", status="old", action="read", iostat=ios)
    if (ios /= 0) then
        print*, "Error al abrir el archivo de entrada."
        stop
    end if

    ! Leer el contenido del archivo
    entrada = ''
    do
        read(unit, '(A)', iostat=ios) linea
        if (ios /= 0) exit   ! Se alcanzó el fin del archivo
        entrada = trim(entrada) // trim(linea) // char(10) ! Concatenar la línea leída al valor de entrada y agregar un salto de línea
    end do
    close(unit)

    ! Llamada al analizador léxico con el texto leído
    
    ! Mostrar el contenido leído
    print*, "Texto leido desde el archivo:"
    print*, trim(entrada)

    ! Llamada al analizador léxico con el texto leído
    call lexer%analizar(trim(entrada))
    if(.not. lexer%existe_error_lexico) then
        call lexer%grafica%generar_dot('grafica.dot')

        
    end if
    call lexer%generar_html('tokens.html')
    call lexer%generar_html_errores('errores_tokens.html')
    ! Mostrar el número de tokens encontrados
    print*, 'Tokens encontrados:', lexer%num_tokens
    do i = 1, lexer%num_tokens
        print*, 'Tipo: ', lexer%tokens(i)%tipo, 'valor: ', lexer%tokens(i)%valor
    end do
    print*, 'errores_encontrados:', lexer%num_errores
    do i = 1, lexer%num_errores
        print*, 'Tipo: ', lexer%errores_list(i)%tipo, 'valor: ', lexer%errores_list(i)%valor
    end do
end program