; ANALISIS SEMANTICO
;-------------------
; Buffet --> En el ambito de este programa hace referencia a una serie de condiciones u opciones que le permitirana
;            a un grupo de personas el acceder a informacion sobre una pequeña empresa expendedora de herramientas
;            para el estudio.

; CODIGO
;-------

; DATOS
;----------------------------------------------------------------------------------------------------
(define PapeleriaRefV  (vector 001 002 003))                                                       ;|
(define PapeleriaProdV (vector "Lapices" "Cuadernos" "Reglas"))                                    ;|
(define PapeleriaPrecV (vector 500 1000 200))                                                      ;|
(define PapeleriaCantV (vector 100 200 30))                                                        ;|
(define PapeleriaMinV  (vector 40 100 5))                                                          ;|
(define PapeleriaMaxV  (vector 100 200 30))                                                        ;|
(define PapeleriaEstadoV (vector 1 1 1)) ; 1 quiere decir que el prod está activo, 0 está "borrado" |
(define PapeleriaLoginV (vector "omar" "Felipe"))                                                  ;|
(define PapeleriaPasswd (vector "lissa" "luisa"))                                                  ;|
;----------------------------------------------------------------------------------------------------

; NIVEL GERENTE-EMPLEADO
;------------------------
; funcion principal
 (define(main)
   (display "BIENVENIDO
===========
Nivel Gerente:
---------------
1. Consultar Productos
2. Vender productos
3. Modificar Datos
4. Borrar Datos
5. Listar

Nivel Empleado:
----------------
1. Consultar Productos
2. Vender productos
3. Listar
")
)


; funcion para login
(define(login o name pass  v_r v_pro v_pre v_c v_m v_max v_es v_lo v_p)
  (display "Ingresa tu nombre de usuario: ")(set! name(read)) 
  (display "Ingresa tu contraseña: ")(set! pass(read))
  (newline)
  (cond
    ((= o 1) ; solo se cumple este trozo de codigo si se quiere modificar el nombre del producto
     (if(and(string=? (vector-ref v_lo 0) name)(string=? (vector-ref v_p 0) pass)) ; compara nombre y contraseña del primer indicide del vector
        (modi_nom_pro 0  v_r v_pro v_pre v_c v_m v_max v_es v_lo v_p)
        (begin
          (display "Nombre de usuario o contraseña incorrecta.")
          (newline)
          (login o 0 0  v_r v_pro v_pre v_c v_m v_max v_es v_lo v_p)
          )
        )
     )
    
   
  ((= o 0) ; solo se cumple este trozo de codigo si se quiere modificar el precio del producto
     (if(and(string=? (vector-ref v_lo 0) name)(string=? (vector-ref v_p 0) pass)) ; compara nombre y contraseña del primer indicide del vector
        (modi_pre_pro 0  v_r v_pro v_pre v_c v_m v_max v_es v_lo v_p)
        (begin
          (display "Nombre de usuario o contraseña incorrecta.")
          (newline)
          (login o 0 0  v_r v_pro v_pre v_c v_m v_max v_es v_lo v_p)
          )
        )
     )
    
    (else
     (begin
       (display "Nombre de usuario o contraseña incorrecta.")
       (newline)
       (login o 0 0  v_r v_pro v_pre v_c v_m v_max v_es v_lo v_p)
       )
     )
    )
  )


; funcion que recibe peticion de usuario
 (define(peticion_o o v_r v_pro v_pre v_c v_m v_max v_es v_lo v_p)
   (display "Opcion: ")(set! o(read))
   (newline)
   (cond
     ((= o 1)
      (cons_produc 0 0 v_r v_pro v_pre v_c v_m v_max v_es v_lo v_p)
      )
     ((= o 2)
      (ven_producto 0 0 v_r v_pro v_pre v_c v_m v_max v_es v_lo v_p)
      )
     ((= o 3)
      (modificar 0 v_r v_pro v_pre v_c v_m v_max v_es v_lo v_p)
      )
     ((= o 4)
      (datos_productos 0 0 v_r v_pro v_pre v_c v_m v_max v_es v_lo v_p)
      )
     ((= o 5)
      (listar 0 v_r v_pro v_pre v_c v_m v_max v_es v_lo v_p)
      )
     (else
      (begin
        (display "Opcion no validad.")
        (newline)
        (peticion_o 0 v_r v_pro v_pre v_c v_m v_max v_es v_lo v_p)
        )
      )
     )
   )


; Consultar productos
;--------------------
; Menu interactivo 1
 (define(cons_produc r o v_r v_pro v_pre v_c v_m v_max v_es v_lo v_p)
   (display "1.1 Existencia.
1.2 Cantidad mínima.
1.3 Dinero en inventario.")
   (newline)
   (newline)
   
   (display "Opcion: ")(set! o(read))
   
   (cond    
    ((= o 1.1)
        (begin
          (display "Referencia: ")(set! r(read))
          (newline)
          (exist_prod r v_r v_pro v_pre v_c v_m v_max v_es v_lo v_p)
          )
        )
     ((= o 1.2)
      (newline)
      (ctdd_min 0 v_r v_pro v_pre v_c v_m v_max v_es v_lo v_p)
      )
     ((= o 1.3)
      (begin
        (newline)
        (display "Dinero en inventario: ")
        (display(din_inv 0 v_pre v_c))
        (newline)
        (newline)
        (main)
        (peticion_o 0 v_r v_pro v_pre v_c v_m v_max v_es v_lo v_p)
        )
      )
     (else
      (begin
        (newline)
        (display "Opcion no validad.")
        (newline)
        (newline)
        (cons_produc 0 0 v_r v_pro v_pre v_c v_m v_max v_es v_lo v_p)
        )
      )
     )
   )

; Existencia
 (define(exist_prod r v_r v_pro v_pre v_c v_m v_max v_es v_lo v_p)
   (cond
     ((= r (vector-ref v_r 0))
      (begin
        (display "La cantidad de existente en el producto es : ")
        (display (vector-ref v_c 0))(display " unidades.")
        (newline)
        (newline)
        (main)
        (peticion_o 0 v_r v_pro v_pre v_c v_m v_max v_es v_lo v_p)
        )
      )
     ((= r (vector-ref v_r 1))
      (begin
        (display "La cantidad de existente en el producto es : ")
        (display (vector-ref v_c 1))(display " unidades.")
        (newline)
        (newline)
        (main)
        (peticion_o 0 v_r v_pro v_pre v_c v_m v_max v_es v_lo v_p)
        )
      )
     ((= r (vector-ref v_r 2))
      (begin
        (display "La cantidad de existente en el producto es :")
        (display (vector-ref v_c 2))(display " unidades.")
        (newline)
        (newline)
        (main)
        (peticion_o 0 v_r v_pro v_pre v_c v_m v_max v_es v_lo v_p)
        )
      )
     )
   )
     

; Cantidad minima
 (define(ctdd_min ind v_r v_pro v_pre v_c v_m v_max v_es v_lo v_p)
   (if(= ind 2)
      (begin
        (display (vector-ref v_pro ind))(display ": ")
        (display (vector-ref v_m ind))
        (newline)
        (newline)
        (main)
        (peticion_o 0 v_r v_pro v_pre v_c v_m v_max v_es v_lo v_p)
        )
      (begin
        (display (vector-ref v_pro ind))(display ": ")
        (display (vector-ref v_m ind))
        (newline)
        (ctdd_min (+ ind 1) v_r v_pro v_pre v_c v_m v_max v_es v_lo v_p)
        )
      )
   )


; Dinero en inventario
 (define(din_inv ind v_c v_p)
   (if(> ind 2)
      0
      (+ (* (vector-ref v_c ind)(vector-ref v_p ind)) (din_inv (+ ind 1) v_c v_p))
      )
   )

   
; Venta de productos
;-------------------
 (define(ven_producto r c v_r v_pro v_pre v_c v_m v_max v_es v_lo v_p)
   (display "Referencia: ")(set! r(read))
   (display "Cantidad: ")(set! c(read))

   (newline)

   (cond
     ((and(= r 001))
      (if(= (vector-ref v_es 0) 1)
        (if(<= (vector-ref v_c 0)(vector-ref v_m 0))
           (begin
             (display "La cantidad que queda es menor o igual a la cantidad minima.")
             (newline)
             (ven_producto 0 0 v_r v_pro v_pre v_c v_m v_max v_es v_lo v_p)
             )
             (if(<= c (vector-ref v_c 0))
                (begin
                  (newline)
                  (venta_positiva 0 c v_r v_pro v_pre v_c v_m v_max v_es v_lo v_p)
                  )
                (begin
                  (display "La venta no se puede realizar debido a que la cantidad solicitada
excede la existente.")
                  (newline)
                  (newline)
                  (main)
                  (newline)
                  (peticion_o 0 v_r v_pro v_pre v_c v_m v_max v_es v_lo v_p)
                  )
                )
             )
        (begin
          (display "Producto inactivo: ")
          (newline)
          (newline)
          (ven_producto 0 0 v_r v_pro v_pre v_c v_m v_max v_es v_lo v_p)
          )
        )
      )

     ((and(= r 002))
      (if(= (vector-ref v_es 1) 1)
        (if(<= (vector-ref v_c 1)(vector-ref v_m 1))
           (begin
             (display "La cantidad que queda es menor o igual a la cantidad minima.")
             (newline)
             (ven_producto 0 0 v_r v_pro v_pre v_c v_m v_max v_es v_lo v_p)
             )
             (if(<= c (vector-ref v_c 1))
                (begin
                  (newline)
                  (venta_positiva 1 c v_r v_pro v_pre v_c v_m v_max v_es v_lo v_p)
                  )
                (begin
                  (display "La venta no se puede realizar debido a que la cantidad solicitada
excede la existente.")
                  (newline)
                  (newline)
                  (main)
                  (newline)
                  (peticion_o 0 v_r v_pro v_pre v_c v_m v_max v_es v_lo v_p)
                  )
                )
             )
        (begin
          (display "Producto inactivo: ")
          (newline)
          (newline)
          (ven_producto 0 0 v_r v_pro v_pre v_c v_m v_max v_es v_lo v_p)
          )
        )
      )

     ((and(= r 003))
      (if(= (vector-ref v_es 2) 1)
        (if(<= (vector-ref v_c 2)(vector-ref v_m 2))
           (begin
             (display "La cantidad que queda es menor o igual a la cantidad minima.")
             (newline)
             (ven_producto 0 0 v_r v_pro v_pre v_c v_m v_max v_es v_lo v_p)
             )
             (if(<= c (vector-ref v_c 2))
                (begin
                  (newline)
                  (venta_positiva 2 c v_r v_pro v_pre v_c v_m v_max v_es v_lo v_p)
                  )
                (begin
                  (display "La venta no se puede realizar debido a que la cantidad solicitada
excede la existente.")
                  (newline)
                  (newline)
                  (main)
                  (newline)
                  (peticion_o 0 v_r v_pro v_pre v_c v_m v_max v_es v_lo v_p)
                  )
                )
             )
        (begin
          (display "Producto inactivo: ")
          (newline)
          (newline)
          (ven_producto 0 0 v_r v_pro v_pre v_c v_m v_max v_es v_lo v_p)
          )
        )
      )

     (else
      (begin
        (display "Referencia no encontrada.")
        (newline)
        (newline)
        (ven_producto 0 0 v_r v_pro v_pre v_c v_m v_max v_es v_lo v_p)
        )
      )
     )
   )


; funcion para realizar proceso de venta en caso de cumplir todas las condiciones una peticion
 (define(venta_positiva ind c v_r v_pro v_pre v_c v_m v_max v_es v_lo v_p)
   (display "La venta se ha realizado con exito.")
   (newline)

   (vector-set! v_c ind (- (vector-ref v_c ind) c))

   (newline)
   (newline)
   (main)
   (newline)
   (peticion_o 0 v_r v_pro v_pre v_c v_m v_max v_es v_lo v_p)
   )          
   

; Listar
;------
; funcion para listar
 (define(listar o v_r v_pro v_pre v_c v_m v_max v_es v_lo v_p)
   (display "5.1 Listado general de productos
5.2 Listado de producto con existencia mínima
5.3 Listado de productso con existencia máxima")
   
   (newline)
   (newline)

   (display "Opcion: ")(set! o(read))

   (newline)
    
   (cond
     ((= o 5.1)
      (listado_general v_r v_pro v_pre v_c v_m v_max v_es v_lo v_p)
      )
     ((= o 5.2)
      (listado_min 0 v_r v_pro v_pre v_c v_m v_max v_es v_lo v_p)
      )
     ((= o 5.3)
      (listado_max 0 v_r v_pro v_pre v_c v_m v_max v_es v_lo v_p)
      )
     (else
      (begin
        (display "Opcion no valida.")
        (newline)
        (newline)
        (listar 0 v_r v_pro v_pre v_c v_m v_max v_es v_lo v_p)
        )
      )
     )
   )


; funcion para realizar listado general
 (define(listado_general v_r v_pro v_pre v_c v_m v_max v_es v_lo v_p)
   (display "Productos disponibles: ")
   (display (vector->list v_pro))
   (newline)
   (display "Cantidad: ")
   (display (vector->list v_c))
   (newline)
   (newline)
   (main)
   (peticion_o 0 v_r v_pro v_pre v_c v_m v_max v_es v_lo v_p)
   )


; funcion para mostrar listado de productos con existencia minima
 (define(listado_min ind v_r v_pro v_pre v_c v_m v_max v_es v_lo v_p)
   (if(> ind 2)
      (begin
        (newline)
        (main)
        (newline)
        (peticion_o 0 v_r v_pro v_pre v_c v_m v_max v_es v_lo v_p)
        )
      (if(= (vector-ref v_c ind)(vector-ref v_m ind))
         (begin
           (display (vector-ref v_pro ind))(display ": ")
           (display (vector-ref v_m))
           (newline)
           (listado_min (+ ind 1) v_r v_pro v_pre v_c v_m v_max v_es v_lo v_p)
           )
         (listado_min (+ ind 1) v_r v_pro v_pre v_c v_m v_max v_es v_lo v_p)
         )
      )
   )


; funcion para listar los productos con existencia maxima
(define(listado_max ind v_r v_pro v_pre v_c v_m v_max v_es v_lo v_p)
  (if(> ind 2)
     (begin
       (newline)
       (main)
       (newline)
       (peticion_o 0 v_r v_pro v_pre v_c v_m v_max v_es v_lo v_p)
       )
     (if(= (vector-ref v_c ind)(vector-ref v_max ind))
        (begin
          (display (vector-ref v_pro ind))(display ": ")
          (display (vector-ref v_c ind))
          (newline)
          (listado_max (+ ind 1) v_r v_pro v_pre v_c v_m v_max v_es v_lo v_p)
          )
        (listado_max (+ ind 1) v_r v_pro v_pre v_c v_m v_max v_es v_lo v_p)
        )
     )
  )
   

; NIVEL GERENTE
;--------------

; Modificacion de datos
;----------------------
; funcion para modificacion de datos
 (define(modificar o  v_r v_pro v_pre v_c v_m v_max v_es v_lo v_p)
   (display "3.1 Modificar Nombre de productos.
3.2 Modificar Precios de productos.")
   (newline)
   (newline)
   (display "Opcion: ")(set! o(read))
   (newline)

   (if(= o 3.1)
      (login 1 0 0  v_r v_pro v_pre v_c v_m v_max v_es v_lo v_p)
      (if(= o 3.2)
         (login 0 0 0  v_r v_pro v_pre v_c v_m v_max v_es v_lo v_p)
         (begin
           (display "Opciones incorrectas.")
           (newline)
           (modificar 0  v_r v_pro v_pre v_c v_m v_max v_es v_lo v_p)
           )
         )
      )
   )


; funcion para modificar nombre del producto
 (define(modi_nom_pro r v_r v_pro v_pre v_c v_m v_max v_es v_lo v_p)
   (display "Referencia: ")(set! r(read))
   (newline)
   (cond
     ((= r (vector-ref v_r 0))
      (begin
        (display "Nuevo nombre: ")
        (vector-set! v_pro 0 (read))
        (newline)
        (efectividad_m_v 1 v_r v_pro v_pre v_c v_m v_max v_es v_lo v_p) ; envia datos modificados a una funcion encargada
        )
      )
     ((= r (vector-ref v_r 1))
      (begin
        (display "Nuevo nombre: ")
        (vector-set! v_pro 1 (read))
        (newline)
        (efectividad_m_v 1 v_r v_pro v_pre v_c v_m v_max v_es v_lo v_p) ; de mencionar la efectividad del cambio
        )
      )
     ((= r (vector-ref v_r 2))
      (begin
        (display "Nuevo nombre: ")
        (vector-set! v_pro 2 (read))
        (newline)
        (efectividad_m_v 1 v_r v_pro v_pre v_c v_m v_max v_es v_lo v_p) ; y enviar cambios a la funcion de peticion
        )
      )
     (else
      (begin
        (newline)
        (display "Referencia no encontrada.")
        (newline)
        (modi_nom_pro 0 v_r v_pro v_pre v_c v_m v_max v_es v_lo v_p)
        )
      )
     )
   )


; funcion para modificar precio del producto
 (define(modi_pre_pro r  v_r v_pro v_pre v_c v_m v_max v_es v_lo v_p)
   (display "Referencia: ")(set! r(read))
   (cond
     ((= r (vector-ref v_r 0))
      (begin
        (display "Nuevo precio: ")
        (vector-set! v_pre 0 (read)) ; leer precio a cambiar para producto indice 0
        (newline)
        (efectividad_m_v 0  v_r v_pro v_pre v_c v_m v_max v_es v_lo v_p) ; envia datos modificados a una funcion encargada
        )
      )
     ((= r (vector-ref v_r 1))
      (begin
        (display "Nuevo precio: ")
        (vector-set! v_pre 1 (read)) ; leer precio a cambiar para producto indice 1
        (newline)
        (efectividad_m_v 0  v_r v_pro v_pre v_c v_m v_max v_es v_lo v_p) ; de mencionar la efectividad del cambio
        )
      )
     ((= r (vector-ref v_r 2))
      (begin
        (display "Nuevo precio: ")
        (vector-set! v_pre 2 (read)) ; leer precio a cambiar para producto indice 2
        (newline)
        (efectividad_m_v  0 v_r v_pro v_pre v_c v_m v_max v_es v_lo v_p) ; y enviar cambios a la funcion de peticion
        )
      )
     (else
      (begin
        (newline)
        (display "Referencia no encontrada.")
        (newline)
        (modi_nom_pro 0  v_r v_pro v_pre v_c v_m v_max v_es v_lo v_p)
        )
      )
     )
   )


; funcion para retornar la el exito de la modificacion para nombre o precio
 (define(efectividad_m_v nop v_r v_pro v_pre v_c v_m v_max v_es v_lo v_p) ; nop = nombre o precio
   (display "La modificacion ha sido exitosa.")
   (newline)
   (newline)
   (if(= nop 1)
      (begin
        (display "Productos: ")
        (display (vector->list v_pro))
        (newline)
        (newline)
        (main)
        (newline)
        (newline)
        (peticion_o 0 v_r v_pro v_pre v_c v_m v_max v_es v_lo v_p)
        )
      (begin
        (display "Precios: ")
        (display (vector->list v_pre))
        (newline)
        (newline)
        (main)
        (newline)
        (newline)
        (peticion_o 0 v_r v_pro v_pre v_c v_m v_max v_es v_lo v_p)
        )
      )
   )

        
     
; Borrar datos
;-------------

;  funcion de borrado para datos
(define(datos_productos o r v_r v_pro v_pre v_c v_m v_max v_es v_lo v_p)
  (display "4.1 Activar Estado de los productos.
4.2 Desactivar Estado de los productos.")
  (newline)
  (newline)
  (display "Opcion: ")(set! o(read))
  (newline)
  (display "Referencia: ")(set! r(read))
  (newline)
  
  (cond
    ((= o 4.1)
     (if(= r (vector-ref v_r 0))
        (begin
          (display "La modificacion ha sido exitosa: ")
          (vector-set! v_es 0 1)
          (display(vector->list v_es))
          (newline)
          (newline)
          (main)
          (newline)
          (peticion_o 0 v_r v_pro v_pre v_c v_m v_max v_es v_lo v_p)
          )
        (if(= r (vector-ref v_r 1))
           (begin
             (display "La modificacion ha sido exitosa: ")
             (vector-set! v_es 1 1)
             (display(vector->list v_es))
             (newline)
             (newline)
             (main)
             (newline)
             (peticion_o 0 v_r v_pro v_pre v_c v_m v_max v_es v_lo v_p)
             )
           (if(= r (vector-ref v_r 2))
              (begin
                (display "La modificacion ha sido exitosa: ")
                (vector-set! v_es 2 1)
                (display(vector->list v_es))
                (newline)
                (newline)
                (main)
                (newline)
                (peticion_o 0 v_r v_pro v_pre v_c v_m v_max v_es v_lo v_p)
                )
              (begin
                (display "Puede ser que la referencia no exista.")
                (newline)
                (datos_productos 0 0 v_r v_pro v_pre v_c v_m v_es v_lo v_p)
                )
              )
           )
        )
     )

    ((= o 4.2)
     (if(= r (vector-ref v_r 0))
        (begin
          (display "La modificacion ha sido exitosa: ")
          (vector-set! v_es 0 0)
          (display(vector->list v_es))
          (newline)
          (newline)
          (main)
          (newline)
          (peticion_o 0 v_r v_pro v_pre v_c v_m v_max v_es v_lo v_p)
          )
        (if(= r (vector-ref v_r 1))
           (begin
             (display "La modificacion ha sido exitosa: ")
             (vector-set! v_es 1 0)
             (display(vector->list v_es))
             (newline)
             (newline)
             (main)
             (newline)
             (peticion_o 0 v_r v_pro v_pre v_c v_m v_max v_es v_lo v_p)
             )
           (if(= r (vector-ref v_r 2))
              (begin
                (display "La modificacion ha sido exitosa: ")
                (vector-set! v_es 2 0)
                (display(vector->list v_es))
                (newline)
                (newline)
                (main)
                (newline)
                (peticion_o 0 v_r v_pro v_pre v_c v_m v_max v_es v_lo v_p)
                )
              (begin
                (display "Puede ser que la referencia no exista.")
                (newline)
                (datos_productos 0 0 v_r v_pro v_pre v_c v_m v_max v_es v_lo v_p)
                )
              )
           )
        )
     )

    (else
     (begin
       (display "Opcion invalida.")
       (newline)
       (newline)
       (datos_productos 0 0 v_r v_pro v_pre v_c v_m v_max v_es v_lo v_p)
       )
     )
    )
  )
           
    

(main)
(peticion_o 0 PapeleriaRefV PapeleriaProdV PapeleriaPrecV PapeleriaCantV PapeleriaMinV PapeleriaMaxV PapeleriaEstadoV PapeleriaLoginV PapeleriaPasswd)