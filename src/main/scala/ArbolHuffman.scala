type Bit = 0 | 1
def cadenaAListaChars(cadena: String): List[Char] = cadena.toList
def listaCharsACadena(listaCar: List[Char]): String = listaCar.mkString
type TablaCodigos = List[(Char,List[Bit])]



abstract class ArbolHuffman {
  def peso:Int = this match
    case RamaHuffman(i,d) => i.peso + d.peso //caso Rama, el peso de la rama es el peso de las ramas/hojas que la componene
    case HojaHuffman(char, frec) => frec // caso Hoja, devuelve el valor de la frecuencia

  def caracteres:List[Char] = this match
    case RamaHuffman(i,d) => i.caracteres:::d.caracteres //caso rama, donde se devuelven los caracteres de las hojas/ramas adyacentes
    case HojaHuffman(char, frec) => List(char) // devuelve el caracter de la hoja en cuestion

  def decodificar(bits:List[Bit]):String =
    def decodificarAux(arbolAux:ArbolHuffman)(bits:List[Bit])(acum:List[Char]):List[Char] = bits match
      case Nil => arbolAux match // caso base donde la lista de bits se ha terminado de recorrer
        case HojaHuffman(char, frec) => acum :+ char //devuelve lo ya acumulado junto con el caracter de la hoja en la que está
        case RamaHuffman(i,d) => acum // devuelve el acumulado sin nada más porque las ramas no tienen caracter (estaria mal la codificacion)

      case cabeza::cola => arbolAux match //caso recursivo
        case HojaHuffman(char, frec) => decodificarAux(this)(bits)(acum:+char)// para reinciar el arbol se da el arbol con la raiz inicial,
        // la lista y el acumulado sumado al caracter de la rama a la que ha llegado
        case RamaHuffman(i,d) => decodificarAux(if (cabeza==0) then i else d)(cola)(acum) //con la rama se pone el subarbol izq o derecha
    // dependiendo del siguiente bit, se quita la cabeza de la cola y no se suma al acumulado

    listaCharsACadena(decodificarAux(this)(bits)(Nil))// llamada a auxiliar

  def caracterEnArbol(caracter: Char): Boolean =
    def caracterEnArbolAux(arbol:ArbolHuffman, caracter: Char):Boolean = arbol match
      case HojaHuffman(char, frec) => char == caracter//caso base donde el caracter es igual al introducido, retorna un boolean
      case RamaHuffman(i, d) => caracterEnArbolAux(i,caracter) | caracterEnArbolAux(d,caracter)// caso recursivo donde se miran en ambos
    // subarboles para comprobar si está el caracter en ellos, se aplica el operando or para que en el mometno que salga true, la funcion de true
    caracterEnArbolAux(this, caracter)

  def codificar(cadena: String): List[Bit]=
    var lista: List[Char] = cadenaAListaChars(cadena) // conversion de String a lista chars
    def codificarAux(arbol:ArbolHuffman, arbolAux: ArbolHuffman, listaChar: List[Char], accum: List[Bit]): List[Bit] = listaChar match
      case Nil => accum.reverse // caso base donde la lista ya esté analizada
      case cabeza :: cola => arbolAux match // caso recursivo, miramos el arbol
        case HojaHuffman(char, frec) => codificarAux(this,this, cola, accum) //si es una hoja, se reincia el arbol, se quita la cabeza a la lista y se pone el acumulado tal cual
        case RamaHuffman(i,d) => if i.caracterEnArbol(cabeza) then codificarAux(this,i, listaChar, 0 :: accum) //si el elemento se encuentra en el arbol de la izq, se mueve a la izquierda y se apunta un 0
                                 else if d.caracterEnArbol(cabeza) then codificarAux(this,d, listaChar, 1 :: accum)//si el elemento se encuentra en el arbol de la derecha, se mueve a la derecha y se apunta un 1
                                 else throw new Error("El caracter no se encuentra en el arbol")
    codificarAux(this,this, lista, Nil)
}
case class RamaHuffman(izq:ArbolHuffman, der:ArbolHuffman) extends ArbolHuffman {} //case class donde el elemento del arbol es una rama

case class HojaHuffman(caracter:Char, frecuencia:Int) extends ArbolHuffman{} //case class donde el elemento del arbol es una hoja


//CONSTRUCCIÓN DEL ÁLBOL

def listaCharsADistFrec(listaChar: List[Char]): List[(Char,Int)] =
  def listaCharsADistFrecAux (listaAux:List[Char], listaAux2:List[Char], caracter:Char, frecuencia:Int, accum:List[(Char,Int)]): List[(Char,Int)] = listaAux match
    case Nil => accum.toSet.toList //Con .toSet.toList elimino los elementos repetidos
    case List(x) => listaAux2 match // Si la listaAux tiene un elemento estonces miro lo casos que puede tener listaAux2, este caso es necesio porque sino no se pasaría por el ultimo elemento de la listaAux
      case Nil => ((caracter,frecuencia)::accum).toSet.toList// guardo el caracter con si freciencia en la lista acumuladora
      case cabeza1 :: cola1 => if cabeza1 == caracter then listaCharsADistFrecAux(listaAux, cola1, caracter, frecuencia+1, accum) //mantengo caracter y voy recorriendo la lista si el caracter coincide con la cabeza de la listaAux2 sumo una unidad a la freciencia
                             else listaCharsADistFrecAux(listaAux,cola1, caracter, frecuencia, accum)// si la cabeza de la listaAux2 no es igual al caracter no añado nada a la freciencia y vuelvo a llamar a la función
    case cabeza :: cola => listaAux2 match
      case Nil => listaCharsADistFrecAux(listaAux.tail, listaChar, cola.head, 0, (caracter,frecuencia)::accum) //cambio el caracter y guardo el caracter con su frecuencia anterior
      case cabeza2 :: cola2 => if cabeza2 == caracter then listaCharsADistFrecAux(listaAux, cola2, caracter, frecuencia+1, accum) //mantengo caracter y voy recorriendo la lista si el caracter coincide con la cabeza de la listaAux2 sumo una unidad a la freciencia
                             else listaCharsADistFrecAux(listaAux,cola2, caracter, frecuencia, accum)// si la cabeza de la listaAux2 no es igual al caracter no añado nada a la freciencia y vuelvo a llamar a la función

  listaCharsADistFrecAux(listaChar,listaChar,listaChar.head,0,Nil)


def distribFrecListaHojas(frec:List[(Char,Int)]):List[HojaHuffman] =
  def cambiarAIntChar(listaCharInt: List[(Char, Int)]): List[(Int, Char)] = // esta funcion se utiliza para cambiar el orden de los elementos de la tupla y asi poder ordenar la lista según la frecuencia
    def cambiarAIntCharAux(listaCharInt: List[(Char, Int)], listaIntChar: List[(Int, Char)]): List[(Int, Char)] = listaCharInt match
      case Nil => listaIntChar
      case cabeza :: cola => cambiarAIntCharAux(cola, (listaCharInt.head._2, listaCharInt.head._1) :: listaIntChar)//crea una lista que va introdiciendo una tupla cuyo primer elemento es el elemento dos de la tupla inicial y el segundo termino en el elemento uno de la tupla inicial
    cambiarAIntCharAux(listaCharInt, Nil)

  val frecNuevo = cambiarAIntChar(frec) //Hecho para poder ordenar las hojas de forma decreciente segun sus frecuencias
  def distribFrecListaHojasAux(frec:List[(Int,Char)], listaHojas:List[HojaHuffman]):List[HojaHuffman] = frec match
    case Nil => listaHojas
    case cabeza :: cola => distribFrecListaHojasAux(cola, HojaHuffman(frec.head._2,frec.head._1)::listaHojas)//la tupla de la lista frec pasa a ser una hoja introduciendo cada elemento de la tupla en el atributo correspondiente de la clase HojaHuffman
  distribFrecListaHojasAux(frecNuevo.sorted.reverse, Nil)//.sorted ordena la lista en función de las frecuencias de forma decreciente y ponemos .reverse para ordenarlos de forma creciente

//Funcion auxiliar para creación arbol codificado a partir de la lista de hojas
def creaRamaHuff(izq: ArbolHuffman, dch:ArbolHuffman):RamaHuffman =
  RamaHuffman(izq, dch)// crea una rama con los parametros introducidos en la funcion

def combinar(nodos: List[ArbolHuffman]): List[ArbolHuffman] = nodos match
  case primero::segundo::cola => (creaRamaHuff(primero,segundo)::cola).sortBy(_.peso)// se cogen el primer y segundo elemento de la lista formando una rama, esta se añade a la lista cola y por ultimo se ordena la lista en función del peso
  case _ => nodos // cuando no hay primer y segundo elemento se devuelve la lista de nodos sin modificar




def esListaSingleton (lista: List[ArbolHuffman]): Boolean = lista.length == 1 //comprueba que la longitud es 1


def repetirHasta(accion: List[ArbolHuffman] => List[ArbolHuffman],condicion: List[ArbolHuffman] => Boolean): List[ArbolHuffman] => List[ArbolHuffman] = list =>
  if(condicion(list)) list else repetirHasta(accion, condicion)(accion(list)) //si no se cumple la condición introducida se llama a la función recursivamente hasta que la cumpla

def crearArbolHuffman(cadena:String): ArbolHuffman = repetirHasta(combinar,esListaSingleton)(distribFrecListaHojas(listaCharsADistFrec(cadenaAListaChars(cadena)))).head
//esta función llama a la función repetirHasta()() cuya funcion es combinar la lista hasta que se cumpla la condición esListaSingleton, y la lista introducida es la cadena convertida en una lista ordenada por frecuencia ascendentemente


//CREACIÓN DE LA TABLA
def deArbolATabla(arbol: ArbolHuffman): TablaCodigos =
  var lista: List[Char] = arbol.caracteres // devuelve la lista de caracteres de albol
  def deArbolATablaAux(arbol:ArbolHuffman, arbolAux: ArbolHuffman, tabla: TablaCodigos,listaChar:List[Char], accum: List[Bit]): TablaCodigos = listaChar match
    case Nil => tabla.reverse //cuando la listaChar es vacía se devuelve la tabla
    case cabeza :: cola => arbolAux match
      case HojaHuffman(char, frec) => deArbolATablaAux(arbol, arbol, (cabeza,accum.reverse)::tabla,cola, Nil)//si la cabeza es una hoja se añade su lista de bits y su caracter en la tabla
      case RamaHuffman(i, d) => if i.caracterEnArbol(cabeza) then deArbolATablaAux(arbol, i,tabla, listaChar, 0 :: accum)// si la cabeza se encuentra en el arbol izquierdo, entonces se añade un 0 a la lista de bits de dicho caracter (cabeza)
                                else if d.caracterEnArbol(cabeza) then deArbolATablaAux(arbol, d,tabla,listaChar, 1 :: accum)// si la cabeza se encuentra en el arbol izquierdo, entonces se añade un 1 a la lista de bits de dicho caracter (cabeza)
                                else throw new Error("El caracter no se encuentra en el arbol")// si la cabeza no esta en ninguno de los dos arboles entonces se envía una excepción
  deArbolATablaAux(arbol, arbol,Nil, lista, Nil)

def codificar(tabla:TablaCodigos)(cadena:String): List[Bit]=
  val list: List[Char]= cadenaAListaChars(cadena) //Devuelve el string en una lista de caracteres
  def codificarAux(tabla: TablaCodigos,tablaAux:TablaCodigos)(lista:List[Char],accum: List[Bit]):List[Bit]= lista match
    case Nil => accum //Si la lista es nula entonces se devuelve accum que es la lista de bits de la codificación realizada
    case cabeza :: cola => tablaAux match //Gracias a este match se va a ir recorriendo cada tupla de la tabla
      case Nil => throw new Error("ese elemento no se encuentra en la tabla") // si la tabla es nulo es porque no se ha encontrado el caracter
      case cabeza2 :: cola2 => if cabeza == cabeza2._1 then codificarAux(tabla, tabla)(cola, accum:::cabeza2._2)//si el caracter buscado coincide con el caracter de la tabla entonces se añade la lista de bits al acumulador de la codificación
                                else codificarAux(tabla,cola2)(lista,accum)//si el caracter buscado no coincide con el caracter de la tabla entonces se vuelve a llamar a la función para ir recorriendo la tabla
  codificarAux(tabla, tabla)(list,Nil)

def decodificar(tabla:TablaCodigos)(lista:List[Bit]):String =
  def decodificarAux(tabla: TablaCodigos)(lista: List[Bit], buscado: List[Bit], caracteres:List[Char]): String = lista match
    case Nil => listaCharsACadena(caracteres).reverse:+ caracterTabla(tabla)(buscado.reverse)// si la lista es vacía entonces se devuelven los caracteres de la decodificación realizada
    case head::tail => if estaEnLaTabla(tabla)(buscado.reverse) then decodificarAux(tabla)(lista, Nil, caracterTabla(tabla)(buscado.reverse)::caracteres)// si la lista buscado de bits se encuentra en la tabla entonces se añade ese caracter a la decodificación
                        else decodificarAux(tabla)(tail, head::buscado ,caracteres)// si la lista de bits buscados no se encuentra en la tabla entonces se añade el siguiente bit de la lista de bits de codificación y se vuelve a llamar a la función
  decodificarAux(tabla)(lista,Nil,Nil)

def estaEnLaTabla(tabla:TablaCodigos)(list:List[Bit]): Boolean =
  if tabla == Nil then false // Si la tabla es nula, entonces la lista de bits no se encuentra en la tabla
  else if list!= tabla.head._2 then estaEnLaTabla(tabla.tail)(list) else true // si la lista de bits no coincide con la lista de la tabla entonces se llama otra vez a la función para ir recorriendo la tabla, pero si si coinciden entonces se devuelve true

def caracterTabla(tabla: TablaCodigos)(list: List[Bit]): Char =
  if list != tabla.head._2 then caracterTabla(tabla.tail)(list) else tabla.head._1 //Si la lista de bits no es como la lista de la tabla entonces se llama a la fuención para recorrer la tabla, pero si coinciden entonces se devuelve el caracter de la tabla


object ArbolHuffman
  def apply(cadena: String):ArbolHuffman = crearArbolHuffman(cadena)











object miPrograma extends App{
  val s = HojaHuffman('s', 4)
  val o = HojaHuffman('o', 3)
  val e = HojaHuffman('e', 2)
  val esp = HojaHuffman(' ', 2)
  val rama4 = RamaHuffman(e, esp)
  val rama7 = RamaHuffman(o, rama4)
  val rama11 = RamaHuffman(s, rama7)
  //Prueba funcion peso
  println("\nprueba de la función peso")
  println (rama11.peso)

  //Prueba fucion caracteres
  println("\nprueba de la función caracteres")
  println (rama11.caracteres)

  //Prueba funciones auxiliares
  println("\nprueba de la función listaCharACadena")
  println(listaCharsACadena(List('a', 'b')))
  println("\nprueba de la función cadenaAListaChars")
  println(cadenaAListaChars("soe "))

  //Prueba funcion decodificar
  val bitList: List[Bit] = List(0, 1, 0, 0, 1, 1, 1, 1, 1, 0, 0, 1, 1, 0, 1, 1, 1, 1, 0, 0, 1, 0)
  //sos ese oso
  val b: List[Bit] = List(0, 1, 0, 1, 1, 0, 1, 1, 1, 0)
  //soe s
  println("\nprueba de la función decodificar")
  println(rama11.decodificar(bitList))
  println(rama11.decodificar(b))

  println("\nprueba de la función caracter en arbol")
  // prueba de la funcion CaracterEnArbol
  println(rama11.caracterEnArbol('p'))
  println(rama11.caracterEnArbol('s'))

  //prueba de la funcion Codificar
  println("\nprueba de la función codificar")
  println(rama11.codificar("sos ese oso"))
  println(rama11.codificar("soe s"))


  val listac = List('o','u',' ','o','u','o','o','i','a')
  val lista2 = List('o','i','u','u')
  println("\n prueba de la función listaCharsADistFrec")
  println(listaCharsADistFrec(lista2))
  println(listaCharsADistFrec(listac))

  println("\n prueba de la función distribFrecListHojas")
  println(distribFrecListaHojas(listaCharsADistFrec(listac)))

  println("\n prueba de la función creaRama")
  println(creaRamaHuff(s,o))

  println("\n prueba de la función combinar")
  println(combinar(distribFrecListaHojas(listaCharsADistFrec(listac))))
  println(combinar(combinar(distribFrecListaHojas(listaCharsADistFrec(listac)))))
  println(combinar(combinar(combinar(distribFrecListaHojas(listaCharsADistFrec(listac))))))
  println(combinar(combinar(combinar(combinar(distribFrecListaHojas(listaCharsADistFrec(listac)))))))
  println(combinar(combinar(combinar(combinar(combinar(distribFrecListaHojas(listaCharsADistFrec(listac))))))))


  println("\n prueba de la función esListaSingleton")
  println(esListaSingleton(List(s)))
  println(esListaSingleton(List()))
  println(esListaSingleton(List(s,o,e)))

  println("\n prueba de la función crearArbolHuffman")
  println(crearArbolHuffman(" sseeee     ttttttth"))
  val r = "aaaaabbbbbcccccdddddeeeeecdbbaaaaaaaaaaaaaaa"
  println(crearArbolHuffman(r))

  println("\nprueba de la función deArbolATabla")
  println(deArbolATabla(crearArbolHuffman(" sseeee     ttttttth")))
  println(deArbolATabla(crearArbolHuffman(r)))

  println("\nprueba de la función codificar con la tabla")
  println(codificar(deArbolATabla(crearArbolHuffman(" sseeee     ttttttth")))("set "))
  println(codificar(deArbolATabla(crearArbolHuffman(r)))("decab"))


  println("\nprueba de la función estaEnLaTabla")
  println(estaEnLaTabla(deArbolATabla(crearArbolHuffman(" sseeee     ttttttth")))(List(1,0)))
  println(estaEnLaTabla(deArbolATabla(crearArbolHuffman(r)))(List(1, 1, 0, 0)))

  println("\nprueba de la función caracterEnLaTabla")
  println(caracterTabla(deArbolATabla(crearArbolHuffman(" sseeee     ttttttth")))(List(1, 1, 1)))
  println(caracterTabla(deArbolATabla(crearArbolHuffman(r)))(List(1, 1, 0)))

  println("\nprueba de la función decodificar con la tabla")
  println(decodificar(deArbolATabla(crearArbolHuffman(" sseeee     ttttttth")))(List(1,1,0,1,1,1,1,0,1,0)))
  println(decodificar(deArbolATabla(crearArbolHuffman(r)))(List(0,1,0,1,1,0,0,1,1,1,1,1,0)))



}
