type Bit = 0 | 1
def cadenaAListaChars(cadena: String): List[Char] = cadena.toList
def listaCharsACadena(listaCar: List[Char]): String = listaCar.mkString
type TablaCodigos = List[(Char,List[Bit])]



abstract class ArbolHuffman {
  def peso:Int = this match
    case RamaHuffman(i,d) => i.peso + d.peso
    case HojaHuffman(char, frec) => frec

  def caracteres:List[Char] = this match
    case RamaHuffman(i,d) => i.caracteres:::d.caracteres
    case HojaHuffman(char, frec) => List(char)

  def decodificar(bits:List[Bit]):String =
    def decodificarAux(arbolAux:ArbolHuffman)(bits:List[Bit])(acum:List[Char]):List[Char] = bits match
      case Nil => arbolAux match
        case HojaHuffman(char, frec) => acum :+ char
        case RamaHuffman(i,d) => acum

      case cabeza::cola => arbolAux match
        case HojaHuffman(char, frec) => decodificarAux(this)(bits)(acum:+char)
        case RamaHuffman(i,d) => decodificarAux(if (cabeza==0) then i else d)(cola)(acum)

    listaCharsACadena(decodificarAux(this)(bits)(Nil))

  def caracterEnArbol(caracter: Char): Boolean =
    def caracterEnArbolAux(arbol:ArbolHuffman, caracter: Char):Boolean = arbol match
      case HojaHuffman(char, frec) => char == caracter
      case RamaHuffman(i, d) => caracterEnArbolAux(i,caracter) | caracterEnArbolAux(d,caracter)
    caracterEnArbolAux(this, caracter)

  def codificar(cadena: String): List[Bit]=
    var lista: List[Char] = cadenaAListaChars(cadena)
    def codificarAux(arbol:ArbolHuffman, arbolAux: ArbolHuffman, listaChar: List[Char], accum: List[Bit]): List[Bit] = listaChar match
      case Nil => accum.reverse
      case cabeza :: cola => arbolAux match
        case HojaHuffman(char, frec) => codificarAux(this,this, cola, accum)
        case RamaHuffman(i,d) => if i.caracterEnArbol(cabeza) then codificarAux(this,i, listaChar, 0 :: accum)
                                 else if d.caracterEnArbol(cabeza) then codificarAux(this,d, listaChar, 1 :: accum)
                                 else throw new Error("El caracter no se encuentra en el arbol")

    codificarAux(this,this, lista, Nil)







}
case class RamaHuffman(izq:ArbolHuffman, der:ArbolHuffman) extends ArbolHuffman {

}

case class HojaHuffman(caracter:Char, frecuencia:Int) extends ArbolHuffman{

}
//CONSTRUCCIÓN DEL ÁLBOL


def listaCharsADistFrec(listaChar: List[Char]): List[(Char,Int)] = //PROBLEMA EN listaAux2 match  case Nil => listaCharsADistFrecAux(listaAux.tail, POR PONER LISTAAUX.TAIL EL FINAL NO LO HACE BIEN
  def listaCharsADistFrecAux (listaAux:List[Char], listaAux2:List[Char], caracter:Char, frecuencia:Int, accum:List[(Char,Int)]): List[(Char,Int)] = listaAux match
    case Nil => accum.toSet.toList //elimino los elementos repetidos
    case List(x) => listaAux2 match
      case Nil => ((caracter,frecuencia)::accum).toSet.toList
      case cabeza1 :: cola1 => if cabeza1 == caracter then listaCharsADistFrecAux(listaAux, cola1, caracter, frecuencia+1, accum) //mantengo caracter y voy recorriendo la lista
                             else listaCharsADistFrecAux(listaAux,cola1, caracter, frecuencia, accum)
    case cabeza :: cola => listaAux2 match
      case Nil => listaCharsADistFrecAux(listaAux.tail, listaChar, cola.head, 0, (caracter,frecuencia)::accum) //cambio el caracter y guardo el caracter con su frecuencia anterior
      case cabeza2 :: cola2 => if cabeza2 == caracter then listaCharsADistFrecAux(listaAux, cola2, caracter, frecuencia+1, accum) //mantengo caracter y voy recorriendo la lista
                             else listaCharsADistFrecAux(listaAux,cola2, caracter, frecuencia, accum)

  listaCharsADistFrecAux(listaChar,listaChar,listaChar.head,0,Nil)

//Función hecha para despues poder ordenar las hojas de forma decreciente
def cambiarAIntChar(listaCharInt: List[(Char, Int)]):List[(Int, Char)] =
  def cambiarAIntCharAux(listaCharInt:List[(Char, Int)], listaIntChar: List[(Int, Char)]):List[(Int, Char)] = listaCharInt match
    case Nil => listaIntChar
    case cabeza :: cola => cambiarAIntCharAux(cola, (listaCharInt.head._2,listaCharInt.head._1)::listaIntChar)

  cambiarAIntCharAux(listaCharInt, Nil)
def distribFrecListaHojas(frec:List[(Char,Int)]):List[HojaHuffman] =
  val frecNuevo = cambiarAIntChar(frec) //Hecho para poder ordenar las hojas de forma decreciente segun sus frecuencias
  def distribFrecListaHojasAux(frec:List[(Int,Char)], listaHojas:List[HojaHuffman]):List[HojaHuffman] = frec match
    case Nil => listaHojas
    case cabeza :: cola => distribFrecListaHojasAux(cola, HojaHuffman(frec.head._2,frec.head._1)::listaHojas)

  distribFrecListaHojasAux(frecNuevo.sorted.reverse, Nil)//.sorted ordena la lista en función de las frecuencias de forma decreciente y lo cambiamos a creciente

//Funcion auxiliar para creación arbol codificado a partir de la lista de hojas
def creaRamaHuff(izq: ArbolHuffman, dch:ArbolHuffman):RamaHuffman =
  RamaHuffman(izq, dch)

def combinar(nodos: List[ArbolHuffman]): List[ArbolHuffman] =
  def combinarAux(nodos: List[ArbolHuffman], listaFinal:List[ArbolHuffman]): List[ArbolHuffman] = nodos match
    case Nil => listaFinal
    case List(x) => (nodos.head :: listaFinal).reverse
    case cabeza :: cola => combinarAux(nodos.tail.tail, creaRamaHuff(nodos.head, cola.head) :: listaFinal)
  combinarAux(nodos, Nil)




def esListaSingleton (lista: List[ArbolHuffman]): Boolean = lista.length == 1


def repetirHasta(accion: List[ArbolHuffman] => List[ArbolHuffman],condicion: List[ArbolHuffman] => Boolean): List[ArbolHuffman] => List[ArbolHuffman] = list =>
  if(condicion(list)) list else repetirHasta(accion, condicion)(accion(list))

def crearArbolHuffman(cadena:String): ArbolHuffman = repetirHasta(combinar,esListaSingleton)(distribFrecListaHojas(listaCharsADistFrec(cadenaAListaChars(cadena)))).head

/***
def deArbolATabla(arbol: ArbolHuffman): TablaCodigos =
  def deArbolATablaAux(arbol: ArbolHuffman, tabla: TablaCodigos): TablaCodigos = arbol match
    case RamaHuffman(izq, der) => deArbolATablaAux(izq,tabla):::(deArbolATablaAux(der,tabla) )
    case HojaHuffman(caracter, frecuencia) => tabla:::List[(caracter,frecuencia)]*/
def deArbolATabla(arbol: ArbolHuffman): TablaCodigos =
  var lista: List[Char] = arbol.caracteres
  def deArbolATablaAux(arbol:ArbolHuffman, arbolAux: ArbolHuffman, tabla: TablaCodigos,listaChar:List[Char], accum: List[Bit]): TablaCodigos = listaChar match
    case Nil => tabla.reverse
    case cabeza :: cola => arbolAux match
      case HojaHuffman(char, frec) => deArbolATablaAux(arbol, arbol, (cabeza,accum.reverse)::tabla,cola, Nil)
      case RamaHuffman(i, d) => if i.caracterEnArbol(cabeza) then deArbolATablaAux(arbol, i,tabla, listaChar, 0 :: accum)
                                else if d.caracterEnArbol(cabeza) then deArbolATablaAux(arbol, d,tabla,listaChar, 1 :: accum)
                                else throw new Error("El caracter no se encuentra en el arbol")

  deArbolATablaAux(arbol, arbol,Nil, lista, Nil)

def codificar(tabla:TablaCodigos)(cadena:String): List[Bit]=
  val list: List[Char]= cadenaAListaChars(cadena)
  def codificarAux(tabla: TablaCodigos,tablaAux:TablaCodigos)(lista:List[Char],accum: List[Bit]):List[Bit]= lista match
    case Nil => accum
    case cabeza :: cola => tablaAux match
      case Nil => throw new Error("ese elemento no se encuentra en la tabla")
      case cabeza2 :: cola2 => if cabeza == cabeza2._1 then codificarAux(tabla, tabla)(cola, accum:::cabeza2._2)
                                else codificarAux(tabla,cola2)(lista,accum)
  codificarAux(tabla, tabla)(list,Nil)

def decodificar(tabla:TablaCodigos)(lista:List[Bit]):String =
  def decodificarAux(tabla: TablaCodigos)(lista: List[Bit], buscado: List[Bit], caracteres:List[Char]): String = lista match
    case Nil => listaCharsACadena(caracteres).reverse:+ caracterTabla(tabla)(buscado)
    case head::tail => if estaEnLaTabla(tabla)(buscado.reverse) then decodificarAux(tabla)(lista, Nil, caracterTabla(tabla)(buscado.reverse)::caracteres)
                        else decodificarAux(tabla)(tail, head::buscado ,caracteres)
  decodificarAux(tabla)(lista,Nil,Nil)

def estaEnLaTabla(tabla:TablaCodigos)(list:List[Bit]): Boolean =
  if tabla == Nil then false
  else if list!= tabla.head._2 then estaEnLaTabla(tabla.tail)(list) else true

def caracterTabla(tabla: TablaCodigos)(list: List[Bit]): Char =
  if list != tabla.head._2 then caracterTabla(tabla.tail)(list) else tabla.head._1


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
  println (rama11.peso)

  //Prueba fucion caracteres
  println (rama11.caracteres)

  //Prueba funciones auxiliares
  println(listaCharsACadena(List('a', 'b')))
  println(cadenaAListaChars("soe "))

  //Prueba funcion decodificar
  val bitList: List[Bit] = List(0, 1, 0, 0, 1, 1, 1, 1, 1, 0, 0, 1, 1, 0, 1, 1, 1, 1, 0, 0, 1, 0)
  //sos ese oso
  val b: List[Bit] = List(0, 1, 0, 1, 1, 0, 1, 1, 1, 0)
  //soe s
  println(rama11.decodificar(bitList))
  println(rama11.decodificar(b))

  // prueba de la funcion CaracterEnArbol
  println(rama11.caracterEnArbol('p'))
  println(rama11.caracterEnArbol('s'))

  //prueba de la funcion Codificar
  println(rama11.codificar("sos ese oso"))
  println(rama11.codificar("soe s"))

  val lista = List(1, 2, 3, 2, 4, 1, 5, 3,7)
  val listaSinRepetidos = lista.toSet.toList
  println(listaSinRepetidos)

  val listac = List('o','u',' ','o','u','o','o','i','a')
  val lista2 = List('o','i','u','u')
  println("lista chars a dist frec")
  println(listaCharsADistFrec(lista2))
  println(listaCharsADistFrec(listac))
  println("distrib frec list hojas")
  println(distribFrecListaHojas(listaCharsADistFrec(listac)))
  println("crea rama")
  println(creaRamaHuff(s,o))
  println("combinar")
  println(combinar(distribFrecListaHojas(listaCharsADistFrec(listac))))
  println("es lista singleton")
  println(esListaSingleton(List(s)))
  println(esListaSingleton(List()))
  println(esListaSingleton(List(s,o,e)))

  println("crearArbolHuffman")

  println(crearArbolHuffman(" sseeee     ttttttth"))
  println(crearArbolHuffman("this is an example of huffman tree"))

  println("\nde arbol a tabla")
  println(deArbolATabla(crearArbolHuffman(" sseeee     ttttttth")))

  println("\n codificar con la tabla")
  println(codificar(deArbolATabla(crearArbolHuffman(" sseeee     ttttttth")))("set "))

  println("\nesta en la tabla")
  println(estaEnLaTabla(deArbolATabla(crearArbolHuffman(" sseeee     ttttttth")))(List(0,0,1)))
  println(estaEnLaTabla(deArbolATabla(crearArbolHuffman(" sseeee     ttttttth")))(List(0, 0, 1,1)))

  println("\ncaracter en la tabla")
  println(caracterTabla(deArbolATabla(crearArbolHuffman(" sseeee     ttttttth")))(List(0, 0, 1)))

  println("\n decodificar con la tabla")
  println(decodificar(deArbolATabla(crearArbolHuffman(" sseeee     ttttttth")))(List(0,0,1,0,1,0,1,0,1,1,0,0,0)))
}
