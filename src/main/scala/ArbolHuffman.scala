type Bit = 0 | 1
def cadenaAListaChars(cadena: String): List[Char] = cadena.toList
def listaCharsACadena(listaCar: List[Char]): String = listaCar.mkString

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
}
