import sun.jvm.hotspot.utilities.Bits
//trait CodigoHuffman

object CodigoHuffman

  abstract class ArbolHuffman
    //def peso: Int

    //def caracter: List[Char]

  case class RamaHuff(izq: ArbolHuffman, der: ArbolHuffman) extends ArbolHuffman

  case class HojaHuff(caracter:Char, frecuencia:Int) extends ArbolHuffman

  /***
  def peso (arbol:ArbolHuffman): Int = arbol match
    case raiz.izq == HojaHuff && raiz.dcha == HojaHuff => raiz.izq.frecuencia + raiz.dcha.frecuencia
    case raiz.izq == HojaHuff && raiz.der == RamaHuff => raiz.izq.frecuencia + peso(raiz.der)
    case raiz.der == HojaHuff && raiz.izq == RamaHuff => raiz.dcha.frecuencia + peso(raiz.izq)
    case raiz.izq == RamaHuff && raiz.dere == RamaHuff => peso(raiz.der) + peso(raiz.izq)
  */
  def peso (arbol: ArbolHuffman):Int = arbol match
    case r: RamaHuff => peso(r.izq) + peso(r.der)
    case h : HojaHuff => h.frecuencia
  def caracteres(arbol: ArbolHuffman): List[Char] = arbol match {
    case r: RamaHuff => caracteres(r.izq) ::: caracteres(r.der)
    case h : HojaHuff => List(h.caracter)
  }
  def cadenaAListaChars(cadena: String): List[Char] =
    cadena.toList
  def listaCharsACadena(listaCar: List[Char]): String =
    listaCar.mkString


  type Bit = 0 | 1

  def decodificar(arbol: ArbolHuffman)(bits: List[Bit]): String =
    def decodificarAux(arbol: ArbolHuffman)(bits: List[Bit])(accum: List[Char]): List[Char] = bits match
      case Nil => arbol match
        case h: HojaHuff => accum :+ h.caracter
        case r: RamaHuff => accum
      case x ::xs  => arbol match
        case h: HojaHuff => decodificarAux(arbol)( bits)( accum :+ h.caracter)
        case r: RamaHuff => decodificarAux(if (x == 0) r.izq else r.der)( xs)(accum)
    listaCharsACadena(decodificarAux(arbol)(bits)(Nil))


val s = HojaHuff('s', 4)
val o = HojaHuff('o', 3)
val e = HojaHuff('e', 2)
val esp = HojaHuff(' ', 2)
val rama4 = RamaHuff(e,esp)
val rama7 = RamaHuff(o,rama4)
val rama11 = RamaHuff(s,rama7)


val Int = peso(rama11)
caracteres(rama11)
listaCharsACadena(caracteres(rama11))
cadenaAListaChars("soe ")

val bitList: List[Bit] = List(0, 1, 0, 0, 1, 1, 1, 1, 1, 0, 0, 1, 1, 0, 1, 1, 1, 1, 0, 0, 1, 0)
decodificar(rama11)(bitList)




