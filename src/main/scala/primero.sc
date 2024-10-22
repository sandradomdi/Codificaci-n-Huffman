//trait CodigoHuffman

object CodigoHuffman

  abstract class ArbolHuffman
    //def peso: Int

    //def caracter: List[Char]

  case class RamaHuff(izq: ArbolHuffman, der: ArbolHuffman) extends ArbolHuffman

  case class HojaHuff(caracter:Char, frecuencia:Int) extends ArbolHuffman

  class Cons(raiz: RamaHuff)
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

  //def cadenaAListaChars (cadena:String): List

val s = HojaHuff('s', 4)
val o = HojaHuff('o', 3)
val e = HojaHuff('e', 2)
val esp = HojaHuff(' ', 2)
val rama4 = RamaHuff(e,esp)
val rama7 = RamaHuff(o,rama4)
val rama11 = RamaHuff(s,rama7)


val Int = peso(rama11)



