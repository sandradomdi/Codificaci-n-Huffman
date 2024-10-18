trait CodigoHuffman

abstract class ArbolHuffman

case class RamaHuff(peso: Int, izq: ArbolHuffman, der: ArbolHuffman) extends ArbolHuffman

case class HojaHuff(caracter:Char, frecuencia:Int) extends ArbolHuffman

class Cons(raiz: RamaHuff)


val S = HojaHuff('s', 4)
val o = HojaHuff('o', 3)
val e = HojaHuff('e', 2)
val esp = HojaHuff(' ', 2)
val rama4 = RamaHuff(4,e,esp)
val rama7 = RamaHuff(7,o,rama4)
val rama11 = RamaHuff(11,S,rama7)
val arbolHuffman = Cons(rama11)



