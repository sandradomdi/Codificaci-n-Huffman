trait Arbol

class Nodo(valor: Int, peso: Int, izq: Arbol, der: Arbol) extends Arbol

class Cons(raiz: Nodo, arbol: Arbol)


class Vacio extends Arbol