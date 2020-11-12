package o3

object Ejecutor imp{
  def ejecutarAST(ast: AST) : ExpresionAtomica = {

    ast.instrucciones.foreach(expr => ejecutarExpresion(expr))

     ejecutarInstrucciones(ast.instrucciones)


    Memoria.flushMemory()
    resultado
  }

  def ejecutarInstrucciones(expresiones: List[Expresion]): ExpresionAtomica = {
    expresiones.map(inst => inst match {
      case ex:DeclararVariable => Memoria.escribir(ex.nombre, ex.valor)
      case _ => ejecutarExpresion(inst)
    }).head
  }

  def ejecutarExpresion(expr: Expresion): ExpresionAtomica = {

    expr match {
      case ex:ExpresionAtomica => ex
      case Variable(nombre) => ejecutarExpresion(Memoria.leer(nombre))
      case Suma(e1,e2) => ejecutarOperacionAritmetica(Suma(ejecutarExpresion(e1),ejecutarExpresion(e2)))
      case Resta(e1, e2) => ejecutarOperacionAritmetica(Resta(ejecutarExpresion(e1),ejecutarExpresion(e2)))
      case Multiplicacion(e1, e2) => ejecutarOperacionAritmetica(Multiplicacion(ejecutarExpresion(e1),ejecutarExpresion(e2)))
      case Division(e1, e2) => ejecutarOperacionAritmetica(Division(ejecutarExpresion(e1),ejecutarExpresion(e2)))
      case Mayor(e1,e2) => ejecutarComparacion(Mayor(ejecutarExpresion(e1), ejecutarExpresion(e2)))
      case Menor(e1,e2) => ejecutarComparacion(Menor(ejecutarExpresion(e1), ejecutarExpresion(e2)))
      case Igual(e1,e2) => ejecutarComparacion(Igual(ejecutarExpresion(e1), ejecutarExpresion(e2)))
      case Distinto(e1,e2) => ejecutarComparacion(Distinto(ejecutarExpresion(e1), ejecutarExpresion(e2)))
    }
  }

  def ejecutarOperacionAritmetica(expresion: Expresion): ExpresionAtomica = {
    expresion match {
      case Resta(Numero(a), Numero(b)) => Numero(a - b)
      case Suma(Numero(a), Numero(b)) => Numero(a + b)
      case Multiplicacion(Numero(a), Numero(b)) => Numero(a * b)
      case Division(Numero(a), Numero(b)) => Numero(a / b)
    }
  }

  def ejecutarComparacion(expr: Expresion): ExpresionAtomica = {
    expr match {
      case Mayor(Numero(a), Numero(b)) => if (a>b) True() else False()
      case Menor(Numero(a), Numero(b)) => if (a<b) True() else False()
      case Igual(Numero(a), Numero(b)) => if (a==b) True() else False()
      case Distinto(Numero(a), Numero(b)) => if (a!=b) True() else False()
    }
  }

}
