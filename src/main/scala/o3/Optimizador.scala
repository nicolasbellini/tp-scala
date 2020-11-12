package o3

case class OptimizadorHelper(){
  var listaVariables = List[String]()

  def agregarVariableUtilizada(ex: Variable): Expresion = {
    listaVariables = ex.nombre:: listaVariables
    ex
  }

  def filtrarIntruccion(inst: Expresion): Boolean = {
    inst match {
      case DeclararVariable(e1, _) => listaVariables.contains(e1)
      case _ => true
    }
  }
}

case class Optimizador() {

  def optimizar(ast: AST): AST ={

    val helper = OptimizadorHelper()

    AST(ast.instrucciones.map(inst => optimizarInstruccion(inst,helper)).filter( inst => helper.filtrarIntruccion(inst)))
  }

  def optimizarInstruccion(instruccion: Expresion, helper: OptimizadorHelper): Expresion = {

    instruccion match {
      case ex:Variable => helper.agregarVariableUtilizada(ex)
      case ex:ExpresionAtomica => ex
      case DeclararVariable(e1,e2) => DeclararVariable(e1, optimizarInstruccion(e2, helper))
      case Suma(e1, e2) => optimizarSuma(Suma(optimizarInstruccion(e1,helper), optimizarInstruccion(e2,helper)))
      case Resta(e1, e2) => optimizarResta(Resta(optimizarInstruccion(e1,helper),optimizarInstruccion(e2, helper)))
      case Multiplicacion(e1, e2) => optimizarMultiplicacion(Multiplicacion(optimizarInstruccion(e1, helper),optimizarInstruccion(e2, helper)))
      case Division(e1, e2) => optimizarDivision(Division(optimizarInstruccion(e1, helper),optimizarInstruccion(e2, helper)))
      case expr => optimizarComparacion(expr)
    }
  }

  def optimizarSuma(suma: Suma): Expresion = {
    suma match {
      case Suma(Numero(0), e2) => e2
      case Suma(e1, Numero(0)) => e1
      case expr => expr
    }
  }

  def optimizarResta(resta: Resta): Expresion = {
    resta match {
      case Resta(a , Numero(0)) => a
      case expr => expr
    }
  }

  def optimizarMultiplicacion(mult: Multiplicacion): Expresion = {
    mult match {
      case Multiplicacion(e1, Numero(1)) => e1
      case Multiplicacion(Numero(1), e2) => e2
      case Multiplicacion(e1,e2) if e1 == Numero(0) || e2 == Numero(0) => Numero(0)
      case expr => expr
    }
  }

  def optimizarDivision(div: Division): Expresion = {
    div match {
      case Division(e1 , Numero(1)) => e1
      case expr => expr
    }
  }

  def optimizarComparacion(inst: Expresion): Expresion = {
    inst match {
      case Igual(n1:Numero, n2:Numero) if n1==n2 => True()
      case Distinto(n1:Numero, n2:Numero) if n1!=n2 => True()
      case Mayor(n1:Numero, n2:Numero) if n1<n2 => False()
      case Menor(n1:Numero, n2:Numero) if n1>n2 => False()
      case expr => expr
    }
  }

}

object Optimizador extends Optimizador



