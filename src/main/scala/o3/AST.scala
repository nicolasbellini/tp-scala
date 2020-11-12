package o3

sealed abstract class Expresion
sealed abstract class ExpresionAtomica extends Expresion
sealed abstract class ExpresionVariable extends Expresion

object Expresion{

 def unapply(expr: Expresion): Option[(Expresion,Expresion)] = {
    expr match{
      case Suma(e1:Expresion,e2:Expresion) => Some(e1,e2)
      case Resta(e1:Expresion,e2:Expresion) => Some(e1,e2)
      case Multiplicacion(e1:Expresion,e2:Expresion) => Some(e1,e2)
      case Division(e1:Expresion,e2:Expresion) => Some(e1,e2)
      case Igual(e1:Expresion,e2:Expresion) => Some(e1,e2)
      case Distinto(e1:Expresion,e2:Expresion) => Some(e1,e2)
      case Menor(e1:Expresion,e2:Expresion) => Some(e1,e2)
      case Mayor(e1:Expresion,e2:Expresion) => Some(e1,e2)
      case MenorOIgual(e1:Expresion,e2:Expresion) => Some(e1,e2)
      case MayorOIgual(e1:Expresion,e2:Expresion) => Some(e1,e2)
      case _ => None
    }
  }

}

object ExpresionAtomica{

  //def unapply(expr : ExpresionAtomica): Option[Either[Boolean,Int]] = {
  def unapply(expr : ExpresionAtomica): Option[Any] = {
    expr match {
      case Numero(i) => Some(i)//Some(Right(i))
      case True() => Some(true)//Some(Left(true))
      case False() => Some(false)//Some(Left(false))
      case Variable(n:String)=> Some(n)
      case _ => None
    }
  }

}

object ExpresionVariable{

  def unapply(expr: ExpresionVariable): Option[(Any,Expresion)] ={
    expr match{
      case DeclararVariable(n:String, v:Expresion) => Some(n,v)
      case Asignar(v1:Variable, v2:Expresion) => Some(v1,v2)
      case _ => None
    }
  }
}

case class AST (instrucciones: List[Expresion])

case class Numero(num:Int) extends ExpresionAtomica{

  def <(n2: Numero): Boolean ={
    num < n2.num
  }

  def >(n2: Numero):Boolean ={
    num > n2.num
  }
}

case class Suma(e1: Expresion, e2: Expresion) extends Expresion

case class Resta(e1: Expresion, e2: Expresion) extends Expresion

case class Multiplicacion(e1: Expresion, e2: Expresion) extends Expresion

case class Division(e1: Expresion, e2: Expresion) extends Expresion

case class Igual (e1: Expresion, e2: Expresion) extends Expresion

case class Distinto (e1: Expresion, e2: Expresion) extends Expresion

case class Menor (e1: Expresion, e2: Expresion) extends Expresion

case class Mayor (e1: Expresion, e2: Expresion) extends Expresion

case class MenorOIgual (e1: Expresion, e2: Expresion) extends Expresion

case class MayorOIgual (e1: Expresion, e2: Expresion) extends Expresion

case class True() extends ExpresionAtomica

case class False() extends ExpresionAtomica

case class Variable(nombre:String) extends ExpresionAtomica

case class DeclararVariable(nombre:String, valor:Expresion) extends ExpresionVariable

case class Asignar(variable:Variable, valor:ExpresionAtomica) extends ExpresionVariable

