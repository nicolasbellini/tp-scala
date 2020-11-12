package o3

object NivelGravedad extends Enumeration{
  type NivelGravedad = Value
  val Error, Advertencia = Value
}

import o3.NivelGravedad.NivelGravedad

case class ResultadoAnalisis(descripcion: String, nivelGravedad: NivelGravedad,elemento: Expresion)


class Analizador{

  def analizar(ast: AST, analizadores: List[Analisis]): List[ResultadoAnalisis] ={
        recorrerYEvaluar(ast.instrucciones,analizadores).filter(r=>r.isDefined).map(o=>o.get)
  }

  def recorrerYEvaluar(instrucciones: List[Expresion], analizadores: List[Analisis]): List[Option[ResultadoAnalisis]] = {
    instrucciones match{
      case Nil => Nil
      case x :: xs => List.concat(analizarInstruccion(x,analizadores), recorrerYEvaluar(xs,analizadores))
    }
  }

    def analizarInstruccion(instruccion: Expresion, analizadores: List[Analisis]): List[Option[ResultadoAnalisis]] ={
      instruccion match {
        case ExpresionAtomica(_) => List()
        case ExpresionVariable(_:String,v:ExpresionAtomica) => analizadores.map(analizador=>analizador.aplicarAnalisis(instruccion))
        case Expresion(a1:ExpresionAtomica, a2:ExpresionAtomica) => analizadores.map(analizador=>analizador.aplicarAnalisis(instruccion))
        case Expresion(e1:Expresion, e2:Expresion) => List.concat(
          analizarInstruccion(e1,analizadores),
          analizarInstruccion(e2,analizadores)
        )
      }
    }

}

object Analizador extends Analizador




