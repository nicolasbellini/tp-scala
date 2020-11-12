package o3

trait Analisis {
  def aplicarAnalisis(expresion: Expresion): Option[ResultadoAnalisis]
}

case class AnalizadorSuma() extends Analisis {

  def aplicarAnalisis(expresion: Expresion): Option[ResultadoAnalisis] = {
    expresion match {
      case Suma(_, Numero(0)) => Option(ResultadoAnalisis("Operacion redundante por derecha", NivelGravedad.Advertencia, expresion))
      case Suma(Numero(0), _) => Option(ResultadoAnalisis("Operacion redundante por izquierda", NivelGravedad.Advertencia, expresion))
      case _ => None
    }
  }
}

case class AnalizadorResta() extends Analisis{

  override def aplicarAnalisis(expresion: Expresion): Option[ResultadoAnalisis] = {
    expresion match {
      case Resta(_ , Numero(0)) => Option(ResultadoAnalisis("Operacion redundante por derecha",NivelGravedad.Advertencia,expresion))
      case _ => None
    }
  }

}

case class AnalizadorMult() extends Analisis {

  def aplicarAnalisis(expresion: Expresion): Option[ResultadoAnalisis] = {
    expresion match {
      case Multiplicacion(_, Numero(1)) => Option(ResultadoAnalisis("Operacion redundante por derecha",NivelGravedad.Advertencia,expresion))
      case Multiplicacion(Numero(1), _) => Option(ResultadoAnalisis("Operacion redundante por izquierda",NivelGravedad.Advertencia,expresion))
      case _ => None
    }
  }
}

case class AnalizadorDiv() extends Analisis{

  override def aplicarAnalisis(expresion: Expresion): Option[ResultadoAnalisis] = {
    expresion match{
      case Division(_ , Numero(0)) => Option(ResultadoAnalisis("No se puede dividir por cero",NivelGravedad.Error,expresion))
      case Division(_ , Numero(1)) => Option(ResultadoAnalisis("Operacion redundante por derecha",NivelGravedad.Advertencia,expresion))
      case _ => None
    }
  }
}

case class AnalizadorIgual() extends Analisis {
  override def aplicarAnalisis(expresion: Expresion): Option[ResultadoAnalisis] = {
    expresion match {
      case Igual(n1: Numero, n2: Numero) if n1 == n2 => Option(ResultadoAnalisis("Comparacion sin sentido (siempre verdadera)", NivelGravedad.Advertencia, expresion))
      case _ => None
    }
  }
}

case class AnalizadorDistinto() extends Analisis {
  override def aplicarAnalisis(expresion: Expresion): Option[ResultadoAnalisis] = {
    expresion match {
      case Distinto(n1: Numero, n2: Numero) if n1 != n2 => Option(ResultadoAnalisis("Comparacion sin sentido (siempre verdadera)", NivelGravedad.Advertencia, expresion))
      case _ => None
    }
  }
}

case class AnalizadorMayor() extends Analisis {
  override def aplicarAnalisis(expresion: Expresion): Option[ResultadoAnalisis] = {
    expresion match {
      case Mayor(n1:Numero, n2:Numero) if n1<n2 => Option(ResultadoAnalisis("Comparacion sin sentido (siempre falsa)",NivelGravedad.Advertencia,expresion))
      case _ => None
    }
  }
}

case class AnalizadorMenor() extends Analisis {
  override def aplicarAnalisis(expresion: Expresion): Option[ResultadoAnalisis] = {
    expresion match{
      case Menor(n1:Numero, n2:Numero) if n1>n2 => Option(ResultadoAnalisis("Comparacion sin sentido (siempre falsa)",NivelGravedad.Advertencia,expresion))
      case _ => None
    }
  }
}

case class AnalizadorVariables() extends Analisis{

  var listaVariables = List[String]()

  override def aplicarAnalisis(expresion: Expresion): Option[ResultadoAnalisis] = {
    expresion match{
      case DeclararVariable(a:String, b:ExpresionAtomica) if !evaluarVariable(a) => Option(ResultadoAnalisis("Declaracion de variable duplicada",NivelGravedad.Error,expresion))
      case Asignar(a:Variable,b:ExpresionAtomica) => None
      case _ => None
    }
  }

  def evaluarVariable(a:String) : Boolean ={
    var res = listaVariables.find(v=>v==a)
    if (res == None){
      listaVariables = a :: listaVariables
      return true
    }
    return false
  }



}

  object AnalizadorSuma extends AnalizadorSuma
  object AnalizadorResta extends AnalizadorResta
  object AnalizadorMult extends AnalizadorMult
  object AnalizadorDiv extends AnalizadorDiv
  object AnalizadorIgual extends AnalizadorIgual
  object AnalizadorDistinto extends AnalizadorDistinto
  object AnalizadorMayor extends AnalizadorMayor
  object AnalizadorMenor extends AnalizadorMenor
  object AnalizadorVariables extends AnalizadorVariables



