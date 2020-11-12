package o3

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers



class AnalizadorSpec extends AnyFunSpec with Matchers {

  describe("Analizador de expresiones") {

    it("Analizar una division explicita por cero") {

      val div = Division(Numero(2), Numero(0))
      val ast = AST(List(div))

      Analizador.analizar(ast, List(AnalizadorDiv)).head.descripcion should be("No se puede dividir por cero")
    }

    it("Analizar una division de un numero por una expresion igual a cero pero no explicita") {

      val div = Division(Numero(3), Resta(Numero(2), Numero(2)))
      val ast = AST(List(div))

      Analizador.analizar(ast, List(AnalizadorDiv)) shouldBe empty
    }

    it("Analizar una suma de una expresion y 0") {

      val sumaCero = Suma(Numero(2), Numero(0))
      val sumaCeroInv = Suma(Numero(0), Numero(2))
      val ast = AST(List(sumaCero, sumaCeroInv))

      Analizador.analizar(ast, List(AnalizadorSuma)) should have length 2
      Analizador.analizar(ast, List(AnalizadorSuma)).map(r => r.descripcion) should contain
      only("Operacion redundante por izquierda", "Operacion redundante por derecha")
    }

    it("Analizar una multiplicacion de una expresion y 1") {

      val multiUno = Multiplicacion(Numero(2), Numero(1))
      val multiUnoInv = Multiplicacion(Numero(1), Numero(2))
      val ast = AST(List(multiUno, multiUnoInv))

      Analizador.analizar(ast, List(AnalizadorMult)) should have length 2
      Analizador.analizar(ast, List(AnalizadorMult)).map(r => r.descripcion) should contain
      only("Operacion redundante por izquierda", "Operacion redundante por derecha")
    }

    it("Analizar una resta de una expresion y 0") {

      val restaCero = Resta(Numero(2), Numero(0))
      val ast = AST(List(restaCero))

      Analizador.analizar(ast, List(AnalizadorResta)) should have length 1
      Analizador.analizar(ast, List(AnalizadorResta)).head.descripcion should be("Operacion redundante por derecha")
    }

    it("Analizar una division de una expresion y 1") {

      val diviUno = Division(Numero(2), Numero(1))
      val ast = AST(List(diviUno))

      Analizador.analizar(ast, List(AnalizadorDiv)) should have length 1
      Analizador.analizar(ast, List(AnalizadorDiv)).head.descripcion should be("Operacion redundante por derecha")
    }

    it("Analizar una comparacion de igualdad entre dos numeros iguales") {

      val sonIguales = Igual(Numero(2), Numero(2))
      val ast = AST(List(sonIguales))

      Analizador.analizar(ast, List(AnalizadorIgual)) should have length 1
      Analizador.analizar(ast, List(AnalizadorIgual)).head.descripcion should be("Comparacion sin sentido (siempre verdadera)")
    }

    it("Analizar una comparacion de desigualdad entre dos numeros distintos") {

      val sonDesiguales = Distinto(Numero(2), Numero(3))
      val ast = AST(List(sonDesiguales))

      Analizador.analizar(ast, List(AnalizadorDistinto)) should have length 1
      Analizador.analizar(ast, List(AnalizadorDistinto)).head.descripcion should be("Comparacion sin sentido (siempre verdadera)")
    }

    it("Analizar una comparacion de mayor entre dos numeros donde el primero es menor al otro") {

      val noEsMayor = Mayor(Numero(3), Numero(4))
      val ast = AST(List(noEsMayor))

      Analizador.analizar(ast, List(AnalizadorMayor)) should have length 1
      Analizador.analizar(ast, List(AnalizadorMayor)).head.descripcion should be("Comparacion sin sentido (siempre falsa)")
    }

    it("Analizar una comparacion de menor entre dos numeros donde el primero es mayor al otro") {

      val noEsMenor = Menor(Numero(4), Numero(3))
      val ast = AST(List(noEsMenor))

      Analizador.analizar(ast, List(AnalizadorMenor)) should have length 1
      Analizador.analizar(ast, List(AnalizadorMenor)).head.descripcion should be("Comparacion sin sentido (siempre falsa)")
    }

    it("Analizar una suma de un numero y cero con un analizador invalido") {

      val sumaCero = Suma(Numero(2), Numero(0))
      val sumaCeroInv = Suma(Numero(0), Numero(2))
      val ast = AST(List(sumaCero, sumaCeroInv))

      Analizador.analizar(ast, List(AnalizadorIgual)) should have length 0
    }

    it("Analizar expresiones anidadas con el completo lote de analizadores") {

      val expCompleja = Suma(Division(Numero(1), Numero(0)), Resta(Numero(3), Numero(0)))
      val ast = AST(List(expCompleja))

      var result = Analizador.analizar(ast, List(
        AnalizadorSuma,
        AnalizadorResta,
        AnalizadorMult,
        AnalizadorDiv,
        AnalizadorIgual,
        AnalizadorDistinto,
        AnalizadorMayor,
        AnalizadorMenor
      ))

      result should have length 2
      result.map(r => r.descripcion) should contain
      only("No se puede dividir por cero", "Operacion redundante por derecha")

    }

    it("Analizar una expresion con declaraciones de variables duplicada") {

      val ast = AST(List(
        DeclararVariable("añoActual", Numero(2020)), DeclararVariable("añoActual", Numero(2020))))
      var result = Analizador.analizar(ast, List(AnalizadorVariables))

      result should have length 1
      result.head.descripcion should be("Declaracion de variable duplicada")
    }

  }
}
