package o3

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class EjecutorSpec extends AnyFunSpec with Matchers {

  describe("Ejecucion de ASTs") {

    it("Puede obtener el resultado de un AST con una Suma"){

      val expresion = Suma(Numero(4), Numero(2))

      val resultado = Ejecutor.ejecutarAST(AST(List(expresion)))

      resultado should be (Numero(6))

    }

    it("Puede obtener el resultado de un AST con una Suma que tiene otra suma dentro"){

      val expresion = Suma(Numero(4), Suma(Numero(1),Numero(2)))

      val resultado = Ejecutor.ejecutarAST(AST(List(expresion)))

      resultado should be (Numero(7))

    }

    it("Puede obtener el resultado de un AST con una Resta que tiene otra suma dentro"){

      val expresion = Resta(Numero(4), Suma(Numero(1),Numero(2)))

      val resultado = Ejecutor.ejecutarAST(AST(List(expresion)))

      resultado should be (Numero(1))

    }

    it("Puede obtener el resultado de un AST con una multiplicacion"){

      val expresion = Multiplicacion(Numero(4), Numero(2))

      val resultado = Ejecutor.ejecutarAST(AST(List(expresion)))

      resultado should be (Numero(8))

    }

    it("Puede obtener el resultado de un AST con una division"){

      val expresion = Division(Numero(4), Numero(2))

      val resultado = Ejecutor.ejecutarAST(AST(List(expresion)))

      resultado should be (Numero(2))

    }

    it("Se obtiene un resultado de una comparacion por mayor"){

      val expresion = Mayor(Numero(4), Numero(2))

      val resultado = Ejecutor.ejecutarAST(AST(List(expresion)))

      resultado should be (True())
    }

    it("Se obtiene un resultado de una comparacion por menor, con una suma dentro"){

      val expresion = Menor(Numero(1), Suma(Numero(2),Numero(1)))

      val resultado = Ejecutor.ejecutarAST(AST(List(expresion)))

      resultado should be (True())
    }

    it("Se obtiene un resultado de una comparacion por igual"){

      val expresion = Igual(Numero(4), Numero(4))

      val resultado = Ejecutor.ejecutarAST(AST(List(expresion)))

      resultado should be (True())
    }

    it("Se obtiene un resultado de una comparacion por distinto"){

      val expresion = Distinto(Numero(4), Numero(4))

      val resultado = Ejecutor.ejecutarAST(AST(List(expresion)))

      resultado should be (False())
    }


  }

}
