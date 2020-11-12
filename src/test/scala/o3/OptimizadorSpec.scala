package o3

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class OptimizadorSpec extends AnyFunSpec with Matchers {

  describe("Optimizador de expresiones") {

    it("Optimizador recibe una suma redundante y la simplifica"){

      val expresion = Suma(Numero(4), Numero(0))

      val astOptimizado = Optimizador.optimizarSuma(expresion)

      astOptimizado should be(Numero(4))
    }

    it("Se optimiza un AST con una suma redundante"){

      val expresion = Suma(Numero(4), Numero(0))

      val astOptimizado = Optimizador.optimizar(AST(List(expresion)))

      astOptimizado.instrucciones.head should be(Numero(4))
    }

    it("Se optimiza un AST con una resta redundante"){

      val expresion = Resta(Numero(4), Numero(0))

      val astOptimizado = Optimizador.optimizar(AST(List(expresion)))

      astOptimizado.instrucciones.head should be(Numero(4))
    }

    it("Se optimiza un AST con una multiplicacion redundante"){

      val expresion = Multiplicacion(Numero(4), Numero(1))

      val astOptimizado = Optimizador.optimizar(AST(List(expresion)))

      astOptimizado.instrucciones.head should be(Numero(4))
    }

    it("Se optimiza un AST con una divicion redundante"){

      val expresion = Division(Numero(4), Numero(1))

      val astOptimizado = Optimizador.optimizar(AST(List(expresion)))

      astOptimizado.instrucciones.head should be(Numero(4))
    }

    it("Se optimiza un AST con una comparacion por igualacion redundante"){

      val expresion = Igual(Numero(4), Numero(4))

      val astOptimizado = Optimizador.optimizar(AST(List(expresion)))

      astOptimizado.instrucciones.head should be(True())
    }

    it("Se optimiza un AST con una comparacion por mayor redundante"){

      val expresion = Mayor(Numero(1), Numero(3))

      val astOptimizado = Optimizador.optimizar(AST(List(expresion)))

      astOptimizado.instrucciones.head should be(False())
    }

    it("Se optimiza un AST con una comparacion por menor redundante"){

      val expresion = Menor(Numero(4), Numero(3))

      val astOptimizado = Optimizador.optimizar(AST(List(expresion)))

      astOptimizado.instrucciones.head should be(False())
    }

    it("Se optimiza un AST con una comparacion por distinto redundante"){

      val expresion = Distinto(Numero(4), Numero(3))

      val astOptimizado = Optimizador.optimizar(AST(List(expresion)))

      astOptimizado.instrucciones.head should be(True())
    }

    it("Se optimiza un AST con una suma redundante que esta dentro de otra expresion"){

      val expresion = Suma(Numero(4), Suma(Numero(0),Numero(3)))

      val astOptimizado = Optimizador.optimizar(AST(List(expresion)))

      astOptimizado.instrucciones.head should be (Suma(Numero(4),Numero(3)))
    }

    it("Optimizar suma que contiene otra suma con dos 0, devuelve solo la expresion atomica"){

      val expresion = Suma(Numero(4), Suma(Numero(0),Numero(0)))

      val astOptimizado = Optimizador.optimizar(AST(List(expresion)))

      astOptimizado.instrucciones.head should be (Numero(4))
    }

    it("Optimizar suma con dos numero que no son 0 no hace nada"){

      val expresion = Suma(Numero(4), Numero(2))

      val astOptimizado = Optimizador.optimizar(AST(List(expresion)))

      astOptimizado.instrucciones.head should be (expresion)
    }

    it("Optimizar suma con dos sumas que no son 0 no hace nada"){

      val expresion = Suma(Suma(Numero(4), Numero(2)), Suma(Numero(4), Numero(2)))

      val astOptimizado = Optimizador.optimizar(AST(List(expresion)))

      astOptimizado.instrucciones.head should be (expresion)
    }

    it("Optimizar resta con una expresion que da 0, se optimiza"){

      val expresion = Resta(Numero(4), Suma(Numero(0),Numero(0)))

      val astOptimizado = Optimizador.optimizar(AST(List(expresion)))

      astOptimizado.instrucciones.head should be (Numero(4))
    }

    it("Optimizar mult con una expresion que da 0, se optimiza"){

      val expresion = Multiplicacion(Numero(4),Numero(0))

      val astOptimizado = Optimizador.optimizar(AST(List(expresion)))

      astOptimizado.instrucciones.head should be (Numero(0))
    }

    it("Optimizar un ast con una variable sin utilizar, la elimina"){

      val ast = AST(List(DeclararVariable("pepe", Numero(1)),
      DeclararVariable("añoActual", Numero(2020)),
      Resta(Variable("añoActual"), Numero(25))))

      val astOptimizado = Optimizador.optimizar(ast)

      astOptimizado.instrucciones should not contain(DeclararVariable("pepe", Numero(1)))

    }


  }

}
