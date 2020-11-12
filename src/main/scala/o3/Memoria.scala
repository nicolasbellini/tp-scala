package o3

trait MemoriaOps[T] {

  def leer(id:String): T

  def escribir(id:String, valor:T)

  def existe(id:String):Boolean
}

case class Memoria() extends MemoriaOps[Expresion] {

  var mem: Map[String,Expresion] = Map()

  override def leer(id: String): Expresion = {
    mem(id)
  }

  override def escribir(id: String, valor: Expresion) = {
    mem = mem + (id -> valor)
  }

  override def existe(id: String): Boolean = {
    mem.contains(id)
  }
}

object Memoria extends Memoria {
  def flushMemory() {
    this.mem = Map()
  }

}



