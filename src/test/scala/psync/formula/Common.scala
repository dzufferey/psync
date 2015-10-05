package psync.formula

object Common {

  val pid = psync.logic.CL.procType

  val a = Variable("a").setType(Bool)
  val b = Variable("b").setType(Bool)

  val x = Variable("x").setType(Int)
  val y = Variable("y").setType(Int)
  
  val p1 = Variable("p1").setType(pid)
  val p2 = Variable("p2").setType(pid)
  val p3 = Variable("p3").setType(pid)

  val f = UnInterpretedFct("f", Some(Function(List(Int), Int)))
  val g = UnInterpretedFct("g", Some(Function(List(Int), Int)))
  
  val p = UnInterpretedFct("p", Some(Function(List(pid), Bool)))
  val q = UnInterpretedFct("q", Some(Function(List(pid), pid)))
  val r = UnInterpretedFct("r", Some(Function(List(pid), Int)))

  val pp1 = Application(p, List(p1))
  val pp2 = Application(p, List(p2))
  val qp1 = Application(q, List(p1))
  val qp2 = Application(q, List(p2))
  val qp3 = Application(q, List(p3))
  val rp1 = Application(r, List(p1))
  val rp2 = Application(r, List(p2))
  val qqp1 = Application(q, List(qp1))
  val qqp2 = Application(q, List(qp2))
  val pqp1 = Application(p, List(qp1))
  val pqp2 = Application(p, List(qp2))
  val rqp1 = Application(r, List(Application(q, List(p1))))
  val rqp3 = Application(r, List(Application(q, List(p3))))

}
