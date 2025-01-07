package cs320

import Value._

object Implementation extends Template {

  def interp(expr: Expr, env: Env, k: Cont): Value = expr match{
    // variable
    case Id(s) => k(lookup(s,env))
    // integer
    case IntE(n) => k(IntV(n))
    // boolean
    case  BooleanE(v) => k(BooleanV(v))
    // addition
    case Add(l,r) => interp(l, env, lv=> interp(r, env, rv=> k(IntVAdd(lv, rv))))
    // multiplication
    case Mul(l,r) => interp(l, env, lv=> interp(r, env, rv=> k(IntVMul(lv, rv))))
    // division
    case Div(l,r) => interp(r, env, {
      case IntV(n) if n == 0 => error(s"not divide")
      case rv: IntV => interp(l, env, lv => k(IntVDiv(lv, rv)))
      case _ => error(s"right operand is not a number: $r")
    })
    // modulo
    case Mod(l,r) =>interp(r, env, {
      case IntV(n) if n == 0 => error(s"not divide")
      case rv: IntV => interp(l, env, lv => k(IntVMod(lv, rv)))
      case _ => error(s"right operand is not a number: $r")
    })
    // equal-to
    case Eq(l, r) => compareBinaryOp(l, r, env, k)(_ == _)
    // less-then
    case Lt(l, r) => compareBinaryOp(l, r, env, k)(_ < _)
    // conditional
    case If(c, t, f) => interp(c, env, condition_check => condition_check match{
      case BooleanV(true) => interp(t, env, k)
      case BooleanV(false) => interp(f, env, k)
      case _ => error(s"not Boolean")
    })
    // tuple
    case TupleE(e) => k(TupleV(e.map(interp(_, env, k))))
    // projection
    case Proj(e, i) =>getTupleElement(interp(e, env, k), i)
    // nil
    case NilE => k(NilV)
    // cons
    case ConsE(h,t) => interp(t, env, tv => interp(h, env, hv => 
      tv match{
        case NilV => k(ConsV(hv, NilV))
        case ConsV(head, tail) => k(ConsV(hv, tv))
        case _ => error(s"not list")
      }))
    // is-empty
    case Empty(e) => interp(e, env, ev=> ev match{
      case NilV => k(BooleanV(true))
      case ConsV(head, tail) => k(BooleanV(false))  
      case _ => error(s"not list")
    })
    // head
    case Head(e) => interp(e, env, ec => ec match{
      case ConsV(head, tail) => k(head)
      case _ => error(s"not list")
    })
    // tail
    case Tail(e) => interp(e, env, ec => ec match{
      case ConsV(head, tail) => k(tail)
      case _ => error(s"not list")
    })
    // local variable
    case Val(x, i, b) => interp(b, env + (x->interp(i, env, k)), k)
    // continuation binding
    case Vcc(x, b) => interp(b, env + (x -> ContV(k)), k)
    // anonymous function
    case Fun(x, b) =>k(CloV(x, b, env))
    // recursive function
    //case RecFuns(functions: List[FunDef], body: Expr) extends Expr
    // function application
    case App(f,a) =>interp(f, env, fv=> fv match{
      case CloV(x, b, fenv) => {
        val avals = a.map(interp(_, env, k))
        if (a.length != x.length) error(s"wrong arity: $x, $a")
        interp(b, fenv ++ (x zip avals), k)
      }
      case v => error(s"not a closure: $v")
    })
    // type test
    //case Test(expression: Expr, typ: Type) extends Expr
    // throwing exception
    //case Throw(expression: Expr) extends Expr
    // handler registration
    //case Try(expression: Expr, handler: Expr) extends Expr
  }
  def lookup(name: String, env: Env): Value = env.getOrElse(name, error(s"free identifier: $name"))
  
  def IntVOp(op: (BigInt, BigInt) => BigInt) : (Value, Value) => IntV = (_, _) match {
    case (IntV(x), IntV(y)) => IntV(op(x,y))
    case (x, y) => error(s"not both numbers: $x, $y")
  }

  val IntVAdd = IntVOp(_ + _)
  val IntVMul = IntVOp(_ * _)
  val IntVDiv = IntVOp(_ / _)
  val IntVMod = IntVOp(_ % _)

  def compareBinaryOp(l: Expr, r: Expr, env: Env, k: Cont)(op: (BigInt, BigInt) => Boolean): Value =
    interp(l, env, check_left =>
      check_left match {
        case IntV(left_value) =>
          interp(r, env, check_right =>
            check_right match {
              case IntV(right_value) => k(BooleanV(op(left_value, right_value)))
              case _ => error(s"not int: $check_right")
            }
          )
        case _ => error(s"not int: $check_left")
      }
    )
  def getTupleElement(value: Value, index: Int): Value = value match {
    case TupleV(v) if v.length >= index => v(index - 1)
    case TupleV(_) => error(s"not proper index: $index")
    case _ => error("not Tuple")
  }
  def interp(expr: Expr): Value = interp(expr, Map(), x=>x)
}