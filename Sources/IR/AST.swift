// AST — the IR of swiftm is a direct AST interpretation (no separate op list
// yet; control flow is modeled explicitly per MControl.dfy/MCommand.dfy).
indirect public enum Expr {
  case lit(MValue)
  case ref(name: String, subs: [Expr])      // name starts with ^ => global
  case intrinsic(fn: String, args: [Expr])
  case extCall(label: String, args: [Expr]) // $$label(args)
  case indirect(Expr)                       // @expr
  case bin(op: String, l: Expr, r: Expr)    // all binary ops, left-to-right
  case un(op: String, e: Expr)              // unary -, ', &
}

indirect public enum WriteArg {
  case expr(Expr)
  case newline      // !
  case col(Expr)    // ?n
  case tab          // ?  (tab to next position — simplified to nothing extra)
  case formfeed     // #
  case char(Expr)   // *n
}

indirect public enum Cmd {
  case set([(lhs: Expr, rhs: Expr)])
  case write([WriteArg])
  case ifCmd(cond: Expr, body: [Cmd])       // I cond <rest of line>
  case elseCmd(body: [Cmd])                 // E  <rest of line>
  case forLoop(ctrl: Expr?, range: (start: Expr, step: Expr?, end: Expr)?, list: [Expr]?, body: [Cmd])
  case kill(names: [Expr], exclusive: Bool)
  case newCmd(names: [Expr]?, exclusive: Bool) // nil = argumentless
  case doCall(label: String?, args: [Expr], postcond: Expr?, body: [Cmd]) // D tag(args) rest
  case goto(label: String, postcond: Expr?)
  case quit(expr: Expr?, postcond: Expr?)
  case xecute(code: Expr, postcond: Expr?)
  case hang(sec: Expr?)
  case merge(dst: Expr, src: Expr)
  case lock(args: [Expr], plus: Bool, minus: Bool, timeout: Expr?)
  case view(args: [Expr])
  case setSpecial(name: String, value: Expr) // $ETRAP=..., via SET $x — handled in set
  case breakCmd
}

public struct MLine {
  public var label: String?     // routine label (nil in direct mode)
  public var cmds: [Cmd]
  public init(label: String? = nil, cmds: [Cmd]) { self.label = label; self.cmds = cmds }
}

public struct Routine {
  public var lines: [MLine]     // index = line number
  public init(lines: [MLine]) { self.lines = lines }
}
