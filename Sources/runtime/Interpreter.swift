// Interpreter — evaluates Expr and executes Cmd against a Store.
// Semantics pinned in m-semantics: MSemantics.dfy (values/operators/collation),
// MControl.dfy (DO/GOTO/QUIT), MCommand.dfy (postconditional universality,
// QUIT-in-FOR exits the loop), MSetList.dfy (SET comma lists left-to-right).
// Reference behavior: mjvm LineEngine (corpus 139) + GT.M V7.2-001.
import Foundation
import IR
import parser

public final class Interp {
  public let store = Store()
  public var out = ""
  public var vx = 0                    // $X — column (chars since last line flush)
  public var vy = 0                    // $Y — line number
  public var test = false              // $TEST
  public var routine: Routine?
  public var routineName = ""          // name of the current routine (for tag^SELF checks)
  public var externals: [String: (routine: Routine, formals: [String: [String]])] = [:]
  public var currentFormals: [String: [String]] = [:] // formals of the current routine
  public var pc = 0                    // current line index
  public var callStack: [(ret: Int, savedTest: Bool, label: String)] = []
  public var newStack: [[String: MValue]] = []  // saved simple locals per NEW
  public var halted = false
  public var error: String? = nil      // runtime error message (RSM-style)
  var quitLoop = false                 // QUIT seen inside FOR body: exits the loop
  var unwound = false                  // a frame was popped by QUIT: return to caller
  var jumped = false                   // GOTO set pc: don't advance

  public init() {}

  func fail(_ msg: String) -> MValue {
    if error == nil { error = msg; halted = true }
    return .null
  }

  // ---------- expression evaluation ----------
  public func eval(_ e: Expr) -> MValue {
    if halted { return .null }
    switch e {
    case .lit(let v): return v
    case .ref(let name, let subs):
      let sv = subs.map { eval($0) }
      if name.hasPrefix("$") { return evalSpecial(name, sv) }
      if let v = store.fetch(name: name, subs: sv) { return v }
      // undefined read is a runtime error (RSM: "Undefined local variable")
      return fail(name.hasPrefix("^") ? "Undefined global variable" : "Undefined local variable")
    case .indirect(let inner):
      let target = eval(inner).raw
      return evalIndirectText(target)
    case .extCall(let label, let args):
      return callExt(label: label, args: args.map { eval($0) })
    case .intrinsic(let fn, let args):
      return evalIntrinsicE(fn, args)
    case .un(let op, let e):
      let v = eval(e)
      switch op {
      case "-": return MValue.number(-v.num)
      case "'": return MValue.number(v.truthy() ? 0 : 1)
      case "&": return MValue.number(v.truthy() ? 1 : 0)
      default: return .null
      }
    case .bin(let op, let l, let r):
      return evalBin(op, eval(l), eval(r))
    }
  }

  // @text: split top-level commas; each part is evaluated as an expression and
  // the results are concatenated (arg-list indirection, C65). A plain
  // reference string evaluates as the variable (C61-C64, C73).
  func evalIndirectText(_ target: String) -> MValue {
    let parts = splitTopCommas(target)
    if parts.count > 1 {
      var r = ""
      for p in parts {
        let v = eval((try? Parser.parseExprString(p)) ?? .lit(MValue(p)))
        r += v.isnum ? MValue.mstr(v.num) : v.raw
      }
      return MValue(r)
    }
    guard let expr = try? Parser.parseExprString(target) else { return .null }
    return eval(expr)
  }

  func splitTopCommas(_ s: String) -> [String] {
    var out: [String] = []
    var cur = ""
    var inStr = false
    var depth = 0
    for ch in s {
      if ch == "\"" { inStr.toggle(); cur.append(ch) }
      else if ch == "(" && !inStr { depth += 1; cur.append(ch) }
      else if ch == ")" && !inStr { depth -= 1; cur.append(ch) }
      else if ch == "," && !inStr && depth == 0 { out.append(cur); cur = "" }
      else { cur.append(ch) }
    }
    if !cur.isEmpty { out.append(cur) }
    return out
  }

  // Comparison per mjvm ExprEval.compare (arbiter GT.M / corpus):
  // "=" numeric iff both operands are numbers or whole-string numeric;
  // relational ops numeric iff either operand is a number (val coercion),
  // else plain string compare.
  func evalBin(_ op: String, _ a: MValue, _ b: MValue) -> MValue {
    func num(_ op: String, _ x: Double, _ y: Double) -> Bool {
      switch op {
      case "<": return x < y
      case ">": return x > y
      case "<=": return x <= y
      case ">=": return x >= y
      case "'<": return !(x < y)
      case "'>": return !(x > y)
      default: return false
      }
    }
    func str(_ op: String, _ x: String, _ y: String) -> Bool {
      switch op {
      case "<": return x < y
      case ">": return x > y
      case "<=": return x <= y
      case ">=": return x >= y
      case "'<": return !(x < y)
      case "'>": return !(x > y)
      default: return false
      }
    }
    func numCollAfter(_ x: MValue, _ y: MValue) -> Bool {
      // GT.M ]]: null sorts first, then numbers (numerically), then strings
      if x.raw.isEmpty { return false }
      if y.raw.isEmpty { return true }
      let xn = x.isnum || MValue.numericStr(x.raw)
      let yn = y.isnum || MValue.numericStr(y.raw)
      if xn && yn { return x.num > y.num }
      if xn { return false }
      if yn { return true }
      return x.raw > y.raw
    }
    let r: Bool
    switch op {
    case "+": return MValue.number(a.num + b.num)
    case "-": return MValue.number(a.num - b.num)
    case "*": return MValue.number(a.num * b.num)
    case "/": return MValue.number(a.num / b.num)
    case "\\": return MValue.number(a.num.truncDiv(b.num))   // integer divide, toward zero
    case "#": return MValue.number(a.num.truncMod(b.num))    // sign of divisor
    case "**": return MValue.number(pow(a.num, b.num))
    case "_": return MValue(a.raw + b.raw)
    case "=":
      let an = a.isnum || MValue.numericStr(a.raw)
      let bn = b.isnum || MValue.numericStr(b.raw)
      r = (an && bn) ? a.num == b.num : a.raw == b.raw
    case "'=": return MValue.number(evalBin("=", a, b).truthy() ? 0 : 1)
    case "<", ">", "<=", ">=", "'<", "'>":
      if a.isnum && b.isnum { r = num(op, a.num, b.num) }
      else if a.isnum || b.isnum { r = num(op, a.num, b.num) }  // val coercion (C23)
      else { r = str(op, a.raw, b.raw) }
    case "[": r = b.raw.isEmpty || a.raw.contains(b.raw)  // contains-empty is true (C33)
    case "'[": r = !(b.raw.isEmpty || a.raw.contains(b.raw))
    case "]": r = a.raw > b.raw            // sorts-after: plain string compare
    case "']": r = !(a.raw > b.raw)
    case "]]": r = numCollAfter(a, b)      // GT.M numeric collation (mjvm#28)
    case "']]": r = !numCollAfter(a, b)
    case "&", "'!": r = op == "&" ? (a.truthy() && b.truthy()) : !(a.truthy() || b.truthy())
    case "!", "'&": r = op == "!" ? (a.truthy() || b.truthy()) : !(a.truthy() && b.truthy())
    case "?": r = matchPattern(a.raw, b.raw)
    default: return .null
    }
    return MValue.number(r ? 1 : 0)
  }

  // ---------- pattern match (port of mjvm PatternMatch; MPattern.dfy) ----------
  // Pattern: items separated implicitly; '.' = 0-or-more (with optional max),
  // digits = exact count or n.m range, letters = class union, "..." = literal,
  // (a,b) = group alternation, ' = negation. Full-string match.
  public func matchPattern(_ s: String, _ pat: String) -> Bool {
    let atoms = parsePattern(Array(pat), 0, pat.count, false)
    guard let atoms else { return false }
    return matchAtoms(Array(s), 0, atoms, 0, true, pat.count)
  }

  struct PAtom {
    var nmin: Int
    var nmax: Int
    var lit: String?        // literal
    var cls: String?        // class union e.g. "AN"
    var negLit: String?
    var negCls: String?
    var alts: [[PAtom]]?    // group
  }

  var patLastEnd = 0

  func parsePattern(_ p: [Character], _ start: Int, _ end: Int, _ inGroup: Bool) -> [PAtom]? {
    var out: [PAtom] = []
    var i = start
    while i < end {
      var c = p[i]
      if c == ")" && inGroup { break }
      if c == "," {
        if inGroup { break } else { return nil }
      }
      var nmin = 1, nmax = 1
      if c == "." {
        i += 1; nmin = 0; nmax = Int.max
        if i < end, p[i].isNumber { let (v, ne) = readPatInt(p, i, end); nmax = v; i = ne }
      } else if c.isNumber {
        let (v, ne) = readPatInt(p, i, end); nmin = v; i = ne
        if i < end, p[i] == "." {
          i += 1; nmax = Int.max
          if i < end, p[i].isNumber { let (v2, ne2) = readPatInt(p, i, end); nmax = v2; i = ne2 }
        } else { nmax = nmin }
      }
      if i >= end { return nil }
      c = p[i]
      if c == "(" {
        i += 1
        var alts: [[PAtom]] = []
        while i < end, p[i] != ")" {
          guard let alt = parsePattern(p, i, end, true) else { return nil }
          alts.append(alt)
          i = patLastEnd
          if i < end, p[i] == "," { i += 1 } else { break }
        }
        if i >= end || p[i] != ")" { return nil }
        i += 1
        if alts.isEmpty { return nil }
        out.append(PAtom(nmin: nmin, nmax: nmax, lit: nil, cls: nil, negLit: nil, negCls: nil, alts: alts))
        continue
      }
      if c == "\"" {
        i += 1
        var lit = ""
        while i < end, p[i] != "\"" { lit.append(p[i]); i += 1 }
        if i >= end { return nil }
        i += 1
        out.append(PAtom(nmin: nmin, nmax: nmax, lit: lit, cls: nil, negLit: nil, negCls: nil, alts: nil))
        continue
      }
      if c == "'" {
        i += 1
        if i < end, p[i] == "\"" {
          i += 1
          var lit = ""
          while i < end, p[i] != "\"" { lit.append(p[i]); i += 1 }
          if i >= end { return nil }
          i += 1
          out.append(PAtom(nmin: nmin, nmax: nmax, lit: nil, cls: nil, negLit: lit, negCls: nil, alts: nil))
          continue
        }
        let cs = i
        while i < end, p[i].isLetter { i += 1 }
        if i == cs { return nil }
        out.append(PAtom(nmin: nmin, nmax: nmax, lit: nil, cls: nil, negLit: nil, negCls: String(p[cs..<i]).uppercased(), alts: nil))
        continue
      }
      if c.isLetter {
        let cs = i
        while i < end, p[i].isLetter { i += 1 }
        out.append(PAtom(nmin: nmin, nmax: nmax, lit: nil, cls: String(p[cs..<i]).uppercased(), negLit: nil, negCls: nil, alts: nil))
        continue
      }
      return nil
    }
    patLastEnd = i
    return out
  }

  func readPatInt(_ p: [Character], _ at: Int, _ end: Int) -> (Int, Int) {
    var v = 0, i = at
    while i < end, p[i].isNumber { v = v * 10 + Int(String(p[i]))!; i += 1 }
    patLastEnd = i
    return (v, i)
  }

  func patClassMatch(_ cls: String, _ c: Character) -> Bool {
    for k in cls {
      switch k {
      case "A", "L", "U": if c.isLetter { return true }
      case "N": if c.isNumber { return true }
      case "P": if !(c.isLetter || c.isNumber) && !c.unicodeScalars.allSatisfy({ $0.properties.generalCategory == .control }) { return true }
      case "C": if c.unicodeScalars.allSatisfy({ $0.properties.generalCategory == .control }) { return true }
      case "E": return true
      default: break
      }
    }
    return false
  }

  func matchAtoms(_ s: [Character], _ si: Int, _ atoms: [PAtom], _ ai: Int, _ full: Bool, _ patLen: Int) -> Bool {
    if ai == atoms.count { return full ? si == s.count : true }
    let a = atoms[ai]
    let min = a.nmin
    var max = a.nmax == Int.max ? s.count - si : a.nmax
    if max < min { return false }
    if max > s.count - si { max = s.count - si }
    var k = max
    while k >= min {
      var next = si
      if matchRepeat(s, si, a, k, &next), matchAtoms(s, next, atoms, ai + 1, full, patLen) { return true }
      k -= 1
    }
    return false
  }

  func matchRepeat(_ s: [Character], _ si: Int, _ a: PAtom, _ k: Int, _ next: inout Int) -> Bool {
    var pos = si
    for _ in 0..<k {
      if let alts = a.alts {
        var matched = false
        for alt in alts {
          var ep = pos
          if matchAtoms(s, pos, alt, 0, false, 0) {
            // find end position: re-run capturing end
            if matchAtomsPos(s, pos, alt, 0, &ep) { pos = ep; matched = true; break }
          }
        }
        if !matched { return false }
        continue
      }
      if let negLit = a.negLit {
        if !negLit.isEmpty, String(s[pos...]).hasPrefix(negLit) { return false }
        if pos >= s.count { return false }
        pos += 1
        continue
      }
      if let negCls = a.negCls {
        if pos >= s.count || patClassMatch(negCls, s[pos]) { return false }
        pos += 1
        continue
      }
      if let lit = a.lit {
        if lit.isEmpty { continue }
        if !String(s[pos...]).hasPrefix(lit) { return false }
        pos += lit.count
        continue
      }
      if let cls = a.cls {
        if pos >= s.count || !patClassMatch(cls, s[pos]) { return false }
        pos += 1
        continue
      }
    }
    next = pos
    return true
  }

  func matchAtomsPos(_ s: [Character], _ si: Int, _ atoms: [PAtom], _ ai: Int, _ ep: inout Int) -> Bool {
    if ai == atoms.count { ep = si; return true }
    let a = atoms[ai]
    let min = a.nmin
    var max = a.nmax == Int.max ? s.count - si : a.nmax
    if max < min { return false }
    if max > s.count - si { max = s.count - si }
    var k = max
    while k >= min {
      var next = si
      if matchRepeat(s, si, a, k, &next), matchAtomsPos(s, next, atoms, ai + 1, &ep) { return true }
      k -= 1
    }
    return false
  }

  // ---------- commands ----------
  public func execLine(_ line: MLine) {
    execCmds(line.cmds)
  }

  func execCmds(_ cmds: [Cmd]) {
    for c in cmds {
      if halted { return }
      execCmd(c)
      if let e = error {
        // $ETRAP: run the trap handler, then continue with the next command
        if let trap = specials["$ETRAP"]?.raw, !trap.isEmpty {
          error = nil; halted = false
          if let (r, _) = try? Parser.parseRoutine(trap) { for l in r.lines { execLine(l) } }
        } else {
          _ = e
          return
        }
      }
    }
  }

  public func execCmd(_ c: Cmd) {
    switch c {
    case .set(let assigns):
      for (lhs, rhs) in assigns {  // MSetList.dfy: left-to-right, each sees prior
        let v = eval(rhs)
        if halted { return }
        setLValue(lhs, v)
      }
    case .write(let args):
      // Line-oriented WRITE (mjvm LineCmds.doWrite): per-command buffer,
      // flush on ! / # / *10 (resets $X, bumps $Y), trailing flush at end of
      // command (does NOT reset $X — C90).
      var buf = ""
      func flush() { out += buf + "\n"; buf = ""; vx = 0; vy += 1 }
      for a in args {
        if halted { return }
        switch a {
        case .expr(let e):
          let v = eval(e)
          let str = v.isnum ? MValue.mstr(v.num) : v.raw
          buf += str; vx += str.count
        case .indirectArg(let e):
          let text = eval(e).raw
          for part in splitTopCommas(text) {
            let v = eval((try? Parser.parseExprString(part)) ?? .lit(MValue(part)))
            let str = v.isnum ? MValue.mstr(v.num) : v.raw
            buf += str; vx += str.count
          }
        case .newline: flush()
        case .formfeed: flush()
        case .col(let e):
          let n = Int(eval(e).num)
          if n > vx { buf += String(repeating: " ", count: n - vx); vx = n }
        case .tab: break
        case .char(let e):
          let n = Int(eval(e).num)
          if n == 10 { flush() }
          else if n >= 0, n <= 255 { buf += String(UnicodeScalar(UInt8(n))); vx += 1 }
          else { buf += "?"; vx += 1 }
        }
      }
      if !buf.isEmpty { out += buf + "\n" }  // trailing flush; $X/$Y unchanged (C90)
      test = true
    case .ifCmd(let cond, let body):
      // Commands after IF on the same line are governed by $TEST; ELSE runs
      // when $TEST is false (ISO 7.2.4).
      test = eval(cond).truthy()
      for c in body {
        if halted { return }
        if case .elseCmd = c { if !test { execCmd(c) } }
        else if test { execCmd(c) }
      }
    case .elseCmd(let body):
      if !test { execCmds(body) }
    case .forLoop(let ctrl, let range, let list, let body):
      if let ctrl = ctrl, let list = list {
        for item in list {
          if halted { return }
          assign(ctrl, eval(item))
          execCmds(body)
          if quitLoop { quitLoop = false; break }  // QUIT in FOR exits the loop (MCommand.dfy)
        }
      } else if let ctrl = ctrl, let range = range {
        var cur = eval(range.start).num
        let step = range.step.map { eval($0).num } ?? 1
        let end = eval(range.end).num
        var guard_ = 0
        while !halted, (step > 0 ? cur <= end : (step < 0 ? cur >= end : true)) {
          guard_ += 1; if guard_ > 100_000_000 { break }
          assign(ctrl, MValue.number(cur))
          let savedTest = test
          execCmds(body)
          if quitLoop { if unwound { return }; quitLoop = false; break }
          test = savedTest
          if step == 0 { continue }
          cur += step
        }
      } else {
        // argumentless FOR: infinite until QUIT
        var guard_ = 0
        while !halted {
          guard_ += 1; if guard_ > 100_000_000 { break }
          execCmds(body)
          if quitLoop { quitLoop = false; break }
        }
      }
    case .kill(let names, let exclusive):
      if names.isEmpty {
        if exclusive { return }
        store.locals.removeAll(); store.localTree = Node()
        return
      }
      for n in names {
        if halted { return }
        let r = resolveRef(n)
        if case .ref(let name, let subs) = r {
          store.kill(name: name, subs: subs.map { eval($0) })
        }
      }
    case .newCmd(let names, let exclusive):
      newStack.append(store.locals)
      if let names = names {
        for n in names { if case .ref(let nm, _) = n { store.locals.removeValue(forKey: nm) } }
      } else {
        store.locals.removeAll()
        store.localTree = Node()
      }
    case .doCall(let label, let args, _, let body):
      if let label = label {
        if label.contains("^") {
          let parts = label.split(separator: "^", omittingEmptySubsequences: false)
          let tag = parts.count > 1 ? String(parts[0]) : ""
          let rout = String(parts.last ?? "")
          if !rout.isEmpty, rout != routineName, let ext = externals[rout] {
            callExternal(ext, tag: tag, args: args.map { eval($0) })
          } else {
            callLabel(tag, args: args.map { eval($0) })
          }
        } else {
          callLabel(label, args: args.map { eval($0) })
        }
        execCmds(body)
      } else {
        execCmds(body) // argumentless DO: body = rest of line / block
      }
    case .goto(let label, _):
      if let idx = routine?.lines.firstIndex(where: { $0.label == label }) {
        pc = idx
        jumped = true
      }
    case .quit(let expr, _):
      if let expr = expr, let er = extReturn { er(eval(expr)) }
      if callStack.isEmpty { halted = true; quitLoop = true }
      else {
        let f = callStack.removeLast()
        pc = f.ret; test = f.savedTest
        quitLoop = true
        unwound = true
      }
    case .xecute(let code, _):
      let text = eval(code).raw
      if let (r, f) = try? Parser.parseRoutine(text) {
        let saved = routine; let savedPc = pc; let savedF = currentFormals
        routine = r; currentFormals = f; pc = 0
        run()
        routine = saved; pc = savedPc; currentFormals = savedF
      }
    case .hang: break // HANG 0 = no-op for our purposes
    case .merge(let dst, let src):
      if case .ref(let dn, let ds) = resolveRef(dst), case .ref(let sn, let ss) = resolveRef(src) {
        let sv = ss.map { eval($0) }
        guard let snode = store.nodeAt(sn, sv) ?? Optional(store.ensureTree(for: sn)) else { return }
        let dv = ds.map { eval($0) }
        let dnode = store.descend(store.ensureTree(for: dn), dv)
        copyNode(snode, into: dnode)
      }
    case .lock, .view: break // LOCK/VIEW: no-op in minimal mode
    case .breakCmd: break
    case .setSpecial: break
    }
  }

  // Resolve @indirection lvalues to a concrete .ref
  func resolveRef(_ e: Expr) -> Expr {
    if case .indirect(let inner) = e {
      let t = eval(inner).raw
      if let r = try? Parser.parseRefString(t) { return r }
    }
    return e
  }

  func setLValue(_ lhs: Expr, _ v: MValue) {
    switch lhs {
    case .ref(let name, let subs):
      if name.hasPrefix("$") { setSpecial(name, v) }
      else { store.set(name: name, subs: subs.map { eval($0) }, v) }
    case .indirect:
      if case .ref = resolveRef(lhs) { }
      let r = resolveRef(lhs)
      if case .ref(let name, let subs) = r {
        if name.hasPrefix("$") { setSpecial(name, v) }
        else { store.set(name: name, subs: subs.map { eval($0) }, v) }
      }
    case .intrinsic(let fn, let args):
      setIntrinsic(fn, args, v)
    default: break
    }
  }

  // SET $E(s,a,b)=v / SET $P(s,d,i[,j])=v
  func setIntrinsic(_ fn: String, _ args: [Expr], _ v: MValue) {
    switch fn {
    case "E", "EXTRACT":
      guard args.count >= 1 else { return }
      let s = eval(args[0]).raw
      let from = args.count > 1 ? Int(eval(args[1]).num) : 1
      let to = args.count > 2 ? Int(eval(args[2]).num) : from
      var arr = Array(s)
      let f = max(from - 1, 0)
      let t = min(to, arr.count)
      if f < arr.count {
        arr.replaceSubrange(f..<t, with: Array(v.raw))
      } else if f == arr.count {
        arr.append(contentsOf: Array(v.raw))
      }
      // write back: the first arg must be a settable ref
      if case .ref(let name, let subs) = args[0] {
        store.set(name: name, subs: subs.map { eval($0) }, MValue(String(arr)))
      }
    case "P", "PIECE":
      guard args.count >= 2 else { return }
      let s = eval(args[0]).raw
      let d = eval(args[1]).raw
      let from = args.count > 2 ? Int(eval(args[2]).num) : 1
      let to = args.count > 3 ? Int(eval(args[3]).num) : from
      var parts = d.isEmpty ? [s] : s.components(separatedBy: d)
      // replace pieces from..to with the single new value (C50)
      while parts.count < to { parts.append("") }
      let f = max(from - 1, 0)
      let t = min(to, parts.count)
      var np = Array(parts[0..<f]) + [v.raw] + Array(parts[min(t, parts.count)...])
      if from - 1 > parts.count { np = parts + [v.raw] } // append beyond end
      let joined = d.isEmpty ? np.joined() : np.joined(separator: d)
      if case .ref(let name, let subs) = args[0] {
        store.set(name: name, subs: subs.map { eval($0) }, MValue(joined))
      }
    default:
      // SET $ETRAP=... / $ECODE=... : letter-only intrinsic name is a special var
      if !fn.isEmpty, fn.allSatisfy({ $0.isLetter }) { setSpecial("$" + fn, v) }
    }
  }

  func assign(_ lhs: Expr, _ v: MValue) { setLValue(lhs, v) }

  func copyNode(_ src: Node, into dst: Node) {
    if let v = src.value { dst.value = v }
    for (k, kid) in src.kids {
      if dst.kids[k] == nil { dst.kids[k] = Node() }
      copyNode(kid, into: dst.kids[k]!)
    }
  }

  // run routine from pc until halted / unwound past this frame
  public func run() {
    guard let r = routine else { return }
    while pc >= 0, pc < r.lines.count, !halted {
      let entryDepth = callStack.count
      let linePc = pc
      let line = r.lines[pc]
      let saveQuit = quitLoop
      quitLoop = false
      execLine(line)
      if quitLoop {
        quitLoop = saveQuit
        if unwound {
          unwound = false
          if callStack.count < entryDepth { return } // return to callLabel/callExt
          continue // pc already set by QUIT
        }
        pc += 1
        continue
      }
      if jumped { jumped = false } else if pc == linePc { pc += 1 }
    }
  }

  func callLabel(_ label: String, args: [MValue]) {
    guard let r = routine, let idx = r.lines.firstIndex(where: { $0.label == label }) else { return }
    let savedPc = pc
    let savedTest = test
    if let formals = currentFormals[label] {
      newStack.append(store.locals)
      for (i, f) in formals.enumerated() {
        store.locals[f] = i < args.count ? args[i] : .null
      }
    }
    callStack.append((ret: savedPc + 1, savedTest: savedTest, label: label))
    pc = idx
    run()
    if currentFormals[label] != nil, let saved = newStack.popLast() { store.locals = saved }
    if !halted { test = savedTest } // pc already set to the return address by QUIT
  }

  public var labelFormals: [String: [String]] = [:]

  // DO tag^ROUTINE(args) into a loaded external routine (MControl.dfy DO semantics)
  func callExternal(_ ext: (routine: Routine, formals: [String: [String]]), tag: String, args: [MValue]) {
    let entry: Int
    let entryLabel: String
    if tag.isEmpty {
      entry = 0
      entryLabel = ext.routine.lines.first?.label ?? ""
    } else if let idx = ext.routine.lines.firstIndex(where: { $0.label == tag }) {
      entry = idx
      entryLabel = tag
    } else {
      return
    }
    let savedRoutine = routine
    let savedFormals = currentFormals
    let savedPc = pc
    let savedTest = test
    let hasFormals = ext.formals[entryLabel] != nil
    if hasFormals {
      newStack.append(store.locals)
      for (i, f) in ext.formals[entryLabel]!.enumerated() {
        store.locals[f] = i < args.count ? args[i] : .null
      }
    }
    callStack.append((ret: savedPc + 1, savedTest: savedTest, label: entryLabel))
    routine = ext.routine
    currentFormals = ext.formals
    pc = entry
    run()
    routine = savedRoutine
    currentFormals = savedFormals
    if hasFormals, let saved = newStack.popLast() { store.locals = saved }
    if !halted { test = savedTest }
  }

  func callExt(label: String, args: [MValue]) -> MValue {
    // $$label(args): label may be "TAG^ROUTINE"
    var lbl = label
    var rout: String? = nil
    if label.contains("^") {
      let parts = label.split(separator: "^", omittingEmptySubsequences: false)
      lbl = String(parts[0])
      if parts.count > 1 { rout = String(parts[1]) }
    }
    if let rout = rout, !rout.isEmpty, rout != routineName, let ext = externals[rout] {
      return extExternal(ext, tag: lbl, args: args)
    }
    guard let r = routine, let idx = r.lines.firstIndex(where: { $0.label == lbl }) else { return .null }
    let savedPc = pc
    callStack.append((ret: savedPc, savedTest: test, label: lbl))
    if let formals = currentFormals[lbl] {
      newStack.append(store.locals)
      for (i, f) in formals.enumerated() { store.locals[f] = i < args.count ? args[i] : .null }
    }
    pc = idx
    var result = MValue.null
    extReturn = { v in result = v }
    run()
    extReturn = nil
    if currentFormals[lbl] != nil, let saved = newStack.popLast() { store.locals = saved }
    if !halted { pc = savedPc }
    return result
  }

  // $$TAG^ROUTINE(args) into a loaded external routine: QUIT value is the result
  func extExternal(_ ext: (routine: Routine, formals: [String: [String]]), tag: String, args: [MValue]) -> MValue {
    guard let idx = ext.routine.lines.firstIndex(where: { $0.label == tag }) else { return .null }
    let savedRoutine = routine
    let savedFormals = currentFormals
    let savedPc = pc
    callStack.append((ret: savedPc, savedTest: test, label: tag))
    let hasFormals = ext.formals[tag] != nil
    if hasFormals {
      newStack.append(store.locals)
      for (i, f) in ext.formals[tag]!.enumerated() { store.locals[f] = i < args.count ? args[i] : .null }
    }
    routine = ext.routine
    currentFormals = ext.formals
    pc = idx
    var result = MValue.null
    extReturn = { v in result = v }
    run()
    extReturn = nil
    routine = savedRoutine
    currentFormals = savedFormals
    if hasFormals, let saved = newStack.popLast() { store.locals = saved }
    if !halted { pc = savedPc }
    return result
  }

  var extReturn: ((MValue) -> Void)?

  // ---------- special variables ----------
  public var specials: [String: MValue] = [:]
  func evalSpecial(_ name: String, _ args: [MValue]) -> MValue {
    let n = name.uppercased()
    switch n {
    case "$T", "$TEST": return MValue.number(test ? 1 : 0)
    case "$X": return MValue.number(Double(vx))
    case "$Y": return MValue.number(Double(vy))
    case "$IO": return specials[n] ?? MValue("0")
    case "$PRINCIPAL": return specials[n] ?? MValue("0")
    case "$H", "$HOROLOG": return MValue(horolog())
    case "$J", "$JOB": return MValue("1")
    case "$STACK": return MValue.number(Double(callStack.count))
    case "$TLEVEL": return MValue.number(0)   // outside a transaction (mjvm/T15)
    case "$ECODE": return specials[n] ?? MValue("")
    case "$ETRAP": return specials[n] ?? MValue("")
    case "$DEVICE": return MValue("")
    case "$STORAGE": return specials[n] ?? MValue.number(1_000_000_000)
    case "$SYSTEM": return specials[n] ?? MValue("swiftm")
    case "$ZVERSION": return specials[n] ?? MValue("swiftm 0.1")
    case "$ZJOB": return specials[n] ?? MValue.number(1)
    case "$ZLEVEL": return specials[n] ?? MValue.number(0)
    case "$ZSYSTEM": return specials[n] ?? MValue("swiftm")
    case "$ZEOF": return specials[n] ?? MValue.number(0)
    default: return specials[n] ?? .null
    }
  }

  func horolog() -> String {
    let t = Date().timeIntervalSince1970
    let day = Int(t / 86400) + 47118   // M day 1 = 1841-01-01
    var sec = Int(t.truncatingRemainder(dividingBy: 86400))
    if sec <= 0 { sec = 1 }
    return "\(day),\(sec)"
  }

  func setSpecial(_ name: String, _ v: MValue) {
    let n = name.uppercased()
    switch n {
    case "$T", "$TEST": test = v.truthy()
    case "$X": vx = Int(v.num)
    case "$Y": vy = Int(v.num)
    default: specials[n] = v
    }
  }
}

extension Double {
  // M integer divide \ : truncate toward zero
  func truncDiv(_ b: Double) -> Double { b == 0 ? 0 : (self / b).rounded(.towardZero) }
  // M modulo #: sign of divisor (a - b*floor(a/b))
  func truncMod(_ b: Double) -> Double {
    if b == 0 { return 0 }
    return self - b * floor(self / b)
  }
}
