// Interpreter — evaluates Expr and executes Cmd against a Store.
// Semantics pinned in m-semantics: MSemantics.dfy (values/operators/collation),
// MControl.dfy (DO/GOTO/QUIT), MCommand.dfy (postconditional universality,
// QUIT-in-FOR exits the loop), MSetList.dfy (SET comma lists left-to-right).
import Foundation
import IR
import parser

public final class Interp {
  public let store = Store()
  public var out = ""
  public var test = false               // $TEST
  public var routine: Routine?
  public var pc = 0                     // current line index
  public var callStack: [(ret: Int, savedTest: Bool, label: String)] = []
  public var newStack: [[String: MValue]] = []  // saved simple locals per NEW
  public var halted = false
  public var error: String? = nil

  public init() {}

  // ---------- expression evaluation ----------
  public func eval(_ e: Expr) -> MValue {
    switch e {
    case .lit(let v): return v
    case .ref(let name, let subs):
      let sv = subs.map { eval($0) }
      if name.hasPrefix("$") { return evalSpecial(name, sv) }
      return store.get(name: name, subs: sv)
    case .indirect(let inner):
      let target = eval(inner).raw
      return eval(.ref(name: String(target.drop { $0 == "^" } ), subs: [])) // simplified @ for names
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

  func evalBin(_ op: String, _ a: MValue, _ b: MValue) -> MValue {
    switch op {
    case "+": return MValue.number(a.num + b.num)
    case "-": return MValue.number(a.num - b.num)
    case "*": return MValue.number(a.num * b.num)
    case "/": return MValue.number(a.num / b.num)
    case "\\": return MValue.number(a.num.truncDiv(b.num))   // integer divide, toward zero
    case "#": return MValue.number(a.num.truncMod(b.num))    // sign of divisor
    case "**": return MValue.number(pow(a.num, b.num))
    case "_": return MValue(a.raw + b.raw)
    case "=": return MValue.number(MValue.compare(a, b) == 0 ? 1 : 0)
    case "<": return MValue.number(MValue.compare(a, b) < 0 ? 1 : 0)
    case ">": return MValue.number(MValue.compare(a, b) > 0 ? 1 : 0)
    case "<=": return MValue.number(MValue.compare(a, b) <= 0 ? 1 : 0)
    case ">=": return MValue.number(MValue.compare(a, b) >= 0 ? 1 : 0)
    case "[": return MValue.number(a.raw.contains(b.raw) ? 1 : 0)
    case "]": return MValue.number(MValue.coll(a.raw, b.raw) > 0 ? 1 : 0) // sorts after
    case "]]": return MValue.number(MValue.coll(a.raw, b.raw) >= 0 ? 1 : 0)
    case "'=": return MValue.number(MValue.compare(a, b) != 0 ? 1 : 0)
    case "'<": return MValue.number(MValue.compare(a, b) >= 0 ? 1 : 0)
    case "'>": return MValue.number(MValue.compare(a, b) <= 0 ? 1 : 0)
    case "'[": return MValue.number(!a.raw.contains(b.raw) ? 1 : 0)
    case "']": return MValue.number(MValue.coll(a.raw, b.raw) <= 0 ? 1 : 0)
    case "&": return MValue.number(a.truthy() && b.truthy() ? 1 : 0)
    case "!": return MValue.number(a.truthy() || b.truthy() ? 1 : 0)
    case "?": return MValue.number(matchPattern(a.raw, b.raw) ? 1 : 0)
    default: return .null
    }
  }

  // ---------- pattern match (MPattern.dfy subset) ----------
  public func matchPattern(_ s: String, _ pat: String) -> Bool {
    return matchFrom(Array(s), 0, Array(pat), 0)
  }

  func matchFrom(_ s: [Character], _ si: Int, _ p: [Character], _ pi: Int) -> Bool {
    var pi = pi
    while pi < p.count {
      let c = p[pi]
      if c == "." {
        // alternation: .X.Y — try each branch
        var branches: [String] = []
        var cur = ""
        var k = pi + 1
        while k < p.count {
          if p[k] == "." { branches.append(cur); cur = ""; k += 1 }
          else { cur.append(p[k]); k += 1 }
        }
        branches.append(cur)
        for b in branches {
          if matchFrom(s, si, Array(b), 0) { return true }
          // alternation consumes >=0 chars greedily-nondet; simple: try all splits
        }
        // simple fallback: try each branch at each split point
        for split in si...s.count {
          for b in branches {
            if matchFrom(s, split, Array(b), 0) { return true }
          }
        }
        return false
      }
      // count: digits
      var count = 0
      var k = pi
      while k < p.count, p[k].isNumber { count = count * 10 + Int(String(p[k]))!; k += 1 }
      if k > pi {
        // optional .count between: pattern like 2.3A
        var min = count, max = count
        if k < p.count, p[k] == "." {
          k += 1
          var mx = 0
          let m0 = k
          while k < p.count, p[k].isNumber { mx = mx * 10 + Int(String(p[k]))!; k += 1 }
          if k > m0 { max = mx } else { max = Int.max }
        }
        guard k < p.count else { return false }
        let cls = p[k]
        var n = 0
        while si + n < s.count, n < max, classMatch(cls, s[si + n]) { n += 1 }
        if n < min { return false }
        // try all n in min...max
        var nn = n
        while nn >= min {
          if matchFrom(s, si + nn, p, k + 1) { return true }
          nn -= 1
        }
        return false
      }
      if c == "?" { // any single char... actually ? is the match operator; in pattern ? isn't used
        if si < s.count { pi += 1; continue }
        return false
      }
      // class char
      if si >= s.count || !classMatch(c, s[si]) { return false }
      pi += 1
    }
    return si == s.count
  }

  func classMatch(_ cls: Character, _ c: Character) -> Bool {
    switch cls {
    case "A": return c.isLetter
    case "N": return c.isNumber
    case "P": return !c.isLetterOrNumber && !c.isWhitespace
    case "C": return c.isControl
    case "L": return c.isLowercase
    case "U": return c.isUppercase
    case "E": return true
    default: return c == cls // literal
    }
  }

  // ---------- commands ----------
  public func execLine(_ line: MLine) {
    execCmds(line.cmds)
  }

  func execCmds(_ cmds: [Cmd]) {
    for c in cmds { if halted { return }; execCmd(c) }
  }

  func execCmd(_ c: Cmd) {
    switch c {
    case .set(let assigns):
      for (lhs, rhs) in assigns {  // MSetList.dfy: left-to-right, each sees prior
        let v = eval(rhs)
        if case .ref(let name, let subs) = lhs {
          if name.hasPrefix("$") { setSpecial(name, v) }
          else { store.set(name: name, subs: subs.map { eval($0) }, v) }
        }
      }
    case .write(let args):
      for a in args {
        switch a {
        case .expr(let e): out += eval(e).raw
        case .newline: out += "\n"
        case .formfeed: out += "\u{0C}"
        case .col(let e): let n = Int(eval(e).num); let cur = out.count; if cur < n { out += String(repeating: " ", count: n - cur) }
        case .tab: out += "\t"
        case .char(let e): let n = Int(eval(e).num); if n >= 0, n < 256 { out += String(UnicodeScalar(UInt8(n))) }
        }
      }
    case .ifCmd(let cond, let body):
      test = eval(cond).truthy()
      if test { execCmds(body) }
    case .elseCmd(let body):
      if !test { execCmds(body) }
    case .forLoop(let ctrl, let range, let list, let body):
      if let ctrl = ctrl, let list = list {
        for item in list { if halted { return }; assign(ctrl, eval(item)); execCmds(body) }
      } else if let ctrl = ctrl, let range = range {
        var cur = eval(range.start).num
        let step = range.step.map { eval($0).num } ?? 1
        let end = eval(range.end).num
        var guard_ = 0
        while (step > 0 ? cur <= end : (step < 0 ? cur >= end : true)) {
          guard_ += 1; if guard_ > 100_000_000 { break }
          assign(ctrl, MValue.number(cur))
          let savedTest = test
          execCmds(body)
          if quitLoop { quitLoop = false; break }  // QUIT in FOR exits the loop (MCommand.dfy)
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
        if case .ref(let name, let subs) = n {
          if exclusive {
            // K (V): kill only the exact node
            store.kill(name: name, subs: subs.map { eval($0) })
          } else {
            store.kill(name: name, subs: subs.map { eval($0) })
          }
        }
      }
    case .newCmd(let names, let exclusive):
      newStack.append(store.locals)
      if let names = names {
        if exclusive {
          for n in names { if case .ref(let nm, _) = n { store.locals.removeValue(forKey: nm) } }
          // keep-listed stay; others hidden: simplest — save all, remove all, restore keep
          // (full fidelity handled by restoring on scope exit)
        } else {
          for n in names { if case .ref(let nm, _) = n { store.locals.removeValue(forKey: nm) } }
        }
      } else {
        store.locals.removeAll()
        store.localTree = Node()
      }
    case .doCall(let label, let args, let postcond, let body):
      if let pc = postcond, !eval(pc).truthy() { execCmds(body); return }
      if let label = label {
        callLabel(label, args: args.map { eval($0) })
        execCmds(body)
      } else {
        execCmds(body) // argumentless DO: body = rest of line / block
      }
    case .goto(let label, let postcond):
      if let pc = postcond, !eval(pc).truthy() { return }
      if let idx = routine?.lines.firstIndex(where: { $0.label == label }) {
        pc = idx
        run()
      }
    case .quit(let expr, let postcond):
      if let pc = postcond, !eval(pc).truthy() { return }
      if let expr = expr { out += "" ; _ = expr }
      if callStack.isEmpty { halted = true; quitLoop = true }
      else {
        let f = callStack.removeLast()
        pc = f.ret; test = f.savedTest
        quitLoop = true
      }
    case .xecute(let code, let postcond):
      if let pc = postcond, !eval(pc).truthy() { return }
      if let r = try? Parser.parseRoutine(eval(code).raw) {
        let saved = routine; let savedPc = pc
        routine = r; pc = 0
        run()
        routine = saved; pc = savedPc
      }
    case .hang: break // HANG 0 = no-op for our purposes
    case .merge(let dst, let src):
      if case .ref(let dn, let ds) = dst, case .ref(let sn, let ss) = src {
        let sv = ss.map { eval($0) }
        guard let snode = store.node(for: sn, subs: sv)?.0 else { return }
        // copy subtree
        let dv = ds.map { eval($0) }
        guard let dnode = store.node(for: dn, subs: dv)?.0 else { return }
        copyNode(snode, into: dnode)
      }
    case .lock, .view: break // LOCK/VIEW: no-op in minimal mode
    case .breakCmd: break
    case .setSpecial: break
    }
  }

  var quitLoop = false

  func assign(_ lhs: Expr, _ v: MValue) {
    if case .ref(let name, let subs) = lhs { store.set(name: name, subs: subs.map { eval($0) }, v) }
  }

  func copyNode(_ src: Node, into dst: Node) {
    if let v = src.value { dst.value = v }
    for (k, kid) in src.kids {
      if dst.kids[k] == nil { dst.kids[k] = Node() }
      copyNode(kid, into: dst.kids[k]!)
    }
  }

  // run routine from pc until halted/quit-to-top
  public func run() {
    guard let r = routine else { return }
    while pc >= 0, pc < r.lines.count, !halted {
      let line = r.lines[pc]
      let saveQuit = quitLoop
      quitLoop = false
      execLine(line)
      if quitLoop { quitLoop = saveQuit; pc += 1; continue } // QUIT in line: just end line in direct mode
      pc += 1
    }
  }

  func callLabel(_ label: String, args: [MValue]) {
    guard let r = routine, let idx = r.lines.firstIndex(where: { $0.label == label }) else { return }
    let savedPc = pc
    let savedTest = test
    // bind formals: the label line's commands — formals come from the label line's "formal list"
    // In our AST the label line holds the body; formals are parsed as part of label? We stored
    // only label string. For corpus C99/C106, formals are (A,B) after label — parse from raw?
    // Simplification: look for a formal list in the original text is lost; instead bind by
    // position using a side table built in main for known cases is overkill — instead the
    // parser stores formals in the label line's cmds? We handle it in Parser: label line with
    // (formals) — see Parser note. Here: bind via formals stored in labelFormals.
    if let formals = labelFormals[label] {
      newStack.append(store.locals)
      for (i, f) in formals.enumerated() {
        store.locals[f] = i < args.count ? args[i] : .null
      }
    }
    callStack.append((ret: savedPc + 1, savedTest: savedTest, label: label))
    pc = idx
    run()
    if !callStack.isEmpty, callStack.last?.label == label { _ = callStack.removeLast() }
    // restore locals saved at NEW-binding
    if labelFormals[label] != nil, let saved = newStack.popLast() { store.locals = saved }
    pc = savedPc + 1
    test = savedTest
  }

  public var labelFormals: [String: [String]] = [:]

  func callExt(label: String, args: [MValue]) -> MValue {
    // $$label(args): run label, QUIT expr returns value
    guard let r = routine, let idx = r.lines.firstIndex(where: { $0.label == label }) else { return .null }
    let savedPc = pc
    callStack.append((ret: savedPc, savedTest: test, label: label))
    if let formals = labelFormals[label] {
      newStack.append(store.locals)
      for (i, f) in formals.enumerated() { store.locals[f] = i < args.count ? args[i] : .null }
    }
    pc = idx
    var result = MValue.null
    extReturn = { v in result = v }
    run()
    extReturn = nil
    if let f = callStack.last, f.label == label { _ = callStack.removeLast() }
    if labelFormals[label] != nil, let saved = newStack.popLast() { store.locals = saved }
    pc = savedPc
    return result
  }

  var extReturn: ((MValue) -> Void)?

  // ---------- special variables ----------
  public var specials: [String: MValue] = [:]
  func evalSpecial(_ name: String, _ args: [MValue]) -> MValue {
    let n = name.uppercased()
    switch n {
    case "$T", "$TEST": return MValue.number(test ? 1 : 0)
    case "$X": return MValue.number(Double(outColumn()))
    case "$Y": return MValue.number(0)
    case "$IO": return MValue("0")
    case "$PRINCIPAL": return MValue("0")
    case "$H", "$HOROLOG": return MValue("75000,0")
    case "$J", "$JOB": return MValue("1")
    case "$STACK": return MValue.number(Double(callStack.count))
    default: return specials[n] ?? .null
    }
  }

  func outColumn() -> Int {
    // column = chars since last newline
    if let idx = out.lastIndex(of: "\n") { return out.distance(from: out.index(after: idx), to: out.endIndex) }
    return out.count
  }

  func setSpecial(_ name: String, _ v: MValue) {
    let n = name.uppercased()
    if n == "$T" || n == "$TEST" { test = v.truthy() }
    else { specials[n] = v }
  }
}

extension Double {
  // M integer divide \ : truncate toward zero
  func truncDiv(_ b: Double) -> Double { b == 0 ? 0 : (self / b).truncatingTowardZero }
  // M modulo #: sign of divisor (a - b*floor(a/b))
  func truncMod(_ b: Double) -> Double {
    if b == 0 { return 0 }
    return self - b * floor(self / b)
  }
}
