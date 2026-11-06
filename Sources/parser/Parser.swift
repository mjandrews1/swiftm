// Parser — M source text -> Routine (lines of commands). Handles the
// minimal-conforming subset: line mode and routine mode with labels,
// postconditionals, comma lists, WRITE forms, FOR forms, DO/GOTO/QUIT/XECUTE.
import IR

public enum ParseError: Error { case msg(String) }

public struct Parser {
  var s: Substring
  var pos: Substring.Index

  public init(_ text: String) { s = text[...]; pos = s.startIndex }

  static func parseRoutine(_ text: String) throws -> (Routine, [String: [String]]) {
    var lines: [MLine] = []
    var formals: [String: [String]] = [:]
    for raw in text.split(separator: "\n", omittingEmptySubsequences: false) {
      var p = Parser(String(raw))
      // leading whitespace = line level (dotted blocks); we flatten levels and
      // treat a dotted block as part of the enclosing line (M semantics).
      var label: String? = nil
      if !p.peek().hasPrefix(" ") && !p.peek().hasPrefix("\t") {
        // could be a label: read a token starting at column 0
        let save = p.pos
        if let tok = p.readWord(), !tok.isEmpty {
          if isCommandWord(tok) {
            p.pos = save
          } else {
            label = tok
            // optional formal list: LABEL(A,B)
            if p.eat("(") {
              var fs: [String] = []
              if !(p.pos < p.s.endIndex && p.s[p.pos] == ")") {
                repeat {
                  if let fw = p.readWord() { fs.append(fw) }
                  p.skipWs()
                } while p.eat(",")
              }
              _ = p.eat(")")
              if !fs.isEmpty { formals[tok] = fs }
            }
          }
        } else { p.pos = save }
      }
      // skip leading whitespace (level)
      p.skipWs()
      let cmds = try p.parseCommands()
      if label != nil || !cmds.isEmpty { lines.append(MLine(label: label, cmds: cmds)) }
    }
    return (Routine(lines: lines), formals)
  }

  static func isCommandWord(_ w: String) -> Bool {
    ["S","SET","W","WRITE","I","IF","E","ELSE","F","FOR","K","KILL","N","NEW","D","DO",
     "G","GOTO","Q","QUIT","X","XECUTE","H","HANG","HALT","M","MERGE","L","LOCK","V","VIEW",
     "B","BREAK","T","TSTART","R","READ","O","OPEN","U","USE","C","CLOSE","J","JOB"].contains(w.uppercased())
  }

  func peek() -> Substring { s[pos...] }
  mutating func skipWs() { while pos < s.endIndex, s[pos] == " " || s[pos] == "\t" { pos = s.index(after: pos) } }
  mutating func eat(_ c: Character) -> Bool {
    if pos < s.endIndex, s[pos] == c { pos = s.index(after: pos); return true }
    return false
  }

  mutating func readWord() -> String? {
    let start = pos
    while pos < s.endIndex, s[pos].isLetter || s[pos].isNumber || s[pos] == "%" || s[pos] == "$" {
      pos = s.index(after: pos)
    }
    return start < pos ? String(s[start..<pos]) : nil
  }

  // ---- commands ----
  mutating func parseCommands() throws -> [Cmd] {
    var cmds: [Cmd] = []
    while pos < s.endIndex {
      skipWs()
      if pos >= s.endIndex { break }
      // IF/ELSE consume the rest of the line as their body
      if let w = peekWordUpper() {
        if w == "I" || w == "IF" {
          pos = s.index(pos, offsetBy: w.count)
          let post = try parsePostcond()
          let cond = try parseExpr()
          let body = try parseCommands()
          cmds.append(.ifCmd(cond: cond, body: body))
          _ = post
          return cmds
        }
        if w == "E" || w == "ELSE" {
          pos = s.index(pos, offsetBy: w.count)
          let body = try parseCommands()
          cmds.append(.elseCmd(body: body))
          return cmds
        }
      }
      let cmd = try parseCommand()
      cmds.append(cmd)
    }
    return cmds
  }

  mutating func peekWordUpper() -> String? {
    var i = pos
    var w = ""
    while i < s.endIndex, s[i].isLetter { w.append(s[i]); i = s.index(after: i) }
    return w.isEmpty ? nil : w.uppercased()
  }

  mutating func parsePostcond() throws -> Expr? {
    skipWs()
    if eat(":") { return try parseExpr() }
    return nil
  }

  mutating func parseCommand() throws -> Cmd {
    guard let w = readWord() else { throw ParseError.msg("expected command at \(peek())") }
    let up = w.uppercased()
    let post = try parsePostcond()
    switch up {
    case "S", "SET":
      var assigns: [(Expr, Expr)] = []
      repeat {
        let lhs = try parseLValue()
        skipWs(); guard eat("=") else { throw ParseError.msg("SET needs =") }
        let rhs = try parseExpr()
        assigns.append((lhs, rhs))
        skipWs()
      } while eat(",")
      return .set(assigns)
    case "W", "WRITE":
      var args: [WriteArg] = []
      repeat {
        skipWs()
        if pos < s.endIndex {
          if s[pos] == "!" { pos = s.index(after: pos); args.append(.newline); skipWs(); if eat(",") { continue } else { break } }
          if s[pos] == "#" { pos = s.index(after: pos); args.append(.formfeed); skipWs(); if eat(",") { continue } else { break } }
          if s[pos] == "?" {
            pos = s.index(after: pos)
            if eat(",") { args.append(.tab); continue }
            let e = try parseExpr(); args.append(.col(e)); skipWs()
            if eat(",") { continue } else { break }
          }
          if s[pos] == "*" { pos = s.index(after: pos); let e = try parseExpr(); args.append(.char(e)); skipWs(); if eat(",") { continue } else { break } }
        }
        let e = try parseExpr(); args.append(.expr(e)); skipWs()
      } while eat(",")
      return .write(args)
    case "F", "FOR":
      // F  | F var | F var=s:e | F var=s:st:e | F var=l1,l2,...
      skipWs()
      if pos >= s.endIndex || s[pos] == " " || s[pos] == "\t" {
        // argumentless FOR: body = rest of line
        let body = try parseCommands()
        return .forLoop(ctrl: nil, range: nil, list: nil, body: body)
      }
      let ctrl = try parseLValue()
      skipWs()
      if eat(":") {
        // has postcond after ctrl? FOR var:cond — treat cond as postcond on whole FOR
        let pc = try parseExpr()
        let body = try parseCommands()
        // represent as ifCmd around forLoop
        return .ifCmd(cond: pc, body: [.forLoop(ctrl: ctrl, range: nil, list: nil, body: body)])
      }
      if eat("=") {
        let first = try parseExpr()
        skipWs()
        if eat(":") {
          let step = try parseExpr(); skipWs(); guard eat(":") else { throw ParseError.msg("FOR range") }
          let end = try parseExpr()
          let body = try parseCommands()
          return .forLoop(ctrl: ctrl, range: (first, step, end), list: nil, body: body)
        } else {
          // list form or start:end
          skipWs()
          if eat(":") {
            let end = try parseExpr()
            let body = try parseCommands()
            return .forLoop(ctrl: ctrl, range: (first, nil, end), list: nil, body: body)
          }
          var list = [first]
          while eat(",") { list.append(try parseExpr()) }
          let body = try parseCommands()
          return .forLoop(ctrl: ctrl, range: nil, list: list, body: body)
        }
      }
      let body = try parseCommands()
      return .forLoop(ctrl: ctrl, range: nil, list: nil, body: body)
    case "K", "KILL":
      skipWs()
      if eat("(") {
        var names: [Expr] = []
        repeat { names.append(try parseLValue()); skipWs() } while eat(",")
        guard eat(")") else { throw ParseError.msg("K ( needs )") }
        return .kill(names: names, exclusive: true)
      }
      var names: [Expr] = []
      if pos < s.endIndex, s[pos] != " ", s[pos] != "\t" {
        repeat { names.append(try parseLValue()); skipWs() } while eat(",")
      }
      return .kill(names: names, exclusive: false)
    case "N", "NEW":
      skipWs()
      if eat("(") {
        var names: [Expr] = []
        repeat { names.append(try parseLValue()); skipWs() } while eat(",")
        guard eat(")") else { throw ParseError.msg("N ( needs )") }
        return .newCmd(names: names, exclusive: true)
      }
      if pos < s.endIndex, s[pos] != " ", s[pos] != "\t" {
        var names: [Expr] = []
        repeat { names.append(try parseLValue()); skipWs() } while eat(",")
        return .newCmd(names: names, exclusive: false)
      }
      return .newCmd(names: nil, exclusive: false)
    case "D", "DO":
      skipWs()
      var label: String? = nil
      var args: [Expr] = []
      if pos < s.endIndex, s[pos] != " ", s[pos] != "\t" {
        // target: label or ^routine or label^routine, optional (args)
        if let w2 = readWord() {
          label = w2
          if eat("^") { label = (label ?? "") + "^" + (readWord() ?? "") }
        }
        if eat("(") {
          if !(pos < s.endIndex && s[pos] == ")") {
            repeat { args.append(try parseExpr()); skipWs() } while eat(",")
          }
          guard eat(")") else { throw ParseError.msg("D ( needs )") }
        }
      }
      let body = try parseCommands()
      return .doCall(label: label, args: args, postcond: post, body: body)
    case "G", "GOTO":
      guard let lbl = readWord() else { throw ParseError.msg("GOTO needs label") }
      return .goto(label: lbl, postcond: post)
    case "Q", "QUIT":
      skipWs()
      if pos < s.endIndex, s[pos] != " ", s[pos] != "\t" {
        let e = try parseExpr()
        return .quit(expr: e, postcond: post)
      }
      return .quit(expr: nil, postcond: post)
    case "X", "XECUTE":
      let e = try parseExpr()
      return .xecute(code: e, postcond: post)
    case "H", "HANG", "HALT":
      if up == "HALT" { return .quit(expr: nil, postcond: nil) }
      skipWs()
      if pos < s.endIndex, s[pos] != " ", s[pos] != "\t" { return .hang(sec: try parseExpr()) }
      return .hang(sec: nil)
    case "M", "MERGE":
      let dst = try parseLValue(); skipWs(); guard eat("=") else { throw ParseError.msg("MERGE =") }
      let src = try parseLValue()
      return .merge(dst: dst, src: src)
    case "L", "LOCK":
      var plus = false, minus = false
      skipWs()
      if eat("+") { plus = true } else if eat("-") { minus = true }
      var args: [Expr] = []
      if pos < s.endIndex, s[pos] != " ", s[pos] != "\t" {
        repeat { args.append(try parseLValue()); skipWs() } while eat(",")
      }
      var to: Expr? = nil
      if eat(":") { to = try parseExpr() }
      return .lock(args: args, plus: plus, minus: minus, timeout: to)
    case "V", "VIEW":
      var args: [Expr] = []
      if pos < s.endIndex, s[pos] != " ", s[pos] != "\t" {
        repeat { args.append(try parseExpr()); skipWs() } while eat(",")
      }
      return .view(args: args)
    case "B", "BREAK":
      return .breakCmd
    default:
      throw ParseError.msg("unknown command \(w)")
    }
  }

  // lvalue: name or name(subs) or ^name(...)
  mutating func parseLValue() throws -> Expr {
    skipWs()
    var name = ""
    if eat("^") { name = "^" }
    guard let w = readWord() else { throw ParseError.msg("expected name at \(peek())") }
    name += w
    var subs: [Expr] = []
    if eat("(") {
      if !(pos < s.endIndex && s[pos] == ")") {
        repeat { subs.append(try parseExpr()); skipWs() } while eat(",")
      }
      guard eat(")") else { throw ParseError.msg("unclosed (") }
    }
    return .ref(name: name, subs: subs)
  }

  // ---- expressions: strict left-to-right, no precedence ----
  mutating func parseExpr() throws -> Expr {
    var lhs = try parseUnary()
    while true {
      skipWs()
      guard pos < s.endIndex else { break }
      let c = s[pos]
      // two-char operators first
      let two = pos < s.index(before: s.endIndex) ? String(s[pos...s.index(pos, offsetBy: 1)]) : ""
      var op: String? = nil
      if ["<=", ">=", "]]", "']]", "'[", "']"].contains(two) { op = two; pos = s.index(pos, offsetBy: 2) }
      else if ["[", "]", "=", "<", ">", "+", "-", "*", "/", "\\", "#", "_", "&", "!", "'", "?"].contains(String(c)) {
        op = String(c); pos = s.index(after: pos)
        if op == "'" || op == "!" {
          // postfix not-operators: '= '< '< '> ' etc. — M: '= is "not equals"
          if pos < s.endIndex, s[pos] == "=" || s[pos] == "<" || s[pos] == ">" || s[pos] == "[" || s[pos] == "]" {
            op = "'" + String(s[pos]); pos = s.index(after: pos)
          }
        }
      } else if c == "*" {
        // could be ** (power)
        op = "*"; pos = s.index(after: pos)
        if pos < s.endIndex, s[pos] == "*" { op = "**"; pos = s.index(after: pos) }
      } else { break }
      guard let o = op else { break }
      let rhs = try parseUnary()
      lhs = .bin(op: o, l: lhs, r: rhs)
    }
    return lhs
  }

  mutating func parseUnary() throws -> Expr {
    skipWs()
    if pos < s.endIndex {
      let c = s[pos]
      if c == "-" { pos = s.index(after: pos); return .un(op: "-", e: try parseUnary()) }
      if c == "'" { pos = s.index(after: pos); return .un(op: "'", e: try parseUnary()) }
      if c == "&" { pos = s.index(after: pos); return .un(op: "&", e: try parseUnary()) }
      if c == "@" { pos = s.index(after: pos); return .indirect(try parseUnary()) }
    }
    return try parsePrimary()
  }

  mutating func parsePrimary() throws -> Expr {
    skipWs()
    guard pos < s.endIndex else { throw ParseError.msg("unexpected end of expr") }
    let c = s[pos]
    if c == "(" {
      pos = s.index(after: pos)
      let e = try parseExpr()
      guard eat(")") else { throw ParseError.msg("unclosed ( in expr") }
      return e
    }
    if c == "\"" {
      pos = s.index(after: pos)
      var str = ""
      while pos < s.endIndex {
        if s[pos] == "\"" {
          if s.index(after: pos) < s.endIndex, s[s.index(after: pos)] == "\"" {
            str += "\""; pos = s.index(pos, offsetBy: 2)
          } else { pos = s.index(after: pos); break }
        } else { str.append(s[pos]); pos = s.index(after: pos) }
      }
      return .lit(MValue(str))
    }
    if c == "$" {
      pos = s.index(after: pos)
      if pos < s.endIndex, s[pos] == "$" {
        pos = s.index(after: pos)
        guard let lbl = readWord() else { throw ParseError.msg("$$ needs label") }
        var args: [Expr] = []
        if eat("(") {
          if !(pos < s.endIndex && s[pos] == ")") {
            repeat { args.append(try parseExpr()); skipWs() } while eat(",")
          }
          guard eat(")") else { throw ParseError.msg("unclosed $$ (") }
        }
        return .extCall(label: lbl, args: args)
      }
      var fn = ""
      while pos < s.endIndex, s[pos].isLetter { fn.append(s[pos].uppercased()); pos = s.index(after: pos) }
      // allow $NAME-style long names; fn already uppercased
      var args: [Expr] = []
      if eat("(") {
        if !(pos < s.endIndex && s[pos] == ")") {
          repeat { args.append(try parseExpr()); skipWs() } while eat(",")
        }
        guard eat(")") else { throw ParseError.msg("unclosed $ (") }
      }
      return .intrinsic(fn: fn, args: args)
    }
    if c.isNumber || (c == "." ) {
      let start = pos
      while pos < s.endIndex, s[pos].isNumber || s[pos] == "." || s[pos] == "E" || s[pos] == "e" || s[pos] == "+" || s[pos] == "-" {
        // careful: +/- only right after E
        if (s[pos] == "+" || s[pos] == "-"), pos == start { pos = s.index(after: pos); continue }
        if (s[pos] == "+" || s[pos] == "-"), s[s.index(before: pos)] != "E", s[s.index(before: pos)] != "e" { break }
        pos = s.index(after: pos)
      }
      let tok = String(s[start..<pos])
      return .lit(MValue(tok, isnum: MValue.isCanonicalNum(tok)))
    }
    if c.isLetter || c == "^" || c == "%" {
      return try parseLValue()
    }
    throw ParseError.msg("unexpected char \(c) in expr")
  }
}
