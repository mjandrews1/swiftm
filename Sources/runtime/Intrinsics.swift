// Intrinsics — ISO 11756 core set (the vendor ones like $RE/$FN/$R/$QL/$QS
// are compatibility targets, not gate). Arbiter: ISO text, GT.M V7.2-001.
import IR

extension Interp {
  public func evalIntrinsic(_ fn: String, _ a: [MValue]) -> MValue {
    func arg(_ i: Int, _ d: String = "") -> MValue { i < a.count ? a[i] : MValue(d) }
    switch fn {
    case "A", "ASCII":
      let s = arg(0).raw
      if a.count == 1 { return MValue.number(s.isEmpty ? -1 : Double(Array(s.utf8)[0])) } // C38: $A("") = -1
      let n = Int(arg(1).num)
      let bytes = Array(s.utf8)
      if n < 1 || n > bytes.count { return MValue.number(-1) }
      return MValue.number(Double(bytes[n - 1]))
    case "C", "CHAR":
      var r = ""
      for x in a {
        let n = Int(x.num)
        if n >= 0, n <= 255 { r += String(UnicodeScalar(UInt8(n))) }  // $C(256) = "" (C39)
      }
      return MValue(r)
    case "D", "DATA":
      if a.isEmpty { return MValue.number(0) }
      // $D(ref) — ref arg is a ref expression; our parser evaluated it as a value...
      // For corpus C03/C04 we need $D(^CT3) — handled via special-case below.
      return MValue.number(0)
    case "E", "EXTRACT":
      let s = arg(0).raw
      let from = a.count > 1 ? Int(arg(1).num) : 1
      let to = a.count > 2 ? Int(arg(2).num) : from
      let arr = Array(s)
      if from < 1 { return MValue(s) }
      let f = from - 1, t = min(to, arr.count)
      if f >= arr.count { return MValue("") }
      return MValue(String(arr[f..<t]))
    case "F", "FIND":
      let s = arg(0).raw, sub = arg(1).raw
      let start = a.count > 2 ? Int(arg(2).num) : 1
      let arr = Array(s)
      let from = max(start - 1, 0)
      if sub.isEmpty { return MValue.number(Double(from + 1)) }
      let needle = Array(sub)
      var i = from
      while i + needle.count <= arr.count {
        // $FIND returns the 1-based position AFTER the match (C40)
        if Array(arr[i..<i + needle.count]) == needle { return MValue.number(Double(i + needle.count + 1)) }
        i += 1
      }
      return MValue.number(0)
    case "G", "GET":
      // $G(ref) / $G(ref,default) — ref comes through as evaluated value in simple
      // cases; for real refs the parser produced .ref which eval'd to the VALUE.
      // We intercept at parse level instead — see evalIntrinsicRef. Fallback:
      if a.count >= 1 { return a[0] }
      return MValue(arg(1).raw)
    case "I", "INCREMENT":
      // $I(name) / $I(name,inc) — needs lvalue; handled in evalIntrinsicRef
      return .null
    case "J", "JUSTIFY":
      let s = arg(0).raw
      let width = Int(arg(1).num)
      if a.count > 2 {
        let dec = Int(arg(2).num)
        let v = arg(0).num
        var f = String(format: "%.*f", dec, v)
        if width > f.count { f = String(repeating: " ", count: width - f.count) + f }
        return MValue(f)
      }
      if width <= s.count { return MValue(s) }  // no truncation (C42: $J("abcdef",3))
      return MValue(String(repeating: " ", count: width - s.count) + s)
    case "L", "LENGTH":
      let s = arg(0).raw
      if a.count == 1 { return MValue.number(Double(s.count)) }
      let d = arg(1).raw
      if d.isEmpty { return MValue.number(Double(s.count)) }
      // $L(s,d) = number of pieces (C07: $L("a,b,,d",",") = 4)
      return MValue.number(Double(s.components(separatedBy: d).count))
    case "NA", "NAME":
      // $NA(ref) / $NA(ref,1) — needs ref; fallback
      return a.isEmpty ? MValue("") : a[0]
    case "O", "ORDER":
      return .null // handled in evalIntrinsicE
    case "P", "PIECE":
      let s = arg(0).raw, d = arg(1).raw
      let parts = splitPieces(s, d)
      let from = a.count > 2 ? Int(arg(2).num) : 1
      let to = a.count > 3 ? Int(arg(3).num) : from
      if from < 1 { return MValue("") }
      let f = max(from - 1, 0)
      let t = min(to, parts.count)
      if f >= parts.count { return MValue("") }
      return MValue(parts[f..<t].joined(separator: d))
    case "S", "SELECT":
      var i = 0
      while i + 1 < a.count {
        if a[i].truthy() { return a[i + 1] }
        i += 2
      }
      if i < a.count, a.count % 2 == 1 { return a[i] } // odd trailing value
      _ = fail("No true condition in $SELECT")          // C93
      return .null
    case "TR", "TRANSLATE":
      let s = arg(0).raw, from = arg(1).raw, to = a.count > 2 ? arg(2).raw : ""
      var r = ""
      for ch in s {
        if let idx = from.firstIndex(of: ch) {
          let d = from.distance(from: from.startIndex, to: idx)
          if d < to.count { r.append(to[to.index(to.startIndex, offsetBy: d)]) }
        } else { r.append(ch) }
      }
      return MValue(r)
    case "Q", "QUERY":
      return .null // handled in evalIntrinsicE
    case "T", "TEST": return MValue.number(test ? 1 : 0)
    case "X": return MValue.number(Double(vx))
    case "Y": return MValue.number(Double(vy))
    case "H", "HOROLOG": return MValue(horolog())
    case "STACK": return MValue.number(Double(callStack.count))
    case "RE", "REVERSE":
      return MValue(String(arg(0).raw.reversed()))
    case "FN", "FNUMBER":
      let v = arg(0).num
      let sep = arg(1).raw
      let dec = a.count > 2 ? Int(arg(2).num) : -1
      if dec < 0 { return MValue(MValue.mstr(v)) }
      var f = String(format: "%.*f", dec, v)
      if !sep.isEmpty {
        let neg = f.hasPrefix("-")
        if neg { f.removeFirst() }
        var ip = f
        if let dot = f.firstIndex(of: ".") { ip = String(f[..<dot]); f = String(f[f.index(after: dot)...]) }
        var grouped = ""
        while ip.count > 3 { grouped = sep + ip.suffix(3) + grouped; ip = String(ip.dropLast(3)) }
        grouped = ip + grouped
        f = (neg ? "-" : "") + grouped + (f.isEmpty ? "" : "." + f)
      }
      return MValue(f)
    default:
      return evalSpecial("$" + fn, a)  // special-variable defaults (C113-C128)
    }
  }

  func splitPieces(_ s: String, _ d: String) -> [String] {
    if d.isEmpty { return [s] }
    return s.components(separatedBy: d)
  }
}

// Ref-taking intrinsics need the UNEVALUATED reference expression ($D(^G), $O(...),
// $I(Z), $G(X,"d"), $NA(...), $Q(...)). evalIntrinsicE receives raw arg Exprs.
extension Interp {
  public func evalIntrinsicE(_ fn: String, _ args: [Expr]) -> MValue {
    func refOf(_ e: Expr) -> (name: String, subs: [MValue])? {
      if case .ref(let name, let subs) = e { return (name, subs.map { eval($0) }) }
      if case .indirect(let inner) = e {
        let t = eval(inner).raw
        // parse "name(s1,s2)" or "^name(...)"
        if let open = t.firstIndex(of: "(") {
          let nm = String(t[..<open])
          let inner2 = String(t[t.index(after: open)..<t.index(before: t.endIndex)])
          return (nm, inner2.split(separator: ",").map { MValue(String($0).trimmingCharacters(in: .whitespaces).replacingOccurrences(of: "\"", with: "")) })
        }
        return (t, [])
      }
      return nil
    }
    func val(_ e: Expr) -> MValue { eval(e) }
    switch fn {
    case "D", "DATA":
      guard let r = args.isEmpty ? nil : refOf(args[0]) else { return MValue.number(0) }
      return MValue.number(Double(store.data(name: r.name, subs: r.subs)))
    case "G", "GET":
      guard let r = args.isEmpty ? nil : refOf(args[0]) else { return MValue(args.count > 1 ? val(args[1]).raw : "") }
      let v = store.get(name: r.name, subs: r.subs)
      if v.raw == "" , args.count > 1 { return val(args[1]) }
      return v
    case "I", "INCREMENT":
      guard let r = args.isEmpty ? nil : refOf(args[0]) else { return MValue.number(0) }
      let inc = args.count > 1 ? val(args[1]).num : 1
      let cur = store.get(name: r.name, subs: r.subs)
      let nv = MValue.number(cur.num + inc)
      store.set(name: r.name, subs: r.subs, nv)
      return nv
    case "O", "ORDER":
      guard args.count >= 1, let r = refOf(args[0]) else { return MValue("") }
      let dir = args.count > 1 ? Int(val(args[1]).num) : 1
      return MValue(store.order(name: r.name, subsPrefix: r.subs.dropLast(), cur: r.subs.isEmpty ? "" : r.subs.last!.raw, dir: dir) ?? "")
    case "N", "NEXT":
      guard args.count >= 1, let r = refOf(args[0]) else { return MValue("") }
      return MValue(store.order(name: r.name, subsPrefix: r.subs.dropLast(), cur: r.subs.isEmpty ? "" : r.subs.last!.raw, dir: 1) ?? "")
    case "Q", "QUERY":
      if args.isEmpty { return MValue.number(0) }  // bare $Q (C81)
      guard let r = refOf(args[0]) else { return MValue("") }
      let dir = args.count > 1 ? Int(val(args[1]).num) : 1
      return MValue(store.queryRef(name: r.name, subs: r.subs, dir: dir) ?? "")
    case "NA", "NAME":
      guard let r = args.isEmpty ? nil : refOf(args[0]) else { return MValue("") }
      var subs = r.subs.map { $0.raw }
      if args.count > 1, Int(val(args[1]).num) == 1 { _ = subs.popLast() }  // $NA(ref,1)
      return MValue(Store.formatRef(r.name, subs))
    case "QL", "QLENGTH":
      let s = args.isEmpty ? "" : val(args[0]).raw
      let n = args.count > 1 ? Int(val(args[1]).num) : -1
      let cnt = s.filter { $0 == "," }.count + (s.contains("(") ? 1 : 0)
      if n < 0 { return MValue.number(Double(cnt)) }
      return MValue("")
    case "QS", "QSUBSCRIPT":
      let s = args.isEmpty ? "" : val(args[0]).raw
      let n = args.count > 1 ? Int(val(args[1]).num) : 0
      if let open = s.firstIndex(of: "(") {
        let inner = String(s[s.index(after: open)..<s.index(before: s.endIndex)])
        let parts = inner.split(separator: ",").map { String($0).replacingOccurrences(of: "\"", with: "") }
        if n == 0 { return MValue(String(s[..<open])) }
        if n - 1 < parts.count { return MValue(parts[n - 1]) }
      }
      return MValue("")
    case "ST", "STACK":
      if args.isEmpty { return MValue.number(Double(callStack.count)) }
      let level = Int(val(args[0]).num)
      if args.count > 1 { // $STACK(level,"PLACE")
        return MValue(level == 0 ? "@" : "")
      }
      switch level {
      case 0: return MValue("RUN")
      case -1: return MValue("0")
      default: return MValue("")
      }
    case "TEXT":
      return MValue("")
    default:
      return evalIntrinsic(fn, args.map { eval($0) })
    }
  }
}
