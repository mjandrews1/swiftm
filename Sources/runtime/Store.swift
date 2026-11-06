// Store — locals (simple map + subscripted tree) and globals tree, with
// M-collation-ordered traversal ($ORDER/$QUERY) per MSemantics.dfy.
import IR

public final class Node {
  public var value: MValue?
  public var kids: [String: Node] = [:]
  public init() {}
}

public final class Store {
  public var locals: [String: MValue] = [:]   // simple local names
  public var localTree = Node()               // subscripted locals
  public var globals: [String: Node] = [:]    // "^NAME" -> tree

  public init() {}

  // Resolve a reference to its (container, key) for SET/KILL, or read value.
  public func node(for name: String, subs: [MValue]) -> (Node, String)? {
    let tree: Node
    if name.hasPrefix("^") {
      let g = String(name.dropFirst())
      if globals[g] == nil { globals[g] = Node() }
      tree = globals[g]!
    } else {
      tree = localTree
    }
    var n = tree
    for s in subs {
      let k = String(s.raw)
      if n.kids[k] == nil { n.kids[k] = Node() }
      n = n.kids[k]!
    }
    return (n, subs.isEmpty ? "" : String(subs.last!.raw))
  }

  public func get(name: String, subs: [MValue]) -> MValue {
    if subs.isEmpty, !name.hasPrefix("^") { return locals[name] ?? .null }
    guard let (n, _) = node(for: name, subs: subs) else { return .null }
    return n.value ?? .null
  }

  public func set(name: String, subs: [MValue], _ v: MValue) {
    if subs.isEmpty, !name.hasPrefix("^") { locals[name] = v; return }
    guard let (n, _) = node(for: name, subs: subs) else { return }
    n.value = v
  }

  public func kill(name: String, subs: [MValue]) {
    if subs.isEmpty {
      if name.hasPrefix("^") { globals.removeValue(forKey: String(name.dropFirst())) }
      else { locals.removeValue(forKey: name); localTree.kids.removeValue(forKey: name) }
      return
    }
    // kill node + subtree
    guard var n = (name.hasPrefix("^") ? globals[String(name.dropFirst())] : localTree) else { return }
    for s in subs.dropLast() { n = n.kids[String(s.raw)] ?? Node(); }
    if let last = subs.last { n.kids.removeValue(forKey: String(last.raw)) }
  }

  // $DATA: 0 none, 1 value only, 10 children only, 11 both
  public func data(name: String, subs: [MValue]) -> Int {
    let tree = name.hasPrefix("^") ? globals[String(name.dropFirst())] : localTree
    guard var n = tree else { return 0 }
    for s in subs {
      if let kid = n.kids[String(s.raw)] { n = kid } else { return 0 }
    }
    let hasVal = n.value != nil
    let hasKids = !n.kids.isEmpty
    return hasVal ? (hasKids ? 11 : 1) : (hasKids ? 10 : 0)
  }

  // $ORDER/$QUERY: next key in collation order after `cur` ("" = first).
  public func order(name: String, subsPrefix: [MValue], cur: String, dir: Int) -> String? {
    let tree = name.hasPrefix("^") ? globals[String(name.dropFirst())] : localTree
    guard var n = tree else { return nil }
    for s in subsPrefix {
      if let kid = n.kids[String(s.raw)] { n = kid } else { return nil }
    }
    let keys = n.kids.keys.sorted { MValue.coll($0, $1) < 0 }
    if dir > 0 {
      return keys.first { cur.isEmpty || MValue.coll($0, cur) > 0 }
    } else {
      return keys.reversed().first { !cur.isEmpty && MValue.coll($0, cur) < 0 }
    }
  }

  // $QUERY: full reference string of next node in collation order after ref.
  public func query(_ ref: String) -> String? {
    // ref like ^G(a,b) or ^G — find next node strictly after
    guard let open = ref.firstIndex(of: "(") else {
      // root: first child of ^G, or next global
      let g = ref.hasPrefix("^") ? String(ref.dropFirst()) : ref
      if let node = (ref.hasPrefix("^") ? globals[g] : localTree), !node.kids.isEmpty {
        let first = node.kids.keys.sorted { MValue.coll($0, $1) < 0 }.first!
        return ref + "(" + first + ")"
      }
      // next global name after g
      let gs = globals.keys.sorted { MValue.coll($0, $1) < 0 }
      if let nx = gs.first(where: { MValue.coll($0, g) > 0 }) { return "^" + nx }
      return nil
    }
    let head = String(ref[..<open])
    let inner = String(ref[ref.index(after: open)..<ref.index(before: ref.endIndex)])
    let subs = splitSubs(inner)
    let tree = head.hasPrefix("^") ? globals[String(head.dropFirst())] : localTree
    guard var n = tree else { return nil }
    for s in subs {
      if let kid = n.kids[s] { n = kid } else { return nil }
    }
    if let v = n.value, subs.isEmpty == false {
      // node itself has value; next is first child if any, else next sibling...
      if !n.kids.isEmpty {
        let first = n.kids.keys.sorted { MValue.coll($0, $1) < 0 }.first!
        return head + "(" + inner + "," + first + ")"
      }
    }
    // next sibling at last level, else bubble up — simplified: next key at this level
    let keys = n.kids.keys.sorted { MValue.coll($0, $1) < 0 }
    let lastKey = subs.last ?? ""
    if let nx = keys.first(where: { MValue.coll($0, lastKey) > 0 }) {
      return head + "(" + subs.dropLast().joined(separator: ",") + (subs.count > 1 ? "," : "") + nx + ")"
    }
    return nil
  }

  func splitSubs(_ inner: String) -> [String] {
    var out: [String] = []
    var cur = ""
    var inStr = false
    for ch in inner {
      if ch == "\"" { inStr.toggle(); cur.append(ch) }
      else if ch == "," && !inStr { out.append(cur); cur = "" }
      else { cur.append(ch) }
    }
    if !cur.isEmpty { out.append(cur) }
    // strip quotes
    return out.map { $0.hasPrefix("\"") ? String($0.dropFirst().dropLast()) : $0 }
  }
}
