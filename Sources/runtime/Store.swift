// Store — locals (simple map + per-name subscripted trees) and globals trees,
// with M-collation-ordered traversal ($ORDER/$QUERY) per MSemantics.dfy.
import IR

public final class Node {
  public var value: MValue?
  public var kids: [String: Node] = [:]
  public init() {}
}

public final class Store {
  public var locals: [String: MValue] = [:]   // simple local names
  public var localTree = Node()               // kids[varName] -> subscripted subtree
  public var globals: [String: Node] = [:]    // "NAME" (caret stripped) -> tree

  public init() {}

  // Root tree for a name: globals["G"] for ^G; localTree.kids["x"] for local x.
  func tree(for name: String) -> Node? {
    if name.hasPrefix("^") { return globals[String(name.dropFirst())] }
    return localTree.kids[name]
  }

  func ensureTree(for name: String) -> Node {
    if name.hasPrefix("^") {
      let g = String(name.dropFirst())
      if globals[g] == nil { globals[g] = Node() }
      return globals[g]!
    }
    if localTree.kids[name] == nil { localTree.kids[name] = Node() }
    return localTree.kids[name]!
  }

  func descend(_ tree: Node, _ subs: [MValue]) -> Node {
    var n = tree
    for s in subs {
      let k = s.raw
      if n.kids[k] == nil { n.kids[k] = Node() }
      n = n.kids[k]!
    }
    return n
  }

  func nodeAt(_ name: String, _ subs: [MValue]) -> Node? {
    guard let t = tree(for: name) else { return nil }
    var n = t
    for s in subs {
      guard let kid = n.kids[s.raw] else { return nil }
      n = kid
    }
    return n
  }

  // Non-creating read; nil = undefined (caller decides error vs null).
  public func fetch(name: String, subs: [MValue]) -> MValue? {
    if subs.isEmpty, !name.hasPrefix("^") { return locals[name] }
    return nodeAt(name, subs)?.value
  }

  public func get(name: String, subs: [MValue]) -> MValue {
    fetch(name: name, subs: subs) ?? .null
  }

  public func set(name: String, subs: [MValue], _ v: MValue) {
    if subs.isEmpty, !name.hasPrefix("^") { locals[name] = v; return }
    descend(ensureTree(for: name), subs).value = v
  }

  public func kill(name: String, subs: [MValue]) {
    if subs.isEmpty {
      if name.hasPrefix("^") { globals.removeValue(forKey: String(name.dropFirst())) }
      else { locals.removeValue(forKey: name); localTree.kids.removeValue(forKey: name) }
      return
    }
    // kill node + subtree
    guard let t = tree(for: name) else { return }
    var n = t
    for s in subs.dropLast() {
      guard let kid = n.kids[s.raw] else { return }
      n = kid
    }
    if let last = subs.last { n.kids.removeValue(forKey: last.raw) }
  }

  // $DATA: 0 none, 1 value only, 10 children only, 11 both
  public func data(name: String, subs: [MValue]) -> Int {
    guard let n = nodeAt(name, subs) else { return 0 }
    let hasVal = n.value != nil
    let hasKids = !n.kids.isEmpty
    return hasVal ? (hasKids ? 11 : 1) : (hasKids ? 10 : 0)
  }

  // $ORDER/$NEXT: next/prev key in collation order after `cur` ("" = first;
  // for dir=-1 with cur="" the last key, per GT.M/RSM).
  public func order(name: String, subsPrefix: [MValue], cur: String, dir: Int) -> String? {
    guard let t = tree(for: name) else { return nil }
    var n = t
    for s in subsPrefix {
      guard let kid = n.kids[s.raw] else { return nil }
      n = kid
    }
    let keys = n.kids.keys.sorted { MValue.coll($0, $1) < 0 }
    if keys.isEmpty { return nil }
    if dir > 0 {
      return keys.first { cur.isEmpty || MValue.coll($0, cur) > 0 }
    } else {
      if cur.isEmpty { return keys.last }
      return keys.reversed().first { MValue.coll($0, cur) < 0 }
    }
  }

  // Canonical reference formatting: numeric subscripts bare, strings quoted.
  public static func canonSub(_ s: String) -> String {
    if MValue.numericStr(s) && !s.hasPrefix("+") { return s }
    return "\"" + s.replacingOccurrences(of: "\"", with: "\"\"") + "\""
  }

  public static func formatRef(_ name: String, _ subs: [String]) -> String {
    if subs.isEmpty { return name }
    return name + "(" + subs.map { canonSub($0) }.joined(separator: ",") + ")"
  }

  // $QUERY: next (dir=1) / previous (dir=-1) node in pre-order traversal of the
  // tree rooted at name. subs == [""] means the root itself.
  public func queryRef(name: String, subs: [MValue], dir: Int) -> String? {
    let subsS = subs.map { $0.raw }
    var path = (subsS == [""]) ? [] : subsS
    if dir > 0 {
      if let n = nodeAt(name, path.map { MValue($0) }), !n.kids.isEmpty {
        let first = n.kids.keys.sorted { MValue.coll($0, $1) < 0 }.first!
        return Store.formatRef(name, path + [first])
      }
      // climb for a next sibling
      while !path.isEmpty {
        let lastKey = path.last!
        let parentPath = Array(path.dropLast())
        guard let parent = nodeAt(name, parentPath.map { MValue($0) }) else { return nil }
        let sibs = parent.kids.keys.sorted { MValue.coll($0, $1) < 0 }
        if let nx = sibs.first(where: { MValue.coll($0, lastKey) > 0 }) {
          return Store.formatRef(name, parentPath + [nx])
        }
        path = parentPath
      }
      // next global name
      if name.hasPrefix("^") {
        let g = String(name.dropFirst())
        let gs = globals.keys.sorted { MValue.coll($0, $1) < 0 }
        if let nx = gs.first(where: { MValue.coll($0, g) > 0 }) { return "^" + nx }
      }
      return nil
    } else {
      // previous node in pre-order
      if !path.isEmpty {
        let lastKey = path.last!
        let parentPath = Array(path.dropLast())
        if let parent = nodeAt(name, parentPath.map { MValue($0) }) {
          let sibs = parent.kids.keys.sorted { MValue.coll($0, $1) < 0 }
          if let pv = sibs.reversed().first(where: { MValue.coll($0, lastKey) < 0 }) {
            // last descendant of pv
            var q = parentPath + [pv]
            var n = nodeAt(name, q.map { MValue($0) })!
            while !n.kids.isEmpty {
              let l = n.kids.keys.sorted { MValue.coll($0, $1) < 0 }.last!
              q.append(l)
              n = n.kids[l]!
            }
            return Store.formatRef(name, q)
          }
          // no previous sibling: the parent node itself
          return Store.formatRef(name, parentPath)
        }
      }
      // previous global name
      if name.hasPrefix("^") {
        let g = String(name.dropFirst())
        let gs = globals.keys.sorted { MValue.coll($0, $1) < 0 }
        if let pv = gs.reversed().first(where: { MValue.coll($0, g) < 0 }) { return "^" + pv }
      }
      return nil
    }
  }
}
