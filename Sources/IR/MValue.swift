// MValue — an M value is its string form; isnum marks a canonical number.
// Semantics pinned in m-semantics MSemantics.dfy (parseNum is the whole-string
// numeric predicate; val() is the longest-numeric-prefix coercion).
import Foundation

public struct MValue: Equatable, Hashable {
  public var raw: String
  public var isnum: Bool

  public static let null = MValue("", isnum: true) // "" is canonical 0

  public init(_ raw: String, isnum: Bool = false) {
    self.raw = raw
    self.isnum = isnum
  }

  public static func number(_ d: Double) -> MValue {
    MValue(MValue.mstr(d), isnum: true)
  }

  // Canonical numeric string check: [+|-]digits[.digits][E[+|-]digits]
  public static func isCanonicalNum(_ s: String) -> Bool {
    parseNum(s, 0) == s.count
  }

  // Longest numeric prefix length, or -1 if none (mirrors MSemantics.parseNum).
  public static func parseNum(_ s: String, _ i0: Int) -> Int {
    let a = Array(s.utf8)
    var i = i0
    if i < a.count, a[i] == 43 || a[i] == 45 { i += 1 } // + or -
    let start = i
    while i < a.count, a[i] >= 48, a[i] <= 57 { i += 1 }
    let intDigits = i - start
    var fracDigits = 0
    if i < a.count, a[i] == 46 { // .
      let j = i + 1
      var k = j
      while k < a.count, a[k] >= 48, a[k] <= 57 { k += 1 }
      fracDigits = k - j
      i = k
    }
    if intDigits == 0 && fracDigits == 0 { return -1 }
    if i < a.count, a[i] == 69 || a[i] == 101 { // E e
      var k = i + 1
      if k < a.count, a[k] == 43 || a[k] == 45 { k += 1 }
      let e0 = k
      while k < a.count, a[k] >= 48, a[k] <= 57 { k += 1 }
      if k == e0 { return -1 } // E with no digits: not numeric
      i = k
    }
    return i
  }

  // val(): longest-numeric-prefix coercion to Double ("1abc"->1, "abc"->0).
  public static func val(_ s: String) -> Double {
    let p = parseNum(s, 0)
    if p <= 0 { return 0 }
    return Double(String(s.prefix(p))) ?? 0
  }

  public var num: Double { MValue.val(raw) }

  // Canonical M number formatting (mstr): no trailing .0, no leading +/zeros,
  // exponent form only for very large/small magnitudes (like GT.M).
  public static func mstr(_ d: Double) -> String {
    if d.isNaN { return "NaN" }
    if d.isInfinite { return d > 0 ? "Inf" : "-Inf" }
    if d == 0 { return "0" } // canonicalizes -0 to 0
    if d == floor(d), abs(d) < 1e15 {
      return String(format: "%.0f", d)
    }
    if abs(d) >= 1e15 || (abs(d) < 1e-6 && d != 0) {
      return String(format: "%.6E", d) // e.g. 1.000000E+15 — refined below if needed
    }
    // trim trailing zeros
    var s = String(format: "%.10f", d)
    if s.contains(".") {
      while s.hasSuffix("0") { s.removeLast() }
      if s.hasSuffix(".") { s.removeLast() }
    }
    return s
  }

  // M truthiness (pymumps m_truthy_str): numeric -> != 0; else scan: skip
  // leading +/- and zeros and one '.', true on first 1-9.
  public func truthy() -> Bool {
    if isnum { return num != 0 }
    var i = raw.startIndex
    if i < raw.endIndex, raw[i] == "+" || raw[i] == "-" { i = raw.index(after: i) }
    var seenDot = false
    while i < raw.endIndex {
      let c = raw[i]
      if c == "." {
        if seenDot { return false }
        seenDot = true
      } else if c == "0" {
        // leading zeros don't make it true
      } else if c >= "1", c <= "9" {
        return true
      } else {
        return false
      }
      i = raw.index(after: i)
    }
    return false
  }

  // M collation: null < numbers (numerically) < non-numeric strings (bytewise).
  public static func coll(_ a: String, _ b: String) -> Int {
    let an = isCanonicalNum(a), bn = isCanonicalNum(b)
    if an, bn {
      let x = val(a), y = val(b)
      return x < y ? -1 : (x == y ? 0 : 1)
    }
    if an { return -1 }
    if bn { return 1 }
    if a == b { return 0 }
    return a < b ? -1 : 1
  }

  // Comparison per ISO 11756: both canonical numeric -> numeric; else string.
  public static func compare(_ a: MValue, _ b: MValue) -> Int {
    if a.isnum && b.isnum {
      let x = a.num, y = b.num
      return x < y ? -1 : (x == y ? 0 : 1)
    }
    if a.raw == b.raw { return 0 }
    return a.raw < b.raw ? -1 : 1
  }
}
