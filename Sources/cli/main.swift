// swiftm CLI — minimal-conforming M interpreter.
//   swiftm run <file.m>        run a routine/direct-mode file
//   swiftm corpus <json>       run the shared corpus, report pass/fail per case
import Foundation
import IR
import parser
import runtime

func runCase(_ code: String) -> String {
  let (routine, formals) = (try? Parser.parseRoutine(code)) ?? (Routine(lines: []), [:])
  let it = Interp()
  it.routine = routine
  it.labelFormals = formals
  if routine.lines.first?.label != nil {
    it.pc = 0
    it.run()
  } else {
    for line in routine.lines { it.execLine(line) }
  }
  // On a runtime error the unflushed WRITE buffer is discarded (mjvm C02):
  // keep only completed lines, then the error message.
  if let e = it.error {
    if let idx = it.out.lastIndex(of: "\n") { return String(it.out[...idx]) + e }
    return e
  }
  return it.out
}

// Corpus expecteds omit trailing newlines from WRITE ! (same normalize as
// mjvm CorpusScore.java).
func normalize(_ s: String) -> String {
  var t = s.replacingOccurrences(of: "\r\n", with: "\n")
  while t.hasSuffix("\n") { t.removeLast() }
  return t
}

// Minimal JSON reader for {"cases":[{"name","code","expected"},...]} (same
// schema as pymumps/tests/conformance.json; no Foundation JSON needed for the
// flat shape — but Foundation is fine on macOS).
struct CorpusCase { var name: String; var code: String; var expected: String }

func loadCorpus(_ path: String) throws -> [CorpusCase] {
  let data = try Data(contentsOf: URL(fileURLWithPath: path))
  guard let obj = try JSONSerialization.jsonObject(with: data) as? [String: Any],
        let cases = obj["cases"] as? [[String: Any]] else { return [] }
  return cases.map {
    CorpusCase(name: $0["name"] as? String ?? "",
               code: $0["code"] as? String ?? "",
               expected: $0["expected"] as? String ?? "")
  }
}

let args = CommandLine.arguments
if args.count >= 3, args[1] == "run" {
  let text = try String(contentsOfFile: args[2], encoding: .utf8)
  print(runCase(text), terminator: "")
} else if args.count >= 3, args[1] == "corpus" {
  let cases = try loadCorpus(args[2])
  var pass = 0, fail = 0
  var failures: [String] = []
  let only = args.count >= 4 ? args[3] : nil
  for c in cases {
    if let only, !c.name.contains(only) { continue }
    let got = runCase(c.code)
    if normalize(got) == normalize(c.expected) { pass += 1 }
    else { fail += 1; failures.append("\(c.name): got=\(String(reflecting: got).prefix(80)) want=\(String(reflecting: c.expected).prefix(80))") }
  }
  print("corpus: pass=\(pass) fail=\(fail) total=\(pass + fail)")
  for f in failures.prefix(40) { print("  FAIL \(f)") }
} else if args.count >= 3, args[1] == "eval" {
  let e = try Parser.parseExprString(args[2])
  let it = Interp()
  let v = it.eval(e)
  print("\(v) isnum=\(v.isnum) err=\(it.error ?? "-")")
} else if args.count >= 3, args[1] == "parse" {
  let text = try String(contentsOfFile: args[2], encoding: .utf8)
  let (routine, _) = try Parser.parseRoutine(text)
  for l in routine.lines { print(l.label ?? "-", l.cmds) }
} else if args.count >= 3, args[1] == "dump" {
  // dump <json>: emit {"cases":[{name,out}]} — same shape as the mjvm harness dump
  let cases = try loadCorpus(args[2])
  var parts: [String] = []
  for c in cases { parts.append("{\"name\":\(jsonStr(c.name)),\"out\":\(jsonStr(runCase(c.code)))}") }
  print("{\"cases\":[" + parts.joined(separator: ",") + "]}")
} else {
  print("usage: swiftm run <file.m> | swiftm corpus <conformance.json> [nameFilter] | swiftm dump <conformance.json>")
}

func jsonStr(_ s: String) -> String {
  var t = "\""
  for ch in s.unicodeScalars {
    switch ch {
    case "\"": t += "\\\""
    case "\\": t += "\\\\"
    case "\n": t += "\\n"
    case "\r": t += "\\r"
    case "\t": t += "\\t"
    default:
      if ch.value < 0x20 { t += String(format: "\\u%04x", ch.value) } else { t.unicodeScalars.append(ch) }
    }
  }
  return t + "\""
}
