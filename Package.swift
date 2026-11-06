// swift-tools-version:5.10
// Scaffold only (Phase 6.2) — module layout per the master critical path;
// no implementation code yet (Phase 6.3).
import PackageDescription

let package = Package(
    name: "swiftm",
    platforms: [.macOS(.v14)],
    products: [
        .library(name: "SwiftM", targets: ["parser", "IR", "runtime", "storage"]),
        .executable(name: "swiftm", targets: ["runtime"])
    ],
    targets: [
        .target(name: "parser", path: "Sources/parser"),
        .target(name: "IR", path: "Sources/IR", dependencies: ["parser"]),
        .target(name: "runtime", path: "Sources/runtime", dependencies: ["IR", "storage"]),
        .target(name: "storage", path: "Sources/storage"),
        .testTarget(name: "SwiftMTests", dependencies: ["runtime"], path: "Tests")
    ]
)
